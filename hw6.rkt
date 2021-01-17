;; CSE341, Programming Languages, Homework 6

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

(define (racketlist->mupllist lst)
  (cond [(null? lst) (munit)]
        [#t (apair (car lst) (racketlist->mupllist (cdr lst)))]))

(define (mupllist->racketlist lst)
  (cond [(munit? lst) null]
        [#t (cons (apair-e1 lst) (mupllist->racketlist (apair-e2 lst)))]))

;; lookup a variable in an environment
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; evaluation of MUPL
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(munit? e) e]
        [(closure? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (cond [(> (int-num v1) (int-num v2)) (int 1)]
                     [#t (int 0)])
               (error "MUPL comparison applied to non-number")))]
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (if (int? v1)
               (cond [(equal? 0 (int-num v1)) (eval-under-env (ifnz-e3 e) env)]
                     [#t (eval-under-env (ifnz-e2 e) env)])
               (error "MUPL ifnz applied to non-number")))]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (let ([name (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons name v) env)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([fun (closure-fun v1)]
                      [name (fun-nameopt fun)]
                      [arg (fun-formal fun)]
                      [body (fun-body fun)]
                      [env (closure-env v1)])
                 (cond [(null? name)
                        (eval-under-env body (cons (cons arg v2) env))]
                       [#t (eval-under-env body (cons (cons arg v2) (cons (cons name v1) env)))]))
               (error "MUPL call applied to non-closure in first expression")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([p (eval-under-env (first-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL first applied to non-pair")))]
        [(second? e)
         (let ([p (eval-under-env (second-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL second applied to non-pair")))]
        [(ismunit? e)
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))
        
;; MUPL macros
(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (cond [(null? bs) e2]
        [#t (mlet (caar bs) (cdar bs) (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifnz (add (isgreater (var "_x") (var "_y"))
                    (isgreater (var "_y") (var "_x")))
               e4 e3)))

;; Forming MUPL expressions
(define mupl-filter
  (fun null "f"
       (fun "loop" "mlist"
            (ifmunit (var "mlist")
                     (munit)
                     (ifnz (call (var "f") (first (var "mlist")))
                                 (apair (first (var "mlist"))
                                  (call (var "loop") (second (var "mlist"))))
                           (call (var "loop") (second (var "mlist"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "i"
             (fun null "mlist"
                  (call
                   (call (var "filter")
                         (fun null "x" (isgreater (var "x") (var "i")))) (var "mlist"))))))