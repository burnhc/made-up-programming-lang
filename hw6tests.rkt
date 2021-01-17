#lang racket

(require "hw6.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 6 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 2, 3, and 5")

   (check-equal? (eval-exp (ifmunit (munit) (int 1) (int 2)))
                  (int 1))

   (check-equal? (eval-exp (ifeq (int 0) (int 1) (int 3) (int 4)))
                 (int 4))

   (check-equal? (eval-exp (ifeq (int 0) (int 0) (int 3) (int 4)))
                  (int 3))

   (check-equal? (racketlist->mupllist '(1 2 3 4))
                 (apair 1 (apair 2 (apair 3 (apair 4 (munit))))))

   (check-equal? (mupllist->racketlist (apair 1 (apair 2 (apair 3 (apair 4 (munit))))))
                  '(1 2 3 4))

   (check-equal? (eval-exp (mlet* (list (cons "x" (int 2)) (cons "y" (int 4))) (add (var "x") (var "y"))))
                  (int 6))
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
