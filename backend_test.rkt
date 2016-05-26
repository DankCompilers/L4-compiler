#lang racket

(require "backend.rkt" "AST.rkt" "lib.rkt" "parser.rkt")
(require rackunit)


(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))


(module+ test
  (define (test-backend func parse-func quoted expected)
   (debug-apply check-equal? (func (parse-func quoted)) expected))
  
  (define (test-backend-v quoted expected)
    (test-backend v-node->L2 parse-v quoted expected))
  
  (define (test-backend-d quoted expected)
    (debug-apply check-equal? (d-node->L2 (parse-d quoted) 'ret) expected))

  
  (define (test-backend-e quoted expected)
    (test-backend e-node->L2 parse-e quoted expected))

  (define (test-backend-f quoted expected)
    (test-backend f-node->L2 parse-f quoted expected))
  
  (define (test-backend-p quoted expected)
    (test-backend p-node->L2 parse-p quoted expected))


  
  ;; v nodes
  (test-backend-v ':hello  ':hello)
  (test-backend-v 'hello   'hello)
  (test-backend-v 5        11)

  ;; test backend d

  ;; v nodes
  


  ;; test biops

  ;; test pred


  ;; test arrays


    ;; test closures

  


  ;; test prints

  ;; test func calls
                  


  ;; test e-nodes


  ;; test lets


  ;; test ifs


  ;; test backend p
  )
#|

|#