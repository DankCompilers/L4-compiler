#lang racket

(require "./backend.rkt"
         "./AST.rkt"
         "./lib.rkt"
         "./parser.rkt")

(require rackunit)

(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))

(module+ test
  (define (test-backend-p quoted)
   (check-equal? (L3-node->quote (parse-p quoted)) quoted))
  
  (define (test-backend-e quoted)
   (check-equal? (L3-node->quote (parse-e quoted)) quoted))

  ;; v nodes
  (test-backend-e ':hello)
  (test-backend-e 'hello)
  (test-backend-e 5)

  ;; test backend d

  ;; test biops
  (test-backend-e `(+ 1 3))
  (test-backend-e `(- v1 3))
  (test-backend-e `(* v1 v2))
  (test-backend-e `(= 1 3))
  (test-backend-e `(<= 1 3))

  ;; test pred
  (test-backend-e `(number? 1))
  (test-backend-e `(number? var))
  (test-backend-e `(a? 1))
  (test-backend-e `(a? v))

  ;; test arrays
  (test-backend-e `(new-array 10 5))
  (test-backend-e `(new-tuple 10 5 avar))
  (test-backend-e `(aref arr 5))
  (test-backend-e `(aref arr i))
  (test-backend-e `(alen arr))
  (test-backend-e `(aset arr i 10))
  (test-backend-e `(aset arr 0 arr))
  
  ;; test closures
  (test-backend-e `(make-closure :hi 5))
  (test-backend-e `(make-closure :hi avar))
  (test-backend-e `(closure-proc thing))
  (test-backend-e `(closure-vars thing))

  ;; test prints
  (test-backend-e `(read))
  (test-backend-e `(print 200))
  (test-backend-e `(print arr))

  ;; test func calls
  (test-backend-e `(f1 1 3 5))
  (test-backend-e `(g a var))

  ;; test e-nodes
  ;; test lets
  (test-backend-e `(let  ([hi 5])
                     (+ hi 1)))

  (test-backend-e `(let  ([hi :label])
                     (+ hi 1)))

  (test-backend-e `(let  ([hi (print 1)])
                     (+ hi 1)))

  ;; test ifs

  (test-backend-e `(if 1
                       (+ hi 1)
                       hi))


  (test-backend-e `(let ([cond (+ -1 2)])
                         (if cond
                           (if 0
                               (+ avar 1) (- avar 1))
                           (if 1
                               (+ avar 1) (- avar 1)))))

  (test-backend-e `(let ([cond (+ -1 2)])
                     (if cond
                           (if 0
                               (let ([total1 (+ avar 1)])
                                 (- total1 1))
                               (let ([total2 (+ avar 1)])
                                 (- total2 1)))
                           (if 1
                               (+ avar 1) (- avar 1)))))

  ;; test backend p

  
  )
