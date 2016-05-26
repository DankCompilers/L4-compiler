#lang racket

(require "AST.rkt" "lib.rkt" "parser.rkt")
(require rackunit)


(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))


(module+ test
  (define (test-parse func quoted expected)
   (debug-apply check-equal? (func quoted) expected))

  ;; v node tests
  (test-parse parse-v ':label      (label-node '(:label)))
  (test-parse parse-v 10           (num-node   '(10)))
  (test-parse parse-v 'avar        (var-node   '(avar)))
  ;; biop op node tests
  (test-parse parse-v '+           (biop-op-node    (list '+)))
  (test-parse parse-v '-           (biop-op-node    (list '-)))
  (test-parse parse-v '*           (biop-op-node    (list '*)))
  (test-parse parse-v '<           (biop-op-node    (list '<)))
  (test-parse parse-v '=           (biop-op-node    (list '=)))
  (test-parse parse-v '<=          (biop-op-node    (list '<=)))

  ;; d node tests

  ;; biop tests
  (test-parse parse-d `(+ 1 3)    (biop-node     (parse-vs '+ 1 3)))
  (test-parse parse-d `(= 1 3)    (biop-node     (parse-vs '= 1 3)))
  (test-parse parse-d `(- 1 3)    (biop-node     (parse-vs '- 1 3)))
  (test-parse parse-d `(* 1 3)    (biop-node     (parse-vs '* 1 3)))

  ;;pred? tests
  (test-parse parse-d `(a? raxor)         (a?-node    (parse-vs 'raxor)))
  (test-parse parse-d `(number? raxor)    (number?-node    (parse-vs 'raxor)))
  (test-parse parse-d `(number? 10)       (number?-node    (parse-vs 10)))

  ;;array tests
  (test-parse parse-d `(new-array 10 25)           (new-array-node (parse-vs 10 25)))
  (test-parse parse-d `(new-array avar bvar)       (new-array-node (parse-vs 'avar 'bvar)))
  (test-parse parse-d `(new-array 10 bvar)         (new-array-node (parse-vs 10 'bvar)))
  
  (test-parse parse-d `(new-tuple 10 25 rac avar)  (new-tuple-node (parse-vs 10 25 'rac 'avar)))
  (test-parse parse-d `(new-tuple avar)            (new-tuple-node (parse-vs 'avar)))

  (test-parse parse-d `(aref avar 10)              (aref-node (parse-vs 'avar 10)))
  (test-parse parse-d `(aref avar bvar)            (aref-node (parse-vs 'avar 'bvar)))

  (test-parse parse-d `(aset aref index val)       (aset-node (parse-vs 'aref 'index 'val)))
  (test-parse parse-d `(aset aref 10    11)        (aset-node (parse-vs 'aref   10 11)))
  (test-parse parse-d `(alen arr)                  (alen-node (parse-vs 'arr)))

  ;; print
  (test-parse parse-d `(print arr)                 (print-node (parse-vs 'arr)))
  (test-parse parse-d `(print 10)                  (print-node (parse-vs 10)))

  ;; closure tests
  (test-parse parse-d `(make-closure :l avar)      (make-closure-node (parse-vs ':l 'avar)))
  (test-parse parse-d `(make-closure :hello 10)    (make-closure-node (parse-vs ':hello 10)))

  (test-parse parse-d `(closure-proc arr)          (closure-proc-node (parse-vs 'arr)))
  (test-parse parse-d `(closure-vars arr)          (closure-vars-node (parse-vs 'arr)))

  ;; func calls
  (test-parse parse-d `(a-func arg1 arg2)          (func-call-node    (parse-vs 'a-func 'arg1 'arg2)))
  (test-parse parse-d `(a-func)                    (func-call-node    (parse-vs 'a-func)))
  (test-parse parse-d `(anything 10 324 avar bvar) (func-call-node    (parse-vs 'anything 10 324 'avar 'bvar)))
  

  ;; v node test parse-d
  (test-parse parse-d ':label      (label-node '(:label)))
  (test-parse parse-d 10           (num-node   '(10)))
  (test-parse parse-d 'avar        (var-node   '(avar)))
  



  ;;;;; e node tests

  ;; d node tests
  (test-parse parse-e `(+ 1 3)    (biop-node     (parse-vs '+ 1 3)))
  (test-parse parse-e `(= 1 3)    (biop-node     (parse-vs '= 1 3)))
  (test-parse parse-e `(- 1 3)    (biop-node     (parse-vs '- 1 3)))
  (test-parse parse-e `(* 1 3)    (biop-node     (parse-vs '* 1 3)))

  ;;pred? tests
  (test-parse parse-e `(a? raxor)         (a?-node    (parse-vs 'raxor)))
  (test-parse parse-e `(number? raxor)    (number?-node    (parse-vs 'raxor)))
  (test-parse parse-e `(number? 10)       (number?-node    (parse-vs 10)))

  ;;array tests
  (test-parse parse-e `(new-array 10 25)           (new-array-node (parse-vs 10 25)))
  (test-parse parse-e `(new-array avar bvar)       (new-array-node (parse-vs 'avar 'bvar)))
  (test-parse parse-e `(new-array 10 bvar)         (new-array-node (parse-vs 10 'bvar)))
  
  (test-parse parse-e `(new-tuple 10 25 rac avar)  (new-tuple-node (parse-vs 10 25 'rac 'avar)))
  (test-parse parse-e `(new-tuple avar)            (new-tuple-node (parse-vs 'avar)))

  (test-parse parse-e `(aref avar 10)              (aref-node (parse-vs 'avar 10)))
  (test-parse parse-e `(aref avar bvar)            (aref-node (parse-vs 'avar 'bvar)))

  (test-parse parse-e `(aset aref index val)       (aset-node (parse-vs 'aref 'index 'val)))
  (test-parse parse-e `(aset aref 10    11)        (aset-node (parse-vs 'aref   10 11)))
  (test-parse parse-e `(alen arr)                  (alen-node (parse-vs 'arr)))

  ;; print
  (test-parse parse-e `(print arr)                 (print-node (parse-vs 'arr)))
  (test-parse parse-e `(print 10)                  (print-node (parse-vs 10)))

  ;; closure tests
  (test-parse parse-e `(make-closure :l avar)      (make-closure-node (parse-vs ':l 'avar)))
  (test-parse parse-e `(make-closure :hello 10)    (make-closure-node (parse-vs ':hello 10)))

  (test-parse parse-e `(closure-proc arr)          (closure-proc-node (parse-vs 'arr)))
  (test-parse parse-e `(closure-vars arr)          (closure-vars-node (parse-vs 'arr)))

  ;; func calls
  (test-parse parse-e `(a-func arg1 arg2)          (func-call-node    (parse-vs 'a-func 'arg1 'arg2)))
  (test-parse parse-e `(a-func)                    (func-call-node    (parse-vs 'a-func)))
  (test-parse parse-e `(anything 10 324 avar bvar) (func-call-node    (parse-vs 'anything 10 324 'avar 'bvar)))
  

  ;; v node test parse-e
  (test-parse parse-e ':label      (label-node '(:label)))
  (test-parse parse-e 10           (num-node   '(10)))
  (test-parse parse-e 'avar        (var-node   '(avar)))

  ;; test let's
  (test-parse parse-e `(let ([avar 10]) (+ avar 20))                (let-node  (list (parse-v 'avar) (parse-d 10) (parse-e `(+ avar 20)))))
  (test-parse parse-e `(let ([avar (aref arr 10)]) (+ avar 20))     (let-node  (list (parse-v 'avar) (parse-d `(aref arr 10)) (parse-e `(+ avar 20)))))
  (test-parse parse-e `(let ([avar (aref arr 10)]) (let ([add-20 (+ avar 20)]) (- add-20 20)))       (let-node  (list (parse-v 'avar) (parse-d `(aref arr 10))
                                                                                                                      (parse-e `(let ([add-20 (+ avar 20)]) (- add-20 20))))))

  ;; test if's
  (test-parse parse-e `(if 1 (+ avar 1) (- avar 1))         (if-node (list (parse-v 1) (parse-e `(+ avar 1)) (parse-e `(- avar 1)))))
  (test-parse parse-e `(if 1
                           (if 0
                               (+ avar 1) (- avar 1))
                           (if 1
                               (+ avar 1) (- avar 1)))       (if-node (list (parse-v 1) (parse-e `(if 0 (+ avar 1) (- avar 1))) (parse-e `(if 1 (+ avar 1) (- avar 1))))))

  (test-parse parse-e `(let ([cond (+ -1 2)])
                         (if cond
                           (if 0
                               (+ avar 1) (- avar 1))
                           (if 1
                               (+ avar 1) (- avar 1))))       (let-node (list (parse-v 'cond) (parse-d `(+ -1 2))
                                                                              (if-node (list (parse-v 'cond)
                                                                                             (parse-e `(if 0 (+ avar 1) (- avar 1)))
                                                                                             (parse-e `(if 1 (+ avar 1) (- avar 1))))))))

  
  (test-parse parse-e `(let ([cond (+ -1 2)])
                         (if cond
                           (if 0
                               (let ([total1 (+ avar 1)])
                                 (- total1 1))
                               (let ([total2 (+ avar 1)])
                                 (- total2 1)))
                           (if 1
                               (+ avar 1) (- avar 1))))       (let-node (list (parse-v 'cond) (parse-d `(+ -1 2))
                                                                              (if-node (list (parse-v 'cond)
                                                                                             (parse-e `(if 0
                                                                                                           (let ([total1 (+ avar 1)]) (- total1 1))
                                                                                                           (let ([total2 (+ avar 1)]) (- total2 1))))
                                                                                             (parse-e `(if 1 (+ avar 1) (- avar 1))))))))


  ;; f node
  (test-parse parse-f `(:a-func (arg1 arg2 arg3) (let ([sum1 (+ arg1 arg2)]) (+ sum1 arg3)))  (f-node (parse-e `(let ([sum1 (+ arg1 arg2)]) (+ sum1 arg3)))
                                                                                                      (parse-v ':a-func ) (parse-vs 'arg1 'arg2 'arg3)))

  )