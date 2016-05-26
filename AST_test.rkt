#lang racket


(require "AST.rkt" "parser.rkt" "lib.rkt")
(require rackunit)


(define-values (debug-print debug-apply set-print-debug) (make-debug-printer))



(module+ test
  (debug-print "~a" (make-object node%))

  ;;check simple
  (debug-print "~a" (make-object var-node%    'raxor))
  (debug-print "~a" (make-object num-node%    10))
  (debug-print "~a" (make-object label-node%  ':label))
  (debug-print "~a" (make-object pred-label-node% 'number?))
  (debug-print "~a" (make-object biop-op-node% '+))

  (debug-apply check-equal? (send (make-object biop-op-node% '+) get-data) '+)

  ;; check v's
  (debug-print "~a" (make-object make-closure-node% `(make-closure raxor)))
  


  ;(debug-print "~a" (make-object program-node%))
  )