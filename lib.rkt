#lang racket

(provide (all-defined-out))

;; symbol|string -> string
(define (to-string token)
  (format "~a" token))


(define (make-debug-printer)
  (define print-debug #t)
  (define (debug-print str . args)
    (when print-debug (apply printf (append (list str) args)) (printf "\n")))
  (define (debug-apply proc . args)
        (when print-debug (printf "given:    ~a\nexpected: ~a\n\n" (first args) (last args)))
        (apply proc args))
  (define (set-print-debug state)
    (set! print-debug state))
  (values debug-print debug-apply set-print-debug))



;; any/c any/c -> lambda->string
(define (make-temp-gen prefix)
  (let ([curr-num -1])
    (lambda (base) (set! curr-num (+ curr-num 1)) (string->symbol (format "~a~a~a" prefix base curr-num)))))




;; helpers for L2/L1 encoding
(define (encode val)
  (if (number? val)
      (+ (arithmetic-shift val 1) 1)
      val))


(define (decode val)
  (if (number? val)
      (arithmetic-shift val -1) 
      val))