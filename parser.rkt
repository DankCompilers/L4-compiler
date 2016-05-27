#lang racket


(require "AST.rkt" "lib.rkt")
;(provide parse-p parse-e parse-d parse-v parse-v)
(provide (all-defined-out))
;;;;;;;;;;; REGEX DECLARATIONS ;;;;;;;;;;;;;;;;;;

(define label-str "^:[a-zA-Z_][a-zA-Z_0-9]*")
(define var-str "^[a-zA-Z_][a-zA-Z_0-9]*")
(define num-str "\\b(?:0|[1-9][0-9]*)\\b")
(define pred-str "\\b(?:number[?]|a[?])\\b")
(define biop-str "\\B(?:[-+=*<]|<=)\\B")

(define label-regexp   (pregexp label-str))
(define var-regexp     (pregexp var-str))
(define num-regexp     (pregexp num-str))
(define pred-regexp    (pregexp pred-str))
(define biop-regexp    (pregexp biop-str))

;;;;;;;;;;;; TYPE CHECKERS ;;;;;;;;;;;;;;;;;;;;;;

;; p/regexp token -> bool
(define (match-token? reg raw-token)
  (regexp-match? reg (to-string raw-token)))


;; token -> bool
(define (is-pred? raw-token)
  (match-token? pred-regexp raw-token))

;; token -> bool
(define (is-biop? raw-token)
  (match-token? biop-regexp raw-token))

;; token -> bool
(define (is-var? raw-token)
  (match-token? var-regexp raw-token))

;; token -> bool
(define (is-label? raw-token)
  (match-token? label-regexp raw-token))



;;;;;;;;;; PARSE DEFINITIONS ;;;;;;;;;;;;;;;;;;;;

;; quoted -> program-node
(define (parse-p quoted-expr)
  (let ([main-e           (parse-e          (first quoted-expr))]
        [functions        (map parse-f      (rest  quoted-expr))])
  (p-node  functions main-e)))


;; quoted -> func-node
(define (parse-f  quoted-expr)
  (let ([func-label (parse-v     (first quoted-expr))]
        [args       (map parse-v (second quoted-expr))]
        [body       (parse-e     (third quoted-expr))])
    (f-node body func-label args)))


;; string -> e-node/d-node/v-node
(define (parse-e quoted-expr)
  (match quoted-expr
    [`(let ([,var ,e1]) ,e2)        (let-node (parse-es var e1 e2))]
    [`(if ,bool-e ,e1 ,e2)          (if-node  (parse-es bool-e e1 e2))]
    [else                           (parse-d quoted-expr)]))


(define (parse-es . exprs)
  (map parse-e exprs))


;; quote -> d-node/v-node/error
(define (parse-d quoted-expr)
  (match quoted-expr
    [`(,(? is-biop? op) ,e1 ,e2)     (biop-node       (parse-es op e1 e2))]
    [`(number? ,e)                   (number?-node    (parse-es e))]
    [`(a? ,e)                        (a?-node         (parse-es e))]
    [`(new-array ,e1 ,e2)            (new-array-node  (parse-es e1 e2))]
    [`(new-tuple ,es ...)            (new-tuple-node  (apply parse-es es))]
    [`(aref ,e1 ,e2)                 (aref-node  (parse-es e1 e2))]
    [`(aset ,e1 ,e2 ,e3)             (aset-node  (parse-es e1 e2 e3))]
    [`(alen ,e)                      (alen-node  (parse-es e))]
    [`(print ,e)                     (print-node (parse-es e))]
    [`(read)                         (read-node  empty)]
    [`(begin ,e1 ,e2)                (begin-node (parse-es e1 e2))]
    [`(make-closure ,label ,e)       (make-closure-node  (parse-es label e))]
    [`(closure-proc ,e)              (closure-proc-node  (parse-es e))]
    [`(closure-vars ,e)              (closure-vars-node  (parse-es e))]
    [`(,es ...)                      (func-call-node     (apply parse-es es))]
    [else                            (parse-v quoted-expr)]))


;; quote -> v-node/error
(define (parse-v quoted-expr)
  (match quoted-expr
    [(? number?   n)       (num-node       (list quoted-expr))]
    [(? is-label? l)       (label-node     (list quoted-expr))]
    [(? is-biop?  op)      (biop-op-node   (list quoted-expr))]
    [(? is-var?   v)       (var-node       (list quoted-expr))]
    [else                (error (format "parse-v: expression not valid: ~a" quoted-expr))]))


;; quote -> (listof v-node)/error
(define (parse-vs . exprs)
  (map parse-v exprs))