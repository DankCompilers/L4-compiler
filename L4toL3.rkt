#lang racket


(require "parser.rkt"
         "AST.rkt"
         "backend.rkt")



(provide L4->L3-compile L4->L3)



;; Consumes an L2 program and returns the related L1 program string
;; string? -> string?
(define (L4->L3-compile quoted-raw-L4)
  (L4->L3 (parse-p quoted-raw-L4)))


(define (L4->L3 p-ast)
  (p-node->L3 p-ast))
  