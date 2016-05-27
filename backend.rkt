#lang racket


(require "AST.rkt"
         "lib.rkt"
         "parser.rkt")

(provide (all-defined-out))


;; e-node? symbol?/bool? -> string
(define (L3-node->quote an-ast)
  ;(printf "~a\n\n" an-ast)
  (let* ([children (get-children an-ast)]
         [c-data   (when (not (v-node? an-ast))
                     (map L3-node->quote children))])
  (match an-ast
    [(? p-node?)  (let* ([main-e       (L3-node->quote     (get-main-e an-ast))]
                         [real-funcs   (map L3-node->quote (get-children an-ast))])
                    (append `(,main-e) real-funcs))]
    
    [(? f-node?)   (let* ([func-label   (L3-node->quote     (get-label an-ast))]
                          [args         (map L3-node->quote (get-args  an-ast))]
                          [body         (L3-node->quote     (first (get-body  an-ast)))])
                     `(,func-label ,args ,body))]
                            
    [(? let-node?)   (let* ([var-name  (first c-data)]
                            [var-val   (second c-data)]
                            [child-e   (third c-data)])
                       `(let ([,var-name ,var-val]) ,child-e))]
    
    [(? if-node?)   (let* ([c1   (first c-data)]
                           [c2   (second c-data)]
                           [c3   (third c-data)])
                      `(if ,c1 ,c2 ,c3))]
    [(? biop-node?)  (let* ([c1   (first c-data)]
                            [c2   (second c-data)]
                            [c3   (third c-data)])
                       `(,c1 ,c2 ,c3))]
                                
    ;;pred nodes
    [(? a?-node?)       (let* ([c1   (first c-data)])
                          `(a? ,c1))]
    
    [(? number?-node?)  (let* ([c1   (first c-data)])
                         `(number? ,c1))]
    
    [(? func-call-node?)    c-data]
         
    ;; array nodes
    [(? new-array-node?)    (let* ([c1   (first c-data)]
                                   [c2   (second c-data)])
                              `(new-array ,c1 ,c2))]
    
    [(? new-tuple-node?)  (append `(new-tuple) c-data)]
    
    
    [(? aref-node?)        (let* ([c1   (first c-data)]
                                  [c2   (second c-data)])
                             `(aref ,c1 ,c2))]
    
    [(? aset-node?)        (let* ([c1   (first c-data)]
                                  [c2   (second c-data)]
                                  [c3   (third c-data)])
                             `(aset ,c1 ,c2 ,c3))]   ;; put the final result for aset into x (always 0)
    
    [(? alen-node?)        (let* ([c1   (first c-data)])
                             `(alen ,c1))]
    
    
    ;; closure nodes
    [(? make-closure-node?)   (let* ([c1   (first c-data)]
                                     [c2   (second c-data)])
                                `(make-closure ,c1 ,c2))]
    
    [(? closure-proc-node?)    (let* ([c1   (first c-data)])
                                 `(closure-proc ,c1))]
    
    [(? closure-vars-node?)    (let* ([c1   (first c-data)])
                             `(closure-vars ,c1))]
    
    
    [(? print-node?)       (let* ([c1   (first c-data)])
                             `(print ,c1))]
    
    [(? read-node?)         `(read)]

    [(? begin-node?)        (let* ([c1   (first c-data)]
                                  [c2   (second c-data)])
                             `(begin ,c1 ,c2))]
    
    [(or (? label-node?)
         (? num-node?)
         (? var-node?)
         (? biop-op-node?))  (first-child an-ast)]
    ;; signal error
    [else              (error "d-node->L2: Did not recognize ~a")])))