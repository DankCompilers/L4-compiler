#lang racket

(require "AST.rkt"
         "lib.rkt"
         "parser.rkt")
(provide (all-defined-out))


;; setup name generators
(define L4-prefix 'L4)
(define label-prefix ':)
(define temp-gen  (make-temp-gen L4-prefix))

(define var-count -1)

(define (make-valid var-name)
  ;(set! var-count (+ var-count 1))
  (string->symbol (format "~a~a" 'L4_ var-name)))



;; e-node? symbol?/bool? -> string
(define (e-node->L3 e-ast)
  ;; match the three possibilites
  (match e-ast
    [(? let-node?)   (let* ([children    (get-children e-ast)]
                            [var-val-e   (second children)]
                            [child-e     (third children)])
                   (append
                    (d-node->L3 var-val  var-name)
                    (e-node->L3 child-e)))]
    
    [(? if-node?)   (let* ([children (get-children e-ast)]
                       [bool-val  (v-node->L3 (first children))]
                       [true-e  (second children)]
                       [false-e  (third children)]
                       [true-label (true-label-gen)]
                       [false-label (false-label-gen)])
                      )]
    ;; if a d node in an e spot, must be the last one, needs to return as well
     [else  (d-node->L3 e-ast)]))


;; e-node -> e-node
(define (lift og-ast child-num)
  (let*   ([lifted    (nth-child og-ast child-num)])
    (match lifted
      [(? d-node?)    (lift-app  og-ast child-num)]
      [(? if-node?)   (lift-if   og-ast child-num)]
      [(? let-node?)  (lift-let  og-ast child-num)]
      [else         (error "lift: attempted to lift on non d/e-node. " og-ast lifted)])))

;; e-node -> e-node
(define (lift-app og-ast child-num)
  (let*   ([lifted    (nth-child og-ast child-num)]
           [tmp       (parse-v (temp-gen))]
           [new-ast   (replace-child og-ast n tmp)])
    (let-node `(,tmp ,lifted ,new-ast))))

;; e-node -> e-node
(define (lift-if og-ast child-num)
  (let*   ([lifted-if  (nth-child og-ast child-num)]
           [if-cond    (nth-child lifted-if 0)]
           [if-e1      (nth-child lifted-if 1)]
           [if-e2      (nth-child lifted-if 2)]
           [new-ast1   (replace-child og-ast n if-e1)]
           [new-ast2   (replace-child og-ast n if-e2)])
    (if-node `(,if-cond ,new-ast1 ,new-ast2))))


;; e-node -> e-node
(define (lift-let og-ast child-num)
  (let*   ([lifted-let (nth-child og-ast child-num)]
           [var-name   (nth-child lifted-let 0)]
           [var-val    (nth-child lifted-let 1)]
           [let-body   (nth-child lifted-let 2)]
           [new-ast    (replace-child og-ast n let-body)])
    (let-node `(,var-name ,var-val ,new-ast))))



;; node -> boolean?
(define (app-lift? an-ast)
  (or (d-node? an-ast) (e-node? an-ast)))


;; node -> boolean/number
(define (check-lift an-ast)
  (let* ([children      (get-children an-ast)]
         [num-children  (length children)]
         [needs-lift    #f])
    (for ([i  (range num-children)]
          #:break (not (boolean? needs-lift)))
         (when (app-lift? (list-ref children i))
             (set! needs-lift i)))))

;; d-node? symbol? -> string?
(define (d-node->L3 d-ast)
  (let ([children   (get-children d-ast)])
  ;(println d-ast)
  (match d-ast
    [(? biop-node?)       (let (;[op         (first  children)]
                                 [arg1       (second children)]
                                 [arg2       (third  children)])
                             (cond
                               [(e-node? arg1)  empty]
                               [(e-node? arg2)  empty]))]
         
    ;;pred nodes
    [(? a?-node?)          (let ([an-e  (first children)])
                             (cond
                               [(e-node? an-e)   empty]
                               [else               d-ast]))]
         
    [(? number?-node?)     (let ([an-e  (first children)])
                             (cond
                               [(e-node? an-e)   empty]
                               [else               d-ast]))]
    
    [(? func-call-node?)   (let* ([func-label (first children)]
                                  [args       (rest  children)]
                                  [num-args   (length args)])
                             (for ([i (range num-args)])
                                  ()))]
         
    ;; array nodes
    [(? new-array-node?)    (let* ([size-arg    (first  children)]
                                   [val-arg     (second children)])
                             (cond
                               [(e-node? size-arg) empty]
                               [(e-node? val-arg)  empty]))]
    
    [(? new-tuple-node?)   (let* ([size-arg    (first  children)]
                                  [val-args    (rest children)]
                                  [num-args    (length args)])
                             (cond
                               [(e-node? size-arg) empty]
                               [else
                                (for ([i (range num-args)])
                                     ())]))]
    
         
    [(? aref-node?)        (let ([an-array    (first  children)]
                                 [an-index    (second children)])
                             (cond
                               [(e-node? an-array)  empty]
                               [(e-node? an-index)  empty]
                               [else                d-ast]))]
         
    [(? aset-node?)        (let ([an-array    (first c-data)]
                                 [an-index    (decode (second c-data))]
                                 [a-val       (third c-data)])
                             (cond
                               [(e-node? an-array)  empty]
                               [(e-node? an-index)  empty]
                               [(e-node? an-val)    empty]
                               [else                d-ast]))]  
         
    [(? alen-node?)        (let ([an-array (first children)])
                             (cond
                               [(e-node? an-array)  empty]
                               [else                d-ast]))]
    
    
    ;; closure nodes
    [(? make-closure-node?)   (let* ([label   (first  children)]
                                     [val     (second children)])
                             (cond
                               [(e-node? label) empty]
                               [(e-node? val)  empty]))]
    
    [(? closure-proc-node?)    (let ([an-array    (first c-data)]
                                     [an-index    0]
                                     [tmp   (temp-gen 'bcheck)])
                                 ;; check bounds, return val
                                      (append (gen-bounds-check x tmp an-array an-index)
                                              `((,x   <- (mem ,an-array 8)))))]
    
    [(? closure-vars-node?)    (let ([an-array    (first c-data)]
                                     [an-index    1]
                                     [tmp   (temp-gen 'bcheck)])
                                 ;; check bounds, return val
                                 (append (gen-bounds-check x tmp an-array an-index)
                                         `((,x   <- (mem ,an-array 16)))))]
    
    
    [(? print-node?)       `((rdi <- ,(first c-data))
                             (call print 1)
                             (,x <- rax))]
    
    [(? read-node?)         d-ast]
    [(? begin-node?)        empty]
         ;; signal error
         [else                  (v-node-> d-ast)]))]))



;;  v-node -> number?/symbol?
(define (v-node->L3 v-ast)
  (match v-ast
    [(? v-node?) (get-first-child v-ast)]
    [else        (error "v-node->L3: did not match any nodes: " v-ast)]))


;; node? -> quoted
(define (f-node->L3 f-ast)
    (match f-ast
      [(? f-node?)   (let* ([func-label   (v-node->L3     (get-label f-ast))]
                            [args         (map v-node->L3 (get-args  f-ast))]
                            [num-args     (length args)]
                            [body         (e-node->L3     (get-body  f-ast))]
                            [num-stacked  (if (> (length args) 6)
                                              (- (length args) 6)
                                              0)]
                            [stack-offset (* 8 num-stacked)]);;(* 8 (- num-stacked 1))])

                       (append `(,func-label ,num-args 0)
                               (for/list ([i (range num-args)])
                                         (cond
                                           ;; transfers registers to variables
                                           [(< i 6)  `(,(list-ref args i) <-  ,(list-ref arg-regs i))]
                                           
                                           [else     (set! stack-offset (- stack-offset 8))
                                                     `(,(list-ref args i) <- (stack-arg ,stack-offset))]))
                               body))]
      
      [else            (error "f-node->L3: provided node is no f-node")]))

;; node? -> quoted
(define (p-node->L3 p-ast)
    (match p-ast
      ;; p nodes
      [(? p-node?)
       ;; idea - treat the main-e as a func. Make it the main func if it has 0 arguments
            (let* ([main-e       (get-main-e p-ast)]
                   [is-func?     (and (func-call-node? main-e)
                                      (= 1 (length (get-children main-e))))]
                   [main-e-label (if is-func?
                                     (first (get-children main-e))
                                     (parse-v ':L_1))]
                   [main-e-func  (if is-func?
                                     #f
                                     (f-node main-e main-e-label empty))]
                   [real-funcs   (get-children p-ast)]
                   [func-asts    (if is-func?
                                     real-funcs
                                     (cons main-e-func real-funcs))]
                   [funcs        (map f-node->L3 func-asts)]
                   )
              ;(printf "main-e:~a\n" main-e)
              ;(println "process pnode fine")
              (cons (v-node->L3 main-e-label) funcs))]
      
      ;; did not match any valid cases
      [else   (error "p-node->L3: invalid node: ~a" p-ast)]))