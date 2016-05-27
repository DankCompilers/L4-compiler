#lang racket


(require "./AST.rkt"
         "./parser.rkt"
         "./backend.rkt"
         "./lib.rkt")

(provide (all-defined-out))
                      


;; Consumes an L4 program and returns the related L3 program string
;; string? -> string?
(define (L4->L3-compile quoted-raw-L4)
  (L4->L3 (parse-p quoted-raw-L4)))

(define (L4->L3 p-ast)
  (p-node->L3 p-ast))


;; node? -> quoted
(define (p-node->L3 p-ast)
    (match p-ast
      ;; p nodes
      [(? p-node?)
       ;; idea - treat the main-e as a func. Make it the main func if it has 0 arguments
            (let* ([main-e       (e-node->L3 (get-main-e p-ast))]
                   [funcs        (get-children p-ast)]
                   [new-funcs    (map f-node->L3 funcs)]
                   )
              ;(printf "funcs:\n")
                     ; (print funcs)
              (p-node new-funcs main-e))]
      ;; did not match any valid cases
      [else   (error "p-node->L3: invalid node: ~a" p-ast)]))



;; node? -> quoted
(define (f-node->L3 f-ast)
 ; (printf "converting f: ~a\n" (L3-node->quote f-ast))
  (match f-ast
    [(? f-node?)   (let* ([func-label   (v-node->L3     (get-label f-ast))]
                          [args         (map v-node->L3 (get-args  f-ast))]
                          [body         (e-node->L3     (first (get-body  f-ast)))])
                    ; (printf "end converting f***\n")
                     (f-node `(,body) func-label args))]
    [else            (error "f-node->L3: provided node is no f-node")]))

;; e-node? symbol?/bool? -> string
(define (e-node->L3 e-ast)
 ; (printf "converting e: ~a\n" (L3-node->quote e-ast))
  (match e-ast
    ;; lift if var val is an e-node
    [(? let-node?)   (let* ([var-name    (nth-child e-ast 0)]
                            [var-val     (e-node->L3 (nth-child e-ast 1))]
                            [new-e-ast   (replace-child e-ast 1 var-val)]
                            [child-e     (nth-child e-ast 2)]
                            [lift?       (e-node?   var-val)])
                      ; (printf "lift?: ~a\n" lift?)
                       (cond
                         [lift?    (e-node->L3 (lift new-e-ast 1))]
                         [else     (let-node `(,var-name ,var-val ,(e-node->L3 child-e)))]))]

    ;; lift if bool val is not v-node
    [(? if-node?)   (let* ([bool-val    (nth-child e-ast 0)]
                           [true-e      (nth-child e-ast 1)]
                           [false-e     (nth-child e-ast 2)]
                           [lift?       (app-lift? bool-val)])
                      (cond
                         [lift?    (e-node->L3 (lift e-ast 0))]
                         [else     (if-node `(,bool-val ,(e-node->L3 true-e) ,(e-node->L3 false-e)))]))]
    ;; d in an e spot
    [else  (d-node->L3 e-ast)]))

;;;;; V NODES ;;;;;;


;; d-node? symbol? -> string?
(define (d-node->L3 d-ast)
  ;(printf "converting d: ~a\n" (L3-node->quote d-ast))
  (match d-ast
    [(? begin-node?)  (let  ([x    (parse-v(make-unique 'begin))]
                             [e1   (nth-child d-ast 0)]
                             [e2   (nth-child d-ast 1)])
                        (e-node->L3 (let-node `(,x ,e1 ,e2))))]
    [(? d-node?)      (let* ([lift?   (d-check-lift d-ast)])
                        ;(printf "lift?: ~a\n" lift?)
                        (if (number? lift?)
                            (e-node->L3 (lift d-ast lift?))
                            d-ast))]
    [else          (v-node->L3 d-ast)]))


;; node -> boolean/number
(define (d-check-lift an-ast)
  (let* ([children      (get-children an-ast)]
         [num-children  (length children)]
         [needs-lift    #f])
    (for ([i  (range num-children)]
          #:break (not (boolean? needs-lift)))
         (when (app-lift? (list-ref children i))
           (set! needs-lift i)))
    needs-lift))


;;;;; V NODES ;;;;;;

;;  v-node -> number?/symbol?
(define (v-node->L3 v-ast)
  (match v-ast
    [(? v-node?) v-ast]
    [else        (error "v-node->L3: did not match any nodes: " v-ast)]))


;;;;;;;   LIFT FUNCTIONS   ;;;;;;;;;


;; e-node -> e-node
(define (lift og-ast child-num)
  (let*   ([lifted    (nth-child og-ast child-num)])
    (match lifted
      [(? d-node?)    (lift-app  og-ast child-num)]
      [(? if-node?)   (lift-if   og-ast child-num)]
      [(? let-node?)  (lift-let  og-ast child-num)]
      [else           (error "lift: attempted to lift on non d/e-node. " og-ast lifted)])))

;; e-node number? -> e-node
(define (lift-app og-ast n)
  ;(printf "lift app: ~a\n" (L3-node->quote og-ast))
  (let*   ([lifted    (nth-child og-ast n)]
           [tmp       (parse-v (make-temp))]
           [new-ast   (replace-child og-ast n tmp)])
    (let-node `(,tmp ,lifted ,new-ast))))

;(define (make-copy-e a-node)
;  (apply (eval (object-name a-node)) (list (get-children a-node))))

;; e-node -> e-node
(define (lift-if og-ast n)
  (let*   ([lifted-if  (nth-child og-ast n)]
           [if-cond    (nth-child lifted-if 0)]
           [if-e1      (nth-child lifted-if 1)]
           [if-e2      (nth-child lifted-if 2)]
           [new-ast1   (replace-child (make-copy og-ast) n if-e1)]
           [new-ast2   (replace-child (make-copy og-ast) n if-e2)])
    #|
    (printf "\n\nif: ~a\n" lifted-if)
    (printf "if-cond: ~a\n" if-cond)
    (printf "if-e1: ~a\n" if-e1)
    (printf "if-e2: ~a\n" if-e2)
    (printf "new-ast1: ~a\n" new-ast1)
    (printf "new-ast2: ~a\n\n" new-ast2)
|#
    (if-node `(,if-cond ,new-ast1 ,new-ast2))))


;; e-node -> e-node
(define (lift-let og-ast n)
 ; (printf "lift let: ~a\n" (L3-node->quote og-ast))
  (let*   ([lifted-let (nth-child og-ast n)]
           [var-name   (nth-child lifted-let 0)]
           [var-val    (nth-child lifted-let 1)]
           [let-body   (nth-child lifted-let 2)]
           [new-ast    (replace-child og-ast n let-body)])
    (let-node `(,var-name ,var-val ,new-ast))))



;; node -> boolean?
(define (app-lift? an-ast)
  (or (d-node? an-ast) (e-node? an-ast)))



;;;;;;;;;;; HELPERS   ;;;;;;;;;;;;;;;;;;;;;;;


;; setup name generators
(define L4-prefix 'L4)

;; temps
(define (new-temp-gen prefix)
  (let ([curr-num -1])
    (lambda (base)
      (set! curr-num (+ curr-num 1))
      (string->symbol (format "~a~a~a" prefix base curr-num)))))
(define temp-gen  (make-temp-gen L4-prefix))
(define (make-temp)
  (temp-gen 'temp))


;; test temps
(define (new-test-temp-gen prefix)
  (let ([curr-num -1])
    (lambda (base)
      (set! curr-num (+ curr-num 1))
      (string->symbol (format "~a~a~a" prefix base curr-num)))))
(define test-temp-gen  (new-test-temp-gen L4-prefix))
(define (make-test-temp)
  (test-temp-gen 'temp))


(define var-count -1)
(define (make-unique var-name)
  ;(set! var-count (+ var-count 1))
  (string->symbol (format "~a~a" 'L4_ var-name)))


#|
(L3-node->quote (L4->L3-compile `((print (:recur 5))
 (:recur (n)
 (new-tuple n
  (if n 
   (:recur (- n 1))
   0))))))

|#
