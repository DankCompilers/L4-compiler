#lang racket


(provide (all-defined-out))


;;;;;;;;;;;;; BASE TYPES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; base struct for all AST nodes
;; (listof node/c) -> node/c
(struct node (children)
  #:transparent
  #:mutable)

(define (get-children a-node)
  ;(-> node? (listof node?))
  (node-children a-node))

(define (nth-child a-node n)
 ; (-> node? number? any/c)
  (list-ref (get-children a-node) n))

(define (first-child a-node)
;  (-> node? any/c)
  (nth-child a-node 0))

(define (second-child a-node)
 ; (-> node? any/c)
  (nth-child a-node 1))

(define (third-child a-node)
 ; (-> node? any/c)
  (nth-child a-node 2))



;; e node
(struct e-node node ()
  #:transparent
  #:mutable)

;; d node

(struct d-node node ()
  #:transparent
  #:mutable)


;; v node

(struct v-node node ()
  #:transparent
  #:mutable)


;;;;;;;;; P-type Nodes ;;;;;;;;;;;;;;;;

;; program-node
(struct p-node node (main-e)
  #:transparent
  #:mutable)

(define (get-main-e a-p-node)
  (p-node-main-e a-p-node))



;; function-node
(struct f-node node (label args)
  #:transparent
  #:mutable)

(define (get-label a-f-node)
  ;(-> f-node? v-node?)
  (f-node-label a-f-node))


(define (get-args a-f-node)
  ;(-> f-node? (listof v-node?))
  (f-node-args a-f-node))

(define (get-body a-f-node)
  ;(-> f-node? (listof e-node?))
  (get-children a-f-node))




;;;;;;;;; E-type Nodes ;;;;;;;;;;;;;;;;
;; if node

(struct if-node e-node ()
  #:transparent
  #:mutable)

(struct let-node e-node ()
  #:transparent
  #:mutable)


;;;;;;;;;;;;;;; D type Nodes ;;;;;;;;;;;;;;;;


;; Biop node for D types
(struct biop-node d-node ()
  #:transparent
  #:mutable)


;; pred node for D types
(struct pred-node d-node ()
  #:transparent
  #:mutable)

;; number? node
(struct number?-node pred-node ()
  #:transparent
  #:mutable)

;; a? node
(struct a?-node pred-node ()
  #:transparent
  #:mutable)

;; function call
(struct func-call-node d-node ()
  #:transparent
  #:mutable)


;; new array
(struct new-array-node d-node ()
  #:transparent
  #:mutable)

;; new tuple
(struct new-tuple-node d-node ()
  #:transparent
  #:mutable)


;; aref
(struct aref-node d-node ()
  #:transparent
  #:mutable)



;; aset
(struct aset-node d-node ()
  #:transparent
  #:mutable)

;; alen
(struct alen-node d-node ()
  #:transparent
  #:mutable)

;; print
(struct print-node d-node ()
  #:transparent
  #:mutable)


;; print
(struct read-node d-node ()
  #:transparent
  #:mutable)



;;;;;;; Closure D's

;; make-closure
(struct make-closure-node d-node ()
  #:transparent
  #:mutable)



;; closure-proc
(struct closure-proc-node d-node ()
  #:transparent
  #:mutable)


;; closure-vars
(struct closure-vars-node d-node ()
  #:transparent
  #:mutable)



;;;;;;;;;;;; V Nodes ;;;;;;;;;;;;;;;;;;;;;;
(struct var-node v-node ()
  #:transparent
  #:mutable)

(struct label-node v-node ()
  #:transparent
  #:mutable)

;; num node
(struct num-node v-node ()
  #:transparent
  #:mutable)


;; bip op node
(struct biop-op-node v-node ()
  #:transparent
  #:mutable)