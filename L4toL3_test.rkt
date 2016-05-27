#lang racket
 (require rackunit)


(require "./parser.rkt"
         "./backend.rkt"
         "./lib.rkt"
         "./L4toL3.rkt")



(module+ test
  (define (test-p L4 L3)
    (check-equal? (L3-node->quote (p-node->L3 (parse-p L4))) L3))

#|
  (test-p `((print (alen (new-array 2 3)))) (let ([tmp1 (make-test-temp)]
                                                [tmp2 (make-test-temp)])
                                              `((let ([,tmp2 (new-array 2 3)])
                                                 (let ([,tmp1 (alen ,tmp2)])
                                                   (print ,tmp1))))))|#

  (test-p `((print (:recur 5))
            (:recur (n)
                    (new-tuple n
                               (if n 
                                   (:recur (- n 1))
                                   0))))        `((let ([a (:recur 5)])
                                                    (print a))
                                                  (:recur (n)
                                                          (if n
                                                              (let ([x_1 (- n 1)])
                                                                (let ([x_2 (:recur x_1)])
                                                                  (new-tuple n x_2)))
                                                              (new-array 2 0)))))
)


#|
(module+ test
 
  (define (test-v L4 L3)
    (check-equal? (v-node->L3  (parse-v L4)) (parse-e L3)))
  
  (test-v  `:hello `:hello)
  (test-v  `hello  `hello)
  (test-v  20      20))
|#


(module+ test
  (define (test-d L4 L3)
    (check-equal? (L3-node->quote (d-node->L3 (parse-e L4))) L3))
#|
  ;; test v nodes
  (test-d  `:hello `:hello)
  (test-d  `hello  `hello)
  (test-d  20      20)

  ;; test L3 d nodes
  (test-d `(new-tuple x1 x2) `(new-tuple x1 x2))
  (test-d `(new-array x1 10) `(new-array x1 10))
  (test-d `(aref arr 5)      `(aref arr 5))
  (test-d `(+ avar 1)        `(+ avar 1))
  (test-d `(number? 5)       `(number? 5))
  (test-d `(a? 5)            `(a? 5))

  ;; test lift-app

  ;; biops
  (test-d `(+ (+ 1 a) 5) (let ([tmp (make-test-temp)])
                         `(let ([,tmp (+ 1 a)])
                            (+ ,tmp 5))))

  (printf "\n\n")
                                  
  (test-d `(+ (+ 1 a) (- 1 b)) (let ([tmp1 (make-test-temp)]
                                     [tmp2 (make-test-temp)])
                               `(let ([,tmp1 (+ 1 a)])
                                  (let ([,tmp2 (- 1 b)])
                                    (+ ,tmp1 ,tmp2)))))

  (printf "\n\n")
  (test-d `(print (alen (new-array 2 3))) (let ([tmp1 (make-test-temp)]
                                                [tmp2 (make-test-temp)])
                                              `(let ([,tmp2 (new-array 2 3)])
                                                 (let ([,tmp1 (alen ,tmp2)])
                                                   (print ,tmp1)))))
  (printf "\n\n")

  (test-d `(print (aref (new-array 2 3) 0)) (let ([tmp1 (make-test-temp)]
                                                  [tmp2 (make-test-temp)])
                                              `(let ([,tmp2 (new-array 2 3)])
                                                 (let ([,tmp1 (aref ,tmp2 0)])
                                                   (print ,tmp1)))))
  (printf "\n\n")
|#
  ;; test lift-if




  ;; test lift-let




  )