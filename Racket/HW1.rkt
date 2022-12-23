#lang slideshow

;Danila Korneenko B20-SD-02


;Exercise 1.1

; Check whether a given expression is a variable
(define (variable? expr)
  (and ( < (length (flatten expr)) 2) (not (number? expr)) (andmap (lambda (x) ( not (equal? (first (flatten expr)) x))) '(+ - * / ^ log sin cos tan)))
)


; Check whether a given expression is a sum
(define (sum? expr)
  (match expr
    [(list '+ _ _) #t] ; "_" from docs.rocket - it indicates a pattern that matches any syntax object
    [dum #f]
  )
)
;(sum? '(- 1 x) ) ;False
;(sum? '(+ 1 x) ) ;True


; Extract first summand from a sum
(define (summand-1 expr)
  (match expr
    [(list '+ x _) x]
  )
)


; Extract second summand from a sum
(define (summand-2 expr)
  (match expr
    [(list '+ _ y) y]
  )
)


; Check whether a given expression is a product
(define (product? expr)
  (match expr
    [(list '* _ _) #t]
    [dum #f]
  )
)


; Extract first multiplier from a product
(define (multiplier-1 expr)
  (match expr
    [(list '* x _) x]
  )
)


; Extract second multipler from a product
(define (multiplier-2 expr)
  (match expr
    [(list '* _ y) y]
  )
)


;
;
; Help functions for 1.6 and 1.7

; Exercise 1.6
; Check whether a given expression is a exponentiation
(define (exp? expr)
  (match expr
    [(list '^ _ _) #t]
    [_ #f]
  )
)

; Extract a base from a exponentiation
(define (exp-base expr)
  (match expr
    [(list '^ x _) x]
  )
)

; Extract a exponent from a exponentiation
(define (exp-power expr)
  (match expr
    [(list '^ _ x) x]
  )
)

; Check whether a given expression is a sin
(define (sin? expr)
  (match expr
    [(list 'sin _) #t]
    [_ #f]))

; Extract an argument from a sin
(define (sin-x expr)
  (match expr
    [(list 'sin x) x]
  )
)

; Check whether a given expression is a cos
(define (cos? expr)
  (match expr
    [(list 'cos _) #t]
    [_ #f]
  )
)

; Extract an argument from a cos
(define (cos-x expr)
  (match expr
    [(list cos x) x]
  )
)

; Check whether a given expression is a tan
(define (tan? expr)
  (match expr
    [(list 'tan _) #t]
    [_ #f]
  )
)

; Extract an argument from a tan
(define (tan-x expr)
  (match expr
    [(list tan x) x]
  )
)

; Check whether a given expression is a natural logarithm
(define (log? expr)
  (match expr
    [(list 'log _) #t]
    [_ #f]
  )
)

; Extract an argument from a natural logarithm
(define (log-x expr)
  (match expr
    [(list log x) x]
  )
)


; Exercise 1.7
; Check whether a given expression is a polyvariadic sum
(define (poly-sum? expr)
  (if (and (list? expr) (> (length expr) 3) (equal? (first expr) '+))
      #t
      #f
   )
)

; Check whether a given expression is a polyvariadic product
(define (poly-prod? expr)
  (if (and (list? expr) (> (length expr) 3) (equal? (first expr) '*))
      #t
      #f
   )
)

;
;
;


;Exercise 1.2
(define (derivative expr v)
  (cond
    ;1.2
    ;Sum case
    [(sum? expr)
        (list '+ (derivative (summand-1 expr) v) (derivative (summand-2 expr) v))
    ]
    ;Product case
    [(product? expr)
     (let ([x (multiplier-1 expr)] [y (multiplier-2 expr)])
        (list '+ (list '*(derivative x v) y) (list '* x(derivative y v)))
      )
    ]
    ; Const case
    [(number? expr) 0]
    ; Variable case
    [(variable? expr) (if (equal? expr v) 1 0)]

    
    ; Exercise 1.6 + 1.7
    ; Exponentiation case
    [(exp? expr)
     ; u - base v - power
     ;(v'lnu + v/u*u')*u^v
     (list '*
           (list '+
                 (list '*(derivative (exp-power expr) v) (list 'log (exp-base expr)                ))
                 (list '* (derivative (exp-base expr) v) (list '/ (exp-power expr) (exp-base expr) ))
           )
           (list '^ (exp-base expr) (exp-power expr))
     )
    ]
    
    ; Sin case
    [(sin? expr)
      (list '* (derivative (sin-x expr) v) (list 'cos (sin-x expr)) )
    ]
    
    ; Cos case
    [(cos? expr)
      (list '* -1 (list '* (derivative (cos-x expr) v) (list 'sin (cos-x expr)) )  )
    ]
    
    ; find a derivative for a tan
    [(tan? expr)
       (list '/ (derivative (tan-x expr) v) (list '^ (list 'cos (tan-x expr)) 2) )
    ]
    
    ; find a derivative for a natural logarithm
    [(log? expr)
       (list '/ (derivative (log-x expr) v) (log-x expr))
    ]

    
    ;1.7
    ; Poly-sum case
    [(poly-sum? expr)
       (cons '+ (map (lambda (x) (derivative x v)) (rest expr) )  )
    ]

    ; Poly-product case
    [(poly-prod? expr)
       (cons '+ (map
                 (lambda (x)(cons '* (cons (derivative x v) (remove x (rest expr)))) )   (rest expr))
       )
    ]
  )
)

(derivative '(+ 1 x) 'x)                        ; '(+ 0 1)

(derivative '(* 2 y) 'y)                        ; '(+ (* 0 y) (* 2 1))

(derivative '(* (+ x y) (+ x (+ x x))) 'x)      ; '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))

"--------"

; Exercise 1.3
(define (simplify expr)
  ; Simplify easy expressions
  (define (simplify-at-root expr)
    (match expr
      ; It's a baaaase
      [(list '+ 0 e) e]
      [(list '+ e 0) e]
      [(list '* 1 e) e]
      [(list '* e 1) e]
      [(list '* 0 e) 0]
      [(list '* e 0) 0]
      [(list 'sin 0) 0]
      [(list 'cos 0) 1]
      [(list 'tan 0) 0]
      [(list 'log 'e) 1]
      [(list '^ e 1) e]
      [(list '^ 1 e) 1]
      
      ; Sum constants
      [(list '+ x y)
       #:when (and (number? x) (number? y)) (+ x y)]
      
      ; Product constants
      [(list '* x y)
       #:when (and (number? x) (number? y)) (* x y)]

      ; Exercise 1.6 + 1.7
      ; Complex func case
      [(list 'sin x)
       #:when (number? x) (sin x)]
      [(list 'cos x)
       #:when (number? x) (cos x)]
      [(list 'tan x)
       #:when (number? x) (tan x)]
      [(list 'log x)
       #:when (number? x) (log x)]
      [(list '^ x y)
       #:when (and (number? x) (number? y)) (expt x y)]

      ; Poly case
      [_ (cond
           
        ;Poly sum
        [(poly-sum? expr)
          ; Sum all numbers and place the result on the first place
          (let ([simple-expr
                 (cons '+ (cons(apply + (filter (lambda (x) (number? x)) (rest expr)))
                          (filter (lambda (x) (not (number? x))) (rest expr))
                          )
                 )
               ])
            (cond
              ; Simple sum - simplify two arguments
              [(=       (length simple-expr) 3)    (simplify-at-root simple-expr)]
              ; Remove 0 in expr
              [(equal?  (second simple-expr) 0)    (remove 0 simple-expr)        ]
              [else simple-expr]
            )
          )
        ]

        ;Poly product
        [(poly-prod? expr)
          ; Multiply all numbers and place the result on the first place
          (let ([simple-expr
                 (cons '* (cons(apply * (filter (lambda (x) (number? x)) (rest expr)))
                          (filter (lambda (x) (not (number? x))) (rest expr))
                          )
                 )
               ])
            
            (cond
              ; Simple product - simplify two arguments
              [(=      (length simple-expr) 3)     (simplify-at-root simple-expr)]
              ; If one of arg is zero - product is zero
              [(equal? (second simple-expr) 0)                                  0]
              ; Remove 1 in expr
              [(equal? (second simple-expr) 1)     (remove 1 simple-expr)        ]
              [else simple-expr]))]

        ;If constant/variable -  return constant
         [else expr]
         )
       ]      
     )
   )
  ; Simplify complex expressions
  (match expr
    ; Simple operations
    [(list '+ x y) (simplify-at-root (list '+ (simplify x) (simplify y)   ))]
    [(list '* x y) (simplify-at-root (list '* (simplify x) (simplify y)   ))]
    [(list '/ x y) (simplify-at-root (list '/ (simplify x) (simplify y)   ))]
    [(list '^ x y) (simplify-at-root (list '^ (simplify x) (simplify y)   ))]

    ; Exercise 1.6 + 1.7
    ; Complex func
    [(list 'sin x) (simplify-at-root (list 'sin (simplify x)              ))]
    [(list 'cos x) (simplify-at-root (list 'cos (simplify x)              ))]
    [(list 'tan x) (simplify-at-root (list 'tan (simplify x)              ))]
    [(list 'log x) (simplify-at-root (list 'log (simplify x)              ))]

    ; Poly case
    [_ (cond
     [(poly-sum? expr)
       (simplify-at-root (cons '+ (map (lambda (x) (simplify x)) (rest expr)))  )
     ]
     
     [(poly-prod? expr)
       (simplify-at-root (cons '* (map (lambda (x) (simplify x)) (rest expr)))  )
     ]
    ; If variable/const - return expr
    [else expr]
    )]
  )
)

(simplify '(+ 0 1))                                                     ; 1

(simplify '(+ (* 0 y) (* 2 1)))                                         ; 2

(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))     ; '(+ (+ x (+ x x)) (* (+ x y) 3))

"--------"

;Exercise 1.4 in progress or not

;Exercise 1.5
(define (to-infix expr)
  (cond
    ; If we get sum - place it between summands
    [(sum? expr)
     (list (to-infix (summand-1 expr)) '+ (to-infix (summand-2 expr)))
    ]
    ; If we get product - place it between multipliers
    [(product? expr)
     (list (to-infix (multiplier-1 expr)) '* (to-infix (multiplier-2 expr)))
    ]

    [else expr]
  )
)

(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3))) ;'((x + (x + x)) + ((x + y) * 3))
"--------"
(derivative '(+ 1 x y (* x y z)) 'x) ; '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))

(simplify '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))) ; '(+ 1 (* y z))
"--------"

; Exercise 1.8

(define (variables-of  expr)
         
 (define (func expr out)
   (cond
     ; If we rich end - return out
     [(empty? expr) out]
     ; If it's variable - input to list
     [(variable? (first expr))
        (func (rest expr) (append out (list (first expr)) ))
     ]
     ; If it's not variable - skip
     [else (func (rest expr) out)]
   )
 )
  ; Remove duplicates and sort variables
  (sort (remove-duplicates(func (flatten expr) empty) ) symbol<?)
)

(variables-of '(+ 1 y x (* z y ))) ; '(x y z)
"--------"

; Exercise 1.9

(define (gradient expr v)
  ;Just get derivative and simplify for all variables in list
  (map (lambda (x) (simplify (derivative expr x)) )  v)
)

(gradient '(+ 1 x y (* x y z)) '(x y z)) ; '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))


(derivative '(^ (+ x 1) (+ x 1)) 'x) 

