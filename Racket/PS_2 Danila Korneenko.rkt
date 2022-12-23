#lang slideshow

;Danila Korneenko B20-SD-02


;Task 1
;a
(define (binary-to-decimal bits)
         
 (define (func bits i curr)
   (cond
     [(empty? bits) curr]

     [else (func (rest bits ) (+ i 1) (+ curr (* (first bits) (expt 2 i) ) ) ) ]
   )
 ) 

 (func (reverse bits) 0 0)
)

(binary-to-decimal '(1 0 1 1 0))


;b
(define (count-zeros  bits)         
 (define (func bits not_leading curr)
  (cond
    [(empty? bits) curr]

    [(= (first bits) 1) (func (rest bits ) #t curr)]
    
    [(and  not_leading (= (first bits) 0)) (func(rest bits) #t (+ curr 1))]

    [else (func (rest bits ) not_leading  curr )]

   )
 ) 
 (func bits #f 0)
)
(count-zeros '(0 0 0 1 0 1 1 0))


;c - p.s. Looks like sad smile, but it's not ;)
(define (encode-with-lengths bits)
  (define (func bits elem counter out)
    (cond
      ( [empty? bits] (append out (list counter)) )
      ( [= (first bits) elem] (func (rest bits) elem (+ counter 1) out) )
      ( [not (= (first bits) elem)] (func bits (first bits) 0 (append out (list counter))) )
    )
  )
  (rest (func bits 0 0 '() ))
)

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0))


;d
(define (binary-odd? bits)
  (cond

    [(empty? bits)   #f]

    [else (= (last bits) 1) ]
    
    ))

;(binary-odd? '())
;(binary-odd? '(1 0 1 1 0))
(binary-odd? '(1 0 1 1 1))


;e
(define (decrement bits)
  (define (func bits slength flag out)
    (cond
      ;end
      [(empty? bits) out]
     
      ;if 1 or zero
      [(= 1 slength) '(0)]

      ;if we rich last elem
      [(and (= 1 (first bits)) flag (empty? (rest bits))) out ]

      ;if 0
      [(and (= 0 (first bits)) (not flag)) (func (rest bits) slength #f (list* 0 out) )  ]  

      [(and (= 0 (first bits)) flag)       (func (rest bits) slength #t (list* 1 out )) ]

      ;if 1
      [(and (= 1 (first bits))(not flag))  (func (rest bits) slength #f (list* 1 out) )  ]
     
      [(and (= 1 (first bits)) flag)       (func (rest bits) slength #f (list* 0 out )) ]
     
     )
   )
  
 (func (reverse bits) (length bits) #t '())
)


(decrement '(1 0 1 1 0)) ; ==> '(1 0 1 0 1)
(decrement '(1 0 0 0 0)) ; ==> '(1 1 1 1)
(decrement '(0)) ; ==> '(0)


;Task 2
;a
(define (alternating-sum num)
  (define (func num i out)
    (cond
     [(empty? num) out] 

     [else (func (rest num) (* i -1) (+ out (* i (first num))) ) ]
     )
  )
  (func num 1 0)
)

(alternating-sum (list 6 2 4 1 3 9))


;b

;(alternating-sum (list 1 2 3 4 5)) 
;(alternating-sum (list 1 2 3 4 5)) 
;(func (list 1 2 3 4 5) 1          0)
;(func (list 2 3 4 5)   (* -1 1)   (+ 0 (* 1 1)))
;(func (list 2 3 4 5)   -1         1)
;(func (list 3 4 5)     (* -1 -1)  (+ 1 (* -1 2)))
;(func (list 3 4 5)     1          -1)
;(func (list 4 5)       (* 1 -1)   (+ -1 (* 1 3)))
;(func (list 4 5)       -1         2)
;(func (list 5)         (* -1 -1)  (+ 2 (-1 4)))
;(func (list 5)         1          -2)
;(func '()              (* 1 -1)   (+ -2 (* 1 5)))
;(func '()              -1         3)
; ANSWER: 3

;c

;It's already tail recursion, so it's optimal by definition. But if it wasn't tail recursion, such method will not use much memory for calls and there can't be any overflows.

;Task 3

(define (dec n) (- n 1))
 (define (f n)
  (cond
   [(<= n 2) (- 10 n)]
   [else (* (f (dec (dec n))) (f (dec n)))]

  )
 )

;(f 3)
;(* (f (dec ( dec 3)))    (f(dec 3)))
;(* (f (dec ( - 3 1)))    (f(-3 1)))
;(* (f (dec ( 2)))        (f 2 ))
;(* (f (dec ( - 2 1)))    (f 2))
;(* (f 1)                 (f 2))
;(* (-10 1)               (-10 2))
;(* 9 8)
;72
