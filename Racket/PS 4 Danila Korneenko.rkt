#lang slideshow

;Danila Korneenko B20-SD-02


;Task 1
"1.a"
(define (replicate num input)
  (define (func outlist curi)
    (cond
      [(< num curi) outlist]
      [else (func (append outlist (list input))(+ curi 1)  ) ] 
    )
  )
  (func null 1)
)


(replicate 10 'a)
; '(a a a a a a a a a a)
(replicate 3 '(1 . 2))
; '((1 . 2) (1 . 2) (1 . 2))


"-------------------------"
"1.b"
(define (split splitlist multi)
    (define (func left right index)
      (cond
        [(and (< index multi) (< index (length splitlist)) )
             (func (append left (list(first right))) (rest right) (+ index 1))
        ]
        
        [else (cons left (list right) )] 
      )   
    )
    (func null splitlist 0)
)

(split '(1 2 3 4 5) 2)
; '((1 2) . (3 4 5))
(split '(a b c d) 4)
; '((a b c d) . ())
(split '(a b c) 4)
; '((a b c) . ())
(split '(a b c) 0)


"-------------------------"
"1.c"
(define (chunks input num)
    (define (func out curr inlist counter)
      (cond
        [(null? inlist) (append out (list curr))]
        [(= counter num) (func (append out (list curr)) null inlist 0 )]
        [(< counter num) (func out (append curr (list(first inlist))) (rest inlist) (+ counter 1)  )]
        
     )   
    )
  (func null null input 0)
)

(chunks '(1 2 3 4 5) 2) ; '((1 2) (3 4) (5))
(chunks '(a b c d e f) 3) ; '((a b c) (d e f))

"-------------------------"
"1.d"
(define (windows input num)
    (define (func out inlist)
      (cond
        [(< (length inlist) num) out]
        [(>= (length inlist) num) (func (append out (list (car (split inlist num)))) (rest inlist) )] 
      )   
    )
    (func null input)
)

(windows '(1 2 3 4 5) 2)
; '((1 2) (2 3) (3 4) (4 5))
(windows '(a b c d e) 3)
; '((a b c) (b c d) (c d e))

"-------------------------"
"-------------------------"

;Task 2
"2.a"
(define (pairs in)
   (chunks (flatten
            (map (lambda (elem cur)
                   (map (lambda (elem2) (cons elem elem2)) (rest (flatten ( cdr (split in cur))))
                   )
                 ) in (range 0 (length in))
            )
           )
    2)
   
)
(pairs '(a b c d))
; '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))

"-------------------------"
"2.b"
(define (splits in)
  (map (lambda (x) (split in x) ) (range 0 (+ 1 (length in)))) 
)
(splits '(a b c))
; '(((a b c) . ()) ((a b) . (c)) ((a) . (b c)) (() . (b c)))

"-------------------------"
"2.c"
(define (max-product in)
  (let ([multi-pairs (pairs in)])
    (define (max pair1 pair2)
      (if (> (* (car pair1) (car (cdr pair1))) (* (car pair2) (car (cdr pair2))) ) pair1 pair2))
    
    (foldl max (first multi-pairs) (rest multi-pairs))
    )
)
  
(max-product '(1 2 3 4 3 2 1))
; '(3 . 4)

"-------------------------"
"2.d"
(define (max-binary-op  op in)
  (let ([multi-pairs (pairs in)])
    (define (max pair1 pair2)
      (if (> (op (car pair1) (car (cdr pair1))) (* (car pair2) (car (cdr pair2))) ) pair1 pair2))
    
    (foldl max (first multi-pairs) (rest multi-pairs))
    )
)
(max-binary-op * '(1 2 3 4 3 2 1))
; '(3 . 4)
(max-binary-op - '(1 2 3 4 3 2 1))
; '(4 . 1)

"-------------------------"
"2.e"
(define (combinations in len)
  (filter (lambda (x) (= (length x) len))
          (foldl
           (lambda (next inp) (append inp (map (lambda (x) (append x (list next))) inp)) )
          '( () ) in)
  )
)
(combinations '(a b c d) 3)
; '((a b c) (a b d) (a c d) (b c d))

"-------------------------"
"-------------------------"
"3.a"
(define (max in)
    (foldl (lambda (x y) (if (> x y) x y)) (first in) (rest in))
)
(max '(1 5 3 6 2 0))
; 6

"-------------------------"
"3.b"
(define (second-max in)
  (let ([fir-max (max in)])
    (foldl (lambda (x y) (if (and (> x y) (not (equal? x fir-max))) x y)) (first in) (rest in))
  )
)
(second-max '(1 6 3 5 2 5))
; 5

"-------------------------"
"3.c"
(define (top-3 in)
  (let ([max-list (list (second-max in) (max in))])
    (flatten (cons (
                   foldl (lambda (x y) (if (and (> x y) (andmap (lambda (m) (not (equal? x m))) max-list)) x y)) (first in) (rest in)) max-list))
  )
)
(top-3 '(5 3 6 2 8 1 0))
; '(5 6 8)

"-------------------------"
"3.d"
(define (group input)

  (reverse (foldl(lambda (out in)
                   (cond
                     [(empty? in) (list (list out))]
                     [(equal? (first (first in)) out) (append (list (append (first in) (list out))) (rest in))]
                     [else (append (list (list out)) in)]
                   )
                 ) '() input))
)
(group '(a b b c c c b a a))
; '(5 6 8)

"-------------------------"
"3.d"
(define (cumulative-sums input)
  (reverse
   (foldl
    (lambda (out in)(append (list (+ (first in) out)) in))
   '(0) input
   )
  )
)
(cumulative-sums '(1 2 3 4 5))
; (0 1 3 6 10 15)
