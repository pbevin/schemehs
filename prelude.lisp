(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . elts) elts)
(define (id obj) obj)
(define (flip func) (lambda (a1 a2) (func a2 a1)))
(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (arg) (f (apply g arg))))
(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (even? arg) (= (mod arg 2) 0))
(define (odd arg) (= (mod arg 2) 1))

(define (foldr func end lst)
  (if (null? lst) end
    (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst) accum
    (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce foldr)

(define (unfold func init pred)
  (if (pred init)
    (cons init '())
    (cons init (unfold (func init) pred))))

(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))
(define (min first . rest) (fold (lambda (a b) (if (< a b) a b)) first rest))
(define (max first . rest) (fold (lambda (a b) (if (> a b) a b)) first rest))
(define (length lst) (fold (lambda (a b) (+ a 1)) 0 lst))
(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f lst) (foldr (lambda (x y) (cons (f x) y)) '() lst))
(define (filter p lst) (foldr (lambda (x y) (if (p x) (cons x y) y)) '() lst))
