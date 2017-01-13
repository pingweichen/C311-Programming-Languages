#lang racket


(define (countdown num [l '()])
  (if (< num 0) l
      (let ([new-l (append l (list num))]
            [new-num (- num 1)])
        (countdown new-num new-l))))

;(define countdown
;  (lambda (num)
;    (if (= 0 num) '(0)
;        (cons num (countdown (- num 1))))))

(define (insertR x y l [r '()])
  (cond [(empty? l) r]
        [(equal? x (car l)) (let ([new-r (append r (list x y))]
                                  [new-l (cdr l)])
                              (insertR x y new-l new-r))]
        [else (let ([new-r (append r (list (car l)))]
                    [new-l (cdr l)])
                (insertR x y new-l new-r))]))


(define (insertR-fr x y l [r '()])
  (cond [(empty? l) r]
        [(equal? x (car l)) (let ([new-r (foldr cons (list x y) r)]
                                  [new-l (cdr l)])
                              (insertR-fr x y new-l new-r))]
        [else (let ([new-r (foldr cons (list (car l)) r)]
                    [new-l (cdr l)])
                (insertR-fr x y new-l new-r))]))


(define (remv-1st letter l [r '()])
  (cond [(empty? l) l]
        [(equal? letter (car l)) (let ([new-r (append r (cdr l))])
                                   new-r)]
        [else (let ([new-l (cdr l)]
                    [new-r (append r (list (car l)))])
                (remv-1st letter new-l new-r))]))


(define (list-index-ofv? letter l [index 0])
  (cond [(empty? l) "bad data"]
        [(equal? letter (car l)) index]
        [else (let ([new-l (cdr l)]
                    [new-index (+ index 1)])
                (list-index-ofv? letter new-l new-index))]))


(define (filter procedure l [r '()])
  (cond [(empty? l) r]
        [(equal? #t (procedure (car l))) (let ([new-l (cdr l)]
                                               [new-r (append r (list (car l)))])
                                           (filter procedure new-l new-r))]
        [(equal? #f (procedure (car l))) (filter procedure (cdr l) r)]))

(define filter-fr
  (lambda (func array)
    (foldr (lambda (item temp-array)
             (if (equal? #t (func item)) (cons item temp-array)
                 temp-array))
           '()
           array)))


(define (zip a b [r '()])
  (if (or (empty? a) (empty? b)) r
      (let ([new-a (cdr a)]
            [new-b (cdr b)]
            [new-r (append r (list (cons (car a) (car b))))])
        (zip new-a new-b new-r))))


(define (map func l [result '()])
  (if (empty? l) result
      (let ([new-list (cdr l)]
            [new-result (append result (list (func (car l))))])
        (map func new-list new-result))))


;(define (map-fr func l [r '()])
;  (if (empty? l) r
;      (let ([new-l (cdr l)]
;            [new-r (foldr cons (list (func (car l))) r)])
;        (map-fr func new-l new-r))))

(define map-fr
  (lambda (func l)
    (foldr (lambda (item temp-item)
             (cons (func item) temp-item))
             '()
             l)))

;(define map-fr
;  (lambda (func l1 [l2 '()])
;    (if (empty? l1) l2
;        (map-fr func (cdr l1) (foldr cons (list (func (car l1))) l2)))))


;(map-fr add1 '(1 2 3 4))

(define append
  (lambda (l1 l2)
    (cond [(null? l1) l2]
          [else (cons (car l1)
                      (append (cdr l1) l2))])))


(define (append-fr l1 l2)
  (if (empty? l1) l2
      (foldr cons l2 l1)))


(define (reverse-fr l [r '()])
  (if (empty? l) r
      (let ([new-r (list* (car l) r)]
            [new-l (cdr l)])
        (reverse-fr new-l new-r))))


(define (fact num [r 1])
  (if (equal? 0 num) r
      (let ([new-num (- num 1)]
            [new-r (* num r)])
        (fact new-num new-r))))


(define (memv letter l)
  (cond [(empty? l) #f]
        [(equal? letter (car l)) l]
        [else (let ([new-l (cdr l)])
                (memv letter new-l))]))


;(define (fib num [r '(0 1)])
;  (cond [(equal? 0 num) 0]
;        [(equal? 1 num) 1]
;        [(empty? r) (let ([last-two-elem (list-tail r (- (length r) 2))])
;                                   (+ (list-ref last-two-elem 0) (list-ref last-two-elem 1)))]
;        [else (let* ([last-two-elem (list-tail r (- (length r) 2))]
;                     [new-r (append r (list (+ (list-ref last-two-elem 0) (list-ref last-two-elem 1))))])
;                (fib num new-r))]))

(define fib
  (lambda (n)
    (cond
      [(< n 2) n]
      [else (+ (fib (- n 1))
               (fib (- n 2)))])))


;(((w . (x .())) . y) . (z . ()))


(define (binary->natural num-l [times 0] [r 0])
  (if (empty? num-l) r
      (let ([new-times (+ times 1)]
            [new-num-l (cdr num-l)]
            [new-r (if (equal? 0 (car num-l)) r
                       (+ r (expt 2 times)))])
        (binary->natural new-num-l new-times new-r))))


(define (binary->natural-fr num-l [times 0] [r 0])
  (if (empty? num-l) r
      (let ([new-times (+ times 1)]
            [new-num-l (cdr num-l)]
            [new-r (if (equal? 0 (car num-l)) r
                       (+ r (expt 2 times)))])
        (binary->natural-fr new-num-l new-times new-r))))


(define (minus large small [r 0])
  (if (or (< large 0) (< small 0)) "support nonnegative number only"
      (if (equal? large small) r
          (let ([new-large (- large 1)]
                [new-r (+ r 1)])
            (minus new-large small new-r)))))


(define (div num divider [r 0])
  (if (equal? 0 divider) "bad data"
      (if (equal? 0 num) r
          (let ([new-num (- num divider)]
                [new-r (+ r 1)])
            (div new-num divider new-r)))))


(define (append-map-fr func l [r l])
  (if (empty? l) r
      (let* ([new-l (cdr l)]
             [new-r (append r new-l)])
        (append-map-fr func new-l new-r))))


(define (powerset l [result '(())])
  (if (null? l) result
      (let ([temp-l '()])
        (for ([y result])
          (set! temp-l (append temp-l (list (append y (list (car l)))))))
        (set! result (append result temp-l))
        (powerset (cdr l) result))))


(define powerset-fr
  (lambda (l)
    (foldr (lambda (item temp-array)
             (append temp-array (map (lambda (x) (cons item x)) temp-array)))
           '(())
           l)))


(define (cartesian-product l [r '()])
  (if (or (null? (car l)) (null? (car (cdr l)))) r
      (let ([temp-l '()])
        (for/list ([item (car (cdr l))])
          (set! temp-l (append temp-l (list (list (car (car l)) item)))))
        (set! r (append r temp-l))
        (set! l (list (cdr (car l)) (car (cdr l))))
        (cartesian-product l r))))

;(cartesian-product '((5 4) (3 2 1)))

(define cartesian-product-fr
  (lambda (l)
    (foldr (lambda (item temp-array)
             (append temp-array (map (lambda (x) (list item x)) (car (cdr l)))))
           '()
           (car l))))


(define set-difference
  (lambda (l1 l2)
    (if (empty? l1) '()
        (if (member (car l1) l2) (set-difference (cdr l1) l2)
            (cons (car l1) (set-difference (cdr l1) l2))))))


(define set-difference-fr
  (lambda (l1 l2)
    (foldr (lambda (item temp-array)
             (if (member item l2) temp-array
                 (cons item temp-array)))
           '()
           l1)))


(define (collatz x)(cond [(= 1 x) 1][(even? x) (collatz (/ x 2))][else (collatz (+ 1 (* x 3)))]))


(define quine ((lambda (x) (list x (list 'quote x))(list x (list 'quote x)))
               '(lambda (x) (list x (list 'quote x))(list x (list 'quote x)))))
