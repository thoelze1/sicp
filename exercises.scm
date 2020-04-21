(define (square x)
  (* x x))

;; Exercise 1.3
(define (sum-of-squares-of-largest a b c)
  (- (+ (square a) (square b) (square c))
     (cond ((and (< a b) (< a c)) a)
           ((< b c) b)
           (else c))))

;; A recursive procedure is simply one that calls itself by name. It
;; is not necessarily a recursive process.

;; “Programs must be written for people to read, and only incidentally
;; for computers to execute”

;; Exercise 1.6
;; Enters a recursive loop not because a cond structure
;; is used instead of an if structure but because a function is called
;; instead of using an if (or cond) structure. If new-if were
;; implemented with if instead of cond, the code would also enter an
;; infinite loop.

;; “The set of expressions for which a binding defines a name is
;; called the scope of that name”

;; Exercise 1.9
;; First (recursive)
;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5)))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;; Second (iterative)
;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; 9

(define (my-count-change amt values)
  (cond ((null? values) 0)
        ((< amt 0) 0)
        ((= amt 0) 1)
        (else (+ (my-count-change (- amt (car values)) values)
                 (my-count-change amt (cdr values))))))

(define (count-change amt)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                   (cc (- amount
                          (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amt 5))

;; Exercise 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1)))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
(define (f-iter n)
  (define (iterate a b c count)
    (if (= count 0)
        a
        (iterate b
                c
                (+ c
                   (* b 2)
                   (* a 3))
                (- count 1))))
  (iterate 0 1 2 n))

;; Exercise 1.12
(define (pascal n m)
  (if (or (= m 0)
          (= m n))
      1
      (+ (pascal (- n 1) (- m 1))
         (pascal (- n 1) m))))

;; Exercise 1.16
;; If n even
;; b -> b2
;; n -> n/2 - 1
;; c -> cb2
;; If n odd
;; b -> b
;; n -> n - 1
;; c -> cb
;; Fibonacci numbers can be computed logarithmically too!

(define (fast-expt base exp)
  (define (fast-iter b n c)
    (cond ((= n 0) c)
          ((even? n) (fast-iter (* b b) (- (/ n 2) 1) (* c (* b b))))
          (else (fast-iter b (- n 1) (* c b)))))
  (fast-iter base exp 1))

(define (even? n)
  (= (remainder n 2) 0))

;; And so here are all of the combinations of space-/time-complexity
;; we've seen so far:

(define (my-expt b n)
  (if (= n 0)
      1
      (* b (my-expt b (- n 1)))))

(define (my-expt-iter base pow)
  (define (iterate n c)
    (cond ((= n 0) c)
          (else (iterate (- n 1) (* base c)))))
  (iterate pow 1))

(define (my-expt-log base pow)
  (cond ((= pow 0) 1)
        ((even? pow) (square (my-expt-log base (/ pow 2))))
        (else (* base (my-expt-log base (- pow 1))))))

(define (my-expt-iter-log base pow)
  (define (iterate b p c)
    (cond ((= p 0) c)
          ((even? p) (iterate (* b b) (/ p 2) c))
          (else (iterate b (- p 1) (* b c)))))
  (iterate base pow 1))

;; Exercise 1.17
(define (halve b)
  (/ b 2))
(define (double b)
  (+ b b))
(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

;; Exercise 1.18
(define (fast-mult x y)
  (define (fast-mult-iter a b c)
    (cond ((= b 0) c)
          ((even? b) (fast-mult-iter (double a) (- (halve b) 1) (+ c (double a))))
          (else (fast-mult-iter a (- b 1) (+ c a)))))
  (fast-mult-iter x y 0))

;; Exercise 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define (my-expmod base exp mod)
  "Generates an iterative process that runs in a logarithmic number of steps"
  (define (iterate b e i)
    (cond ((= e 0) i)
          ((even? e) (iterate (remainder (* b b) mod) (/ e 2) i))
          (else (iterate b (- e 1) (remainder (* b i) mod)))))
  (iterate base exp 1))

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (cond ((< a b) (timed-prime-test a)
                 (search-for-primes (+ a 1) b))))
(define (prime? n)
  (define (iterate i)
    (cond ((= (remainder n i) 0) #f)
          ((> (* i i) n) #t)
          (else (iterate (+ i 1)))))
  (iterate 2))

;; Exercise 1.26
;;
;; From a high level, calling expmod with (/ exp 2) halves the
;; problem. It is this halving, at each iteration of the process, that
;; allows it to run in a number of steps that is logarithmically
;; related to the size of the input. When Louis calls expmod twice,
;; each with a halved problem (the same half) he is doing twice of
;; half of the original amount of work. Therefore he is doing the
;; original amount of work, which in the case of computing an exponent
;; would be n multiplications where n is the power being raised to.
 
;; Exercise 1.27
(define (fermat-condition? a n)
  (= (my-expmod a n n) (remainder a n)))
(define (fermat-test? n)
  (define (satisfies? a)
    (cond ((>= a n) #t)
          ((fermat-condition? a n) (satisfies? (+ a 1)))
          (else #f)))
  (satisfies? 2))
(define (carmichael? n)
  (and (not (prime? n)) (fermat-test? n)))

;; Exercise 1.29
(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (step x) (+ x h h))
  (define (cf c x) (* c (f x)))
  (define (2f x) (cf 2 x))
  (define (4f x) (cf 4 x))
  (* (/ h 3.0)
     (+ (f a)
        (sum 4f (+ a h) step b)
        (sum 2f (+ a h h) step b)
        (f b))))

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; Exercise 1.32
(define (accumulate combiner null-val term a next b)
  (if (> a b)
      null-val
      (combiner (term a)
                (accumulate combiner null-val term (next a) next b))))
(define (accumulate-iter combiner null-val term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-val))

;; Exercise 1.33
(define (filtered-accumulate filter combiner null-val term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) (combiner result null-val)))))
  (iter a null-val))
(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))
(define (product-coprimes n)
  (define (filt a)
    (= (gcd a n) 1))
  (filtered-accumulate filt * 1 (lambda (x) x) 2 inc n))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (factorial b)
  (product (lambda (x) x) 2 (lambda (x) (+ x 1)) b))

;; Let is syntactic sugar for the underlying lambda application!

;; Iterative approximation of pi
(define (4square x)
  (* 4 (square x)))
(define (4square1 x)
  (- (4square x) 1))
(define (inc x)
  (+ x 1))
(define (pi-approx n)
  (* 2.0 (/ (product-iter 4square 1 inc n)
            (product-iter 4square1 1 inc n))))

;; Exercise 1.35
;; x^2 |-> x + 1
;; x^2 - x - 1 = 0
;; (-(-1) +- sqrt((-1)^2 - 4(-1)))/2
;; (1 +- sqrt(5))/2

;; Exercise 1.36
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Exercise 1.37
(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
        0
        (/ (n i)
           (+ (d i) (recurse (+ i 1))))))
  (recurse 1))
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i)
                         (+ (d i) result)))))
  (iter k 0))

;; Exercise 1.38
(define (e-approx k)
  (define (d k)
    (if (= (remainder k 3) 2)
        (+ 2.0 (* 2 (quotient k 3)))
        1.0))
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (* -1.0 (square x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))
