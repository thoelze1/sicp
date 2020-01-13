(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1)))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f2 n)
  (f-iter 0 1 2 n))

(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b
              c
              (+ c
                 (* b 2)
                 (* a 3))
              (- count 1))))

(define (pascal n m)
  (if (or (= m 0)
          (= m n))
      1
      (+ (pascal (- n 1) (- m 1))
         (pascal (- n 1) m))))

(define (count-change amount)
  (cc amount 5))
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

(define (expt b n)
  (if (= n 0)
      1
      (* (expt b (- n 1))
         b)))

(define (exptt b n)
  (expt-iter b n 1))

(define (expt-iter b n c)
  (if (= n 0)
      c
      (expt-iter b (- n 1) (* c b))))

(define (fast-expt b n)
  (fast-iter b n 1))
(define (square x)
  (* x x))
(define (fast-iter b n c)
  (cond ((= n 0) c)
        ((even? n) (fast-iter (* b b) (- (/ n 2) 1) (* c (* b b))))
        (else (fast-iter b (- n 1) (* c b)))))
(define (even? n)
  (= (remainder n 2) 0))

(define (halve b)
  (/ b 2))
(define (double b)
  (+ b b))
(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

(define (fast-mult a b)
  (fast-mult-iter a b 0))
(define (fast-mult-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (fast-mult-iter (double a) (- (halve b) 1) (+ c (double a))))
        (else (fast-mult-iter a (- b 1) (+ c a)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))
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

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))))
(define (divides? m n)
  (= (remainder n m) 0))
(define (prime? n)
  (= (smallest-divisor n) n))
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
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (carmichael? n)
  (check n 2))

(define (check n a)
  (cond ((< a n) (and (check n (+ a 1)) (passes? a n)))
        (else #t)))
  
(define (passes? a n)
  (= (expmod a n n) (remainder a n)))

(define (sum-integers a b)
  (sum-integers-iter a b 0))

(define (sum-integers-iter a b c)
  (cond ((> a b) c)
        (else (sum-integers-iter (+ a 1) b (+ a c)))))

(define (sum term a next b)
  (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (+ result (term a)))))
  (iter a 0))

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

(define (cube x)
  (* x x x))

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

(define (filtered-accumulate filter combiner null-val term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) (combiner result null-val)))))
  (iter a null-val))

(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))
          
(define (factorial b)
  (product (lambda (x) x) 2 (lambda (x) (+ x 1)) b))

(define (4square x)
  (* 4 (square x)))

(define (4square1 x)
  (- (4square x) 1))

(define (inc x)
  (+ x 1))

(define (pi-approx n)
  (* 2.0 (/ (product-iter 4square 1 inc n)
            (product-iter 4square1 1 inc n))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-coprimes n)
  (define (filt a)
    (= (gcd a n) 1))
  (filtered-accumulate filt * 1 (lambda (x) x) 2 inc n))

(define (f g)
  (g 2))

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

;; (define (cont-frac n d k)
;;   (define (recurse i)
;;     (if (= i k)
;;         0
;;         (/ (n i)
;;            (+ (d i) (recurse (+ i 1))))))
;;   (recurse 1))

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i)
                         (+ (d i) result)))))
  (iter k 0))

(define (e-approx k)
  (define (d k)
    (if (= (remainder k 3) 2)
        (+ 2.0 (* 2 (quotient k 3)))
        1.0))
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (* -1.0 (square x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))
