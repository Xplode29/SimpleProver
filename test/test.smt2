(set-logic NIA)
(declare-fun power (Int Int) Int)

; Power function axioms with Z3 patterns
(assert (forall ((x Int)) 
  (! (= (power x 0) 1)
   :pattern ((power x 0)))))  ; Pattern for base case

(assert (forall ((x Int) (n Int)) 
  (! (=> (> n 0) 
        (= (power x n) (* x (power x (- n 1)))))
   :pattern ((power x n)))))  ; Pattern for recursive case

; Critical lemmas for exponentiation
(assert (forall ((x Int) (k Int))
  (! (=> (>= k 0)
         (= (power x (* 2 k)) (power (* x x) k)))
   :pattern ((power x (* 2 k)) (div (* 2 k) 2)))))  ; Pattern for squaring lemma

(assert (forall ((x Int) (k Int))
  (! (=> (>= k 0)
         (= (power x (+ (* 2 k) 1)) 
                 (* x (power (* x x) k))))
   :pattern ((power x (+ (* 2 k) 1))))))  ; Pattern for odd exponent

; Declare constants
(declare-const a Int)
(declare-const b Int)
(declare-const base Int)
(declare-const res Int)
(declare-const exp Int)

; Split into two separate checks: even and odd cases

; Case 1: exp is odd (mod exp 2 = 1)
(assert (and
  (> exp 0)
  (= (mod exp 2) 1)
  (= (* res (power base exp)) (power a b))
  (>= exp 0)
  (not (= (* (* res base) (power (* base base) (div exp 2))) (power a b)))
))

(check-sat)
(reset)

; Case 2: exp is even (mod exp 2 ≠ 1)
(set-logic NIA)
(declare-fun power (Int Int) Int)

; Re-declare power axioms and lemmas (Z3 needs per-context declarations)
(assert (forall ((x Int)) (= (power x 0) 1)))
(assert (forall ((x Int) (n Int)) 
  (=> (> n 0) (= (power x n) (* x (power x (- n 1)))))))
(assert (forall ((x Int) (k Int))
  (=> (>= k 0) (= (power x (* 2 k)) (power (* x x) k)))))
(assert (forall ((x Int) (k Int))
  (=> (>= k 0) (= (power x (+ (* 2 k) 1)) (* x (power (* x x) k))))))

(declare-const a Int)
(declare-const b Int)
(declare-const base Int)
(declare-const res Int)
(declare-const exp Int)

(assert (and
  (> exp 0)
  (not (= (mod exp 2) 1))  ; Even case
  (= (* res (power base exp)) (power a b))
  (>= exp 0)
  (not (= (* res (power (* base base) (div exp 2))) (power a b)))
))

(check-sat)