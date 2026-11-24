(use-modules (ice-9 format))
(define (square x)(* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
;; Exercise 1.1: Below is a sequence of expressions.
;; What is the result printed by the interpreter in response to each expression?
;; Assume that the sequence is to be evaluated in the order in which it is presented.
(display 10) ;10
(newline)
(display "(+ 5 3 4) -- ")
(display (+ 5 3 4)) ; 12
(newline)
(display "(- 9 1) -- ")
(display (- 9 1)) ; 8
(newline)
(display "(/ 6 2) -- ")
(display (/ 6 2)) ; 3
(newline)
(display "(+ (* 2 4) (- 4 6)) -- ")
(display (+ (* 2 4) (- 4 6))) ; 6
(newline)
(define a 3)
(display "(define a 3) -- ")
(display a) ; 3
(newline)
(define b (+ a 1))
(display "(define b (+ a 1)) -- ")
(display b) ; 4
(newline)
(display "(+ a b (* a b)) -- ")
(display (+ a b (* a b))) ; 19
(newline)
(display "(= a b) -- ")
(display (= a b)) ; #f
(newline)
(display "(if (and (> b a) (< b (* a b)))
    b
    a) -- ")
(display (if (and (> b a) (< b (* a b)))
    b
    a)) ; b = 4
(newline)
(display "(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) -- ")
(display (cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))) ; 16
(newline)
(display "(+ 2 (if (> b a) b a)) -- ")
(display (+ 2 (if (> b a) b a))) ; 6
(newline)
(display "(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) -- ")
(display (* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
	    (+ a 1))) ; 16
(newline)
;; Exercise 1.2
(define my-result (/ (+ 6 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))
(format #t "~f" my-result)
(newline)
;; Exercise 1.3 Define a procedure that takes three numbers
;; as arguments and returns the sum of the squares of the two larger numbers.
(define (sum-of-largest a b c) (cond ((and (< a b) (< a c)) (sum-of-squares b c))
					((and (< b a) (< b c)) (sum-of-squares a c))
					(else (sum-of-squares a b))))
(display "Sum of largest (1 2 2): ") ; 8 expected
(display (sum-of-largest 1 2 2))
(newline)
(display "Sum of largest (3 2 3): ") ; 18 expected
(display (sum-of-largest 3 2 3))
(newline)
(display "Sum of largest (4 2 1): ") ; 20 expected
(display (sum-of-largest 4 2 1))
(newline)
;; Exercise 1.4: Observe that our model of evaluation allows for combinations whose operators are compound expressions.
;; Use this observation to describe the behavior of the following procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; If b is greater than 0, then (+ a b), else (- a b). Basically, making sure b is always aboslute.
(display "a-plus-abs-b (2 2): ")
(display (a-plus-abs-b 2 2)) ; 4 expected
(newline)
(display "a-plus-abs-b (2 -6): ")
(display (a-plus-abs-b 2 -6)) ; 8 expected
(newline)
(display "a-plus-abs-b (-10 8): ")
(display (a-plus-abs-b -10 8)) ; -2 expected
(newline)
(display "a-plus-abs-b (-5 -5): ")
(display (a-plus-abs-b -5 -5)) ; 0 expected
(newline)
;; Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures: 
(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))
;; Then the evaluates the expression:
;; (display (test 0 (p)))

;; What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
;; (Assume that the evaluation rule for the special form if is the same whether the interpreter is using normal or applicative order: The predicate expression is evaluated first, and the result determines whether to evaluate the consequent or the alternative expression.)
;; Answer
;; Applicative-order, eager, gets stuck on 'p'("evaluate the arguments and then apply"):
;; (test 0 (p))
;; (if (= 0 0) 0 (p)
;; (if (= 0 0) 0 (p)
;; (if (= 0 0) 0 (p)
;; ...forever
;; Normal-order, lazy, never touchs 'p' so doesn't get stuck ("fully expand and then reduce"):
;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t 0 (p))
;; 0

;; Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks.
;; -- Functions used in exercise
(define (average x y) 
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

;; Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;; Eva demonstrates the program for Alyssa:
;;(new-if (= 2 3) 0 5)
;;5
;;
;;(new-if (= 1 1) 0 5)
;;0
;;
;;Delighted, Alyssa uses new-if to rewrite the square-root program:
;;
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; (display "(sqrt-iter 1 2): ")
;; (display (sqrt-iter 1 2))
;; (newline)
;; ^ Runs forever
;;
;;What happens when Alyssa attempts to use this to compute square roots? Explain.
;; The program run forever. Each part of the `new-if` needs to be resolved before evaluating
;; the `new-if` procedure. One of those pieces is the `sqrt-iter` so it calls it self, then
;; repeats this by trying to evalute the body of the `new-if` in there, and so forth.
