;; You can start by following the instructions at the adress below:
;; https://emina.github.io/rosette/rosette-guide/ch_getting-started.html
;; Once you have installed Rosette, you can use Emacs (like in the live tutorial)
;; or any Racket editor you prefer.
;; If you are familiar with Racket and you have raco as a package manager, just:
;; raco pkg install rosette

;; For those not familiar with Racket, I have to declare what 'language' I am
;; using.
#lang rosette/safe
;; You might wonder, why safe? The full language can actually be used:
;; #lang rosette
;; But it contains 'unsafe' constructs. If you don't understand some the 'unsafe'
;; features of Rosette, stick to the safe subset.
;; We will do so in the tutorial.

;; Set the precision of the solver
(current-bitwidth #f)

;; 1 - Symbolic values
;; In a Rosette program, you can use Racket variables like in any Racket program:
(define variable_with_concrete_value 1)
;; We say this variable has a concrete value, to distinguish it from another
;; kind of value in a Rosette program: symbolic values.
;; A symbolic value can be a constant:
(define-symbolic var_with_symbolic_bool boolean?)
(define-symbolic var_with_symbolic_int integer?)
;; A few symbolic integers:
(define-symbolic a0 integer?)
(define-symbolic a1 integer?)
(define-symbolic a2 integer?)
;; We can make a vector of them:
(define A (vector a0 a1 a2))
;; We can define symbolic functions
;; For example f is an uninterpreted function from integers to booleans
(define-symbolic f (~> integer? boolean?))

;; Another possiblity: define a 'dynamic' variable
;; It gives a different symbolic value each tiem you evaluate
;; it
(define (static)
 (define-symbolic x boolean?) ; creates the same constant when evaluated
 x)

(define (dynamic)
 (define-symbolic* y integer?) ; creates a different constant when evaluated
 y)

;; If you run the following commands, you will see the difference between the
;; two declarations:
;; > (eq? (static) (static))
;; #t
;; > (eq? (dynamic) (dynamic))
;; (= y$0 y$1)


;; And we can make assertions about these variables
(assert (> a1 0))


;; 2 - Synthesis
;; We need the synthesis library for that
(require rosette/lib/synthax)
;; Example taken from Rosette's tutorial
;; We want to factorize the following expression:
;; x^4 + 6*x^3 + 11* x^2 + 6*x
;;
;; Without using the ^ (power) operator:
;;(+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x))
;; We need to declare one integer symbolic value:
(define-symbolic x integer?)

;; The function corresponding to the specification
(define poly (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

;; USING (??)
;; factored is our program sketch here, the (??) construct can be replaced
;; by any constant (integer/boolean/..)
;; Then factored is therefore a factored expression of poly, provided the solver
;; replaces the (??) by a correct integer value.
(define factored (* (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))

;; The binding variable will contains all the bindings (concrete values
;; assigned to the holes in the sketch). In this case, the (??) holes
;; are bound to integers.
(define binding
  (synthesize #:forall (list x)
              #:guarantee (assert (= poly factored))))

;; In the console, try to type:
;; > (print-forms binding)

;; However, (??) can only be replaced by integer constants.
;; What if we just want to put an oeprator or a variable?
;; I can use a 'choose ...' construct for that.
(define factored2 ((choose * /) 1 (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))
(define binding2
  (synthesize #:forall (list x)
              #:guarantee (assert (= poly factored2))))

;; Now if I want to define something that gives me factorized expressions with n
;; factors, we use the (define-synthax ...) construct.
;; You can look up the definition in the Rosette manual
;; here : https://emina.github.io/rosette/rosette-guide/sec_rosette-libs.html#%28form._%28%28lib._rosette%2Flib%2Fsynthax%2Fcore..rkt%29._define-synthax%29%29

(define-synthax (factored_poly x n)
  #:base (+ x (??))
  #:else (* (+ x (??)) (factored_poly x (sub1 n))))

;; And we can use it in our sketch: the factored expression that is equivalent of
;; poly is a factored_poly expression using the varaible x, of height 3.
(define factored3 (factored_poly x 3))
(define binding3
  (synthesize #:forall (list x)
              #:guarantee (assert (= poly factored3))))

;; > (print-forms factored3)
;; /home/nicolet/projects/csc410-p4parser/rosette-tutorial.rkt:108:0
;; (define factored3 (* (+ x 1) (* (+ x -13) (* (+ x -14) (+ x 0)))))
