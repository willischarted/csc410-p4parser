#lang rosette
(require racket/match)
(require rosette/lib/synthax)

;; ----------------------------------------------------------------------------
;;                                  Starter Code
;; ----------------------------------------------------------------------------

;; Returns a syntax object from reading the contents of a file.
(define (file->syntax file)
  (define (read-syntax/count-lines)
    (port-count-lines! (current-input-port))
    (read-syntax))
  (define-values (base _ __) (split-path file))
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx (with-handlers ([exn:fail? (const #f)])
                    (with-input-from-file file read-syntax/count-lines)))
    stx))
(define keywords (list '+ '-           ;Arithmetic
                       'sub1 'add1     ; (sub1 a) is (- a 1) and (add1 a) (+ a 1)
                       'or 'and '! '=  ;Boolean
                       '#t '#f         ;Boolean true/false
                       'max 'min       ;Conditional arithmetic
                       ;; '& '\| '^           ;Bitwise operator
                       '== '>= '<= '> '<   ;Integer comparison
                       'integer? 'boolean? ;Types
                       'void?              ;Void type
                       'if                 ;Conditionals
                       ))

;; Type of the operands of an operator.
;; The void? type represents either integer or boolean.
;; The %top operator is the default operator used as the top
;; operator when starting to parse an expression.
(define (optype? op)
  (match op
    [(or '+ '- 'min 'max '>= '> '< '<= '==) 'integer?]
    [(or 'or 'and '! '=) 'boolean?]
    [(or 'if '%top) 'void?]))

;; Returns true is the syntax objects represents an id
;; that is not a keyword of the language.
(define (is-syntax-of-id stx)
  (if (identifier? stx)
      (let ([x (syntax-e stx)])
      (<= (count (lambda (k) (equal? k x)) keywords) 0))
      false))

;; Builds a list of pairs (identifier, indentfier-type)
;; The type is inferred from the operator above the identifier occurrence
;; so this is not a correct type inference, just a hint.
;; (add-ids top_op stx l) adds the ids encountered in the syntax-object
;; stx  with top-operator top_op into list l.
(struct typed-var (id type) #:transparent #:mutable)
(define (add-ids top_op stx l)
  (if (is-syntax-of-id stx)
      (append l (list (typed-var (syntax->datum stx) (optype? top_op))))
      (let ([expanded-stx (syntax-e stx)])
        (if (list? expanded-stx)
            (foldl (lambda (maybe-id id-list)
                     (add-ids (syntax-e (car expanded-stx))
                              maybe-id id-list))
                   l
                   (cdr expanded-stx))
            l))))

;; Wrapper for add-ids starting from an empty list and
;; a dummy operator that types void
(define (get-ids stx) (remove-duplicates (add-ids '%top stx (list ))) )
;; Create Rosette symbolic definition from identifier + type
(define (make-rosette-decl v)
  (with-syntax
    ([vname (datum->syntax #f (typed-var-id v))]
     [vtype (datum->syntax #f (typed-var-type v))])
    #'(define-symbolic vname vtype)))
(define (make-all-rosette-decl lv)
  (datum->syntax #f (map make-rosette-decl lv)))

;; ----------------------------------------------------------------------------
;;                                    Our Code
;; ----------------------------------------------------------------------------

;; Our make-rosette-simple function
(define (make-rosette-simple formula) (append
                                       (generate-declarations (determineType (instance-types (syntax->datum formula))))
                                       (list (list
                                              'synthesize
                                              '#:forall (append (list (quote list)) (getvars formula))
                                              '#:guarantee (quasiquote((assert (eq?
                                                                                (unquote (syntax->datum formula))
                                                                                (unquote (append (list (quote gen-expression))
                                                                                                 (getvars formula)
                                                                                                 (list (compute-height (syntax->datum formula) 0))))))))))
                                       ))

;; Our define-synthax function
(define-synthax(gen-expression (booleanvariables ...) (integervariables ...) height)
 #:base (choose booleanvariables ... integervariables ...)
 #:else (choose
         booleanvariables ... integervariables ...
          ((choose >= > <= < + - && ||) (gen-expression (booleanvariables ...)  (integervariables ...) (- height 1))
                        (gen-expression (booleanvariables ...)  (integervariables ...) (- height 1)))
          )
  )

;; A helper function to improve on make-all-rosette-decl, using our improved type
;; inferences. Generates the declarations of all variables in the function
(define (generate-declarations variables) (if (null? variables)
                                              (list)
                                              (append (list (list 'define-symbolic (second (car variables)) (car (car variables))))
                                                      (generate-declarations (rest variables)))))

;; A helper function that takes the given formula and returns a list of variables
(define (getvars lst)
  (remove-duplicates (filter notinteger (removeops (syntax->datum lst))))
  )

;; A recursive helper function that does most of the work for instance-types
(define (process-elements elements type) (if (null? elements)
                                               (list)
                                               (if (list? (car elements))
                                                   (append (instance-types (car elements)) (process-elements (rest elements) type))
                                                   (if (or (number? (car elements)) (boolean? (car elements)))
                                                       (process-elements (rest elements) type)
                                                       (append (list (list type (car elements))) (process-elements (rest elements) type))))))

;; Infers the types of all variables in the formula
(define (determineType lst)(determineTypeHelp (remove-duplicates lst)))

;; Helper function for determineType
(define (determineTypeHelp lst )(if (empty? lst)
                                   '()
                                   (append
                                        (if (isValid (car lst)(cdr lst))
                                           (if(isVoid (car lst)(cdr lst))
                                              (list(append (list 'integer?) (cdr (car lst))))
                                              (if (equal? (car (car lst)) 'void?)
                                                  '()
                                                  (list(car lst))
                                                  )
                                              )
                                           (raise 'failed #t)
                                       )
                                    (determineTypeHelp (cdr lst)))
                                   ))

;; A helper function that lists the inferred types of all instances of all variables in the given formula.
;; Variables may appear more than once with different inferred types - other helper functions analyze
;; these types and determine what type each variable should ultimately be assigned
(define (instance-types formula) (process-elements (rest formula) (optype? (car formula))))

(define (isVoid item lst)(if (equal? (car item) 'void?)
                             (andmap (lambda (item2) (equal? (cdr item2) (cdr item))) lst)
                             #f))

(define (isValid item lst) (andmap (lambda (item2)
                                                 (if (equal? (cdr item) (cdr item2))
                                                     (or (equal? (car item) 'void?)
                                                         (equal? (car item2) 'void?))
                                                     #t
                                                     ))
                                               lst))

;; A helper function that computes the height of the expression
(define (compute-height formula height) (if (list? formula)
                                            (listmax (map (lambda (f) (compute-height f (+ 1 height))) formula))
                                            height))

;; Helper function for compute-height
(define (listmax lst) (if (null? lst)
                          0
                          (max (car lst) (listmax (rest lst)))))

(define (notinteger i)
  (cond
   [(integer? i) #f]
   [else #t])
  )

(define (flat-list lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (flat-list (car lst))
                 (flat-list (cdr lst))))
        (else
         (list lst))))

(define (removeops lst)
  (remove* (list '+ '- 'or 'and 'min 'max '>= '> '< '<= '== '! '= 'if '%top '#t '#f ) (flat-list lst)))

;; ----------------------------------------------------------------------------
;;                                         TESTS
;; ----------------------------------------------------------------------------
;; Parsing an expression from a file an getting the variables used in the
;; expression
;; (define expr-syntax (file->syntax "./example.expr"))
;; (get-ids expr-syntax)
;; Other examples/tests with string inputs converted to syntax objects
;;(get-ids (syntax a)) ;; --> (a, void?)
;;(get-ids #'(+ a b));; --> '((a . integer?) (b . integer?))
;;(get-ids #'(if (== x y) #t #f)) ;; --> '((x . integer?) (y . integer?))
;;(get-ids #'(if (or x y) #t #f)) ;; --> '((x . boolean?) (y . boolean?))
;; From the project inputs
;;(get-ids #'(+ a b))
;;(get-ids #'(if #t (+a 0) (- b 9)))
;;(get-ids #'(+ (+ 1 a) (+ -1 b)))
;;(get-ids #'(or (and a b) (and a b)))
;;(get-ids #'(min (max (+ xm (min 0 0)) lm) (min xm2 lm2)))
;;(get-ids #'(if (> (+ (- xmts lmts) xpos) (+ xaux_1 xpos)) xpos lpos))
;; Creates a syntax objects representing the declarations to make to
;; make sure all identifiers are defined in the expression.
;(define declarations
;  (make-all-rosette-decl (get-ids #'(+ a b (- x y)))))
;; Pretty print the racket declarations.
;; Using pretty for a nicer output. In this case, it will automatically
;; jump lines between the declarations.
;(require racket/pretty)
;(println "Variable declarations")
;(pretty-print (syntax->datum declarations))

;; ----------------------------------------------------------------------------
;;                                  Check-in 3
;; ----------------------------------------------------------------------------
(println "--------------Type Inference--------------")
(println "Ex 1")
(pretty-print (make-rosette-simple (syntax (if #t (+ a 0) (- b 9)))))
(println "Ex 2")
(pretty-print (make-rosette-simple (syntax (+ (+ 1 a) (+ -1 b)))))
(println "Ex 3")
(pretty-print (make-rosette-simple (syntax (or (and a b) (and a b)))))
(println "Ex 4")
(pretty-print (make-rosette-simple (syntax (min (max (+ xm (min 0 0)) lm) (min xm2 lm2)))))
(println "Ex 5")
(pretty-print (make-rosette-simple (syntax (if (> (+ (- xmts lmts) xpos) (+ xaux_1 xpos)) xpos lpos))))

(println "--------------gen-expression--------------")
(define-symbolic a e f o boolean?)
(define-symbolic c d j k integer?)

(define (gen x b g)
  (gen-expression (x) (b g) 2))
(print-forms(synthesize
    #:forall (list a c d)
    #:guarantee (assert (equal? (and a (< c d)) (gen a
                                             c d)))))

(define (gen2 x z b g)
  (gen-expression (x z) (b g) 2))
(print-forms(synthesize
    #:forall (list a e c d)
    #:guarantee (assert (equal? (and (or a e) (<= c d)) (gen2 a e
                                             c d)))))

(define (gen3 x y z b g h)
  (gen-expression (x y z) (b g h) 3))
(print-forms(synthesize
    #:forall (list a e f c d j)
    #:guarantee (assert (equal? (or (< (+ c d) j) (or (and a e) f)) (gen3 a e f
                                             c d j)))))

(define (gen4 x y b g h i)
  (gen-expression (x y) (b g h i) 4))
(print-forms(synthesize
    #:forall (list a e c d j k)
    #:guarantee (assert (equal? (and (or a e) (> (- c (+ d j)) k)) (gen4 a e 
                                             c d j k)))))

(define (gen5 w x y z b g h i)
  (gen-expression (w x y z) (b g h i) 4))
(print-forms(synthesize
    #:forall (list a e f o c d j k)
    #:guarantee (assert (equal? (and (or (and (<= (- c d) (+ j k)) (or a e)) f) o) (gen5 a e f o
                                             c d j k)))))