#lang racket
;; compiler
;; supports addition, multiplication and single assignment
(define regClear 0) ;; next empty register

;; checks if a value is one of a list of symbols
(define (operator? op)
  (and (symbol? op)
       (or (symbol=? '+ op)
           (symbol=? '* op)
           (symbol=? '- op)
           (symbol=? '/ op))))

;; Checks if a lower level bracket is empty
(define (lower-invalid? expr)
  (cond [(empty? expr) #t]
        [(not (cons? expr)) #f]
        [(cons? (rest expr)) #f]
        [else (lower-invalid? (first expr))]))
;(lower-invalid? '(()))

;; Checks for errors, removes top level extraneous brackets
(define (valid? expr acc)
  (cond [(empty? expr) empty]
        [(lower-invalid? (first expr)) (valid? (rest expr) acc)]
        [(boolean=? (operator? (first expr)) (operator? acc)) (error "bad expression")]
        [else (cons (first expr) (valid? (rest expr) (first expr)))]))

;(valid? '(1 + (()) () 2 + 3 * 4 * 5 + 6 * 7) '+)

;; gets the order of the operands and values (reverse order)
(define (get-order expr vals ops)
  (cond [(empty? expr) (list vals ops)]
        [(operator? (first expr)) (get-order (rest expr) vals (cons (first expr) ops))]
        [(cons? (first expr)) (get-order (rest expr) (cons (in->pre-help (first expr)) vals) ops)]
        [else (get-order (rest expr) (cons (first expr) vals) ops)]))

;; Builds the new prefix equation (without the first plus, will be added later)
;; val is always 1 element greater than op
(define (build-eq vals ops part eq)
  (cond [(empty? ops) (cond [(empty? vals) (error "bad expression")]
                            [(empty? part) (cons (first vals) eq)]
                            [else (cons (cons '* (cons (first vals) part)) eq)])]
        [(symbol=? '+ (first ops))
         (cond [(empty? part) (build-eq (rest vals) (rest ops) empty (cons (first vals) eq))]
               [else (build-eq (rest vals) (rest ops) empty (cons (cons '* (cons (first vals) part)) eq))])]
        [(symbol=? '- (first ops))
         (cond [(empty? part) (build-eq (rest vals) (rest ops) empty (cons (first vals) eq))]
               [else (build-eq (rest vals) (rest ops) empty (cons (cons '* (cons (first vals) part)) eq))])]
        [(symbol=? '* (first ops)) (build-eq (rest vals) (rest ops) (cons (first vals) part) eq)]
        [(symbol=? '/ (first ops)) (build-eq (rest vals) (rest ops) (cons (first vals) part) eq)]))

;; Adds a plus if necessary
(define (add-plus expr)
  (cond [(empty? (rest expr)) (first expr)]
        [else (cons '+ expr)]))

;; transforms an infix equation into racket prefix equation
(define (in->pre-help expr)
  (define simp-expr (valid? expr '+)) ;; should start with a number, so the '+ (or any "operator") causes the function to evaluate as expected
  (define vals-ops (get-order simp-expr empty empty))
  (add-plus (build-eq (first vals-ops) (second vals-ops) empty empty)))

;; does some basic checks before conversion
(define (in->pre expr)
  (cond [(empty? expr) expr]
        [(not (cons? expr)) (cond [(operator? expr) (error "bad expression")]
                                  [else expr])]
        [else (in->pre-help expr)]))

(define make-expr-code 1)
(in->pre '(1 * 2 + y * x))

(define (make-reg-sym reg)
  (cond [(number? reg) (string-append "$" (number->string reg))]
        [else (string-append "$" (symbol->string reg))]))
(define (optrans op)
  (cond [(symbol=? op '+) "ADD "]
        [(symbol=? op '*) "MUL "]))
(define (convertToString val)
  (cond [(number? val) (number->string val)]
        [else (string-append "$" (symbol->string val))]))
;; gives a list of mmix expressions corresponding to the math
(define (compile-math lst-infix)
  (cond [(empty? (rest lst-infix)) empty]
        [(list? (second lst-infix))
         (begin (set! regClear (+ regClear 1))
                (append (compile-math (second lst-infix))
                      (begin (set! regClear (- regClear 1))
                             (cons (string-append (optrans (first lst-infix)) (make-reg-sym regClear) "," (make-reg-sym (+ regClear 1)) "," (make-reg-sym regClear))
                                    (compile-math (cons (first lst-infix) (rest (rest lst-infix))))))))]
        [else (cons (string-append (optrans (first lst-infix)) (make-reg-sym regClear) "," (make-reg-sym regClear) "," (convertToString (second lst-infix)))
                    (compile-math (cons (first lst-infix) (rest (rest lst-infix)))))]))
(compile-math '(* (* 1 2 3 4 5) (+ 1 2) (* y x) (* y x) (* y x)))

         
;(get-order '((1 + (2 + 3 + 4)) * 5) empty empty)
;(get-order '(1 + 2 + 3 * 4 * 5 + 6 * 7) empty empty)
;(in->pre '(1 + 2 + 3 * 4 * 5 + 6 * 7 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * (1 + (4 + 5) + 2 + 3) * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6 * 6))

;(in->pre '(1 + 2 + 3 * 4 * 5 + 6 * 7))
;(in->pre '((1 + (2 + 3 + 4)) * 5))
;(in->pre '(1 + ((2)) + x))
;(in->pre '(((2 * 2 * 2)) * x)) ;;issue, extra brackets
;(in->pre 4)
;(in->pre '(#\a + "a"))
;(in->pre '(1 + (()) () 2 + 3 * 4 * 5 + 6 * 7))
;(in->pre '(1 +))
