;;------------------------------------------------------
;;
;;  P R O J E C T  1 :  R E A L L Y - T I N Y   
;;    T Y P E   R E C O N S T R U C T I O N
;;
;;             CS515  Fall 2018
;;------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CurrentCounter 0)

(define newtvar
   (lambda()
      (set! CurrentCounter (+ CurrentCounter 1))
      (string->symbol (string-append "_a" (number->string CurrentCounter)))))

(define name?
  (lambda (s)
    (and (symbol? s)
         (not (memq s '(lambda))))))

(define parse
  (lambda (m)
    (cond
      ((number? m)  `(&const ,m))
      ((eq? #t m)   `(&const true))
      ((eq? #f m)   `(&const false))
      ((name? m)    `(&var ,m))
      ((pair? m)    (cond 
                      ((eq? `lambda (car m))
                        (if (and (= 3 (length m))
                                 (list? (cadr m))
                                 (= 1 (length (cadr m)))
                                 (name? (caadr m)))
                            `(&lambda ,(cadr m) ,(parse (caddr m)))
                            (error 'parse "Syntax error")))
                      (else
                        `(&apply ,(parse (car m)) ,(parse (cadr m))))))
      (else         (error 'parse "Syntax error")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Initial Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define init_E  ;; Type Environment 
  '((add1 (int -> int)) (sub1 (int -> int))
    (zero? (int -> bool)) (not (bool -> bool))
    (and (bool -> (bool -> bool))) (or (bool -> (bool -> bool)))))

(define init_C '()) ;; Type Constraints


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type reconstruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TR
  (lambda (ast E C)
     (error 'TR: "still an empty body"))) ;; YOUR CODE GOES HERE

(define TRec
  (lambda (m)
       ;;; extract type expression from compound return type
     (TR (parse m) init_E init_C)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Sample Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M1 '((lambda (x) x) 5))
(define M2 '((lambda (x) (sub1 x)) 5))
(define M3 '((lambda (x) 1) ((lambda (x) (x x)) (lambda (x) (x x)))))
(define M4 '((lambda (x) ((and x) #f)) #t))
