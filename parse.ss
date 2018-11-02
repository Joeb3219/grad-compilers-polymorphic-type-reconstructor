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

;; Okay, let's be organized this time.
;; We will use several functions, outlined here:
;; insert: Takes two arguments, list and item, and inserts item into list
;;
;; insertKeyVal: Takes three arguments, list, key, and value, and inserts key => val into list
;;
;; findKey: Takes two arguemnts, list and key, and returns '() if key isn't in list, or its value if it is.
;;
;; getKey: Takes one argument, item, and returns the key of the item
;;
;; getVal: Takes one argument, item, and return the value of the item
;;
;; inList?: Takes two arguments, list and target, and will return #t if target is in list, and #f otherwise.
;; 
;; filter: Takes two arguments, list and targets, and will return a list of items from list iff their key is in targets.
;;
;; pack: Takes three arguments, typeexp (the returned Type Expression), typeconst (the returned Type Constraints), and typevars (The type variables you'd like to keep)
;;       Will create a return list consisting of (e C_m), where C_m is only the type constraints needed in typevars

(define findKey
  (lambda (list key)
    (cond
      ((null? list) '())
      ((eq? list '()) '())
      ((eq? (getKey (car list)) key) (getVal (car list)))
      (else (findKey (cdr list) key))
    )
  )
)

(define insert
  (lambda (list item)
    (cons item list)
  )
)

(define insertKeyVal
  (lambda (list key val)
    (insert list (cons key (cons val '())))
  )
)

(define getKey
  (lambda (item)
    (car item)
  )
)

(define getVal
  (lambda (item)
    (cadr item)
  )
)

(define inList?
  (lambda (list target)
    (cond
      ((null? list) #f) ; Empty list -> #f
      ((eq? '() list) #f) ; Empty list -> #f
      ((eq? (car list) target) #t) ; Is it a match?
      (else (inList? (cdr list) target)) ; If not, check the rest of the list.
    )
  )
)

(define filter
  (lambda (list targets)
    (cond
      ((null? list) '()) ; base case, return null list
      ((eq? '() list) '()) ; base case, return null list
      ((inList? targets (getKey (car list))) (insert (filter (cdr list) targets) (car list)))
      (else (filter (cdr list) targets))
    )
  )
)

(define findUnfulfilled
  (lambda (typeexp)
    (cond
      ((null? typeexp) '())
      ((not (pair? typeexp)) '())
      ((not (or (eq? typeexp `int) (eq? typeexp `bool))) (findUnfulfilled (cdr typeexp)))
      (else (cons (car typeexpr) (findUnfulfilled (cdr typeexp))))
    )
  )
)

(define pack
  (lambda (typeexp typeconst)
    (display "I'm packing up! ") (display typeexp) (display typeconst) (newline)
    (cons
      (substitute typeexp typeconst)
      typeconst
      ;      (filter typeconst (findUnfulfilled typeexp))
    )
  )
)

(define isYields?
  (lambda (type)
    (cond
      ((eq? '() type) #f)
      ((not (list? type)) #f)
      ((not (pair? type)) #f)
      ((eq? '-> (cadr type)) #t)
      (else #f)
    )
  )
)

(define yields
  (lambda (yieldsIn yieldsOut)
    (cons yieldsIn (cons '-> (cons yieldsOut '())))
  )
)

(define yieldsIn
  (lambda (yields)
    (if (isYields? yields) 
        (car yields)
        '()
    )
  )
)

(define yieldsOut
  (lambda (yields)
    (if (isYields? yields)
        (caddr yields)
        yields
    )
  )
)

(define getPackedTypeExpr
  (lambda (typeexpr)
    (car typeexpr)
  )
)

(define getPackedConstraints
  (lambda (typeexpr)
    (cdr typeexpr)
  )
)

(define mergeC
  (lambda (C1 C2)
    (cond
      ((null? C2) C1)
      ((eq? '() C2) C1)
      ((eq? '() (findKey C1 (getKey (car C2)))) (mergeC (insertKeyVal C1 (getKey (car C2)) (getVal (car C2))) (cdr C2)))
      (else (mergeC C1 (cdr C2)))
    )
  )
)

;(define error
;  (lambda (A B)
;    (display "Err:") (display A) (display "->") (display B) (newline)
;  )
;)

(define substitute
  (lambda (expression constraints)
    (
      (lambda (firstRun)
        (
          (lambda (secondRun)
            (if (equal? secondRun firstRun) secondRun (substitute secondRun constraints))
          )
          (substituteRun firstRun constraints)
        )
      )
      (substituteRun expression constraints)
    )
  )
)

(define substituteRun
  (lambda (expression constraints)
    (cond
      ((null? constraints) expression)
      ((eq? '() constraints) expression)
      (else (
             (lambda (currentTarget Replacement Next)
               (substituteRun (substituteOne expression currentTarget Replacement) Next)
             )
             ; CurrentTarget
             (getKey (car constraints))
             ; Replacement
             (getVal (car constraints))
             ; Next
             (cdr constraints)
            ))
    )
  )
)

(define substituteOne
  (lambda (expression target replacement)
    (cond
      ((null? expression) '())
      ((eq? '() expression) '())
      ((and (not (pair? expression)) (eq? expression target)) replacement)
      ((and (not (pair? expression)) (not (eq? expression target))) expression)
      ((list? (car expression)) (cons (substituteOne (car expression) target replacement) (substituteOne (cdr expression) target replacement)))
      ((eq? (car expression) target) (cons  replacement (substituteOne (cdr expression) target replacement)))
      (else (cons (car expression) (substituteOne (cdr expression) target replacement)))
    )
  )
)

(define isTypeVar?
  (lambda (expression)
    (cond
      ((eq? expression `int) #f)
      ((eq? expression `bool) #f)
      ((list? expression) #f)
      (else #t)
    )
  )
)

(define lambdaUnify
  (lambda (type1 type2 Constraints)
    (
      (lambda (firstRun)
        (
          (lambda (secondRun)
            (cond
              ( (and (equal? (cdr firstRun) (cdr secondRun)) (equal? (car firstRun) (car secondRun)) ) secondRun)
              (else (lambdaUnify (substitute type1 (cdr secondRun)) (substitute type2 (cdr secondRun)) (cdr secondRun)))

            )
          )
          ; secondRun
          (lambdaUnify_internal (substitute type1 (cdr firstRun)) (substitute type2 (cdr firstRun)) (cdr firstRun))
        )
      )
      ; firstRun
      (lambdaUnify_internal type1 type2 Constraints)
    )
  )
)

(define lambdaUnify_internal
  (lambda (type1 type2 Constraints)
    (display "Being asked to LAMBDA unify") (display type1) (display " and ") (display type2) (newline)
    (
      ; We begin by examining the first term of each sequence
      ; If we can unify these, then we can substitute into the remainder of the expression
      ; and then attempt to unify the remaining bounds.
      ; Note: we may later need to add a check to unify a second time, and see if any changes happened. If so, we'd need to continue unifying until no changes happened
      ; IE: fixed point, but sad.
      (lambda (term1 term2)
        (display "Lambda unify:: Let's see if") (display term1) (display " and ") (display term2) (display "unify!") (newline)
        (
          (lambda (termUnifier)
            ; Now we've unified term1 and term2.
            ; First, we see if they actually unified.
            ; If not, we can return false here because nothing else will matter.
            (if
              (eq? (car termUnifier) #f)
              (cons #f Constraints)
              (
                ; Since we did unify the two terms correctly, we now attempt to unify the remaining terms, while using this modified set.
                (lambda (remaininderTerm1 remaininderTerm2)
                   (unify remaininderTerm1 remaininderTerm2 (cdr termUnifier))

                )
                ;remainingTerm1
                (if
                  (or (pair? type1) (list? type1))
                  (yieldsOut type1)
                  '()
                )
                ;remainingTerm2
                (if
                  (or (pair? type2) (list? type2))
                  (yieldsOut type2)
                  '()
                )
              )
            )
            
          )
          ; termUnifier
          (unify term1 term2 Constraints)
        )
      )
      ; term1
      (yieldsIn type1)
      ; term2
      (yieldsIn type2)
    )
  )
)

; Returns a compound return
; #t/#f, and the needed constraints
; simply cons'd together.
(define unify
  (lambda (type1 type2 Constraints)
    (display "Being asked to unify") (display type1) (display " and ") (display type2) (newline) (display "MY constraints are ") (display Constraints) (newline)
    (cond
      ((or (isYields? type1) (isYields? type2)) (lambdaUnify type1 type2 Constraints))
      ((eq? type1 '()) (cons #t Constraints))
      ((eq? type1 type2) (cons #t Constraints)) ; If type1 and type2 are equal, return #t
      ((and (eq? type1 `int) (eq? type2 `bool)) (cons #f Constraints)) ; Both are constants, but not equivalent kinds of constants.
      ((and (eq? type1 `bool) (eq? type2 `int)) (cons #f Constraints)) ; Both are constants, but not equivalent kinds of constants.
      ((and (not (isTypeVar? type1)) (isTypeVar? type2)) (unify type2 type1 Constraints))
      ((and (isTypeVar? type1) (not (isTypeVar? type2)))
        (
          ; First we try to set the constraints to eachother generically.
          (lambda (modifiedConstraints)
            (
              (lambda (type1Mod type2Mod)
                (unify type1Mod type2Mod modifiedConstraints)
              )
              ; type1Mod
              (substitute type1 modifiedConstraints)
              ; type2Mod
              (substitute type2 modifiedConstraints)
            )
          )
          ; modifiedConstraints
          (insertKeyVal Constraints type1 type2)
        )
      )
      (else (cons #f Constraints))
    )
  )
)

(define occurs
  (lambda (type1 type2)
    (display "OCCURS: " ) (display type1) (display ", ") (display type2) (newline)
    (if (equal? type1 type2) #t #f)
  )
)

; Depending on the current symbol on the left side,
; we may need to forcibly create a function type.
(define functionParser
  (lambda (ast E C)
    (
      (lambda (type expr)
        (cond
          ((eq? type `&var) (
                              (lambda (return)
                                (display "I'm a special case!") (display ast) (display " => ") (display return) (newline)
                                ; Now we have the type of the variable, and can associate it with a new yields.
                                (if (isYields? (getPackedTypeExpr return))
                                    return
                                    (pack
                                      (getPackedTypeExpr return)
                                      (insertKeyVal (getPackedConstraints return) (getPackedTypeExpr return) (yields (newtvar) (newtvar)))
                                    )
                                )
                              )
                              ; return
                              (TR ast E C)
                            )
          )
          (else (TR ast E C))
        )
      )
      (car ast)
      (cdr ast)
    )
  )
)

(define TR
  (lambda (ast E C)
    (display "TR Entry: ") (display ast) (newline)
    (if (null? ast) '()
        ; Non null branch, we actually decode.
        (
          (lambda (label)
            ; Here we select based on what label we are viewing
            (cond
              ; APPLY
              ((eq? `&apply label) (begin
                                     (display "Apply") (newline)
                                     (
                                       (lambda (func appl)
                                         (display "func ")(display func) (newline)
                                         (display "appl ")(display appl) (newline)
                                         
                                         
                                         ; We will first parse the type of the lambda
                                         (
                                          (lambda (funcReturn)
                                            (display "func: " )(display func) (display " => ") (display funcReturn) (display "; C: ") (display (getPackedConstraints funcReturn)) (newline)
                                            ; We will now create a modified environment based on the returns 
                                            (
                                              (lambda (EL CL)
                                                (display "CL: ") (display CL) (newline)

                                                ; Now that we have a modified E and C, we can parse through the
                                                ; application to find its types
                                                (
                                                  (lambda (applReturn)
                                                    (display "appl: ")(display appl) (display " => ") (display applReturn) (newline)
                                                    ; With applReturn, we now do an unification of the two to see if we can yield a result.
                                                    (
                                                      (lambda (LeftExpr RightExpr EF CF)
                                                        
                                                         (
                                                           (lambda (in out)
                                                             ; Now, we must find some pairing of *in* and RightExpr that unifies.
                                                             ; If so, our result is out.
                                                             ; Otherwise, our result is failure.
                                                             (
                                                              (lambda (unifier)
                                                                 (
                                                                     (lambda (success finalConstraints)
                                                                       (display "Unify result ") (display success) (display  " with constraints ") (display finalConstraints) (newline)
                                                                       (display "so my type expression is all ") (display (substitute out finalConstraints)) (newline)
                                                                       (display "as a reminder that was for ") (display ast) (newline)
                                                                       (if (eq? success #f)
                                                                          (begin
                                                                            (error 'TR "Could not unify type expression")
                                                                            (pack '() '())
                                                                          )
                                                                          (pack
                                                                            (substitute out finalConstraints)
                                                                            finalConstraints
                                                                          )
                                                                       )   
                                                                     )
                                                                   ; Success
                                                                   (car unifier)
                                                                   ; Final Constraints
                                                                   (cdr unifier)
                                                                 )
                                                               )
                                                               (if (eq? (occurs LeftExpr RightExpr) #t)
                                                                   (cons #f '())
                                                                   (unify in RightExpr CF)
                                                               )
                                                             )
                                                           )
                                                           ; in
                                                           (yieldsIn LeftExpr)
                                                           ; out
                                                           (yieldsOut LeftExpr)
                                                         )
                                                      )
                                                      ; LeftExpr
                                                      (getPackedTypeExpr funcReturn)
                                                      ; RightExpr
                                                      (getPackedTypeExpr applReturn)
                                                      ; EF
                                                      EL
                                                      ; CF
                                                      (mergeC CL (getPackedConstraints applReturn))
                                                    )
                                                  )
                                                  (TR appl EL CL)
                                                )
                                                
                                              )
                                              ; EL is E with e added to it.
                                              (insertKeyVal E func (getPackedTypeExpr funcReturn))
                                              ; CL is C merged with the returned constraints added.
                                              (mergeC C (getPackedConstraints funcReturn))
                                            )
                                          )
                                          ; funcReturn
                                          (functionParser func E C)
                                         )
                                       )
                                       ; func
                                       (cadr ast)
                                       ; application
                                       (caddr ast)
                                    )
                                   )
              )
              ; LAMBDA
              ((eq? `&lambda label) (begin
                                      (display "Lambda") (newline)
                                      (
                                        ; Generate the type of the variable we are using
                                        (lambda (varName varTypeVar)
                                          (
                                            ; Generate the type of the expression
                                            (lambda (E1 C1 expr)
                                              (
                                                ; Now we will recurse and parse expr, using the modified environments.
                                                (lambda (exprReturn)
                                                  ; Our return type is a yielding of varTypeVar -> (getE exprReturn)
                                                  ; The returned constraints are the result of merging all constraints,
                                                  ;     and then extracting the relevant.
                                                  (display "exprreturn: ") (display exprReturn) (newline)
                                                  (display "vartypevar: ") (display varTypeVar) (newline)
                                                  (pack
                                                    ; E
                                                    (yields (substitute varTypeVar (getPackedConstraints exprReturn)) (substitute (getPackedTypeExpr exprReturn) (getPackedConstraints exprReturn)))
                                                    ; C
                                                    (mergeC (mergeC C1 (getPackedConstraints exprReturn)) (getPackedConstraints exprReturn))
                                                  )
                                                )
                                                ; Parse expr
                                                (TR expr E1 C1)
                                              )

                                            )
                                            ; E1 is the merging of E and the type expression for the variable.
                                            (insertKeyVal E varName varTypeVar)
                                            ; C1 is still just C
                                            C
                                            ; expr
                                            (caddr ast)
                                          )
                                        )
                                       (caadr ast) (newtvar)
                                      )
                                   )
              )
              ; CONST
              ((eq? `&const label) (begin
                                     (display "Const") (newline)
                                     ( (lambda (val)
                                         (pack
                                            (cond
                                              ((eq? `false val) `bool)
                                              ((eq? `true val) `bool)
                                              (else `int)
                                            )
                                            C
                                          )
                                        )
                                        ; Argument to val lambda.
                                        (cadr ast)
                                     )
                                   )
              )
              ; VAR
              ((eq? `&var label) (begin
                                     (display "Var") (newline)
                                     (
                                       ; Get the name of this variable
                                       (lambda (varName)
                                         (display (findKey E varName)) (display "<<<") (newline)
                                         (pack
                                           (if (eq? (findKey E varName) '())
                                               ; We need to generate a new variable.
                                               (newtvar)
                                               ; Otherwise, we can just look it up.
                                               (findKey E varName)
                                           )
                                           C
                                         )
                                       )
                                       (cadr ast)
                                     )
                                 )
              )
              (else (error 'TR "Unable to parse label"))
            )
          )
          (car ast)
        )
    )
  )
)

(define debug #t)

(define newline
  (lambda ()
    (if debug (printf "\n") > (void))
  )
)

(define display
  (lambda (x)
    (if debug (printf "~a" x) > (void))
  )
)

(define fdisplay
  (lambda (x)
    (printf "~a" x)
  )
)

(define fnewline
  (lambda ()
    (printf "\n")
  )
)

(define TRec
  (lambda (m)
       ;;; extract type expression from compound return type
    (
      (lambda (return)
        (substitute (getPackedTypeExpr return) (getPackedConstraints return))
      )
      (TR (parse m) init_E init_C)
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Sample Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M1 '((lambda (x) x) 5))
(define M2 '((lambda (x) (sub1 x)) 5))
(define M3 '((lambda (x) 1) ((lambda (x) (x x)) (lambda (x) (x x)))))
(define M4 '((lambda (x) ((and x) #f)) #t))

(define test
  (lambda (Case ExpectedResult)
    (begin
    (set! CurrentCounter 0)
    (if (eq? runtests #f) (fnewline) 
    (
      (lambda (Result)
        (fdisplay "Test [") (fdisplay Case) (fdisplay "]: ") (fnewline)
        (fdisplay "\tExpected: ") (fdisplay ExpectedResult) (fnewline)
        (fdisplay "\tActual: ") (fdisplay Result) (fnewline)
        (fdisplay "\tPassed?: ") (fdisplay (equal? Result ExpectedResult))
        (if (equal? Result ExpectedResult) (fnewline) (error 'Test "Test failed"))
      )
      (TRec Case)
    )
    )
    )
  )
)

(define runtests #f)

(test '((lambda (x) x) 1) 'int)
(test M1 'int)
(test M2 'int)
(test M4 'bool)
; (define N1 '(lambda (x) x))
(test '1 'int)
(test '#f 'bool)
(test '((lambda (x) 1) 5) 'int)
(test '((lambda (x) 1) #f) 'int)
(test '((or #t) #f) 'bool)
(test '(((lambda (x) (lambda (y) ((or (zero? x)) (zero? y)) )) 10) 12) 'bool)
; (test '(((lambda (x) (lambda (y) ((or (zero? x)) (zero? y)) )) #f) 12) 'bool)
; (define N3 '(lambda (x) (x x)))
; (define N4 '((TRec '(lambda (x) (add1 (add1 (add1 (add1 (add1 x))))))) ((lambda (y) 1) 4)))
; (define N6 '(lambda (x) (lambda (y) (x y))))
; (define N7 '(((lambda (x) (lambda (y) ((or x) y) )) 10) 12))
; (define N11 '(((lambda (x) (lambda (y) ((or (zero? x)) (zero? y)) )) 10) 12))
; (define N9 '(((lambda (x) (lambda (y) ((or x) y) )) 10) #f))
; (define N10 '((or 10) #f))


(define Q1 '((lambda (x) (zero? x)) 3))
(define Q4 '((lambda (x) (zero? x)) #f))
(define Q5 '((lambda (x) (zero? x)) (lambda (y) 1)))
(define Q2 '((lambda (x) (x 1)) add1))
(define Q3 '(lambda (x) (zero? x)))
(define Q6 '(lambda (x) (x 1)))

(define Q7 '((lambda (x) (or (zero? x) (zero? (add1 x)))) 1))
(define Q8 '(lambda (x) (or (zero? x) (zero? (add1 x)))))
(define Q9 '(((
             lambda (m)
                          (lambda (x) ((or (zero? x)) (zero? (add1 m))))
                                       ) 1) #f)
             
  ) ;; We aren't doing this right, _a1 and _a2 should be resolve to be integers!

(define Q10 '(
               (lambda (x) (lambda (y) (add1 (x 5)))) add1
             )
)

(test '(lambda (x) x) '(_a1 -> _a1))
(test '1 'int)
(test '#f 'bool)
; (test '((TRec '(lambda (x) (add1 (add1 (add1 (add1 (add1 x))))))) ((lambda (y) 1) 4)))
;(test '(lambda (x) (lambda (y) (x y))))
(test '((lambda (x) 1) 5) 'int)
;;;; (test '(((lambda (x) (lambda (y) ((or x) y) )) 10) 12))
(test '(((lambda (x) (lambda (y) ((or (zero? x)) (zero? y)) )) 10) 12) 'bool)
(test '(lambda (x) (lambda (y) ((or (zero? x)) (zero? y)) )) '(int -> (int -> bool)))
;(define N9 '(((lambda (x) (lambda (y) ((or x) y) )) 10) #f))
(test '((or #t) #f) 'bool)
;(define N10 '((or 10) #f))


(test '((lambda (x) (lambda (y) (add1 (x 5)))) sub1) '(_a2 -> int))

; (test '((lambda (x) (x #f)) (lambda (y) (lambda (z) ((or y) z)))))

(TRec '((lambda (x) (x #f)) (lambda (y) (lambda (z) ((or y) z)))))