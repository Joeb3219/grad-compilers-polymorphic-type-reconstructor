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
    (and (bool -> (bool -> bool)))
    (or (bool -> (bool -> bool)))
    )
  )

(define init_C '()) ;; Type Constraints


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type reconstruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reduce
  (lambda (op l id)
    (if (null? l)
       id
(op (car l) (reduce op (cdr l) id)) )))

(define printE
  (lambda (E)
    (cond
      ((eq? E '()) (display ""))
      (else (begin
              (display (caar E)) (display "  =>  ") (display (cdar E)) (display "; ")
              (printE (cdr E))
            )
       )
    )  
  )
)

(define printPacked
  (lambda (P)
    (display "Pack: e:") (display (getE P)) (newline)
    (display "C: ")
    (printE (getC P))
   )
)

; I: The input type of the yields
; O: Output type of the yields
(define newYields
  (lambda (I O)
    (cons I (cons `-> (cons O '())))
  )
)

(define isYields?
  (lambda (R)
    (cond
      ((null? R) #f)
      ((eq? '() (cdr R)) #f)
      ((not (pair? (cdr R))) #f)
      ((eq? (cadr R) `->) #t)
      (else #f)
    )
  )
)

(define yieldsIn
  (lambda (R)
    (if (isYields? R) (car R) '())
  )
)

(define yieldsOut
  (lambda (R)
    (if (isYields? R) (caddr R) '())
  )
)

; Go through C2 until we find an entry in C2 s.t. C2 is the yieldsIn of yields.
; if we find such a match, we immediately return yieldsOut of yields.
; If no such match is found, we return an empty list.
(define findMatch
  (lambda (yields C2)
    (cond
      ((eq? C2 '()) '())
      ((null? C2) '())
      ((eq? (yieldsIn yields) (car C2)) (yieldsOut yields))
      (else (findMatch yields (cdr C2)))

    )
  )
)

(define yieldsMatch?
  (lambda (e a)
    (cond
      ((not (isYields? a)) #f)
      ((eq? (yieldsOut a) e) #t)
      (else #f)
    )
  )
)

(define unifyPack
  (lambda (C P1 P2 ast)
    (uPack
     
     (merge (getC P1) (merge C (getC P2)))
     ast
    )
  )
)

(define union
  (lambda (E e1 e2 C)
    (pack E C)
  )
)

(define nodeEqual?
  (lambda (e1 e2)
    #f
  )
)

(define unifier
  (lambda (E P1 A P2 B)
    (printPacked P1) (newline)
    (printPacked P2) (newline)
    (printPacked (mergeReturn E P1 P2)) (newline)
    (display "unifier");(unify (mergeReturn P1 P2) A B)
  )
)

(define unify
  (lambda (M e1 e2)
    (cond
      ((nodeEqual? e1 e2) #t)
      (else #f)
    )
  )
)

(define error
  (lambda (A B)
    (newline)
    (display "ERROR: ")
    (display A) (display " -> ")
    (display B)
    (newline)
  )
)

; Searches for K._ and returns _ in L.
(define search
  (lambda (L k)
    (cond
      ((null? L) '())
      ((eq? K (caar L)) (cdar L))
      (else (search (cdr L) K))
    )
  )
)

; Searches for K._ and returns K._ in L.
(define searchFull
  (lambda (L k)
    (cond
      ((null? L) '())
      ((eq? K (caar L)) (car L))
      (else (searchFull (cdr L) K))
    )
  )
)

; Inserts only if K isn't already in L.
(define insNew
  (lambda (L K V)
    (if (eq? '() (search L K)) (insert L K V) L)
  )
)

; Inserts the pair K.V into L.
(define insert
  (lambda (L K V)
    (cons (cons K V) L)
  )
)

; Returns the type expression e from the return type. This is just one item.
(define getE
  (lambda (P)
    (car P)
  )
)

; Returns the type constraints C from the return type. This is a list.
(define getC
  (lambda (P)
    (cdr P)
  )
)

; Packs a type variable e and a type constraint list C into a list.
; Retrieve values through getE and getC.
(define pack
  (lambda (E C)
    (cons E C)
  )
)

; Packs the expression K's type expression & all constraints
; IE: E contains all type variables, but we will extract only K's.
(define uPack
  (lambda (E C K)
    (pack
     (searchFull E K)
     C
    )
  )
)

(define mergeWithC
  (lambda (P C)
    (pack
      (getE P)
      (merge (getC P) C)
    )
  )
)

; This is meant to combine multiple returns into one big pack.
; Given a currently Packed environment P, function F, and extraction key K,
; will pack the result of calling F w/ the E of P, and extracting key K.
; Merges the containers of F(E) w/ P_C
(define pfPack
  (lambda (P F K)
    (mergeWithC
      (ufpack
       (getE P)
       F
       K
      )
      (getC P)
    )
  )
)

; Given a type environment E, and a function F, and a key K
; We compute C with E as an argument, and extract the type expression e from E w/ a key K
; We then return the packed version.
(define ufPack
  (lambda (E F K)
    (uPack E (F E) K)
  )
)

; Insert a type expression T into C with a key L.
(define cIns
  (lambda (C L T)
    (if (eq? (search C L) '()) (insert C L T) C)
  )
)

; Returns #t if I is in L, or #f otherwise.
(define inList?
  (lambda (L I)
    (cond
      ((null? L) #f)
      ((eq? (car L) I) #t)
      (else #f)
    )
  )
)

; Merges two lists, A and B, into one list.
; Only adds non-duplicate entries of B into A.
(define merge
  (lambda (A B)
    (reduce (lambda (N C)
                            (if (inList? N C) N (cons N C))
                          ) A B)
  )
)

(define mergeE
  (lambda (E m)
    (insert E (car m) (cdr m))
  )
)

(define mergeReturn
  (lambda (E A B)
    (pack
      (mergeE (mergeE E (getE A)) (getE B))
      (merge (getC A) (getC B))
    )
  )
)

; Inserts a new variable into E and generates its type variable IFF it isn't already in the system.
(define eInsTerm
  (lambda (E L)
    (if (eq? (search E L) '()) (insert E L (newtvar)) E)
  )
)

(define lastIns
  (lambda (L)
    (cond
      ((null? L) '())
      ((eq? '() (cdr L)) (car L))
      (else (lastIns (cdr L)))
    )
  )
)

; Solves a constraint m that is in C.
; If m isn't in C, or can't be solved, returns '().
; Otherwise, returns the updated type constraints.
(define solve
  (lambda (C m)
    (if (not (inList? C m)) '()
        ((lambda (L R)
          (display "Attempting to solve the constraint ") (display L) (display " => ") (display R) (newline)
          C
        ) ((car m) (cdr m)))
    )
  )
)

 ;(lambda (E C1 ast VRP V)
  ;  ((lambda (C)
   ;    (uPack
    ;    (insert E ast (newYields (search VP V) (cdr (lastIns RP))))
     ;   (merge (getC VP) (merge C RP))
      ;  ast
 ;     )
  ;  ) (getC VRP) )
   ;)
; Similar to eInsTerm, but inserts a lambda expression.
; V is the variable name, which will be used to create the yields in
; R is the return name, which will be used to create the yields out
(define eInsLambda
  (lambda (E1 C1 ast VRP V R)
    ((lambda (C)
       ((lambda (E)
          (uPack
            E
            (insert C (search E ast) (newYields (search (getE VRP) V) (search (getE VRP) R)))
            ast
          )
       ) (insert E1 ast (newtvar)))
    ) (merge C1 (getC VRP)) )
  )
)

; Executes F and merges the result into P.
; Returns the packed return type that results.
; Only executes and merges if K is not already in E. 
(define execAndMerge
  (lambda (P K F)
    (mergeReturn P (F))
  )
)

; Returns in format of (typeExpression constraints)
; type expression is in terms of type variables, ie x -> beta
; the constraints map a type variable to their definition
; as an example, if there are (x -> alpha) and (y -> beta) in E,
; and (x -> int), (x -> (int -> bool)), and (y -> int) in C,
; There are several possible configurations to return: 
(define TR
  (lambda (ast E C)

    ; Check if this ast is empty
    (if (null? ast)
        (cons E (cons C '())) ; Return the current list
        (begin ; Otherwise, let's dive in.
          (display (car ast))
          (cond
            ((eq? (car ast) `&apply) (begin
                                       ; Apply works roughly as follows
                                       ; Parse the lambda and get its type expression.
                                       ; Next, parse the argument
                                       ; Now, we try to see if there are any argument types M that satisy M->N.
                                       ; If so, we return the type N as our type expression, and the constraints.
                                       ; If not, we signal an error.
                                       (display "I'm apply!") (newline)
                                       (unifier
                                        E
                                        (TR (cadr ast) E C) ; Parse lambda
                                        (cadr ast)
                                        (TR (caddr ast) E C) ; Parse argument
                                        (caddr ast)
                                       )
                                     )
            )
            ((eq? (car ast) `&var) (begin
                                     (display "I'm a var!") (newline)
                                     (uPack
                                      ; First, let's add a definition for the variable to our E
                                      (eInsTerm E (cadr ast))
                                      ; Now, we have no idea what its type is, so we leave the type as C.
                                      C
                                      (cadr ast)
                                     )
                                   )
            )
            ((eq? (car ast) `&const) (begin
                                       (display "I'm a const!") (newline)
                                       (display ast) (newline)
                                       (ufPack
                                         ; Add our e to this if it doesn't exist yet.
                                         (eInsTerm E (cadr ast))
                                         ; Add our c to this if it doesn't exist it
                                         (lambda (E)
                                           (cIns C (search E (cadr ast))
                                                (cond
                                                  ((eq? `false (cadr ast)) `bool)
                                                  ((eq? `true (cadr ast)) `true)
                                                  (else `int)
                                                )
                                          )
                                         )
                                         (cadr ast)
                                       )
                                     )
            )
            ((eq? (car ast) `&lambda) (begin
                                        ; (ufPack (E F K)), (pfPack (P F K))
                                        ; (lambda (E C ast VP V RP R)
                                        (display "I'm a lambda!")
                                        (eInsLambda
                                         E
                                         C
                                         ast
                                         ; VRP
                                         ((lambda (VP)
                                            (mergeReturn E VP (TR (caddr ast) E C))
                                         ) (TR (cadr ast) E C))
                                         (caadr ast)
                                         (car (cdaddr ast))
                                        )
                                       )
            )
            (else (begin
                                     (display "I'm a var!") (newline)
                                     (uPack
                                      ; First, let's add a definition for the variable to our E
                                      (eInsTerm E (car ast))
                                      ; Now, we have no idea what its type is, so we leave the type as C.
                                      C
                                      (car ast)
                                     )
                                   )
            )
          )

        )
    )
    
  )
) ;; YOUR CODE GOES HERE

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

(define N1 '(lambda (x) x))

;; Basic testing commands
;; (search (getE (TRec '6)) '6) -> int

(TRec N1)