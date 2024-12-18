#lang eopl

(require rackunit racket/list "interpretador.rkt")

;; Pruebas basicas

(define pruebas-if
   (lambda ()
      (define exp1 (scan&parse "if <(1, 10) then 1 else 0 end"))
      (check-equal? (evaluar-programa exp1) 1 "Error en la prueba 1")

      (define exp2 (scan&parse "if <(10, 1) then 1 else 0 end"))
      (check-equal? (evaluar-programa exp2) 0 "Error en la prueba 2")   
   )
)

(define pruebas-let
   (lambda ()
      (define exp1 (scan&parse "let x = 1 in x end"))
      (check-equal? (evaluar-programa exp1) 1 "Error en la prueba 1")

      (define exp2 (scan&parse "let x = 1 in let y = 2 in +(x,y) end end"))
      (check-equal? (evaluar-programa exp2) 3 "Error en la prueba 2")

      (define exp3 (scan&parse "let x = 1 in let y = 2 in -(y,x) end end"))
      (check-equal? (evaluar-programa exp3) 1 "Error en la prueba 3")

      (define exp4 (scan&parse "let x = 1, y = 2 in +(x,y) end"))
      (check-equal? (evaluar-programa exp4) 3 "Error en la prueba 4")

      (define exp5 (scan&parse "let x = 1, y = let x = 2 in +(x,1) end in +(x,y) end"))
      (check-equal? (evaluar-programa exp5) 4 "Error en la prueba 5")

      (define exp6 (scan&parse "let x = 1 in set x := 2 end"))
      (check-equal? (evaluar-programa exp6) 1 "Error en la prueba 6")

      (define exp7 (scan&parse "let x = 1 in begin set x := 2; x end end"))
      (check-equal? (evaluar-programa exp7) 2 "Error en la prueba 7")
   )
)

(define pruebas-var
   (lambda ()
      (define exp1 (scan&parse "var x = 1 in x end"))
      (check-equal? (evaluar-programa exp1) 1)

      (define exp2 (scan&parse "var x = 1 in var y = 2 in +(x,y) end end"))
      (check-equal? (evaluar-programa exp2) 3)

      (define exp3 (scan&parse "var x = 1 in var y = 2 in -(y,x) end end"))
      (check-equal? (evaluar-programa exp3) 1)

      (define exp4 (scan&parse "var x = 1, y = 2 in +(x,y) end"))
      (check-equal? (evaluar-programa exp4) 3 "Error en la prueba 4")

      (define exp5 (scan&parse "var x = 1, y = var x = 2 in +(x,1) end in begin set x := 3; +(x,y) end end"))
      (check-equal? (evaluar-programa exp5) 6 "Error en la prueba 5")
   )
)

(define pruebas-letrec
   (lambda ()
      (define exp1 (scan&parse "letrec f(x) = if <(x, 1) then 1 else *(x, apply f(-(x, 1))) end in apply f(5) end"))
      (check-equal? (evaluar-programa exp1) 120 "Error en la prueba 1")

      (define exp2 (scan&parse "letrec f(x) = if is(x,0) then 0 elseif is(x,1) then 1 else +(apply f(-(x, 1)), apply f(-(x, 2))) end in apply f(5) end"))
      (check-equal? (evaluar-programa exp2) 5 "Error en la prueba 2")

      (define exp3 (scan&parse "let x = proc(y) *(y, y) end in letrec f(a,b) = +(b, apply x(a)) in apply f(2,3) end end"))
      (check-equal? (evaluar-programa exp3) 7 "Error en la prueba 3")
   )
)

(define pruebas-begin
   (lambda ()
      (define exp1 (scan&parse "begin 1; 2; 3 end"))
      (check-equal? (evaluar-programa exp1) 3 "Error en la prueba 1")

      (define exp2 (scan&parse "begin let x = letrec f(y) = if is(y,0) then 1 else +(y, apply f(-(y,1))) end in apply f(5) end in +(x,2) end end"))
      (check-equal? (evaluar-programa exp2) 18 "Error en la prueba 2")

      (define exp3 (scan&parse "begin var x = proc(name) &(\"Hola \", name) end in apply x(\"Manuel\") end end"))
      (check-equal? (evaluar-programa exp3) "Hola Manuel" "Error en la prueba 3")
   )
)

(define run-pruebas
   (lambda ()
      (pruebas-if)
      (pruebas-let)
      (pruebas-var)
      (pruebas-letrec)
      (pruebas-begin)
   )
)

(run-pruebas)