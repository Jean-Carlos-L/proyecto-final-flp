#lang eopl

(require rackunit
         racket/list
         "interpretador.rkt")

;; Pruebas basicas

(define pruebas-if
  (lambda ()
    (define exp1 (scan&parse "if <(1, 10) then 1 else 0 end"))
    (check-equal? (evaluar-programa exp1) 1 "Error en la prueba 1")

    (define exp2 (scan&parse "if <(10, 1) then 1 else 0 end"))
    (check-equal? (evaluar-programa exp2) 0 "Error en la prueba 2")))

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
    (check-equal? (evaluar-programa exp7) 2 "Error en la prueba 7")))

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

    (define exp5
      (scan&parse "var x = 1, y = var x = 2 in +(x,1) end in begin set x := 3; +(x,y) end end"))
    (check-equal? (evaluar-programa exp5) 6 "Error en la prueba 5")))

(define pruebas-letrec
  (lambda ()
    (define exp1
      (scan&parse
       "letrec f(x) = if <(x, 1) then 1 else *(x, apply f(-(x, 1))) end in apply f(5) end"))
    (check-equal? (evaluar-programa exp1) 120 "Error en la prueba 1")

    (define exp2
      (scan&parse
       "letrec f(x) = if is(x,0) then 0 elseif is(x,1) then 1 else +(apply f(-(x, 1)), apply f(-(x, 2))) end in apply f(5) end"))
    (check-equal? (evaluar-programa exp2) 5 "Error en la prueba 2")

    (define exp3
      (scan&parse
       "let x = proc(y) *(y, y) end in letrec f(a,b) = +(b, apply x(a)) in apply f(2,3) end end"))
    (check-equal? (evaluar-programa exp3) 7 "Error en la prueba 3")))

(define pruebas-proc
   (lambda ()
      (define exp1 (scan&parse "proc() 2 end"))
      (check-equal? (evaluar-programa exp1)closure "proc simple, retorna closure") 

      (define exp2 (scan&parse "proc(r) 9 end"))
      (check-equal? (evaluar-programa exp2)closure "proc simple con 1 variable, retorna closure")
      
      (define exp3 (scan&parse "let x=5, y=proc(z) +(x,z) end in y end"))
      (check-equal? (evaluar-programa exp3)closure "prueba de proc con let")

      (define exp4 (scan&parse "let x=8 in let y=proc(z) +(x,z) end in let w=y in apply w(2) end end end"))
      (check-equal? (evaluar-programa exp4)10 "Prueba procedimientos con let y apply")

      (define exp5 (scan&parse "proc(a,b,c,r) *(2,3) end"))
      (check-equal? (evaluar-programa exp5)closure "multiplicacion procedimiento")     
   )
)

(define pruebas-begin
  (lambda ()
    (define exp1 (scan&parse "begin 1; 2; 3 end"))
    (check-equal? (evaluar-programa exp1) 3 "Error en la prueba 1")

    (define exp2
      (scan&parse
       "begin let x = letrec f(y) = if is(y,0) then 1 else +(y, apply f(-(y,1))) end in apply f(5) end in +(x,2) end end"))
    (check-equal? (evaluar-programa exp2) 18 "Error en la prueba 2")

    (define exp3
      (scan&parse "begin var x = proc(name) &(\"Hola \", name) end in apply x(\"Manuel\") end end"))
    (check-equal? (evaluar-programa exp3) "Hola Manuel" "Error en la prueba 3")))

(define pruebas-for
  (lambda ()
    (define exp1
      (scan&parse
       "let prueba = object { x=> meth(s,m) +(m,2) end } in for i=0 to 10 do
       send prueba.x(i) end end"))
    (check-equal? (evaluar-programa exp1) '(2 3 4 5 6 7 8 9 10 11 12) "Prueba for y objetos")
   ;  Haz prueba con doble for
    (define exp2
      (scan&parse
       "for i=0 to 3 do
       for j=0 to 3 do +(i,j) end end"))

      (check-equal? (evaluar-programa exp2) 
      '((0 1 2 3) (1 2 3 4) (2 3 4 5) (3 4 5 6)) "Prueba doble for")
    ))


(define pruebas-object
  (lambda ()
    (define exp1
      (scan&parse
       "let prueba = object{x=>5 y=>6 z=>meth(s) +( get s.x, get s.y) end} 
       in send prueba.z() end"))
    (check-equal? (evaluar-programa exp1) 11 "Prueba objetos con get y send")
    
    (define exp2
      (scan&parse
       "let prueba = object{x=>5 y=>6 z=>meth(s,p) begin update s.x := p; get s.x end end} 
       in send prueba.z(1) end"))
    (check-equal? (evaluar-programa exp2) 1 "Prueba objetos con updated y send")

      (define exp3
         (scan&parse
         "let prueba = object{y=>6 z => meth(s) get s.y end 
         w => meth(s) update s.y := 8 end x => meth(s) let adentro = clone(s) in
         begin send adentro.w(); send adentro.z() end end end }
         in
         begin  
         send prueba.w();
         send prueba.z()
         end
         end"))
      (check-equal? (evaluar-programa exp3) 8 "Prueba objetos con clone y send")
    ))

(define run-pruebas
  (lambda ()
    (pruebas-if)
    (pruebas-let)
    (pruebas-var)
    (pruebas-letrec)
    (pruebas-proc)
    (pruebas-begin)
    (pruebas-for)
    (pruebas-object)))

(run-pruebas)
