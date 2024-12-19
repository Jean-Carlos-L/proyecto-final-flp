; Proyecto 1 - Lenguajes de Programación
; Autores:
; Diana Marcela Oviedo 2459375
; Juan Camilo García Saenz 2259416
; Jean Carlos Lerma Rojas 2259305
; Juan Pablo Ospina Vanegas 2411023

#lang eopl

;;define las reglas léxicas para identificar diferentes tipos de tokens en el lenguaje
(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("(" "*" (arbno (not #\newline)) "*" ")") skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (arbno (or digit letter whitespace)) "\"") string)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
)

;; define las reglas gramaticales del lenguaje, especificando cómo se pueden combinar los tokens para formar expresiones válidas.
(define especificacion-gramatical
  '((programa (expresion) a-program)
    (expresion (numero) literal-expresion)
    (expresion (identificador) variable-expresion)
    (expresion (caracter) caracter-expresion) ;; pendiente
    (expresion (cadena) cadena-expresion)
    (expresion ("ok") ok-expresion)
    ;; Ligaduras
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end")
               var-expresion)
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end")
               let-expresion)
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion "end") letrec-expresion)
    (expresion ("set" identificador ":=" expresion) set-expresion)

    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-expresion)
    ;; Condicionales
    (expresion ("if" expresion "then" expresion (arbno "elseif" expresion "then" expresion) "else" expresion "end") if-expresion)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-expresion)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-expresion)
    ;; For
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-expresion)

    ;; Primitivas Aritmeticas
    (expresion (primitiva "(" (separated-list expresion ",") ")") primitiva-expresion)
    (primitiva ("+") suma-primitiva)
    (primitiva ("-") resta-primitiva)
    (primitiva ("*") multiplicacion-primitiva)
    (primitiva ("%") modulo-primitiva)
    (primitiva ("&") concatenacion-primitiva)
    ;; Primitivas Booleanas
    (expresion (boolenas-expresion) boolenas-expresion-expresion)
    (boolenas-expresion ("true") true-booleana)
    (boolenas-expresion ("false") false-booleana)

    (expresion (primitiva-booleana "(" (separated-list expresion ",") ")") primitiva-booleana-expresion)
    (primitiva-booleana ("<") menor-expresion)
    (primitiva-booleana (">") mayor-expresion)
    (primitiva-booleana ("<=") menor-igual-expresion)
    (primitiva-booleana (">=") mayor-igual-expresion)
    (primitiva-booleana ("is") igual-expresion)

    ;; Operaciones booleanas
    (expresion (operaciones-booleanas "(" (separated-list expresion ",") ")") operaciones-booleanas-expresion)
    (operaciones-booleanas ("not") not-booleana)
    (operaciones-booleanas ("and") and-booleana)
    (operaciones-booleanas ("or") or-booleana)
    ;; Expresion booleanas
    
    ;;Objetos
    (expresion ("object" "{" (arbno identificador "=>" expresion) "}") obj-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("meth" "(" identificador (arbno "," identificador) ")" expresion "end")
               meth-expresion)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")")
               send-expresion)
    (expresion ("clone" "(" identificador ")") clone-expresion)))

;; Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Procedimientos

(define-datatype procedimiento procedimiento?
  (closure 
    (variable (list-of symbol?)) 
    (cuerpo expresion?) 
    (ambiente ambiente?)
  )
)
#|
Function: aplicar-procedimiento
Description: Esta función aplica un procedimiento a una lista de argumentos en un ambiente extendido.
Parameters:
  - procd: El procedimiento a aplicar.
  - args: La lista de argumentos a pasar al procedimiento.
Returns: El resultado de evaluar el cuerpo del procedimiento en el ambiente extendido con los valores de los argumentos.
|#
(define aplicar-procedimiento
	(lambda (procd args)
		(cases procedimiento procd
			(closure (variable cuerpo ambiente)
				;; se evalua el cuerpo de el procedimiento en un ambiente extendido con los valores de los argumentos
				(evaluar-expresion cuerpo (ambiente-extendido variable args ambiente) cuerpo)))))               
;; Fin de Procedimientos 


;; Definimos los ambientes
(define-datatype ambiente
                 ambiente?
                 (ambiente-vacio)
                 (ambiente-extendido-referencia (lista-ids (list-of symbol?))
                                                (lista-valores vector?)
                                                (old-env ambiente?)))
;;Ambiente extendido con referencias a procedimientos
(define ambiente-extendido
  (lambda (lista-ids lista-valores old-env)
    (ambiente-extendido-referencia lista-ids (list->vector lista-valores) old-env)))
;;Ambiente extendido recursivo 
(define ambiente-extendido-recursivo
  (lambda (nombres-procedimientos lista-ids cuerpos old-env)
    (let 
      ([vectores-clausuras (make-vector (length nombres-procedimientos))])
      (letrec
        (
          [ambiente (ambiente-extendido-referencia nombres-procedimientos vectores-clausuras old-env)]
          [obtener-clausuras
            (lambda (lista-ids cuerpos i)
              (cond
                [(null? lista-ids) ambiente]
                [else
                  (begin
                    (vector-set! vectores-clausuras i (closure (car lista-ids) (car cuerpos) ambiente))
                    (obtener-clausuras (cdr lista-ids) (cdr cuerpos) (+ i 1)))])
                  )
          ]
        )
        (obtener-clausuras lista-ids cuerpos 0)
      )
    )
  )
)

(define-datatype referencia referencia? (a-ref (i number?) (v vector?)))

(define primitiva-deref (lambda (ref) (cases referencia ref (a-ref (i v) (vector-ref v i)))))

(define deref (lambda (ref) (primitiva-deref ref)))

(define primitva-setref!
  (lambda (ref valor) (cases referencia ref (a-ref (i v) (vector-set! v i valor)))))
(define setref! (lambda (ref valor) (primitva-setref! ref valor)))

(define aplicar-ambiente (lambda (ambi variable) (deref (aplicar-ambiente-referencia ambi variable))))

(define aplicar-ambiente-referencia
  (lambda (ambi variable)
    (cases
     ambiente
     ambi
     (ambiente-vacio () (eopl:error "No se encuentra la variable" variable))
     (ambiente-extendido-referencia
      (lista-ids lista-vector old-env)
      (letrec ([buscar-variable (lambda (lista-ids lista-vector i)
                                  (cond
                                    [(null? lista-ids) (aplicar-ambiente-referencia old-env variable)]
                                    [(equal? (car lista-ids) variable) (a-ref i lista-vector)]
                                    [else (buscar-variable (cdr lista-ids) lista-vector (+ i 1))]))])
        (buscar-variable lista-ids lista-vector 0))))))

(define operacion-primitiva
  (lambda (lval operacion identidad)
    (cond
      [(null? lval) identidad]
      [else (operacion (car lval) (operacion-primitiva (cdr lval) operacion identidad))])))
;;primitiva
#|
Function: evaluar-primitiva
Description: Esta función evalúa una primitiva con una lista de valores.
Parameters:
  - prim: La primitiva a evaluar.
  - lval: la lista de valores a evaluar.
Returns: El resultado de evaluar la primitiva con los valores.
|#
(define evaluar-primitiva
  (lambda (prim lval)
    (cases primitiva
           prim
           (suma-primitiva () (operacion-primitiva lval + 0))
           (resta-primitiva () (operacion-primitiva lval - 0))
           (multiplicacion-primitiva () (operacion-primitiva lval * 1))
           (modulo-primitiva () (modulo (car lval) (cadr lval)))
           (concatenacion-primitiva () (string-append (car lval) (cadr lval)))
          )))
;;primitiva booleana
#|
Function: evaluar-primitiva-booleana
Description: Esta función evalúa una primitiva booleana con una lista de valores.
Parameters:
  - prim: La primitiva booleana a evaluar.
  - lval: La lista de valores a evaluar.
Returns: El resultado de evaluar la primitiva booleana con los valores.
|#
(define evaluar-primitiva-booleana
  (lambda (prim lval)
    (cases primitiva-booleana prim
      (menor-expresion () (<(car lval) (cadr lval)))
      (menor-igual-expresion () (<= (car lval) (cadr lval)))
      (mayor-expresion () (>(car lval) (cadr lval)))
      (mayor-igual-expresion () (>=(car lval) (cadr lval)))
      (igual-expresion () (=(car lval) (cadr lval)))
    )
  )
)

;; Objetos
#| 
Datatype: objeto
Description: Este datatype representa un objeto con sus propiedades y métodos específicos. 

Datatype: metodo
Description: Este datatype representa un método que puede ser invocado en el objeto, especificando su comportamiento y funcionalidad.
|#
(define-datatype objeto objeto? (un-objeto (fields (list-of symbol?)) (exps vector?)))

(define-datatype metodo metodo?
  (un-metodo (args (list-of symbol?)) (body expresion?)))

; Esta función toma un método `mth` y devuelve sus argumentos.
;; Utiliza la construcción `cases` para coincidir con la estructura del método y extraer los argumentos.
;; Parámetros:
;; - `mth`: El método del cual extraer los argumentos.
;;
;; Retorna:
;; - Los argumentos del método.
(define metodo-args
  (lambda (mth)
    (cases metodo mth
      (un-metodo (args body) args))))

; Esta función toma un método `mth` y devuelve su cuerpo.
;; Parámetros:
;; - `mth`: El método del cual extraer el cuerpo.
;;
;; Retorna:
;; - El cuerpo del método.
(define metodo-body
  (lambda (mth)
    (cases metodo mth
      (un-metodo (args body) body))))


;; Esta función recupera el valor de un atributo de un objeto.
;; Utiliza la construcción `cases` para coincidir con la estructura del objeto y encontrar la posición del atributo.
;; Si se encuentra el atributo, evalúa su expresión en el entorno dado.
;; Si no se encuentra el atributo, lanza un error.
;; 
(define valor-atributo
  (lambda (sym obj env old-expresion)
    (cases objeto
           obj
           (un-objeto (fields exps)
                      (let ([pos (find-position sym fields)])

                        (if (number? pos)
                            (let ([value (vector-ref exps pos)])

                              (evaluar-expresion value env old-expresion))

                            (eopl:error 'objeto "Field ~s has not been defined" sym)))))))

;; Esta función encuentra la posición de un símbolo en una lista de símbolos.
;; Delegar la búsqueda real a `list-find-position`.
(define find-position (lambda (sym los) (list-find-position sym los)))

;; `list-find-position`
;; Esta función encuentra la posición de un símbolo en una lista de símbolos.
(define list-find-position (lambda (sym los) (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else
       (let ([list-index-r (list-index pred (cdr ls))])
         (if (number? list-index-r) (+ list-index-r 1) #f))])))

;; Esta función actualiza el valor de un atributo en un objeto.
;; Utiliza la construcción `cases` para coincidir con la estructura del objeto y encontrar la posición del atributo.
(define actualizar-atributo
  (lambda (obj id new-value)
    (cases objeto
           obj
           (un-objeto (fields exps)
                      (let ([pos (find-position id fields)])

                        (if (number? pos)

                            (vector-set! exps pos new-value)

                            (eopl:error 'update "El campo ~s no ha sido definido" id)))))))
;;Validar set
#|
Function: validar-set
Description: Esta función valida si una expresión es un set.
Parameters:
  - exp: La expresión a validar.
Returns: #t si la expresión es un set, #f en caso contrario.
|#
(define validar-set
  (lambda (exp)
    (cases expresion exp
      (set-expresion (variable exp) #t)
      (else #f)
    )
  )
)

(define validar-let
  (lambda (exp)
    (cases expresion exp
      (let-expresion (lista-ids lista-expresiones exp) #t)
      (else #f)
    )
  )
)

(define evaluar-expresion
  (lambda (exp ambi old-expresion)
    (cases expresion exp
      (literal-expresion (dato) dato)
      (variable-expresion (variable) (aplicar-ambiente ambi variable))
      (caracter-expresion (caracter)  caracter)
      (cadena-expresion (cadena) (substring cadena 1 (- (string-length cadena) 1)))
      (for-expresion
        (var start end body)
        (let loop ([i (evaluar-expresion start ambi old-expresion)]
                  [resultados '()])
          (if (<= i (evaluar-expresion end ambi old-expresion))
              (let ([resultado (evaluar-expresion body (ambiente-extendido (list var) (list i) ambi) old-expresion)])
                (loop (+ i 1) (cons resultado resultados)))
              (reverse resultados)))
      )
      (ok-expresion () 'ok)
      (boolenas-expresion-expresion (exp1)
        (cases boolenas-expresion exp1 
          (true-booleana () #t) 
          (false-booleana () #f)
          ;;; (list-primitivas-booleanas (lista-valores expresion) lista-valores) 
        )
      )
      (primitiva-booleana-expresion (exp lval)
        (let
          ([lval (map (lambda (exp) (evaluar-expresion exp ambi old-expresion)) lval)])
          (evaluar-primitiva-booleana exp lval)
        )
      )
      (primitiva-expresion (primitiva lista-valores)
        (let 
          ([lval (map (lambda (exp) (evaluar-expresion exp ambi old-expresion)) lista-valores)])
          (evaluar-primitiva primitiva lval)
        )
      )
      (operaciones-booleanas-expresion (lista-operaciones-booleanas lista-valores)
        (let 
          ([lval (map (lambda (exp) (evaluar-expresion exp ambi old-expresion)) lista-valores)])
          (cases operaciones-booleanas lista-operaciones-booleanas
            (not-booleana () (not (car lval)))
            (and-booleana () (and (car lval) (cadr lval)))
            (or-booleana () (or (car lval) (cadr lval)))
          )
        )
      )
      (var-expresion (lista-ids lista-expresiones exp)
        (let 
          ([valores (map (lambda (exp) (evaluar-expresion exp ambi old-expresion)) lista-expresiones)])
          (evaluar-expresion exp (ambiente-extendido lista-ids valores ambi) old-expresion)
        )
      )
      (let-expresion (lista-ids lista-expresiones sub-exp)
          (if (validar-set sub-exp)
              (eopl:error 'evaluar-expresion "ERROR: No se puede hacer un set en un let")
              (let 
                ([valores (map (lambda (exp) (evaluar-expresion exp ambi old-expresion)) lista-expresiones)])
                (evaluar-expresion sub-exp (ambiente-extendido lista-ids valores ambi) old-expresion)
              )
          )
      )
      (letrec-expresion (lista-ids lista-params cuerpos exp)
        (let
          ([ambiente (ambiente-extendido-recursivo lista-ids lista-params cuerpos ambi)])
          (evaluar-expresion exp ambiente old-expresion)
        )
        ;;; (evaluar-expresion exp (ambiente-extendido-recursivo lista-ids lista-params cuerpos ambi) old-expresion)
      )
     ;; Procedimientos expresiones
     (proc-expresion (variable cuerpo) (closure variable cuerpo ambi))
     ;; Apply
     (apply-expresion(identificador args)
     ;;se aplica el ambiente para conocer el valor del id y se liga a proced
				(let ((procedimiento (aplicar-ambiente ambi identificador))
					;;se evaluan los argumentos y se ligan a args
					(args (evaluar-rands args ambi)))
					;;si proced es un procedimiento, se usa la funcion para aplicar un proc con proced y args
					(if (procedimiento? procedimiento) (aplicar-procedimiento procedimiento args)
						;;en el caso contrario se muestra un error
						(eopl:error 'evaluar-expresion "ERROR: NO es un procedimiento -> ~s" identificador))))
     
     (begin-expresion
      (exp lista-expresiones)
      (if (null? lista-expresiones)
          (evaluar-expresion exp ambi old-expresion)
          (begin
            (evaluar-expresion exp ambi old-expresion)
            (letrec ([evaluar-begin (lambda (lista-expresiones)
                                      (cond
                                        [(null? (cdr lista-expresiones))
                                         (evaluar-expresion (car lista-expresiones) ambi old-expresion)]
                                        [else
                                         (begin
                                           (evaluar-expresion (car lista-expresiones) ambi old-expresion)
                                           (evaluar-begin (cdr lista-expresiones)))]))])
              (evaluar-begin lista-expresiones)))))
     (set-expresion (variable exp)
      (begin
        (if (validar-let old-expresion)
          (eopl:error 'evaluar-expresion "ERROR: No se puede hacer un set en un let")
          (setref! 
            (aplicar-ambiente-referencia ambi variable)
            (evaluar-expresion exp ambi old-expresion)
          )
        )
      )
    )

    (if-expresion (condicion exp lista-condiciones lista-expresiones expresion-else)
      (if (evaluar-expresion condicion ambi old-expresion)
          (evaluar-expresion exp ambi old-expresion)
          (letrec ([evaluar-elseif
                    (lambda (lista-condiciones lista-expresiones)
                      (cond
                        [(null? lista-condiciones) (evaluar-expresion expresion-else ambi old-expresion)]
                        [else
                          (if (evaluar-expresion (car lista-condiciones) ambi old-expresion)
                              (evaluar-expresion (car lista-expresiones) ambi old-expresion)
                              (evaluar-elseif (cdr lista-condiciones) (cdr lista-expresiones))
                          )
                        ]
                      )
                    )
                  ])
            (evaluar-elseif lista-condiciones lista-expresiones)
          )
      )
    )


     (obj-exp (ids exps) (un-objeto ids (list->vector exps)))
     (get-exp (obj-id sym)
              (let ([obj (aplicar-ambiente ambi obj-id)])

                (valor-atributo sym obj ambi old-expresion)))
     (update-exp (obj-id field-id new-val)
                 (let ([obj (aplicar-ambiente ambi obj-id)])

                   (actualizar-atributo obj field-id new-val)))
     (meth-expresion (self-id args-ids body-exp)
      (un-metodo (cons self-id args-ids) body-exp))
     (send-expresion (obj-id meth-id args)
                     (let ([args (evaluar-rands args ambi)]

                           [obj (aplicar-ambiente ambi obj-id)])

                       (aplicar-metodo obj meth-id args ambi old-expresion)))
     (clone-expresion (id)
                      (let ([obj (aplicar-ambiente ambi id)])

                        (if (objeto? obj)
                            obj

                            (eopl:error 'clone "Esto no es un objeto -> ~s" id)))))))

(define ambiente-inicial
  (ambiente-extendido 
    '(var1 var2 var3) 
    (list 2 "Hola mundo" "hola") 
    (ambiente-vacio)
  )
)

(define evaluar-rands
  (lambda (exps env)
    (map (lambda (exp)
      (if (objeto? exp) exp
        (evaluar-expresion exp env exp)
      )) 
      exps
    )
  )
)

(define (aplicar-metodo obj meth-id args ambi old-expresion)
  (let* ((metodo (valor-atributo meth-id obj ambi old-expresion))
         (self obj)
         (nuevo-ambiente (ambiente-extendido (metodo-args metodo)
                                     (cons self args)
                                     ambi)))
    (evaluar-expresion (metodo-body metodo) nuevo-ambiente old-expresion)))


(define evaluar-programa
  (lambda (program)
    (cases programa program 
      (a-program (exp) (evaluar-expresion exp ambiente-inicial exp))
    )
  )
)

(define interpretador
  (sllgen:make-rep-loop "interpretador> "
    evaluar-programa
    (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)
  )
)

; (interpretador)

(define scan&parse
   (sllgen:make-string-parser especificacion-lexica especificacion-gramatical)
)

(provide (all-defined-out))
