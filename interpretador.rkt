#lang eopl

(define especificacion-lexica
   '(
      (espacio-blanco (whitespace) skip)
      (comentario-linea ("*" (arbno (not #\newline) "*")) skip)
      (comentario-bloque ("(*" (arbno (not "*)") "*)") skip))
      (identificador (letter (arbno (or letter digit "?" "$"))) symbol)
      (numero (digit (arbno digit)) number)
      (numero ("-" digit (arbno digit)) number)
      (numero (digit (arbno digit) "." digit (arbno digit)) number)
      (numero ("-" digit (arbno digit)"." digit (arbno digit)) number)
   )
)

(define especificacion-gramatical
   '(
      (programa ((arbno declaracion-clase) expresion) a-program)
      (expresion (numero) literal-expresion)
      (expresion (identificador) variable-expresion)
      (expresion ("'" letter "'") caracter-expresion)
      (expresion ("\"" (letter (arbno (or letter digit))) "\"") cadena-expresion)
      (expresion ("ok") ok-expresion)

      ;; Ligaduras
      (expresion ("var" (arbno identificador "=" expresion) "in" expresion "end") var-expresion)
      (expresion ("let" (arbno identificador "=" expresion) "in" expresion "end") let-expresion)
      (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-expresion)
      (expresion ("set" identificador ":=" expresion) set-expresion)
      (expresion ("begin" expresion (arbno ";" expresion) "end") begin-expresion)

      ;; Condicionales
      (expresion ("if" boolenas-expresion "then" expresion (arbno "elseif" boolenas-expresion "then" expresion) "else" expresion "end") if-expresion)

      ;; Primitivas Aritmeticas
      (expresion (primitiva "(" (separated-list expresion ",") ")") primitiva-expresion)
      (primitiva ("+") suma-primitiva)
      (primitiva ("-") resta-primitiva)
      (primitiva ("*") multiplicacion-primitiva)
      (primitiva ("%") modulo-primitiva)
      (primitiva ("&") concatenacion-primitiva)

      ;; Primitivas Booleanas
      (expresion (primitiva-booleana "(" (separated-list expresion ",") ")") primitiva-booleana-expresion)
      (primitiva-booleana ("is" expresion expresion) igual-expresion)
      (primitiva-booleana ("<" expresion expresion) menor-expresion)
      (primitiva-booleana (">" expresion expresion) mayor-expresion)
      (primitiva-booleana ("<=" expresion expresion) menor-igual-expresion)
      (primitiva-booleana (">=" expresion expresion) mayor-igual-expresion)

      ;; Operaciones booleanas
      (expresion (operaciones-booleanas "(" (separated-list expresion ",") ")") operaciones-booleanas-expresion)
      (operaciones-booleanas ("not") not-booleana)
      (operaciones-booleanas ("and") and-booleana)
      (operaciones-booleanas ("or") or-booleana)

      ;; Expresion booleanas
      (expresion (boolenas-expresion "(" (separated-list expresion ",") ")") boolenas-expresion)
      (boolenas-expresion ("true") true-booleana)
      (boolenas-expresion ("false") false-booleana)
      (boolenas-expresion primitiva-booleana "(" (separated-list expresion ",") ")" list-primitivas-booleana)
      (boolenas-expresion operaciones-booleanas "(" (separated-list boolenas-expresion ",") ")" list-operaciones-booleana)
   )
)

;; Creamos los datatypes automaticamente
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; Definimos los ambientes
(define-datatype ambiente ambiente?
   (ambiente-vacio)
   (ambiente-exntedio-referencia 
      (lista-ids (list-of symbol?))
      (lista-valores vector?)
      (old-env ambiente?)
   )
)

(define-datatype ambiente-extendido
   (lambda (lista-ids lista-valores old-env)
      (ambiente-extendido-referencia 
         lista-ids 
         (lista->vector lista-valores) 
         old-env
      )
   )
)

(define-datatype ambiente-extendido-recursivo
   (lambda (nombres-procedimientos lista-ids cuerpos old-env)
      (let
         ((vectores-clausuras (make-vector (length nombres-procedimientos))))
         (letrect
            (
               (ambiente (ambiente-extendido-referencia nombres-procedimientos vectores-clausuras old-env))
               (obtener-clausuras 
                  (lambda (lista-ids cuerpos i)
                     (cond
                        [(null? lista-ids) ambiente]
                        [else
                           (begin
                              (vector-set! vectores-clausuras i (closure (car lista-ids) (car cuerpos) ambiente))
                              (obtener-clausuras (cdr lista-ids) (cdr cuerpos) (+ i 1))
                           )
                        ]
                     )
                  )
               )
            )
            (obtener-clausuras lista-ids cuerpos 0)
         )
      )
   )
)

(define aplicar-ambiente
   (lambda (ambi variable)
      (deref (aplicar-ambiente-referencia ambi variable))
   )
)

(define aplicar-ambiente-referencia
   (lambda (ambi variable)
      (cases ambiente ambi
         (ambiente-vacio () (eopl:error "No se encuentra la variable" variable))
         (ambiente-extendido-referencia (lista-ids lista-vector old-env) 
            (letrec
               ((buscar-variable 
                  (lambda (lista-ids lista-vector i)
                     (cond
                        [(null? lista-ids) (aplicar-ambiente-referencia old-env variable)]
                        [(equal? (car lista-ids) variable) (a-ref i lista-vector)]
                        [else (buscar-variable (cdr lista-ids) lista-vector (+ i 1))]
                     )
                  )
               ))
               (buscar-variable lista-ids lista-vector 0)
            )
         )
      )
   )
)


;; Siguientes pasos

;; 1. Definir evaluar primitiva
;; 2. Definir opracion primitiva
;; 3. Definir evaluar expresion
;; 4. Probar el interprete