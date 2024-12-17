#lang eopl

(define especificacion-lexica
  '((espacio-blanco (whitespace) skip)
    (comentario ("(" "*" (arbno (not #\newline)) "*" ")") skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (arbno (or digit letter whitespace)) "\"") string)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)))

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
    (expresion
     ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)
               "in"
               expresion)
     letrec-expresion)
    (expresion ("set" identificador ":=" expresion) set-expresion)
    ; (expresion ("begin" expresion (arbno ";" expresion) "end") begin-expresion)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-expresion)
    ;; Condicionales
    (expresion ("if" expresion
                     "then"
                     expresion
                     (arbno "elseif" expresion "then" expresion)
                     "else"
                     expresion
                     "end")
               if-expresion)
    (expresion ("proc" "(" (separated-list identificador ",") ")" expresion "end") proc-expresion)
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-expresion)
    ;; For
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-expresion)
    ;;(expresion ("clone" "(" identificador (separated-list identificador ",") ")") clone-expresion)
    ;; Primitivas Aritmeticas
    (expresion (primitiva "(" (separated-list expresion ",") ")") primitiva-expresion)
    (primitiva ("+") suma-primitiva)
    (primitiva ("-") resta-primitiva)
    (primitiva ("*") multiplicacion-primitiva)
    (primitiva ("%") modulo-primitiva)
    (primitiva ("&") concatenacion-primitiva)
    (primitiva ("is") igual-expresion)
    ;; Primitivas Booleanas
    (expresion (primitiva-booleana "(" (separated-list expresion ",") ")")
               primitiva-booleana-expresion)
    ;;(primitiva-booleana ("is" expresion expresion) igual-expresion)
    (primitiva-booleana ("<") menor-expresion)
    (primitiva-booleana (">") mayor-expresion)
    (primitiva-booleana ("<=") menor-igual-expresion)
    (primitiva-booleana (">=") mayor-igual-expresion)
    ;; Operaciones booleanas
    (expresion (operaciones-booleanas "(" (separated-list expresion ",") ")")
               operaciones-booleanas-expresion)
    (operaciones-booleanas ("not") not-booleana)
    (operaciones-booleanas ("and") and-booleana)
    (operaciones-booleanas ("or") or-booleana)
    ;; Expresion booleanas
    (expresion (boolenas-expresion) boolenas-expresion-expresion)
    (boolenas-expresion ("true") true-booleana)
    (boolenas-expresion ("false") false-booleana)
    ;;(boolenas-expresion primitiva-booleana "(" (separated-list expresion ",") ")" list-primitivas-booleana)
    ;;(boolenas-expresion operaciones-booleanas "(" (separated-list boolenas-expresion ",") ")" list-operaciones-booleana)
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

(define-datatype procedimiento
                 procedimiento?
                 (closure (variable (list-of symbol?)) (cuerpo expresion?) (ambiente ambiente?)))
(define aplicar-procedimiento
	(lambda (procd args)
		(cases procedimiento procd
			(closure (variable cuerpo ambiente)
				;; se evalua el cuerpo de el procedimiento en un ambiente extendido con los valores de los argumentos
				(evaluar-expresion cuerpo (ambiente-extendido variable args ambiente))))))               
;; Fin de Procedimientos 


;; Definimos los ambientes
(define-datatype ambiente
                 ambiente?
                 (ambiente-vacio)
                 (ambiente-extendido-referencia (lista-ids (list-of symbol?))
                                                (lista-valores vector?)
                                                (old-env ambiente?)))

(define ambiente-extendido
  (lambda (lista-ids lista-valores old-env)
    (ambiente-extendido-referencia lista-ids (list->vector lista-valores) old-env)))

(define ambiente-extendido-recursivo
  (lambda (nombres-procedimientos lista-ids cuerpos old-env)
    (let ([vectores-clausuras (make-vector (length nombres-procedimientos))])
      (letrec
          ([ambiente
            (ambiente-extendido-referencia nombres-procedimientos vectores-clausuras old-env)]
           [obtener-clausuras
            (lambda (lista-ids cuerpos i)
              (cond
                [(null? lista-ids) ambiente]
                [else
                 (begin
                   (vector-set! vectores-clausuras i (closure (car lista-ids) (car cuerpos) ambiente))
                   (obtener-clausuras (cdr lista-ids) (cdr cuerpos) (+ i 1)))]))])
        (obtener-clausuras lista-ids cuerpos 0)))))

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

(define evaluar-primitiva
  (lambda (prim lval)
    (cases primitiva
           prim
           (suma-primitiva () (operacion-primitiva lval + 0))
           (resta-primitiva () (operacion-primitiva lval - 0))
           (multiplicacion-primitiva () (operacion-primitiva lval * 1))
           (modulo-primitiva () (modulo (car lval) (cadr lval)))
           (concatenacion-primitiva () (string-append (car lval) (cadr lval)))
           (igual-expresion () (equal? (car lval) (cadr lval))))))

(define evaluar-primitiva-booleana
  (lambda (prim lval)
    (cases primitiva-booleana
           prim
           (menor-expresion () (< (car lval) (cadr lval)))
           (menor-igual-expresion () (<= (car lval) (cadr lval)))
           (mayor-expresion () (> (car lval) (cadr lval)))
           (mayor-igual-expresion () (>= (car lval) (cadr lval))))))

;; Objetos

(define-datatype objeto objeto? (un-objeto (fields (list-of symbol?)) (exps vector?)))

(define-datatype metodo metodo? (un-metodo (ids (list-of symbol?)) (body expresion?)))

(define valor-atributo
  (lambda (sym obj env)
    (cases objeto
           obj
           (un-objeto (fields exps)
                      (let ([pos (rib-find-position sym fields)])

                        (if (number? pos)
                            (let ([value (vector-ref exps pos)])

                              (evaluar-expresion value env))

                            (eopl:error 'objeto "Field ~s has not been defined" sym)))))))

(define rib-find-position (lambda (sym los) (list-find-position sym los)))

(define list-find-position (lambda (sym los) (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else
       (let ([list-index-r (list-index pred (cdr ls))])
         (if (number? list-index-r) (+ list-index-r 1) #f))])))

(define actualizar-atributo
  (lambda (obj id new-value)
    (cases objeto
           obj
           (un-objeto (fields exps)
                      (let ([pos (rib-find-position id fields)])

                        (if (number? pos)

                            (vector-set! exps pos new-value)

                            (eopl:error 'update "El campo ~s no ha sido definido" id)))))))

(define evaluar-expresion
  (lambda (exp ambi)
    (cases
     expresion
     exp
     (literal-expresion (dato) dato)
     (variable-expresion (variable) (aplicar-ambiente ambi variable))
     (caracter-expresion (caracter) caracter)
     (cadena-expresion (cadena) (substring cadena 1 (- (string-length cadena) 1)))
     (for-expresion
      (var start end body)
      (let loop ([i (evaluar-expresion start ambi)]
                 [resultados '()])
        (if (<= i (evaluar-expresion end ambi))
            (let ([resultado (evaluar-expresion body (ambiente-extendido (list var) (list i) ambi))])
              (loop (+ i 1) (cons resultado resultados)))
            (reverse resultados))))
     (ok-expresion () 'ok)
     (boolenas-expresion-expresion
      (exp1)
      (cases boolenas-expresion exp1 (true-booleana () #t) (false-booleana () #f)))
     (primitiva-expresion (primitiva lista-valores)
                          (let ([lval (map (lambda (exp) (evaluar-expresion exp ambi))
                                           lista-valores)])
                            (evaluar-primitiva primitiva lval)))
     (primitiva-booleana-expresion (primitiva-booleana lista-valores)
                                   (let ([lval (map (lambda (exp) (evaluar-expresion exp ambi))
                                                    lista-valores)])
                                     (evaluar-primitiva-booleana primitiva-booleana lval)))
     (operaciones-booleanas-expresion (lista-operaciones-booleanas lista-valores)
                                      (let ([lval (map (lambda (exp) (evaluar-expresion exp ambi))
                                                       lista-valores)])
                                        (cases operaciones-booleanas
                                               lista-operaciones-booleanas
                                               (not-booleana () (not (car lval)))
                                               (and-booleana () (and (car lval) (cadr lval)))
                                               (or-booleana () (or (car lval) (cadr lval))))))
     (var-expresion (lista-ids lista-expresiones exp)
                    (let ([valores (map (lambda (exp) (evaluar-expresion exp ambi))
                                        lista-expresiones)])
                      (evaluar-expresion exp (ambiente-extendido lista-ids valores ambi))))
     (let-expresion (lista-ids lista-expresiones exp)
                    (let ([valores (map (lambda (exp) (evaluar-expresion exp ambi))
                                        lista-expresiones)])
                      (evaluar-expresion exp (ambiente-extendido lista-ids valores ambi))))
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
     (letrec-expresion
      (lista-ids lista-params cuerpos exp)
      (evaluar-expresion exp (ambiente-extendido-recursivo lista-ids lista-params cuerpos ambi)))
     (begin-expresion
      (exp lista-expresiones)
      (if (null? lista-expresiones)
          (evaluar-expresion exp ambi)
          (begin
            (evaluar-expresion exp ambi)
            (letrec ([evaluar-begin (lambda (lista-expresiones)
                                      (cond
                                        [(null? (cdr lista-expresiones))
                                         (evaluar-expresion (car lista-expresiones) ambi)]
                                        [else
                                         (begin
                                           (evaluar-expresion (car lista-expresiones) ambi)
                                           (evaluar-begin (cdr lista-expresiones)))]))])
              (evaluar-begin lista-expresiones)))))
     (set-expresion (variable exp)
                    (begin
                      (setref! (aplicar-ambiente-referencia ambi variable)
                               (evaluar-expresion exp ambi))))
     (if-expresion
      (condicion exp lista-condiciones lista-expresiones expresion-else)
      (if (evaluar-expresion condicion ambi)
          (evaluar-expresion exp ambi)
          (letrec ([evaluar-elseif
                    (lambda (lista-condiciones lista-expresiones)
                      (cond
                        [(null? lista-condiciones) (evaluar-expresion expresion-else ambi)]
                        [else
                         (if (evaluar-expresion (car lista-condiciones) ambi)
                             (evaluar-expresion (car lista-expresiones) ambi)
                             (evaluar-elseif (cdr lista-condiciones) (cdr lista-expresiones)))]))])
            (evaluar-elseif lista-condiciones lista-expresiones))))
     (obj-exp (ids exps) (un-objeto ids (list->vector exps)))
     (get-exp (obj-id sym)
              (let ([obj (aplicar-ambiente ambi obj-id)])

                (valor-atributo sym obj ambi)))
     (update-exp (obj-id field-id new-val)
                 (let ([obj (aplicar-ambiente ambi obj-id)])

                   (actualizar-atributo obj field-id new-val)))
     (meth-expresion (id args-ids body-exp) (un-metodo args-ids body-exp))
     (send-expresion (obj-id meth-id args)
                     (let ([args (evaluar-rands args ambi)]

                           [obj (aplicar-ambiente ambi obj-id)])

                       (aplicar-metodo obj meth-id args ambi)))
     (clone-expresion (id)
                      (let ([obj (aplicar-ambiente ambi id)])

                        (if (objeto? obj)
                            obj

                            (eopl:error 'clone "Esto no es un objeto -> ~s" id)))))))

(define ambiente-inicial
  (ambiente-extendido '($var1 $var2 $var3) '(2 "Hola mundo" 'a) (ambiente-vacio)))

(define evaluar-rands
  (lambda (exps env)
    (map (lambda (exp)

           (if (objeto? exp)
               exp

               (evaluar-expresion exp env)))
         exps)))

(define aplicar-metodo
  (lambda (obj meth args env)
    (cases objeto
           obj
           (un-objeto
            (fields exps)
            (let ([pos (rib-find-position meth fields)])

              (if (number? pos)

                  (let ([met (evaluar-expresion (vector-ref exps pos) env)])
                    (if (metodo? met)
                        (cases metodo
                               met
                               (un-metodo (ids body)
                                          (if (equal? (length args) (length ids))

                                              (evaluar-expresion body
                                                                 (ambiente-extendido ids args env))
                                              (eopl:error 'send "Número de argumentos incorrecto"))))
                        (eopl:error 'send "No es un método -> ~s" meth)))
                  (eopl:error 'send "No se ha definido el método ~s" meth)))))))

(define evaluar-programa
  (lambda (program)
    (cases programa program (a-program (exp) (evaluar-expresion exp ambiente-inicial)))))

(define interpretador
  (sllgen:make-rep-loop "interpretador> "
                        evaluar-programa
                        (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)))

(interpretador)
