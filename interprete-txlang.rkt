#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Interpretador de TxLang-UV.
  Proyecto final del curso Fundamentos de Interpretación y Compilación de
  Lenguajes de Programación, Universidad del Valle.

  El archivo va en nueve secciones, en el orden en que se construye un
  interpretador. La primera entrega llena las secciones 1 a 6 y la 8; la
  segunda agrega al almacén el registro que permite revertir, los registros
  del lenguaje y el chequeador de tipos de la sección 7.

  Decisiones del léxico:
  - Un entero con signo es un solo token: "-7" es numero, pero "-(a, b)" es
    la primitiva - porque el token numero exige un dígito después del "-".
  - Las palabras reservadas son los literales de la gramática; SLLGEN les da
    prioridad sobre identificador cuando el largo del lexema es el mismo.
  - Los separated-list admiten lista vacía en la gramática; la aridad de cada
    primitiva y de cada let se valida al evaluar.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los dos tipos de comentario, los literales de cada clase y los
;; identificadores. El comentario de bloque no admite | adentro

(define especificacion-lexica
  '((espacio (whitespace) skip)
    (comentario-linea ("//" (arbno (not #\newline))) skip)
    (comentario-bloque ("#|" (arbno (not #\|)) "|#") skip)
    (identificador (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" (not #\') "'") string)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Un datatype por no terminal: programa, declaración de registro, tipo,
;; expresión y primitiva.

(define especificacion-gramatical
  '((programa ((arbno decl-registro) expresion) un-programa)

    (decl-registro
     ("record" identificador "{"
      (separated-list identificador ":" tipo ",") "}")
     registro-decl)

    ;; tipos 
    (tipo ("int") int-tipo)
    (tipo ("float") float-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("string") string-tipo)
    (tipo ("char") char-tipo)
    (tipo ("unit") unit-tipo)
    (tipo ("ref" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-tipo)
    (tipo (identificador) nombre-tipo)

    ;; expresiones 
    (expresion (numero) lit-exp)
    (expresion (flotante) flotante-exp)
    (expresion (cadena) cadena-exp)
    (expresion (caracter) caracter-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)

    (expresion
     ("let" (separated-list identificador "=" expresion ",")
      "in" expresion "end")
     let-exp)
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion
     ("if" expresion "then" expresion
      (arbno "elif" expresion "then" expresion)
      "else" expresion "end")
     if-exp)
    (expresion ("while" expresion "do" expresion "end") while-exp)
    (expresion
     (primitiva "(" (separated-list expresion ",") ")")
     prim-exp)
    (expresion
     ("proc" "(" (separated-list identificador ":" tipo ",") ")" expresion "end")
     proc-exp)
    (expresion
     ("apply" expresion "(" (separated-list expresion ",") ")")
     apply-exp)
    (expresion
     ("letrec"
      (arbno tipo identificador
             "(" (separated-list identificador ":" tipo ",") ")"
             "=" expresion)
      "in" expresion "end")
     letrec-exp)
    (expresion
     ("atomic" expresion "onfail" "(" identificador ")" expresion "end")
     atomic-exp)
    (expresion ("abort" "(" expresion ")") abort-exp)
    (expresion
     ("new" identificador "{"
      (separated-list identificador "=" expresion ",") "}")
     new-exp)
    (expresion ("field" "(" expresion "," identificador ")") field-exp)

    ;; primitivas 
    (primitiva ("+") suma-prim)
    (primitiva ("-") resta-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">=") mayor-igual-prim)
    (primitiva ("<=") menor-igual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)))

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

;; Muestra los define-datatype que SLLGEN generó a partir de la gramatica
(define mostrar-datatypes
  (lambda ()
    (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;; SLLGEN deja las comillas dentro del lexema: cadena-exp guarda "\"hola\"" y
;; caracter-exp guarda "'a'". Estos dos helpers entregan el contenido limpio;
;; unparse escribe el lexema tal cual, por eso el texto vuelve a parsear
(define cadena->string
  (lambda (lexema)
    (substring lexema 1 (- (string-length lexema) 1))))

(define caracter->char
  (lambda (lexema)
    (string-ref lexema 1)))

;;; ==========================================================
;;; 3. Unparse: del árbol de sintaxis abstracta al texto
;;; ==========================================================
;; Une una lista de textos con un separador
(define unir
  (lambda (textos sep)
    (cond ((null? textos) "")
          ((null? (cdr textos)) (car textos))
          (else (string-append (car textos) sep (unir (cdr textos) sep))))))

(define unparse
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo)
        (unir (append (map unparse-decl decls) (list (unparse-expresion cuerpo)))
              "\n")))))

(define unparse-decl
  (lambda (d)
    (cases decl-registro d
      (registro-decl (nombre campos tipos)
        (string-append "record " (symbol->string nombre) " { "
                       (unparse-anotados campos tipos) " }")))))

;; "a: int, b: bool"
(define unparse-anotados
  (lambda (ids tipos)
    (unir (map (lambda (id t) (string-append (symbol->string id) ": " (unparse-tipo t)))
               ids tipos)
          ", ")))

(define unparse-tipo
  (lambda (t)
    (cases tipo t
      (int-tipo () "int")
      (float-tipo () "float")
      (bool-tipo () "bool")
      (string-tipo () "string")
      (char-tipo () "char")
      (unit-tipo () "unit")
      (ref-tipo (t1) (string-append "ref(" (unparse-tipo t1) ")"))
      (proc-tipo (args ret)
        (string-append "(" (unir (map unparse-tipo args) ", ") " -> "
                       (unparse-tipo ret) ")"))
      (nombre-tipo (id) (symbol->string id)))))

(define unparse-primitiva
  (lambda (p)
    (cases primitiva p
      (suma-prim () "+")
      (resta-prim () "-")
      (mult-prim () "*")
      (div-prim () "/")
      (mod-prim () "%")
      (mayor-prim () ">")
      (menor-prim () "<")
      (mayor-igual-prim () ">=")
      (menor-igual-prim () "<=")
      (igual-prim () "==")
      (diferente-prim () "!=")
      (and-prim () "and")
      (or-prim () "or")
      (not-prim () "not")
      (concat-prim () "concat")
      (length-prim () "length")
      (to-string-prim () "to-string")
      (to-number-prim () "to-number")
      (print-prim () "print"))))

(define unparse-lista
  (lambda (exps)
    (unir (map unparse-expresion exps) ", ")))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (lit-exp (n) (number->string n))
      (flotante-exp (n) (number->string n))
      (cadena-exp (s) s)
      (caracter-exp (c) c)
      (true-exp () "true")
      (false-exp () "false")
      (unit-exp () "unit")
      (var-exp (id) (symbol->string id))
      (let-exp (ids exps cuerpo)
        (string-append
         "let "
         (unir (map (lambda (id e) (string-append (symbol->string id) " = " (unparse-expresion e)))
                    ids exps)
               ", ")
         " in " (unparse-expresion cuerpo) " end"))
      (ref-exp (e) (string-append "ref(" (unparse-expresion e) ")"))
      (deref-exp (e) (string-append "deref(" (unparse-expresion e) ")"))
      (set-exp (e1 e2)
        (string-append "set " (unparse-expresion e1) " := " (unparse-expresion e2)))
      (begin-exp (e1 es)
        (string-append "begin " (unir (map unparse-expresion (cons e1 es)) "; ") " end"))
      (if-exp (prueba entonces pruebas-elif ramas-elif sino)
        (string-append
         "if " (unparse-expresion prueba) " then " (unparse-expresion entonces)
         (apply string-append
                (map (lambda (p r)
                       (string-append " elif " (unparse-expresion p)
                                      " then " (unparse-expresion r)))
                     pruebas-elif ramas-elif))
         " else " (unparse-expresion sino) " end"))
      (while-exp (cond-e cuerpo)
        (string-append "while " (unparse-expresion cond-e) " do "
                       (unparse-expresion cuerpo) " end"))
      (prim-exp (p args)
        (string-append (unparse-primitiva p) "(" (unparse-lista args) ")"))
      (proc-exp (ids tipos cuerpo)
        (string-append "proc (" (unparse-anotados ids tipos) ") "
                       (unparse-expresion cuerpo) " end"))
      (apply-exp (rator rands)
        (string-append "apply " (unparse-expresion rator) " (" (unparse-lista rands) ")"))
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos cuerpo)
        (string-append
         "letrec "
         (unir (map (lambda (tr nombre ids ts c)
                      (string-append (unparse-tipo tr) " " (symbol->string nombre)
                                     " (" (unparse-anotados ids ts) ") = "
                                     (unparse-expresion c)))
                    tipos-ret nombres idss tiposs cuerpos)
               " ")
         " in " (unparse-expresion cuerpo) " end"))
      (atomic-exp (cuerpo id manejador)
        (string-append "atomic " (unparse-expresion cuerpo)
                       " onfail (" (symbol->string id) ") "
                       (unparse-expresion manejador) " end"))
      (abort-exp (e) (string-append "abort(" (unparse-expresion e) ")"))
      (new-exp (nombre campos exps)
        (string-append
         "new " (symbol->string nombre) " { "
         (unir (map (lambda (c e) (string-append (symbol->string c) " = " (unparse-expresion e)))
                    campos exps)
               ", ")
         " }"))
      (field-exp (e campo)
        (string-append "field(" (unparse-expresion e) ", " (symbol->string campo) ")")))))

;;; ==========================================================
;;; 6. Valores expresados (va antes del ambiente: ambiente usa valor?)
;;; ==========================================================
;; Los valores denotados son los mismos que los expresados: el ambiente liga
;; identificadores directamente a valores, y la mutación pasa por las
;; referencias (ref-val), que son índices al almacén. Esto difiere de EOPL 4.2,
;; donde el ambiente liga identificadores a referencias.

(define cualquier?
  (lambda (x) #t))

(define-datatype valor valor?
  (int-val (n integer?))
  (float-val (n real?))
  (string-val (s string?))
  (char-val (c char?))
  (bool-val (b boolean?))
  (unit-val)
  ;; índice de la celda en el almacén
  (ref-val (indice integer?))
  ;; clausura: parámetros, cuerpo y ambiente de definición (sin predicado,
  ;; ver el comentario del encabezado)
  (proc-val (ids (list-of symbol?)) (cuerpo expresion?) (amb cualquier?))
  ;; registro: nombre del tipo, campos y valores en el mismo orden
  (record-val (nombre symbol?) (campos (list-of symbol?)) (vals (list-of valor?))))

;;; Extractores: reportan error con eopl:error si el valor no es del tipo.

(define valor-error
  (lambda (esperado v)
    (eopl:error 'valor "se esperaba ~a y se encontró ~a" esperado (valor->texto v))))

(define valor->int
  (lambda (v)
    (cases valor v
      (int-val (n) n)
      (else (valor-error "int" v)))))

(define valor->float
  (lambda (v)
    (cases valor v
      (float-val (n) n)
      (else (valor-error "float" v)))))

(define valor->string
  (lambda (v)
    (cases valor v
      (string-val (s) s)
      (else (valor-error "string" v)))))

(define valor->char
  (lambda (v)
    (cases valor v
      (char-val (c) c)
      (else (valor-error "char" v)))))

(define valor->bool
  (lambda (v)
    (cases valor v
      (bool-val (b) b)
      (else (valor-error "bool" v)))))

;; índice de la celda de una referencia
(define valor->ref
  (lambda (v)
    (cases valor v
      (ref-val (i) i)
      (else (valor-error "ref" v)))))

(define valor->proc?
  (lambda (v)
    (cases valor v
      (proc-val (ids cuerpo amb) #t)
      (else #f))))

(define valor->record
  (lambda (v)
    (cases valor v
      (record-val (nombre campos vals) v)
      (else (valor-error "registro" v)))))

;; Valor del campo de un registro; error claro si el campo no existe.
(define record-campo
  (lambda (v campo)
    (cases valor v
      (record-val (nombre campos vals)
        (let buscar ((cs campos) (vs vals))
          (cond ((null? cs)
                 (eopl:error 'field "el registro ~s no tiene el campo ~s" nombre campo))
                ((eqv? (car cs) campo) (car vs))
                (else (buscar (cdr cs) (cdr vs))))))
      (else (valor-error "registro" v)))))

;;; Representación como texto (lo que imprime correr, print y to-string).
;;; Cadenas y caracteres se muestran sin comillas.

(define valor->texto
  (lambda (v)
    (cases valor v
      (int-val (n) (number->string n))
      (float-val (n) (number->string n))
      (string-val (s) s)
      (char-val (c) (string c))
      (bool-val (b) (if b "true" "false"))
      (unit-val () "unit")
      (ref-val (i) (string-append "<ref " (number->string i) ">"))
      (proc-val (ids cuerpo amb) "<procedimiento>")
      (record-val (nombre campos vals)
        (string-append (symbol->string nombre) " { "
                       (campos->texto campos vals) " }")))))

(define campos->texto
  (lambda (campos vals)
    (cond ((null? campos) "")
          ((null? (cdr campos))
           (string-append (symbol->string (car campos)) " = " (valor->texto (car vals))))
          (else
           (string-append (symbol->string (car campos)) " = " (valor->texto (car vals))
                          ", " (campos->texto (cdr campos) (cdr vals)))))))

;;; ==========================================================
;;; 4. TAD ambiente
;;; ==========================================================
;; Liga identificadores a valores (no a referencias). Interfaz:
;;   (ambiente-vacio), (extender-ambiente ids vals amb),
;;   (extender-ambiente-rec nombres params cuerpos amb), (buscar-ambiente amb id)
;; El ambiente de letrec guarda parámetros y cuerpos; la clausura se arma al
;; buscar el nombre, con el mismo ambiente recursivo como entorno.

(define-datatype ambiente ambiente?
  (amb-vacio)
  (amb-extendido
   (ids (list-of symbol?))
   (vals (list-of valor?))
   (padre ambiente?))
  (amb-extendido-rec
   (nombres (list-of symbol?))
   (params (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (padre ambiente?)))

(define ambiente-vacio
  (lambda () (amb-vacio)))

;; Verifica que ids y vals tengan la misma longitud y que amb sea un ambiente.
(define extender-ambiente
  (lambda (ids vals amb)
    (if (= (length ids) (length vals))
        (amb-extendido ids vals amb)
        (eopl:error 'extender-ambiente
                    "se esperaban ~s valores y llegaron ~s" (length ids) (length vals)))))

(define extender-ambiente-rec
  (lambda (nombres params cuerpos amb)
    (amb-extendido-rec nombres params cuerpos amb)))

;; Posición de x en la lista, o #f
(define posicion
  (lambda (x lista)
    (let buscar ((l lista) (i 0))
      (cond ((null? l) #f)
            ((eqv? (car l) x) i)
            (else (buscar (cdr l) (+ i 1)))))))

(define buscar-ambiente
  (lambda (amb id)
    (cases ambiente amb
      (amb-vacio ()
        (eopl:error 'buscar-ambiente "variable no ligada: ~s" id))
      (amb-extendido (ids vals padre)
        (let ((i (posicion id ids)))
          (if i
              (list-ref vals i)
              (buscar-ambiente padre id))))
      (amb-extendido-rec (nombres params cuerpos padre)
        (let ((i (posicion id nombres)))
          (if i
              (proc-val (list-ref params i) (list-ref cuerpos i) amb)
              (buscar-ambiente padre id)))))))

;;; ==========================================================
;;; 5. TAD almacén
;;; ==========================================================
;; Liga referencias (índices) a valores. Interfaz:
;;   (inicializar-almacen!), (nueva-celda! v) -> índice,
;;   (leer-celda i), (escribir-celda! i v), (tamano-almacen)
;; Toda escritura pasa por nueva-celda! y escribir-celda!, que son los dos
;; puntos donde la segunda entrega registrará los cambios de una transacción.

(define capacidad-inicial 16)
(define almacen-celdas (make-vector capacidad-inicial #f))
(define almacen-tamano 0)

(define inicializar-almacen!
  (lambda ()
    (set! almacen-celdas (make-vector capacidad-inicial #f))
    (set! almacen-tamano 0)))

(define tamano-almacen
  (lambda () almacen-tamano))

(define copiar-vector!
  (lambda (origen destino n)
    (let copiar ((i 0))
      (if (< i n)
          (begin (vector-set! destino i (vector-ref origen i))
                 (copiar (+ i 1)))
          destino))))

(define crecer-almacen!
  (lambda ()
    (set! almacen-celdas
          (copiar-vector! almacen-celdas
                          (make-vector (* 2 (vector-length almacen-celdas)) #f)
                          almacen-tamano))))

(define nueva-celda!
  (lambda (v)
    (begin
      (if (= almacen-tamano (vector-length almacen-celdas))
          (crecer-almacen!)
          #f)
      (vector-set! almacen-celdas almacen-tamano v)
      (set! almacen-tamano (+ almacen-tamano 1))
      (- almacen-tamano 1))))

(define verificar-celda
  (lambda (operacion i)
    (if (and (integer? i) (>= i 0) (< i almacen-tamano))
        #t
        (eopl:error operacion "la referencia ~s no existe en el almacén" i))))

(define leer-celda
  (lambda (i)
    (begin
      (verificar-celda 'deref i)
      (vector-ref almacen-celdas i))))

(define escribir-celda!
  (lambda (i v)
    (begin
      (verificar-celda 'set i)
      (vector-set! almacen-celdas i v))))


;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================
(define chequear-programa
  (lambda (pgm) pgm))

;; Tipo del programa, escrito como texto: "int", "ref(bool)", "(int -> int)".
(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "el chequeador de tipos llega en la segunda entrega")))

;;; ==========================================================
;;; 8a. Primitivas
;;; ==========================================================
;; (aplicar-primitiva prim args): prim es una variante de primitiva y args
;; una lista de VALORES ya evaluados. Devuelve un valor.
;;
;; Decisiones:
;; - Las aritméticas exigen dos operandos del mismo tipo numérico (int con
;;   int, float con float). Entre enteros, / es la división entera
;;   (quotient) y % el resto (remainder); % solo acepta enteros.
;; - La división y el resto por cero son error, también con flotantes.
;; - Las de orden (> < >= <=) son numéricas; == y != comparan dos valores
;;   del mismo tipo (int, float, string, char, bool, unit o ref).
;; - and y or reciben los dos argumentos ya evaluados (sin cortocircuito).
;; - print escribe el texto en la consola y devuelve unit.

;; Nombre del tipo de un valor, para los mensajes de error.
(define tipo-valor
  (lambda (v)
    (cases valor v
      (int-val (n) "int")
      (float-val (n) "float")
      (string-val (s) "string")
      (char-val (c) "char")
      (bool-val (b) "bool")
      (unit-val () "unit")
      (ref-val (i) "ref")
      (proc-val (ids cuerpo amb) "procedimiento")
      (record-val (nombre campos vals) (symbol->string nombre)))))

(define verificar-aridad
  (lambda (p args n)
    (if (= (length args) n)
        #t
        (eopl:error 'primitiva "~a espera ~s argumento(s) y recibió ~s"
                    (unparse-primitiva p) n (length args)))))

(define error-tipos
  (lambda (p a b)
    (eopl:error 'primitiva
                "~a esperaba dos operandos del mismo tipo numérico y encontró ~a y ~a"
                (unparse-primitiva p) (tipo-valor a) (tipo-valor b))))

;; Los dos números de args como lista (x y); error si no son del mismo tipo.
(define par-numerico
  (lambda (p args)
    (verificar-aridad p args 2)
    (let ((a (car args)) (b (cadr args)))
      (cases valor a
        (int-val (x)
          (cases valor b
            (int-val (y) (list x y))
            (else (error-tipos p a b))))
        (float-val (x)
          (cases valor b
            (float-val (y) (list x y))
            (else (error-tipos p a b))))
        (else (error-tipos p a b))))))

(define op-entera
  (lambda (p x y)
    (cases primitiva p
      (suma-prim () (+ x y))
      (resta-prim () (- x y))
      (mult-prim () (* x y))
      (div-prim ()
        (if (= y 0)
            (eopl:error 'primitiva "división por cero")
            (quotient x y)))
      (mod-prim ()
        (if (= y 0)
            (eopl:error 'primitiva "resto por cero")
            (remainder x y)))
      (else (eopl:error 'op-entera "no es aritmética")))))

(define op-flotante
  (lambda (p x y)
    (cases primitiva p
      (suma-prim () (+ x y))
      (resta-prim () (- x y))
      (mult-prim () (* x y))
      (div-prim ()
        (if (= y 0)
            (eopl:error 'primitiva "división por cero")
            (/ x y)))
      (mod-prim ()
        (eopl:error 'primitiva "% solo acepta enteros y encontró float"))
      (else (eopl:error 'op-flotante "no es aritmética")))))

(define aplicar-aritmetica
  (lambda (p args)
    (let* ((xy (par-numerico p args))
           (x (car xy))
           (y (cadr xy)))
      (cases valor (car args)
        (int-val (n) (int-val (op-entera p x y)))
        (float-val (n) (float-val (op-flotante p x y)))
        (else (error-tipos p (car args) (cadr args)))))))

(define aplicar-orden
  (lambda (p args)
    (let* ((xy (par-numerico p args))
           (x (car xy))
           (y (cadr xy)))
      (bool-val
       (cases primitiva p
         (mayor-prim () (> x y))
         (menor-prim () (< x y))
         (mayor-igual-prim () (>= x y))
         (menor-igual-prim () (<= x y))
         (else (eopl:error 'aplicar-orden "no es relacional")))))))

;; #t si a y b son iguales; error si son de tipos distintos o no comparables.
(define valores-iguales?
  (lambda (p a b)
    (let ((error-cmp
           (lambda ()
             (eopl:error 'primitiva "~a esperaba dos valores comparables del mismo tipo y encontró ~a y ~a"
                         (unparse-primitiva p) (tipo-valor a) (tipo-valor b)))))
      (cases valor a
        (int-val (x) (cases valor b (int-val (y) (= x y)) (else (error-cmp))))
        (float-val (x) (cases valor b (float-val (y) (= x y)) (else (error-cmp))))
        (string-val (x) (cases valor b (string-val (y) (string=? x y)) (else (error-cmp))))
        (char-val (x) (cases valor b (char-val (y) (char=? x y)) (else (error-cmp))))
        (bool-val (x) (cases valor b (bool-val (y) (eq? x y)) (else (error-cmp))))
        (unit-val () (cases valor b (unit-val () #t) (else (error-cmp))))
        (ref-val (x) (cases valor b (ref-val (y) (= x y)) (else (error-cmp))))
        (else (error-cmp))))))

;; Texto -> int-val o float-val. Solo acepta dígitos, "-" y ".".
(define texto->numero
  (lambda (s)
    (let ((n (if (and (> (string-length s) 0)
                      (not (member #f (map (lambda (c) (or (char-numeric? c)
                                                           (char=? c #\-)
                                                           (char=? c #\.)))
                                           (string->list s)))))
                 (string->number s)
                 #f)))
      (cond ((not n) (eopl:error 'to-number "no se puede convertir ~s a número" s))
            ((and (integer? n) (exact? n)) (int-val n))
            ((and (real? n) (inexact? n)) (float-val n))
            (else (eopl:error 'to-number "no se puede convertir ~s a número" s))))))

(define aplicar-primitiva
  (lambda (p args)
    (cases primitiva p
      (suma-prim () (aplicar-aritmetica p args))
      (resta-prim () (aplicar-aritmetica p args))
      (mult-prim () (aplicar-aritmetica p args))
      (div-prim () (aplicar-aritmetica p args))
      (mod-prim () (aplicar-aritmetica p args))
      (mayor-prim () (aplicar-orden p args))
      (menor-prim () (aplicar-orden p args))
      (mayor-igual-prim () (aplicar-orden p args))
      (menor-igual-prim () (aplicar-orden p args))
      (igual-prim ()
        (begin (verificar-aridad p args 2)
               (bool-val (valores-iguales? p (car args) (cadr args)))))
      (diferente-prim ()
        (begin (verificar-aridad p args 2)
               (bool-val (not (valores-iguales? p (car args) (cadr args))))))
      (and-prim ()
        (begin (verificar-aridad p args 2)
               (bool-val (and (valor->bool (car args)) (valor->bool (cadr args))))))
      (or-prim ()
        (begin (verificar-aridad p args 2)
               (bool-val (or (valor->bool (car args)) (valor->bool (cadr args))))))
      (not-prim ()
        (begin (verificar-aridad p args 1)
               (bool-val (not (valor->bool (car args))))))
      (concat-prim ()
        (begin (verificar-aridad p args 2)
               (string-val (string-append (valor->string (car args))
                                          (valor->string (cadr args))))))
      (length-prim ()
        (begin (verificar-aridad p args 1)
               (int-val (string-length (valor->string (car args))))))
      (to-string-prim ()
        (begin (verificar-aridad p args 1)
               (string-val (valor->texto (car args)))))
      (to-number-prim ()
        (begin (verificar-aridad p args 1)
               (texto->numero (valor->string (car args)))))
      (print-prim ()
        (begin (verificar-aridad p args 1)
               (eopl:printf "~a~%" (valor->texto (car args)))
               (unit-val))))))

;;; ==========================================================
;;; 8. Interpretador
;;; ==========================================================
;; Los argumentos y las ligaduras se evalúan de izquierda a derecha.
;; atomic, abort, new y field llegan en la segunda entrega.

(define evaluar-lista
  (lambda (exps amb)
    (if (null? exps)
        '()
        (let ((v (value-of (car exps) amb)))
          (cons v (evaluar-lista (cdr exps) amb))))))

(define evaluar-secuencia
  (lambda (ultimo exps amb)
    (if (null? exps)
        ultimo
        (evaluar-secuencia (value-of (car exps) amb) (cdr exps) amb))))

(define evaluar-prueba
  (lambda (contexto exp amb)
    (let ((v (value-of exp amb)))
      (cases valor v
        (bool-val (b) b)
        (else (eopl:error contexto "la prueba debe ser bool y se encontró ~a"
                          (tipo-valor v)))))))

(define evaluar-condicional
  (lambda (pruebas ramas sino amb)
    (cond ((null? pruebas) (value-of sino amb))
          ((evaluar-prueba 'if (car pruebas) amb) (value-of (car ramas) amb))
          (else (evaluar-condicional (cdr pruebas) (cdr ramas) sino amb)))))

(define evaluar-ciclo
  (lambda (prueba cuerpo amb)
    (if (evaluar-prueba 'while prueba amb)
        (begin (value-of cuerpo amb)
               (evaluar-ciclo prueba cuerpo amb))
        (unit-val))))

(define verificar-no-vacio
  (lambda (contexto ids)
    (if (null? ids)
        (eopl:error contexto "se requiere al menos una ligadura")
        #t)))

(define aplicar-procedimiento
  (lambda (f args)
    (cases valor f
      (proc-val (ids cuerpo amb-def)
        (if (= (length ids) (length args))
            (value-of cuerpo (extender-ambiente ids args amb-def))
            (eopl:error 'apply "el procedimiento espera ~s argumento(s) y recibió ~s"
                        (length ids) (length args))))
      (else (eopl:error 'apply "se esperaba un procedimiento y se encontró ~a"
                        (tipo-valor f))))))

(define value-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (n) (int-val n))
      (flotante-exp (n) (float-val n))
      (cadena-exp (s) (string-val (cadena->string s)))
      (caracter-exp (c) (char-val (caracter->char c)))
      (true-exp () (bool-val #t))
      (false-exp () (bool-val #f))
      (unit-exp () (unit-val))
      (var-exp (id) (buscar-ambiente amb id))
      (let-exp (ids exps cuerpo)
        (begin
          (verificar-no-vacio 'let ids)
          (value-of cuerpo (extender-ambiente ids (evaluar-lista exps amb) amb))))
      (ref-exp (e)
        (ref-val (nueva-celda! (value-of e amb))))
      (deref-exp (e)
        (leer-celda (valor->ref (value-of e amb))))
      (set-exp (e1 e2)
        (let* ((r (valor->ref (value-of e1 amb)))
               (v (value-of e2 amb)))
          (begin (escribir-celda! r v)
                 (unit-val))))
      (begin-exp (e1 es)
        (evaluar-secuencia (value-of e1 amb) es amb))
      (if-exp (prueba entonces pruebas-elif ramas-elif sino)
        (evaluar-condicional (cons prueba pruebas-elif)
                             (cons entonces ramas-elif)
                             sino amb))
      (while-exp (prueba cuerpo)
        (evaluar-ciclo prueba cuerpo amb))
      (prim-exp (p args)
        (aplicar-primitiva p (evaluar-lista args amb)))
      (proc-exp (ids tipos cuerpo)
        (proc-val ids cuerpo amb))
      (apply-exp (rator rands)
        (let ((f (value-of rator amb)))
          (aplicar-procedimiento f (evaluar-lista rands amb))))
      (letrec-exp (tipos-ret nombres idss tiposs cuerpos cuerpo)
        (begin
          (verificar-no-vacio 'letrec nombres)
          (value-of cuerpo (extender-ambiente-rec nombres idss cuerpos amb))))
      (else (eopl:error 'value-of "se implementa en la segunda entrega")))))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================
(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (inicializar-almacen!)
        (cases programa pgm
          (un-programa (decls cuerpo) (value-of cuerpo (ambiente-vacio))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

(provide (all-defined-out))