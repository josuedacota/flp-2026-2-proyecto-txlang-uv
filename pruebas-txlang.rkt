#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334 

#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")

;; Agregue aquí las pruebas de la primera entrega: literales de cada clase,
;; let, ref/deref/set, begin, if con elif, while, cada primitiva, proc y
;; apply, letrec con recursión mutua. Por ejemplo:
;;
;; (prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
;; (prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
;; (prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")

(ida-y-vuelta "unparse: literales de cada clase"
  "let a = 3.14, b = -7, c = 'x', d = \"hola mundo\", e = true, f = unit in a end")
(ida-y-vuelta "unparse: comentarios de línea y de bloque"
  "// comentario de línea
   #| comentario
      de bloque |#
   +(1, 2) // otro")
(ida-y-vuelta "unparse: if con varios elif"
  "if true then 1 elif false then 2 elif true then 3 else 4 end")
(ida-y-vuelta "unparse: proc con tipos de orden superior"
  "let f = proc (g: (int, bool -> unit), r: ref(ref(float))) 1 end in apply f (2, 3) end")
(ida-y-vuelta "unparse: letrec con recursión mutua"
  "letrec bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end
          bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end
   in apply par (4) end")
(ida-y-vuelta "unparse: record, new y field"
  "record Cuenta { titular: string, saldo: ref(int) }
   let c = new Cuenta { titular = \"Ana\", saldo = ref(300) } in deref(field(c, saldo)) end")
(ida-y-vuelta "unparse: atomic anidado"
  "atomic begin atomic abort(\"x\") onfail (m) print(m) end; 1 end onfail (m) -1 end")

;; Rechazos del parser 
(prueba-rechazo "parser: | dentro de un comentario de bloque" "#| a | b |# 1")
(prueba-rechazo "parser: if sin else" "if true then 1 end")
(prueba-rechazo "parser: palabra reservada como identificador" "let let = 1 in 2 end")

(prueba "literal flotante" "3.14" "3.14")
(prueba "literal cadena" "\"hola\"" "hola")
(prueba "literal carácter" "'a'" "a")
(prueba "comentarios no cambian el valor" "// hola
  #| bloque |# 5" "5")

;; Primitivas
(prueba "suma" "+(1, 2)" "3")
(prueba "resta con negativo literal" "-(5, -3)" "8")
(prueba "multiplicación" "*(6, 7)" "42")
(prueba "división entera" "/(7, 2)" "3")
(prueba "resto" "%(7, 2)" "1")
(prueba "suma de flotantes" "+(1.5, 2.5)" "4.0")
(prueba "división de flotantes" "/(1.0, 4.0)" "0.25")
(prueba-rechazo "división por cero entera" "/(1, 0)")
(prueba-rechazo "resto por cero" "%(1, 0)")
(prueba-rechazo "división por cero flotante" "/(1.0, 0.0)")
(prueba-rechazo "suma de int y float" "+(1, 2.5)")
(prueba-rechazo "resto con flotantes" "%(5.0, 2.0)")
(prueba "menor" "<(1, 2)" "true")
(prueba "mayor o igual" ">=(2, 2)" "true")
(prueba "igualdad de cadenas" "==(\"a\", \"a\")" "true")
(prueba "diferente" "!=(1, 2)" "true")
(prueba-rechazo "igualdad entre int y float" "==(1, 1.0)")
(prueba "and" "and(true, false)" "false")
(prueba "or" "or(false, true)" "true")
(prueba "not" "not(true)" "false")
(prueba-rechazo "not con un entero" "not(1)")
(prueba "concat" "concat(\"ho\", \"la\")" "hola")
(prueba "length" "length(\"hola\")" "4")
(prueba "to-string" "to-string(42)" "42")
(prueba "to-number entero" "to-number(\"42\")" "42")
(prueba "to-number flotante" "to-number(\"3.5\")" "3.5")
(prueba-rechazo "to-number de texto no numérico" "to-number(\"abc\")")
(prueba "print devuelve unit" "print(\"hola\")" "unit")
(prueba-rechazo "aridad: + con un argumento" "+(1)")

;; Ambiente, let y variables
(prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
(prueba "let: las ligaduras no se ven entre sí"
  "let a = 1 in let a = 2, b = a in b end end" "1")
(prueba "let anidado sombrea" "let x = 1 in let x = 2 in x end end" "2")
(prueba-rechazo "variable no ligada" "x")

;; Almacén: ref, deref, set
(prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
(prueba "set devuelve unit" "let c = ref(0) in set c := 1 end" "unit")
(prueba "celdas independientes"
  "let a = ref(1), b = ref(2) in begin set a := 10; deref(b) end end" "2")
(prueba "alias de una celda"
  "let a = ref(1) in let b = a in begin set b := 7; deref(a) end end end" "7")
(prueba-rechazo "deref de un entero" "deref(5)")
(prueba-rechazo "set sobre un valor que no es referencia" "set 1 := 2")
(begin
  (inicializar-almacen!)
  (let* ((i (nueva-celda! (int-val 9)))
         (j (nueva-celda! (int-val 1))))
    (begin
      (escribir-celda! i (int-val 5))
      (verificar "almacén: escritura y lectura" (valor->texto (leer-celda i)) "5")
      (verificar "almacén: celdas distintas" (valor->texto (leer-celda j)) "1")
      (verificar "almacén: tamaño" (number->string (tamano-almacen)) "2"))))
(begin
  (inicializar-almacen!)
  (let llenar ((n 0))
    (if (< n 100) (begin (nueva-celda! (int-val n)) (llenar (+ n 1))) #f))
  (verificar "almacén: crece más allá de la capacidad inicial"
             (valor->texto (leer-celda 99)) "99"))
(begin
  (inicializar-almacen!)
  (verificar "almacén: referencia inexistente"
             (with-handlers ((exn:fail? (lambda (e) "error")))
               (valor->texto (leer-celda 0)))
             "error"))

;; Secuencia, condicionales y ciclo
(prueba "begin devuelve el último" "begin 1; 2; 3 end" "3")
(prueba "if rama then" "if true then 1 else 2 end" "1")
(prueba "if rama else" "if false then 1 else 2 end" "2")
(prueba "if con varios elif" "if false then 1 elif false then 2 elif true then 3 else 4 end" "3")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")
(prueba-rechazo "elif con prueba no booleana" "if false then 1 elif 5 then 2 else 3 end")
(prueba "while devuelve unit" "while false do 1 end" "unit")
(prueba "while acumula"
  "let i = ref(0), s = ref(0) in
   begin
     while <(deref(i), 5) do
       begin set i := +(deref(i), 1); set s := +(deref(s), deref(i)) end
     end;
     deref(s)
   end end" "15")
(prueba-rechazo "while con condición no booleana" "while 0 do 1 end")

;; Procedimientos, clausuras y letrec
(prueba "proc y apply" "let f = proc (x: int, y: int) +(x, y) end in apply f (3, 4) end" "7")
(prueba "apply de proc anónimo" "apply proc (x: int) *(x, 2) end (21)" "42")
(prueba "alcance léxico"
  "let x = 1 in let f = proc (y: int) +(x, y) end in let x = 100 in apply f (1) end end end" "2")
(prueba "clausura que recuerda su celda"
  "let mk = proc (n: int) let c = ref(n) in proc () begin set c := +(deref(c), 1); deref(c) end end end end
   in let k = apply mk (10) in begin apply k (); apply k () end end end" "12")
(prueba "referencia como argumento"
  "let c = ref(10), inc = proc (r: ref(int)) set r := +(deref(r), 1) end
   in begin apply inc (c); apply inc (c); deref(c) end end" "12")
(prueba-rechazo "apply de un valor que no es procedimiento" "apply 5 (1)")
(prueba-rechazo "apply con aridad incorrecta" "let f = proc (x: int) x end in apply f (1, 2) end")
(prueba "letrec factorial"
  "letrec int fact (n: int) = if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end
   in apply fact (5) end" "120")
(prueba "letrec recursión mutua"
  "letrec bool par (n: int) = if ==(n, 0) then true else apply impar (-(n, 1)) end
          bool impar (n: int) = if ==(n, 0) then false else apply par (-(n, 1)) end
   in apply par (10) end" "true")
(prueba "letrec con ciclo y celdas"
  "letrec int suma (c: ref(int), n: int) =
     if ==(n, 0) then deref(c) else begin set c := +(deref(c), n); apply suma (c, -(n, 1)) end end
   in apply suma (ref(0), 4) end" "10")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")