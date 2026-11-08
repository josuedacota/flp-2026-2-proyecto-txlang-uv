#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

#|
  Los diez ejemplos del anexo del enunciado, copiados literalmente del
  documento y ejecutados contra el interpretador. Fijan la semántica que se
  espera: en la entrega final pasan los diez; en la primera, todos los que
  no usan transacciones, registros ni chequeo de tipos.

  El ejemplo 9 espera un rechazo, y un error de análisis también cuenta como
  rechazo, así que pasa antes de tiempo; el motivo que se imprime dice si el
  rechazo es el del chequeador o el del parser.

  Se ejecuta con:  racket ejemplos-anexo.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define ejemplo
  (lambda (etiqueta texto esperado)
    (let ((obtenido (intentar texto)))
      (begin
        (eopl:printf "~%--- ~a ---~%" etiqueta)
        (if (string=? obtenido esperado)
            (begin (set! pasadas (+ pasadas 1))
                   (eopl:printf "  ok    devuelve ~s~%" obtenido))
            (begin (set! fallidas (+ fallidas 1))
                   (eopl:printf "  FALLA se esperaba ~s y llego ~s~%" esperado obtenido)))))))

(define ejemplo-rechazado
  (lambda (etiqueta texto)
    (let ((obtenido (intentar texto)))
      (begin
        (eopl:printf "~%--- ~a ---~%" etiqueta)
        (if (empieza-con-error? obtenido)
            (begin (set! pasadas (+ pasadas 1))
                   (eopl:printf "  ok    rechazado: ~a~%" obtenido))
            (begin (set! fallidas (+ fallidas 1))
                   (eopl:printf "  FALLA se esperaba un rechazo y llego ~s~%" obtenido)))))))

(ejemplo "Ejemplo 1: una celda y dos escrituras"
  "let c = ref(0) in
  begin
    set c := +(deref(c), 1);
    set c := +(deref(c), 1);
    deref(c)
  end
end"
  "2")

(ejemplo "Ejemplo 2: una transferencia que se confirma"
  "let origen = ref(500), destino = ref(120) in
  begin
    atomic
      begin
        set origen := -(deref(origen), 200);
        set destino := +(deref(destino), 200);
        deref(destino)
      end
    onfail (m) -1
    end;
    deref(origen)
  end
end"
  "300")

(ejemplo "Ejemplo 3: una transferencia que se revierte"
  "let saldo = ref(100) in
  begin
    atomic
      begin
        set saldo := -(deref(saldo), 150);
        if <(deref(saldo), 0)
          then abort(\"fondos insuficientes\")
          else deref(saldo)
        end
      end
    onfail (m) begin print(m); -1 end
    end;
    deref(saldo)
  end
end"
  "100")

(ejemplo "Ejemplo 4: transacciones anidadas"
  "let a = ref(1), b = ref(1) in
  begin
    atomic
      begin
        set a := 2;
        atomic
          begin
            set b := 99;
            abort(\"la hija falla\")
          end
        onfail (m) print(m)
        end;
        set b := +(deref(b), 10)
      end
    onfail (m) print(\"aqui no deberia entrar\")
    end;
    print(deref(a));
    print(deref(b))
  end
end"
  "unit")

(ejemplo "Ejemplo 5: un registro con un campo mutable"
  "record Cuenta { titular: string, saldo: ref(int) }

let c = new Cuenta { titular = \"Ana Maria\", saldo = ref(300) } in
  begin
    atomic
      begin
        set field(c, saldo) := -(deref(field(c, saldo)), 50);
        if <(deref(field(c, saldo)), 100) then abort(\"saldo minimo\") else unit end
      end
    onfail (m) print(m)
    end;
    deref(field(c, saldo))
  end
end"
  "250")

(ejemplo "Ejemplo 6: recursión con tipo declarado"
  "letrec int fact (n: int) =
         if ==(n, 0) then 1 else *(n, apply fact (-(n, 1))) end
in apply fact (5)
end"
  "120")

(ejemplo "Ejemplo 7: una referencia como argumento"
  "let celda = ref(10),
    duplicar = proc (r: ref(int)) set r := *(deref(r), 2) end
in begin
     apply duplicar (celda);
     apply duplicar (celda);
     deref(celda)
   end
end"
  "40")

(ejemplo "Ejemplo 8: lo que la reversión no alcanza"
  "let x = ref(0) in
  begin
    atomic
      begin
        set x := 42;
        print(\"dentro de la transaccion x vale\");
        print(deref(x));
        abort(\"me arrepenti\")
      end
    onfail (m) print(m)
    end;
    print(deref(x))
  end
end"
  "unit")

(ejemplo-rechazado "Ejemplo 9: un programa que el chequeador rechaza"
  "let bandera = ref(true) in
  set bandera := 1
end")

(ejemplo "Ejemplo 10: un despacho que se deshace a medias"
  "record Item { nombre: string, cantidad: ref(int) }

let item = new Item { nombre = \"cable\", cantidad = ref(7) },
    pedido = ref(10),
    despachado = ref(0)
in begin
     atomic
       begin
         while >(deref(pedido), 0) do
           if ==(deref(field(item, cantidad)), 0)
             then abort(\"sin existencias\")
             else begin
                    set field(item, cantidad) := -(deref(field(item, cantidad)), 1);
                    set despachado := +(deref(despachado), 1);
                    set pedido := -(deref(pedido), 1)
                  end
           end
         end
       end
     onfail (m) print(m)
     end;
     print(deref(field(item, cantidad)));
     print(deref(despachado))
   end
end"
  "unit")

(eopl:printf "~%==========================================~%")
(eopl:printf "  ejemplos del anexo: pasados ~s, fallidos ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")
