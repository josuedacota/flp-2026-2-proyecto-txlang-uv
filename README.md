# Proyecto final — Interpretador de TxLang-UV

Fundamentos de Interpretación y Compilación de Lenguajes de Programación
Escuela de Ingeniería de Sistemas y Computación, Universidad del Valle
Carlos Andrés Delgado Saavedra

Un interpretador para TxLang-UV, un lenguaje imperativo con referencias de
primera clase, bloques `atomic` que confirman o revierten sus escrituras,
registros y chequeo estático de tipos, escrito en Racket con `#lang eopl` y
SLLGEN. El enunciado completo, con la gramática, la semántica, las rúbricas y
los diez programas del anexo, está publicado en el Campus Virtual. Este
repositorio es el punto de partida del grupo.

## Integrantes

Esta tabla es lo que identifica al grupo, y coincide con el grupo conformado
en el Campus Virtual. Sin ella, o con algún dato incompleto, la entrega
pierde el 20 %; quien no aparezca aquí no cuenta como parte del grupo.

| Nombre completo | Código | Correo institucional |
|---|---|---|
| Josue David Cocoma Tascon | 2477087 | correo@correounivalle.edu.co |
| Juan Diego Montaño Vergara | 2477334 | correo@correounivalle.edu.co |

## Entregas

Las dos entregas se hacen sobre este mismo fork; lo que cambia es el estado
del repositorio en cada fecha.

| Entrega | Fecha y hora | Alcance | Nota |
|---|---|---|---|
| Primera | lunes 9 de noviembre de 2026, 17:59 | Especificación léxica y gramatical, datatypes del árbol, `unparse`, ambiente, almacén y `value-of` de todo lo que no sea transacciones, registros ni tipos; al menos 15 programas de prueba | Sin nota directa; no hacerla descuenta el 25 % de la nota del proyecto |
| Final | lunes 14 de diciembre de 2026, 17:59 | Todo lo anterior, más transacciones anidables, registros, chequeador de tipos, los dos informes en `docs/` y al menos 20 programas de prueba con 5 mal tipados | Es la que se califica |
| Sustentación | martes 15 de diciembre de 2026 | Individual; su nota, entre 0 y 1, multiplica la del grupo | |

## Cómo se entrega

1. **Se trabaja sobre un fork de este repositorio.** El grupo hace fork, lo
   deja público y trabaja ahí. Una entrega que no sea fork de este
   repositorio pierde el 30 % de la nota.
2. **El enlace del fork se pega en la actividad de cada entrega en el
   Campus Virtual.** Es el mismo enlace las dos veces. Nada más: ni
   archivos, ni comprimidos, ni código por correo. El grupo que no lo
   registre dentro del plazo tiene 0.0 en esa entrega.
3. **Se califica el último commit anterior a la fecha y hora de cierre.** Lo
   que se suba después no se tiene en cuenta. La primera entrega se
   comprueba en la sustentación, en el historial de commits.
4. Solo archivos de texto: código `.rkt` en la raíz e informes `.md` en
   `docs/`. Fórmulas en LaTeX y diagramas en Mermaid dentro del Markdown; no
   se aceptan PDF, DOCX ni imágenes.
5. Las primeras líneas de cada `.rkt` llevan los autores:
   `;Autores: Nombre1 Codigo1, Nombre2 Codigo2`.
6. El historial de commits se revisa para ver el aporte de cada integrante,
   y ese aporte es un factor de la nota. Los cambios de puro formato o de
   comentarios no cuentan.

## Cómo está organizado

```
interprete-txlang.rkt        el interpretador, en nueve secciones
pruebas-txlang.rkt           la batería de pruebas del grupo
ejemplos-anexo.rkt           los diez programas del anexo del enunciado
docs/informe-correccion.md   se llena la plantilla (entrega final)
docs/informe-ast.md          se llena la plantilla (entrega final)
```

`interprete-txlang.rkt` trae el andamiaje mínimo para que las baterías
corran: una gramática con un solo literal y la interfaz que usan las pruebas
(`scan&parse`, `unparse`, `correr`, `tipo-del-programa`). Las nueve secciones
están marcadas y en el orden en que se construye un interpretador; el grupo
las llena siguiendo el enunciado. Si hacen falta funciones auxiliares, van
en el mismo archivo. Las plantillas de `docs/` se llenan en el mismo
archivo, reemplazando los marcadores `{{...}}` y sin quitar las secciones.

## Cómo se corre

```bash
racket pruebas-txlang.rkt     # la batería del grupo
racket ejemplos-anexo.rkt     # los diez ejemplos del enunciado
```

También desde DrRacket, abriendo cualquiera de los dos archivos y
ejecutándolo. Para probar un programa suelto:

```racket
(require "interprete-txlang.rkt")
(correr "let c = ref(0) in begin set c := 5; deref(c) end end")
(unparse (scan&parse "if true then 1 else 2 end"))
```
