!    2021-05-17
!    goldbach.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este programa demuestra la conjetura de Golbach
!    encontrando los números primos menores a un número
!    luego genera dos archivos de texto mostrando cuantas formas se puede 
!    escribir un conjunto de números como su suma de 2 y 3 primos.   
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o goldbach.o goldbach.f90
!    gfortran -o goldbach.x goldbach.o 
!       
!    Copyright (C) 2021
!    Este programa es de autoría original y  propiedad de Laura Portillo (lauraportillo720@gmail.com) 
!    expuesto en el curso de Física Computacional en el año 2021. Ella autorizó 
!    el uso de este programa para la implementación de algoritmos y pruebas 
!    de rendimiento.

!    Felipe Ixcamparic
!    (felipechoy1@gmail.com)

!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.


!===============================================================================================
!                                       PROGRAMA PRINCIPAL
!===============================================================================================




PROGRAM Goldbach
    IMPLICIT NONE
    integer,external :: primo               !estos serán los return de las funciones este de la función primo
    integer,external :: sumadeprimos2       !de la función suma de primos 2, es decir, v_{n,2}
    integer,external :: sumadeprimos3       !de la función suma de primos 2, es decir, v_{n,2}
    INTEGER :: i,n

    n=2500

    !lAS 9 LÍNEAS QUE ESTÁN COMENTADAS ABAJO NO SON NECESARIAS, ES SÓLO LA IMPLEMENTACIÓN
    !DE LA INTERACCIÓN CON EL USUARIO, PERO SE PUEDE HACER QUE SE INGRESE EN NÚMERO EN EL CÓDIGO

    !INTEGER :: n
    !WRITE(*,*)'DAME UN NUMERO' 
    !READ (*,*) n

    !Lo siguiente da todos los números primos menores al n ingresados por el usuario
    !DO i=2, n-1
    !    IF(primo(i).NE.0)THEN
    !        PRINT*, primo(i)
    !    END IF
    !END DO

  

    !Escribe de cuántas formas se esccribe como suma de 2 primos en un archivo
    open(11, file="datos2.txt")
        do i=4,n !Lo hace para todos los números desde el 4 al 1000, encuentra de cuántas formas se puede escribir como suma de dos primos y lo guarda en un archivo
             IF(MOD(i,2)==0)THEN
                 write(11,*)i,";    ",sumadeprimos2(i)
             END IF
         end do
     close(11)


    !Escribe de cuántas formas se esccribe como suma de 3 primos en un archivo
     open(12, file="datos3.txt")
        do i=6,n
            write(12,*)i,";     ",sumadeprimos3(i) !Caclula v3 para todos lo números entre 6 y 1000.
         end do
     close(12)

END PROGRAM Goldbach
!Aquí termina el programa



!===============================================================================================
!                                         FUNCION PRIMOS
!===============================================================================================


!Si el número es primo devuelve el número, sino devuelve cero.


INTEGER FUNCTION primo(n)
    !Argumentos
    INTEGER, INTENT(IN) :: n  !este es el valor del cual se quiere saber si es primo o no
    integer :: incremen  !esto es una bandera que irá incrementando
    !variables
    LOGICAL :: pri !esto es un booleano que índica que es primo si es positivo
    pri = .true. !Se inicializa de esta forma
    incremen = 2  !incremen será el número por el que se irá dividiendo el número n, entonces inicia desde 2. Ya que los primos sólo son divisibles por 1 y por él mismo
    !Cuerpo de la función
    DO WHILE(pri .eqv. .true. .and. incremen<n) !Mientras el número siga siendo primo y el incremento no haya llegado a "n" se irá evaluando el residuo de las divisiones.
        IF (MOD(n,incremen)==0) THEN !Si el residuo entre n y el incremen es cero quiere decir que es divisible y por lo tanto no es primo, es compuesto
        pri= .false. !Sólo si lo de arriba pasa pri pasa a ser falso y se sale del while. 
        END IF
        incremen = incremen + 1 !Aunque no se cumpla el if, incremen va aumentando por 1 para ir probando todas opciones en las que n puede ser dividido.
    END DO
    IF(pri .eqv..true.)THEN !Si el booleano se mantuvo como verdadero quiere decir que no se encontro un valor de incremen que fuera un factor de n, por lo tanto es un número primo
        primo=n !la variable primo la refinimos como el return de esta función hasta arriba. Así que si "n" es primo, el return de esta función será el número n.
    ELSE  
        primo=0 !si no es primo, el return será cero.
    END IF    
END FUNCTION primo




!===============================================================================================
!                                     FUNCION SUMADEPRIMOS2
!===============================================================================================


!dice de cuántas formar se puede escribir n como la suma de dos primos. 
!imprime de qué forma puede escribirse.


INTEGER FUNCTION sumadeprimos2 (n)
    integer,external :: primo !Esto se hace para poder usar una función dentro de otra, llamamos a la función primo aquí dentro.
    INTEGER, INTENT(IN) :: n !este será el número del cuál queremos saber de cuántas formas se puede escribir como la suma de dos primos.
    INTEGER :: v2, i, m  !m, i son contadores y v2 es de cuántas formas se puede escribir como la suma de dos primos.
    m = n/2 !Esto se hace porque vamos a considerar todas las posibles combinaciones de suma de los números primos menores a n, para evitar casos repetidos es decir 10=7+3 y 10=3+7 de esos solo tomar uno.
    !Es decir se va a dividir los números primos menores a n en dos, y se agarrará la primera mitad "m" para ir probando si sumando con n-m (que es lo que le falta para llegar a n) y verificar que  m y (n-m) sean primos.
    v2=0 !Se inicializa la variable en cero siempre para que no se sobrecargue.
    DO i=2, m !Entonces desde 2 que es el número primo más pequeño hasta m.
        IF(primo(i)== i) THEN !si i es primo
            IF (primo(n-i)== n-i) THEN !se verifica que (n-i) también sea primo
                !PRINT*, n, " = " , i, " + " , n-i !si ambos son primos entonces n se puede escribir como la suma de i + n-i, dos primos
                v2 = v2 + 1 !ya que se encontró una forma para escribir a n como la suma de dos primos v2 va aumentando hasta haber terminado el ciclo for que evalua todas las posibles sumas desde i=2 hasta i=m (:
            END IF    
        END IF
    END DO
    !PRINT*, n, " se puede escribir de " , v2 , " formas como la suma de 2 números primos" !Eso es para imprimir de cuántas formas se puede escribir n como la suma de dos primos
    sumadeprimos2 = v2 !sumadeprimos es nuestro return, entonces lo igualamos a v2. así que esta función regresa de cuántas formas se puede escribir n como la suma de 2 primos.
END FUNCTION sumadeprimos2




!===============================================================================================
!                                     FUNCION SUMADEPRIMOS3
!===============================================================================================



!dice de cuántas formas se puede escribir n como la suma de tres primos.
!imprime de qué forma puede escribirse.



INTEGER FUNCTION sumadeprimos3 (n)
    integer,external :: primo !función externa primo
    INTEGER, INTENT(IN) :: n !entrada
    INTEGER, dimension(n) :: v !vector v de dimensión n
    INTEGER :: i, p, q, r, v3, c, suma !i, p, q, r son contadores de ciclos, "c" es el tamaño del vector con los primos menores a n, suma es una variable y v3 será nuestro return
    v3=0 !Se inicializa v3 como cero para no sobrecargar la variable
    c=1! se asumen que la cantidad de primos menores a n es cero.
    DO i=2, n !Esto es para encontrar el valor del tamaño "c" del vector que contendrá los primos menores a n.
        IF(primo(i)==i)THEN !Se evaluan los números menores a "n" desde dos para ver cuáles son primos.
            v(c)=i !Como c comienza en 1 y "i" es primo entonces se almacena este primo en la primera casilla del vector c.
            c=c+1 !luego se va corriendo a la siguente casilla.
        END IF 
    END DO
    !ahora sabemos que el vector "v" tiene lleno hasta la casilla "c" de números primos menores a "n".
    !lo siguiente que se hará es colocar 3 veces el vector v y evaluar todas las posibles combinaciones y ver si suman n. 
    !es decir, casilla 1 + casilla 1 + casilla 1, luego casilla 1 + casilla 1 + casilla 2, etc. 
    !notar que primero va cambiando la última posición, luego la segunda como si fueran 3 ciclos for anidados. 
    !para resolver el problema de tener combinaciones con los mismos primos pero en distinta posición for de hasta adentro comenzará con el índice del for que lo envuelve y este comenzará con el índice del for de hasta afuera.
    !Y el de hasta afuera irá desde la posición 1 hasta la c. Ya que así sabemos que sólo habrán números primos.
    DO p=1, c !Se harán 3 ciclos for o DO.
        DO q=p, c
            DO r=q, c
                suma= v(p) + v(q) + v(r) !Se calcula la suma de las distintas casillas
                IF (suma==n)THEN !Se evalúa que la variable suma sí sume n.
                    v3= v3 + 1 !Si se encontró una posible combinación entonces el contador aumenta
                    !PRINT*, n, "n = " , v(p) , " + " , v(q) , " + " , v(r) !esto es para imprimir cuáles son las formas de escribir n como la suma de 3 primos.
                END IF
            END DO
        END DO
    END DO
    !PRINT*, n, " se puede escribir de " , v3 , " formas como la suma de 3 números primos"
    sumadeprimos3 = v3  !sumadeprimos es el return de la función que se iguala a v3, que es de cuántas formas se puede escribir n como la suma de 3 primos.
END FUNCTION sumadeprimos3

