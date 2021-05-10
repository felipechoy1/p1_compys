!    2021-05-17
!    montyh.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este programa realiza una simulación del juego de 
!    Monty hall con los casos en donde se hace un cambio de puerta
!    y donde no hay cambio de puerta

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o montyh.o montyh.f90
!    gfortran -o montyh.x montyh.o 


!
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


PROGRAM montyhall
IMPLICIT NONE
INTEGER::persona,carro=1,cabra1=1,cabra2=1,monty=1,aux !Variables para cada participante 
REAL::u !Variable para el número aleatorio
INTEGER::i,n !Iterador
INTEGER::vic=0,lose=0 !Contadores de victorias o derrotas


n=1000000
DO i=1,n

!Asignación de posiciones a cada elemento

!Se genera un número aleatorio para el carro
CALL RANDOM_NUMBER(u)
carro = 1+ FLOOR(3*u)  

DO WHILE (cabra1 == carro)!Evita que sea la misma puerta para el carro
    CALL RANDOM_NUMBER(u)
    cabra1=1+FLOOR(3*u)
    END DO

DO WHILE (cabra2==cabra1 .OR. cabra2==carro) !Elige la puerta restante
    CALL RANDOM_NUMBER(u)
    cabra2=1+FLOOR(3*u)
END DO



!Selección de puerta por la persona
CALL RANDOM_NUMBER(u)
persona=1+FLOOR(3*u)

!Selección de Monty
CALL RANDOM_NUMBEr(u)
DO WHILE (monty==persona .OR. monty==carro) !Evita que elija el carro o lo mismo de la persona
    CALL RANDOM_NUMBER(u)
    monty=1+FLOOR(3*u)
END DO

!Debugging
! PRINT*,"Carro en",carro,"Cabras en",cabra1,cabra2
! PRINT*,"La persona elige la puerta", persona,"Pero monty abre",monty


IF (persona==carro) THEN
    vic=vic+1
ELSE
    lose=lose+1
END IF

END DO

PRINT *,"Caso sin cambio:: ","Victorias",vic,"Derrotas",lose




!CASO CON CAMBIO

!Reseteo de contadores
vic=0
lose=0

DO i=1,n

!Asignación de posiciones a cada elemento

!Se genera un número aleatorio para el carro
CALL RANDOM_NUMBER(u)
carro = 1+ FLOOR(3*u)  

DO WHILE (cabra1 == carro)

    CALL RANDOM_NUMBER(u)
    cabra1=1+FLOOR(3*u)
    END DO

DO WHILE (cabra2==cabra1 .OR. cabra2==carro)
    CALL RANDOM_NUMBER(u)
    cabra2=1+FLOOR(3*u)
END DO



!Selección de puerta
CALL RANDOM_NUMBER(u)
persona=1+FLOOR(3*u)

!Selección de Monty

CALL RANDOM_NUMBER(u)
DO WHILE (monty==persona .OR. monty==carro)
    CALL RANDOM_NUMBER(u)
    monty=1+FLOOR(3*u)
END DO

aux=persona
CALL RANDOM_NUMBER(u)
DO WHILE (persona==monty .OR. persona==aux)
    CALL RANDOM_NUMBER(u)
    persona=1+FLOOR(3*u)
END DO


!Debugging
! PRINT *,"Carro",carro,"cabras",cabra1,cabra2
! PRINT *,"Eleccion inicial",aux,"monty",monty
! PRINT *,"Cambio",persona


!Comprobación de victoria o derrota
IF (persona==carro) THEN
    vic=vic+1
ELSE
    lose=lose+1
END IF

END DO

PRINT *,"Caso con cambio:: ","Victorias",vic,"Derrotas",lose

END PROGRAM montyhall