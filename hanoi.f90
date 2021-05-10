!    2021-05-17
!    hanoi.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Programa que resuelve y enumera el rompecabezas de las 
!    torres de hanoi para n discos. Imprimiendo y enumerando
!    el número de pasos a realizar para resolverlo en el menor
!    número de pasos posible

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (SUSE Linux) 7.3.1, GNU Fortran
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o hanoi.o hanoi.f90
!    gfortran -o hanoi.x hanoi.o 
!    /usr/bin/time -f "%e %M %P" -a -o datos ./hanoi.x

!    Copyright (C) 2021 
!    Éste código toma como corazón la presentación mostada por
!    Jefferson S. Rodríguez (jefersonrodriguezleon@gmail.com) 
!     en el curso de Física computacional 2021 por lo que
!    no es considerado de mi autoría original. El autorizó el uso de este
!    código de manera parcial como total para implementaciones como 
!    pruebas de rendimiento 
!    
!    Felipe Ixcamparic
!    felipechoy1@gmail.com
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
!


PROGRAM Thanoi
  IMPLICIT NONE
  
  !Declaración de variables
  
  CHARACTER(*), PARAMETER :: A='A', B='B', C='C'!Etiquetas para cada poste
  INTEGER(4):: n,i=1 !Número de discos y el iterador de posición
  LOGICAL(4)::mov,pasos !Booleanos para indicar si se desean imprimir los pasos o cada movimiento
  
  !Condiciones iniciales (modificar según se desee)
  n =2 !Número de discos
  mov= .FALSE. !Desplegar cada movimiento en consola
  pasos=.TRUE. !Desplegar solamente cuántos movimientos toma
  
  !Llamada a la función hanoi
  call hanoi(n,A,B,C,i,mov)

  !Condición en la que se quieren mostrar el número de movimientos
  IF (pasos .eqv. .TRUE.) THEN
    !Imrpime el valor almacenado en el iterador i
    PRINT *,'Toman', i-1,'pasos para ',n,'discos'
  END IF

END PROGRAM Thanoi 
  
  
!Subrutina para realizar el método
RECURSIVE SUBROUTINE hanoi(n,A,B,C,i,movimiento)
IMPLICIT NONE
!Declaración de variables
INTEGER(4), INTENT(IN) :: n
INTEGER(4),INTENT(INOUT)::i
CHARACTER(*), INTENT(IN):: A,B,C
LOGICAL,INTENT(IN)::movimiento

!Caso base con un solo disco (también para cerrar cuando se necesita mover solo un disco en la torre)
  IF (n==1) THEN
    IF (movimiento .eqv.  .TRUE.) THEN
    !Este caso designa movimiento entre lo que se designe "último" y "final"
    !Realiza el movimiento
      PRINT*,i, 'Mover de ',A,' hacia ',C
    END IF
    !Se aumenta el valor de i para almacenar el paso realizado. 
    i = i + 1
  ELSE
    !Se aplica el método de ordenamiento hasta que no haya 
    !movimiento que hacer ya que estará ordenado
    
    !Se reduce el número con esta función hasta que n=1, cuando se llega
    !a  1 intercala entre "A" y "C" o "A" y "B" , dependiendo de si n es par o impar
    CALL hanoi(n-1, A,C,B,i,movimiento)

    !Esta función siempre actúa luego de que pasa la primera , intercala del A hacia el C
    !dependiendo de su etiqueta en el ciclo que se halle
    CALL hanoi(1, A, B, C,i,movimiento)

    !Realiza las intercalaciones intermedias haciendo los pasos en regreso de B hacia a o de B hacia C
    !Que denuevo, depende que queiran significar en el grado o función que se encuentre. 
    CALL hanoi(n-1, B, A, C,i,movimiento)
    
  END IF
      
END SUBROUTINE hanoi
