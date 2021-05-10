!    2021-05-17
!    gamma.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!   Este es un programa que encuentra la función
!   gamma para un z dado utilizando su definición 
!   de limite

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu Linux) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o gamma.o gamma.f90
!    gfortran -o gamma.x gamma.o 
!
!    Copyright (C) 2021 
!    Éste código toma como corazón la presentación mostada por
!    Hugo Galileo Cardona en el curso de Física computacional por lo que
!    no es considerado de mi autoría original. El autorizó el uso de su estructura
!    de manera parcial. 
!    hugo.ecfm@gmail.com
!    felipechoy1@gmail.com

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

program gammaf

IMPLICIT NONE

!Declaración de variables a utilizar
REAL(16) :: z,fac, frac, gamma !Variables auxiliares para numerador, denominador y resultado respectivamente
INTEGER(16) :: n, i !Auxiliares para cíclos do

!Se inicializan las variables auxiliares
frac=1
fac=1

!Se lee el z asginado
z=9/5

!Se ajusta el n correspondiente
n=1750-z

!Programación defensiva
IF (z<1) THEN
    PRINT*, "Z debe ser mayor o igual  a 1"  

!Ejecución para el numerador como denominador    
ELSE
    !Se calculae el factorial de n y se guarda en fac
    DO i=1, n
        fac=fac*i
    END DO

    !Se obtiene el denominador por la definición dada en el límite, se realiza hasta el tope asignado
    DO i=0, n
        frac=frac*(z+i)
    END DO

    !Se retorna el valor de gamma
    gamma=(fac/frac)*(n**z)

    PRINT*, "El valor de gamma es",gamma,"Para un z=",z

END IF

END PROGRAM gammaf