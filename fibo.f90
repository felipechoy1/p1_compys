!    2021-05-17
!    fibo.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este es un programa que encuenta el n-ésimo término
!    de la serie de Fibonacci empezando por n=1. 

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (UBUNTU 20.04) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o fibo.o fibo.f90
!    gfortran -o fibo.x fibo.o 
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

PROGRAM fibo

INTEGER(8)::n
!Se anota el n-ésimo término a mostrar
n=96

!Programación defensiva
IF (n<=0) THEN
    PRINT *,"n debe ser mayor o igual a 1"
ELSE
!Se muestra utilizando el método matricial
CALL matfibo(n-1)
!Se muestra usando el método secuencial 
CALL secfibo(n-1)
END IF
END PROGRAM fibo


!Subrutina para el método por matrices
SUBROUTINE matfibo(n)

IMPLICIT NONE

INTEGER(8),INTENT(IN) :: n 
INTEGER(8), DIMENSION(2,2)::fib,base
INTEGER(8)::i

!Se crea la base la cual se elevará al n-ésimo término
base(1,1)=1
base(1,2)=1
base(2,1)=1
base(2,2)=0

!Se inicializa la salida 
fib=base

!Casos base
IF (n==1) THEN
    fib=base
ELSE IF (n==0) THEN
    fib=0
END IF

!Se realiza la potencia multiplicando la matríz por la base
!las veces que dicte n, se inicia en 2 porque los demás casos
!ya estan contemplados 
DO i=2,n
    fib=MATMUL(fib,base)
END DO

!Se imprime el resutlado
PRINT *,"El",n+1,"término de la serie de Fibonacci es",fib(2,1), "(método por matriz)"
RETURN
END SUBROUTINE matfibo


!Subrutina para la implementación secuencial
SUBROUTINE secfibo(n)

INTEGER(8),INTENT(IN) :: n

!Se llaman a las variables que ya son conocidas y al iterador
INTEGER(8) :: f0 , f1,fn,i

!Se inicializan por los datos ya conocidos
f0=0
f1=1

!Casos iniciales 
IF (n==0) THEN
    fn=f0
ELSE IF (n==1) THEN
    fn=f1
END IF

!Cíclo para encontrar el n-éximo, denuevo, se inicia en 2
!porqrue los demás ya estan contemplados
DO i=2,n 
    !Para cada cíclo se hace la suma de los dos anteriores
    fn = f0 + f1 
    !Luego se hace un corrimiento
    f0 = f1 
    f1 = fn 
END DO
!Se imprime el resultado
PRINT *,"El ",n+1," término de la serie de Fibonacci es ", fn ,"(método secuencial)" !Se imprime el resultado
RETURN
END SUBROUTINE secfibo