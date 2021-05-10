!    2021-05-17
!    buffon.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este programa realiza una simulación de 
!    la aguja de Buffon arrojando una cantidad n
!    de ajugas , mostrando la aproximación de PI
!    como su error absoluto. 

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o buffon.o buffon.f90
!    gfortran -o buffon.x buffon.o 
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


PROGRAM buffon
IMPLICIT NONE

REAL(16)::pi = 3.1415926535897932384626433832795028841971693993751058209749445923078,abserr

REAL(8)::theta,x,piexp

INTEGER(8)::n,i,contador,j

!Número de agujas a simular
n=500000000


!Inicialización de variables
theta=0
contador=0
abserr=0

!Descomentar si se desea imprimir en un archivo de texto
! j=1000000 
! open(1, file="buf.txt")

! DO n=10,j,100
!Simulación de las agujas de Buffon
DO i=1,n
    
    theta=0
    !Se llama a un aleatorio que ocupará theta
    !luego a este se le deja de 0 a pi/2
    CALL RANDOM_NUMBER(theta)
    theta=theta*3.14/2
    
    !Se llama a un aleatorio de 0 a 1/2 para x
    CALL RANDOM_NUMBER(x)
    x=x/2

    !Se pasa la condición para que toque o no la línea
    !paralela
    IF(x <= 0.5*SIN(theta) ) THEN
        !Si se da el caso se aumenta en 1 el contador
        contador=contador+1  

        !Debugging
        ! PRINT*,"Le pego"
    END IF 
END DO

piexp=2/(REAL(contador)/REAL(n))
abserr=abs(piexp-pi)

!Descomentar si se quiere rellenar un archivo de texto 
!con distintos valores de n
! write(1, *) n, ";",piexp ,";", abserr
! contador=0
! END DO
! close(2)

!Se guardan los resultados en sus respectivas variables

!Impresión de resultados
PRINT*,"pi",piexp
PRINT*,"Error absoluto",abserr

END PROGRAM buffon