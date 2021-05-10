!    2021-05-17
!    kronecker.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este es un programa que calcula el producto de kronecker de dos matrices de dimensión arbitraria

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 20.04 WSL) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o kronecker.o kronecker.f90
!    gfortran -o kronecker.x kronecker.o

!    Copyright (C) 2021
!    Este programa toma de estructura en gran medida de los programas
!    trabajados por Pablo Martínez (pabloversion1.0@gmail.com) y 
!    Bryant Morazán (bryant.morazan@gmail.com) quienes autorizaron su 
!    uso por lo que no es considerado de autoría original mía. 

!    Felipe Ixcamparic
!    (felipechoy1@gmail.com)
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

PROGRAM kronecker


IMPLICIT NONE

!Se declaran las matrices a multiplicar y la que almacena el resultado inicializadas en 0
REAL(8),DIMENSION(:,:),ALLOCATABLE :: A,B,C
!Indicadores de posición en cada uno (y para dimensiones)
INTEGER(4):: An,Am,Bn,Bm,Cn,Cm
!Iteradores auxiliares
INTEGER(4)::i,j,k,l


!Declarar dimensiones de las matrices
An=120
Am=120
ALLOCATE(A(An,Am))

Bn=120
Bm=120
ALLOCATE(B(Bn,Bm))

!Declarar componentes de cada matríz
!Para A
!A(n,m,....,r) = ... 
!Por motivos del funcionamiento dejare inicializada cada una 
!con un número cualquiera
A=5

!Para B
!B(n,m,...r)= ...
B=4


!Ejecución del producto de kroenecker
!Primero se le da el tamaño a C
allocate(C(An*Bn,Am*Bm))
DO i = 1,An
    DO k=1,Bn
        DO j=1,Am
            DO l=1,Bm
            !Se rellena por definición cada componente
                C((i-1)*Bn + k,(j-1)*Bm+l) = A(An,Am)*B(Bn,Bm)
            END DO
        END DO  
    END DO 
END DO  




!Se muestra la matríz resultante en pantalla
! PRINT *, 'A \kron B = C=' 
! DO i=1, Am*Bm
!     PRINT *, '|',C(:,i),'|'
! END DO
DEALLOCATE(A,B,C)
END PROGRAM kronecker
