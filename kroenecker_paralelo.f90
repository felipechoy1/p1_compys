!    2021-05-17
!    kronecker_paralelo.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este es un programa que calcula el producto de kronecker de dos matrices de dimensión arbitraria
!    implementado de forma paralela para 2 núcleos usando MPI

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 20.04 WSL) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpif90 -Wall -pedantic -std=f95 -c -o kronecker_paralelo.o kronecker_paralelo.f90
!    mpif90 -o kronecker_paralelo.x kronecker_paralelo.o
!    mpirun -np 2 kroenecker_paralelo.x

!    Copyright (C) 2021
!    Este programa toma de estructura en gran medida de los programas
!    trabajados por Pablo Martínez () y Bryant Morazán (bryant.morazan@gmail.com)
!    quienes autorizaron su uso por lo que no es considerado de autoría original
!    mía. 
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

USE mpi
!Se incluye el módulo de MPI
IMPLICIT NONE   
!Variables para MPI
INTEGER :: ierr,rank,size
INTEGER(4),DIMENSION(MPI_STATUS_SIZE)::status 

    
!Se declaran las matrices a multiplicar y la que almacena el resultado inicializadas en 0
REAL(8),DIMENSION(:,:),ALLOCATABLE :: A,B,C1,C2
!Indicadores de posición en cada uno (y para dimensiones)
INTEGER(4):: An,Am,Bn,Bm,Cn,Cm
!Iteradores auxiliares
INTEGER(4)::i,j,k,l,r


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


!INICIALIZACIÓN DE MPI
CALL MPI_INIT(ierr)
!Detener programa con errores en ierr de MPI
IF (ierr /= 0 ) STOP 'MPI_INIT error'

!Se inicia el numero de núcleos a utilizar para el comunicador
CALL MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)

!Variable que separa los gajos que serán iterados en cada núcleo, ya que aquí son 2 
!se divide por esa cantidad.
r=An/2

IF (size==2) THEN 

    CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

    IF(rank == 0) THEN
        !Ejecución del producto de kroenecker
        !Primero se le da el tamaño a C1
        ALLOCATE(C1(r*Bn,Am*Bm))
        DO i = 1,r !Iteración para repartir el primer bloque
            DO k=1,Bn
                DO j=1,Am
                    DO l=1,Bm
                    !Se rellena por definición cada componente
                        C1((i-1)*Bn + k,(j-1)*Bm+l) = A(An,Am)*B(Bn,Bm)
                    END DO
                END DO  
            END DO 
        END DO  

           !Se muestra la matríz resultante en pantalla
        ! PRINT *,'C1=' 
        ! DO i=1, Am*Bm
        !     PRINT *, '|',C1(:,i),'|'
        ! END DO
    END IF

    IF(rank == 1) THEN
        
        !Ejecución del producto de kroenecker
        !Primero se le da el tamaño a C2
        ALLOCATE(C2(An*Bn-r*Bn,An*Bm))
        DO i = 1,An-r !Iteraciones para repartir el segundo bloque
            DO k=1,Bn
                DO j=1,Am
                    DO l=1,Bm
                    !Se rellena por definición cada componente
                        C2((i-1)*Bn + k,(j-1)*Bm+l) = A(An,Am)*B(Bn,Bm)
                    END DO
                END DO  
            END DO 
        END DO  
        !  PRINT *, 'C2=' 
        ! DO i=1, Am*Bm
        !     PRINT *, '|',C2(:,i),'|'
        ! END DO
    END IF    
END IF


CALL MPI_FINALIZE(ierr)
END PROGRAM kronecker
