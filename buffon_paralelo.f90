!    2021-05-17
!    buffon_paralelo.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)
!
!    Este programa realiza una simulación de 
!    la aguja de Buffon de forma paralela utilizando MPI
!    arrojando una cantidad n de ajugas , mostrando la
!    a aproximación de PI como su error absoluto. 
!  
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    mpif90 -Wall -pedantic -std=f95 -c -o buffon_paralelo.o buffon_paralelo.f90
!    mpif90 -o buffon_paralelo.x buffon_paralelo.o 
!    mpirun -np 2 ./buffon_paralelo.x
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
USE mpi
IMPLICIT NONE

!Variables MPI
integer(4) :: rank, ierr, size 
INTEGER(4),DIMENSION(MPI_STATUS_SIZE)::status 

!Valor de Pi para comparar en el error absoluto 
REAL(16)::pi = 3.1415926535897932384626433832795028841971693993751058209749445923078,abserr

REAL(8)::theta,x,piexp
INTEGER(8)::n,i,contador1,contador2,m,test

!Número de agujas a simular
n=1000000000
!Separador de iteraciones
m=n/2
!Inicialización de variables
theta=0
contador1=0
contador2=0
abserr=0
test=0


!Simulación de las agujas de Buffon

!Se inicializa MPI 
CALL MPI_INIT(ierr)                                  
IF ( ierr /= 0 ) stop "Error al inicializar MPI"       
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)       


IF (size==2) THEN

    !Se establece la comuniación entre los núcleos
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)        

    IF (rank==0) THEN

        DO i=1,m
            
            theta=0
            x=0
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
                contador1=contador1+1  

                !Debugging
                ! PRINT*,"Le pego"
            END IF 
        END DO

        !Se recible el número de iteraciones recibido de contador 2
        !y se almacena en la variable test
        CALL MPI_RECV(test,1,MPI_INT,1,1,MPI_COMM_WORLD,status,ierr)


        !Se guarda pi como su error absoluto 
        piexp=2/(REAL(contador1+test)/REAL(n))
        abserr=abs(piexp-pi)


        !Impresión de resultados
        PRINT*,"pi",piexp
        PRINT*,"Error absoluto",abserr


    END IF



    IF (rank==1) THEN

        DO i=m,n
        
        theta=0
        x=0
        !Se llama a un aleatorio que ocupará theta
        !luego a este se le deja de 0 a pi/2
        CALL RANDOM_NUMBER(theta)
        theta=theta*3.1415/2
        
        !Se llama a un aleatorio de 0 a 1/2 para x
        CALL RANDOM_NUMBER(x)
        x=x/2

        !Se pasa la condición para que toque o no la línea
        !paralela
        IF(x <= 0.5*SIN(theta) ) THEN
            !Si se da el caso se aumenta en 1 el contador
            contador2=contador2+1  

            !Debugging
            ! PRINT*,"Le pego"
        END IF 

        END DO

        !Se envía el resultado almacenado de contador2 hacia el primer núcleo
        CALL MPI_SEND(contador2,1,MPI_INT,0,1,MPI_COMM_WORLD,ierr)

    END IF



ELSE
PRINT*,"Este programa solo funciona utilizando 2 núcleos"
END IF

!Se finaliza MPI
call MPI_FINALIZE(ierr)


END PROGRAM buffon