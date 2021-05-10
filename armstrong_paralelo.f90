!    2021-05-17
!    pingpong4.f90
!    Pablo Martínez (pabloversion1.0@gmail.com)

!    Programa que encuentra los números de armstrong para distintas bases de b
!    y extrae los resultados en base decimal en un archivo de texto llamado
!    'Armstrong.txt', el programa se lleva a cabo en distintos núcleos


!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 9.3.0-17ubuntu1~20.04) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o ArmstrongParalelo.o ArmstrongParalelo.f90
!    mpifort -o ArmstrongParalelo.x ArmstrongParalelo.o
!    mpirun -np 8 ./ArmstrongParalelo.x
!    Para obervar el análisis de tiempo, memoria y procesador, se sustituye 
!    la linea anterior por la siguiente
!    /usr/bin/time -f "%e %M %P" ./ArmstrongParalelo.x

!    Copyright (C) 2021
!    P. D. Martínez Zeceña
!    pabloversion1.0@gmail.com
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

PROGRAM ArmstrongParalelo
USE MPI
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER :: ierr                                     ! Variable para la señal de error
    INTEGER :: rank                                     ! ID del núcleo a usar
    INTEGER :: nprocs                                   ! Número de procesadores a usar
!    INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status      ! Estatus de envio o recibido
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER :: npb=50000000                        ! Cantidad de números por base
    INTEGER :: bmin=1, bama=10, bcal                    ! Base mínima, Base máxima
    INTEGER :: b, x, k, d, f , i                        ! Variables del problema
    REAL :: xreal,breal                                 ! Necesario para la función que calcula "k"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER, DIMENSION(8) :: wvec=(/0,0,0,0,0,0,0,0/)
    INTEGER :: suminf, sumsup                           ! Valor inferior y superior para definir el rango de bases por núcleo
    CALL MPI_Init(ierr)                                 ! Inicializar MPI
    IF (ierr/=0) STOP 'MPI_Init error'                  ! En caso de error se detiene el proceso
    CALL MPI_Comm_size(MPI_COMM_WORLD,nprocs,ierr)      ! Configura el tamaño de comunicación
    IF (ierr/=0) STOP 'MPI_Comm_size error'             ! En caso de error se detiene el proceso
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)        ! Configura el ID de cada proc.
    IF (ierr/=0) STOP 'MPI_Comm_rank error'             ! En caso de error se detiene el proceso
    bcal=bama-bmin
    IF (bcal<=0) STOP 'Orden incorrecto de cálculo'             ! Solo interesa calculo de bases ascendentes
    IF (nprocs>=9) STOP 'Más procesadores de los programados'   ! Programa para 8 o menos núcleos
    
    CALL vt(bcal,nprocs,wvec)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF (rank==0) THEN                                   ! Si estamos en el núcleo 0
        print*, wvec(1),wvec(2),wvec(3),wvec(4),wvec(5),wvec(6),wvec(7),wvec(8) ! Trabajo de cada núcleo
        OPEN(rank,file='Armstrong_N0.txt')              ! Abre y/o crea el archivo de texto para guardar los datos
        IF (wvec(rank+1)==0)THEN                        ! Si la cordenada 1 de wvec es cero, implica que el núcleo no trabaja
            print*, 'Núcleo ',rank,' sin uso'
        ELSE                                            ! En caso contrario, el procesador trabaja
            DO b=bmin, bmin+wvec(rank+1)-1              ! Desde la base mínima hasta la asignada
                WRITE(rank,*) 'Base=',b              
                DO x=1, npb                             ! Se consideran solo números positivos
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)              ! Se calcula el valor de "k" y se almacena en la variable k
                    DO i=0, k-1                         ! Se realiza una sumatoria desde i=0 hasta k-1
                        CALL di(i,x,b,d)                ! Se calcula el factor d_i y se almacena en la variable d                  
                        f=d**k+f                        ! Se realiza la sumatoria de d_i^k
                    END DO
                    IF (f==x)THEN                       ! Si f=x, este es un número de Armstrong
                        WRITE(rank,*) f                 ! y se almacena en un archivo .txt
                    END IF
                    f=0                                 ! Se eliminan los resultados anteriores
                END DO
            END DO
        END IF
    ELSE IF (rank==1) THEN                              ! Si estamos en el núcleo 1 
        OPEN(rank,file='Armstrong_N1.txt')              
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)
            sumsup=bmin+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup                          ! Rango de las bases asignadas al núcleo
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    ELSE IF (rank==2) THEN                              ! Si estamos en el núcleo 2
        OPEN(rank,file='Armstrong_N2.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)
            sumsup=bmin+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup                          ! Rango de las bases asignadas
                WRITE(rank,*) 'Base=',b              
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    ELSE IF (rank==3) THEN                              ! Si estamos en el núcleo 3
        OPEN(rank,file='Armstrong_N3.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)+wvec(rank-2)
            sumsup=bmin+wvec(rank-2)+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF 
    ELSE IF (rank==4) THEN                            ! Si estamos en el núcleo 4
        OPEN(rank,file='Armstrong_N4.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)+wvec(rank-2)+wvec(rank-3)
            sumsup=bmin+wvec(rank-3)+wvec(rank-2)+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    ELSE IF (rank==5) THEN                          ! Si estamos en el núcleo 5
        OPEN(rank,file='Armstrong_N5.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)+wvec(rank-2)+wvec(rank-3)+wvec(rank-4)
            sumsup=bmin+wvec(rank-4)+wvec(rank-3)+wvec(rank-2)+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    ELSE IF (rank==6) THEN                          ! Si estamos en el núcleo 6
        OPEN(rank,file='Armstrong_N6.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)+wvec(rank-2)+wvec(rank-3)+wvec(rank-4)+wvec(rank-5)
            sumsup=bmin+wvec(rank-5)+wvec(rank-4)+wvec(rank-3)+wvec(rank-2)+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    ELSE IF (rank==7) THEN                          ! Si estamos en el núcleo 7
        OPEN(rank,file='Armstrong_N7.txt')
        IF (wvec(rank+1)==0)THEN
            print*, 'Núcleo ',rank,' sin uso'
        ELSE
            suminf=bmin+wvec(rank)+wvec(rank-1)+wvec(rank-2)+wvec(rank-3)+wvec(rank-4)+wvec(rank-5)+wvec(rank-6)
            sumsup=bmin+wvec(rank-6)+wvec(rank-5)+wvec(rank-4)+wvec(rank-3)+wvec(rank-2)+wvec(rank-1)+wvec(rank)+wvec(rank+1)-1
            DO b=suminf,sumsup
                WRITE(rank,*) 'Base=',b
                DO x=1, npb
                    xreal=REAL(x)
                    breal=REAL(b)
                    CALL kb(xreal,breal,k)
                    DO i=0, k-1
                        CALL di(i,x,b,d)
                        f=d**k+f
                    END DO
                    IF (f==x)THEN
                        WRITE(rank,*) f
                    END IF
                    f=0
                END DO
            END DO
        END IF
    END IF
    Call MPI_FINALIZE(ierr)                         ! Finaliza MPI    
END PROGRAM ArmstrongParalelo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE di(i,n,b,y)                              ! Subrutina para encontrar el coeficiente d_i
    INTEGER, INTENT(IN) :: i, n, b
    INTEGER, INTENT(OUT) :: y
    y=(MOD(n,b**(i+1))-MOD(n,b**i))/(b**i)          ! Fórmula dada para calcular
END SUBROUTINE di

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE kb(x,b,y)                                ! Subrutina para encontrar el coeficiente k
    REAL, INTENT(IN) :: b,x
    INTEGER, INTENT(OUT) :: y
    REAL :: z
    z=LOG(x)/LOG(b)                                 ! Formula para el cambio de logaritmo a base b
    y=INT(z)+1                                      ! El resultado es un entero y se devuelve como tal
END SUBROUTINE kb

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE vt(b,c,y)                                ! Subrutina para crear el vector que asigna trabajo a cada núcleo
    INTEGER, INTENT(IN) ::  b, c
    INTEGER, DIMENSION(8), INTENT(OUT) :: y
    INTEGER :: a, rond, paso
    IF (c==b) THEN                                  ! Si el número de núcleos es igual al número de bases, todos hacen 
        DO a=1, c                                   ! una operación, se asigna el valor 1 a todas las coordenadas
            y(a)=1
        END DO
    ELSE IF (c>b) THEN                              ! Si el número de núcleos es mayor al número de bases b, solo hacen 
        DO a=1, b+1                                 ! una operación los primeros b núcleos, se asigna 1 a estas coordenadas
            y(a)=1
        END DO
    ELSE IF(c<=b)THEN                               ! Si el número de núcleos es menor al número de bases b, entonces
        rond=FLOOR(REAL(b)/REAL(c))                 ! El número aproximado de la división entre el número de procesadores/b es 
        DO a=1, c                                   ! la cantidad de procesos que todos los núcleos van a realizar
            y(a)=1*rond
        END DO                                      ! El resto dado por MOD(), se distribuye entre los núcleos necesarios 
        paso=MOD(b,c)+1                             ! uno a la vez
        DO a=1, paso
            y(a)=y(a)+1
        END DO
    END IF
END SUBROUTINE vt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!