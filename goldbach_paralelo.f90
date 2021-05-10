!    2021-05-17
!    goldbach.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este programa demuestra la conjetura de Golbach
!    encontrando los números primos menores a un número
!    luego genera dos archivos de texto mostrando cuantas formas se puede 
!    escribir un conjunto de números como su suma de 2 y 3 primos. 
!    Esto lo realiza de forma paralela utilizando 2 núcleos.  
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    mpif90 -Wall -pedantic -std=f95 -c -o goldbach_paralelo.o goldbach_paralelo.f90
!    mpirun -np 2 ./goldbach_paralelo
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
    USE mpi
    IMPLICIT NONE
    integer,external :: primo
    integer,external :: sumadeprimos2
    integer,external :: sumadeprimos3
    INTEGER :: i,n
    integer(4) :: rank, ierr, size !Variables MPI

    n=400
    !---------------------------------------------Iniciando MPI-------------------------------------------


    CALL MPI_INIT(ierr)                                   !Inicializamos MPI
    if ( ierr /= 0 ) stop "Error al inicializar MPI"      !variable error diferente a 0 se detiene   
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)        !Se indica el número de núcleos
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)        !Se define cada núcleo


    !----------------------------------------------Nucleo 0----------------------------------------------


    IF (rank==0)THEN
        !Escribe de cuántas formas se esccribe como suma de 2 primos.
        open(11, file="datos2.txt")
            do i=4,n
                IF(MOD(i,2)==0)THEN
                    write(11,*) i, sumadeprimos2(i)
                END IF
            end do
        close(11)
    END IF


    !----------------------------------------------Nucleo 1----------------------------------------------

    

    IF (rank==1)THEN
        !Escribe de cuántas formas se esccribe como suma de 3 primos
        open(12, file="datos3.txt")
            do i=6,n
                write(12,*) i, sumadeprimos3(i)
            end do
        close(12)
    END IF


    call MPI_FINALIZE(ierr) !finaliza mpi
END PROGRAM Goldbach




!===============================================================================================
!                                         FUNCION PRIMOS
!===============================================================================================


!Si el número es primo devuelve el número, sino devuelve cero.


INTEGER FUNCTION primo(n)
    !Argumentos
    INTEGER, INTENT(IN) :: n
    integer :: incremen 
    !variables
    LOGICAL :: pri
    pri = .true.
    incremen = 2   
    !Cuerpo de la función
    DO WHILE(pri .eqv. .true. .and. incremen<n)
        IF (MOD(n,incremen)==0) THEN
        pri= .false.
        END IF
        incremen = incremen + 1
    END DO
    IF(pri .eqv..true.)THEN
        primo=n
    ELSE  
        primo=0
    END IF    
END FUNCTION primo




!===============================================================================================
!                                     FUNCION SUMADEPRIMOS2
!===============================================================================================


!dice de cuántas formar se puede escribir n como la suma de dos primos.
!imprime de qué forma puede escribirse.


INTEGER FUNCTION sumadeprimos2 (n)
    integer,external :: primo
    INTEGER, INTENT(IN) :: n
    INTEGER :: v2, i, m 
    m = n/2
    v2=0
    DO i=2, m
        IF(primo(i)== i) THEN
            IF (primo(n-i)== n-i) THEN
                !PRINT*, n, " = " , i, " + " , n-i
                v2 = v2 + 1
            END IF    
        END IF
    END DO
    !PRINT*, n, " se puede escribir de " , v2 , " formas como la suma de 2 números primos"
    sumadeprimos2 = v2
END FUNCTION sumadeprimos2




!===============================================================================================
!                                     FUNCION SUMADEPRIMOS3
!===============================================================================================


!dice de cuántas formar se puede escribir n como la suma de tres primos.
!imprime de qué forma puede escribirse.


INTEGER FUNCTION sumadeprimos3 (n)
    integer,external :: primo
    INTEGER, INTENT(IN) :: n
    INTEGER, dimension(n) :: v
    INTEGER :: i, p, q, r, v3, c, suma
    v3=0
    c=1
    DO i=2, n
        IF(primo(i)==i)THEN
            v(c)=i
            c=c+1
        END IF 
    END DO
    DO p=1, c
        DO q=p, c
            DO r=q, c
                suma= v(p) + v(q) + v(r)
                IF (primo(v(p)).NE.0  .and. primo(v(q)).NE.0 .and.  primo(v(r)).NE.0 .and. suma==n )THEN 
                    v3= v3 + 1
                    !PRINT*, n, "n = " , v(p) , " + " , v(q) , " + " , v(r)
                END IF
            END DO
        END DO
    END DO
    !PRINT*, n, " se puede escribir de " , v3 , " formas como la suma de 3 números primos"
    sumadeprimos3 = v3  
END FUNCTION sumadeprimos3
