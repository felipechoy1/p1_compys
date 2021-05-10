!    2021-05-17
!    armstrong.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este programa encuentra una cantidad de números
!    de Armstrong para un rango de bases dadas.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubunutu 20.04 WSL) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o armstrong.o armstrong.f90
!    gfortran -o armstrong.x armstrong.o 
!       
!    Copyright (C) 2021
!    Este programa es propiedad de Pablo Martínez(pabloversion1.0@gmail.com) 
!    expuesto en el curso de Física Computacional en el año 2021. Él autorizó 
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


PROGRAM Armstrong
    IMPLICIT NONE
    INTEGER :: npb=55000000     ! Cantidad de números por base
    INTEGER :: bmin=4, bam=10               ! Base mínima, Base máxima
    INTEGER :: b, x, k, d, f , i            ! Variables del problema
    REAL :: xreal,breal                     ! Necesario para la función que calcula "k"
    OPEN(12,file='Armstrong.txt')           ! Abre y/o crea el archivo de texto para guardar los datos
    print*, 'números por base=',npb
    DO b=bmin, bam                          ! Desde la base mínima hasta la base máxima                  
        WRITE(12,*) 'Base=',b              
        DO x=0, npb                         ! Se consideran solo números no negativos
            xreal=REAL(x)
            breal=REAL(b) 
            CALL kb(xreal,breal,k)          ! Se calcula el valor de "k" y se almacena en la variable k
            DO i=0, k-1                     ! Se realiza una sumatoria desde i=0 hasta k-1
                CALL di(i,x,b,d)            ! Se calcula el factor d_i y se almacena en la variable d                  
                f=d**k+f                    ! Se realiza la sumatoria de d_i^k
            END DO
            IF (f==x)THEN                   ! Si f=x, este es un número de Armstrong
                WRITE(12,*) f
            END IF
            f=0                             ! Se eliminan los resultados anteriores
        END DO
    END DO
END PROGRAM Armstrong

SUBROUTINE di(i,n,b,y)                      ! Subrutina para encontrar el coeficiente d_i
    INTEGER, INTENT(IN) :: i, n, b
    INTEGER, INTENT(OUT) :: y
    y=(MOD(n,b**(i+1))-MOD(n,b**i))/(b**i)  ! Fórmula dada para calcular
END SUBROUTINE di

SUBROUTINE kb(x,b,y)                        ! Subrutina para encontrar el coeficiente k
    REAL, INTENT(IN) :: b,x
    INTEGER, INTENT(OUT) :: y
    REAL :: z
    z=LOG(x)/LOG(b)                         ! Formula para el cambio de logaritmo a base b
    y=INT(z)+1                              ! El resultado es un entero y se devuelve como tal
END SUBROUTINE kb