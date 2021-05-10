!    2021-05-17
!    funcionesprob.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este es un programa que genera una distribucion uniforme de numeros aleatorios, para luego transformar 
!    esta a una distribucion normal y exponencial, además almacena dichos datos en un archivo txt.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu Linux) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o funcionesprob.o funcionesprob.f90
!    gfortran -o funcionesprob.x funcionesprob.o

!    Este programa es propiedad de Bryant Morazán(bryant.morazan@gmail.com) 
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
!-----------------------------------------------------------------------------------------------------

!-------------------------------------------Inicia el programa----------------------------------------
program funcionesprob
!----------------------------------------------Variables----------------------------------------------
implicit none                                              
integer(8) :: n=10000000                             !Tamaño muestral
real(8) :: random                                          !Para almacenar los numeros aleatorios
real(8), ALLOCATABLE, dimension(:) :: u, t, x              !Vectores dinamicos: distribucion uniforme, normal y exp
integer(8) :: i                                            !Iteradora
!-----------------------------------------------------------------------------------------------------
!---Defifiendo el tamaño del arreglo que representa a cada distribución igual al tamaño "n" de la muestra
allocate(u(n), t(n), x(n))
!----Generando distribución uniforme-------------------------------------------------------------------
do i=1,n
    call random_number(random)
    u(i)=random
end do
!----Generando distribucion normal y exponencial------------------------------------------------------
t = (-log( u**2 ))**0.5
x = -log( u**2)*(1/1)
!----------------------------------------Imprimiendo en archivo--------------------------------------
!-----Imprimiendo archivo de datos: normal-----------------------------------------------------------
open(12, file="normal.txt")
        do i=1,n
            write(12,*) t(i), u(i)
            write(12,*) -t(i), u(i)
        end do
    close(12)
!-----Imprimiendo archivo de datos: exponencial------------------------------------------------------
    open(13, file="exponencial.txt")
        do i=1,n
            write(13,*) x(i), u(i)
        end do
    close(13)

deallocate(u, t, x)

end program funcionesprob
!----------------------------------------------------------------------------------------------------