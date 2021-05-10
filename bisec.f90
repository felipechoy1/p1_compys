!    2021-05-17
!    bisec.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)

!    Este es un programa que realiza el método de bisección
!    para una función arbitraria en un intervalo arbitrario

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Ubuntu 20.04 WSL) 9.3.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o bisec.o bisec.f90
!    gfortran -o bisec.x bisec.o 


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


PROGRAM main
  IMPLICIT NONE
  !Variables a utilizar para el programa 

  INTEGER(8) :: n !Número de iteraciones
  REAL(8)    :: a,b,err !Cotas y variable para almacenar el error
  REAL(8)    :: bisec,func !Llamado a la función de bisección 
  
  
  !Se declaran las condiciones iniciales
  n=10 !iteraciones
  a=0 !cota inferior
  b=6.28  !cota superior
  
  !Programación defensiva
  IF (func(b)*func(a)>0) THEN
    PRINT *,"No existe una raíz (un 0) en esta función para este intervalo"
  ELSE IF (a>b) THEN
    PRINT *,"b debe ser superior a a"
  ELSE
  
  !Se imprimen los resultados
    PRINT *, "Existe un 0 en " , bisec(a,b,n,err), "con", n,"iteraciones"
    PRINT*, "El error es", err
  END IF
END PROGRAM


!===================================================
!               ::::FUNCIONES::::
!===================================================


!---------------------------------------------------
!               Función a evaluar
!---------------------------------------------------
FUNCTION func(x) result(y)
  
  REAL(8), intent(in) :: x ! input
  REAL(8)             :: y ! output
  y=SIN(x)!Se anota aquí la función a utilizar
  
END FUNCTION


!--------------------------------------------------
!               Proceso de bisección 
!--------------------------------------------------
FUNCTION bisec(a,b,n,err) result(w)

    REAL(8), INTENT(in)    ::a,b !input
    INTEGER(8), INTENT(in) ::n !input
    REAL(8)                ::liminf,limsup !Variables para ir reduciendo los límites
    REAL(8)                ::w,h,err,func !output y llamada de la función 
    INTEGER(8)             ::i !Iterador entero 


    liminf = a
    limsup = b
    
    !Cíclo de iteraciones para ir minimizandno el ancho 
    DO i=1,n
    
    !Se asigna el punto medio entre el límite superior como el inferior
        h=(liminf+limsup)/2
        IF (func(h)==0) THEN !Se valúa si este da con un 0 exacto en la función, procediendo a asignarlo al valor de salida
            w=h
        
        ELSE IF (func(h)*func(liminf)<0) THEN !Se verifica que cumpla con la condición de caso de signo, si se cumple se reduce la cota superior
            limsup=h
        ELSE            !Si pasa lo contrario se desplaza la cota inferior hacia h 
            liminf=h
        END IF
    END DO

err=ABS(liminf-limsup)/2  !Se obtiene el error absoluto de la medición obtenida

w=h                     !Se almacena el valor encontrado tras las n iteraciones en w (que es el output)
   
END FUNCTION

