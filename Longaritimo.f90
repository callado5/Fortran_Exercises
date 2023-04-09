!-------------------------------------!
!        Lista 1. Programa 3          !
!          Marco Callado              !
!-------------------------------------!
real function longa(x,a)
	
	implicit none
	real, intent(in) :: x, a !argumento x na base a
	!real :: b
	longa= log(x)/log(a) !calcula razão entre o log de x e a na base 10
end function longa

program Longaritimo
    
	implicit none
    real :: x, a, b, longa
	write(*,*) "Insira os valor de x"
	read(*,*) x
	write(*,*) "Insira os valor da base a"
	read(*,*) a
	write(*,*) longa(x,a)

end program Longaritimo