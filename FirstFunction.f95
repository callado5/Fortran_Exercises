program teste
    implicit none
    
    Real:: expcos
    real::x,y

    write(*,*) 'x:'
    read(*,*) x
    
    y =expcos(x)
    
    write(*,*) 'Valor da funcao:', y
    
    Contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Real function expcos(x)
    
        Real, intent(in)::x
        expcos = exp(-x**2)*cos(10*x)
    
    end function expcos
end program teste