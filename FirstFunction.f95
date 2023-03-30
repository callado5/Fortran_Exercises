program teste
    implicit none
    
    Real:: expcos
    real::x,y

    write(*,*) 'x:'
    read(*,*) x
    
    y =expcos(x) !using the function bellow
    
    write(*,*) 'Valor da funcao:', y
    
    Contains !Allows me to use the function bellow 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    Real function expcos(x) 
    
        Real, intent(in)::x !here I'm inserting the input varibles of the function
        expcos = exp(-x**2)*cos(10*x)!declaring the function
    
    end function expcos
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
end program teste
