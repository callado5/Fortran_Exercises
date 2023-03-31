program Serie_de_Taylor
    !Taylor Series for exponencial function
    implicit none
    
    real :: x,e !Declaring variables
    write(*,*) 'x?'
    read(*,*) x
    e = exponencial(x) !using the function
    write(*,*) 'Exp(x)', e
    write(*,*) '',exp(x) !to check if the exponencial is right with the exp(x) intriscec function
    Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function exponencial(x)

    real,intent(in) :: x
    integer :: n
    real :: t
!Declaring varibles for the first order
    exponencial =1.
    t=1.
    n=0
    do !the do doesnt have a defined loop because thats the number we want in the output
        n = n+1 !here i add the loop to N where he increases 1
        t = t * x/n !the next term in the taylor series
        if (exponencial+t == exponencial) exit !If the exponencial doesnt make a signifacative diffente exit the function
        exponencial = exponencial+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
    end do
    write(*,*) 'N:',n !printing the number of loops
end function exponencial

end program 