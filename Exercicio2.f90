real function sinc(x)
!--------------------------------------------------------------------------
!function to calculate sin
!f(x) = seno(x)/x, where if x is different than 0, you execute the function
!if not just notice that equals 1, 
!--------------------------------------------------------------------------    
    implicit none
        real, intent(in) :: x
    
        if (x /= 0.) then
        sinc = sin(x)/x
        else
            sinc=1.
        end if

end function sinc

program Exercise2 
    implicit none
    
    real :: x, sinc

    write(*,*)"Insert x in degree:"
    read(*,*) x
    write(*,*)"The result of f(x) is:", sinc(x)

end program Exercise2
