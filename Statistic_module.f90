!==================================!
!     Module with Statistics       !
!          Marco Callado           !
!==================================!
Module Estatistica
implicit none
Contains

real function media(x, n) !array, lenght
    real,    intent(in)    :: x(n)
    integer, intent(in)    :: n
    
    media = sum(x)/n

end function media

subroutine bubblesort(x, n)! array and lenght
    integer :: i,j
    integer, intent(in) :: n
    real, intent(inout) :: x(n)
    real :: temp
    logical :: swap
    do j = 1, n-1
        swap = .true.
        do i = 1,n-1
            if( x(i) >x(i+1)) then
                temp   = x(i)
                x(i)   = x(i+1)
                x(i+1) = temp
                swap   = .false.
            end if
        end do 
        if (swap) exit
            write(*,*) 'Volta', j
        do i = 1,n
            write(*,*) x(i)
        end do
    end do
end subroutine bubblesort

subroutine median(x,  n,  med) !array, lenght and median
    real,    intent(in)    ::    x(n)
    integer, intent(in)    ::    n
    real,    intent(out)   ::    med
    real                   ::    temp(n)
    
    temp = x(n) !Vetor temporário para ordenar os valores no calculo da mediana
    call bubblesort(temp,n)
    
    if (mod(n,2)==1) then
        med = temp((n+1)/2)
    else
        med = temp((n/2)+1) + x((n/2)) / 2
    end if
    write(*,100) med
    100 format ('3x3f3')
end subroutine median
