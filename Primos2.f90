subroutine Primo(x,a)
    implicit none
    integer,intent(in) :: x
    integer :: n,t
    integer, intent(out) :: a
    
    a = 1 !0 n primo, 1 primo
    t = mod(x,2) !divisão por dois t = 0 

    if (t==0) then
        a = 0
     else
         do n = 3, x-1, 2
             t = mod(x,n)
             if (t==0) then
                a = 0
                exit
             end if
         end do
     end if
    
end subroutine Primo

program NumeroPrimo
    implicit none
    integer :: x
    integer :: a
    
do x = 1,10000
    call Primo(x,a)
    if (a==1) then
        write(*,*) x
    end if
end do
end program NumeroPrimo