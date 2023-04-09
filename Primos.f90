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
    
    write(*,*) 'Insira um valor'
    read(*,*) x
    call Primo(x,a)
    
    if (a == 0) then
        write(*,*)'Nao e primo'
    else
        write(*,*)'O numero e primo'
    end if

end program NumeroPrimo