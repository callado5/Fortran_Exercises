program trianglepossibilities
implicit none
integer :: a,b,c !medidas
!logical :: cond !condições de um triangulo pitagórico

do a = 1,500
    do b = 1,500
        do c = 1,500
            if (c**2 == b**2 + a**2) then
                write(*,*) a,b,c
            else
            end if
        end do
    end do
end do

end program trianglepossibilities