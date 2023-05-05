!============================!
!  Program Used to sort data !
!       Marco Callado        !
!============================!
program Ascending
implicit none

real, dimension(5) :: zoo = [ 10., 3., 6., 4., 9. ] !We have to reorder this array
real :: temp
integer :: i, j

write(*,100) zoo
100 format ('Desordenado:',/,5F8.2) 
do j = 1,5
    do i = 1,i
        if (zoo(i) > zoo(i+1)) then
            temp = zoo(i+1)
            zoo(i+1) = zoo(i)
            zoo(i) = temp
        end if 
    end do
end do 

write(*,110) zoo
110 format ('Ordenado:',/,5F8.2) !Formato de saída

end program Ascending
