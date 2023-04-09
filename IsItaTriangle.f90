logical function tri(a,b,c)
implicit none

real, intent(in) :: a,b,c
logical :: cond1, cond2

cond1 = a>0 .and. b>0 .and. c>0
cond2 = a+b>c .and. a+c>b .and. c+b>a

if (cond1 .and. cond2) then
    tri = .true.
else 
    tri = .false.
end if
end function tri
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
logical function trir(a,b,c)
implicit none

real, intent(in) :: a,b,c
real :: cond1, cond2, cond3

cond1 = a**2 + b**2
cond2 = b**2 + c**2
cond3 = a**2 + c**2

if (cond1 == c**2) then
    trir = .true.
elseif (cond2 == a**2) then
    trir = .true.
elseif (cond3 == b**2) then
    trir = .true.
else
    trir = .false.
end if
end function trir
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program IsItaTriangle
    implicit none
    real :: x,y,z
    logical :: tri, trir, A, B

    write(*,*) 'Insira as medidas do triangulo'
    read(*,*) x,y,z

    A = tri(x,y,z)
    B = trir(x,y,z)

    if (A .and. B) then
        write(*,*)'As medidas formam um triangulo retangulo'
    else if (A) then
        write(*,*)'As medidas formam um triangulo, nao retangulo'
    else 
        write(*,*)'As medidas nao formam um triangulo'
    end if

end program IsItaTriangle