!-------------------------------------!
!        Lista 1. Programa 2          !
!          Marco Callado              !
!-------------------------------------!
subroutine deg2rad(rad,deg,min,seg)

    implicit none
    ! a equação para converção de radianos para grau é rad*180/pi
    real, intent(inout) :: rad
    integer, intent(out):: deg, min 
    real, intent(out) :: seg
    real, parameter :: pi = 3.14159265
    real :: trans 

    trans= rad*180./pi
   
    deg = trans
    trans = (trans - int(trans))*60
    min = int(trans) 
    trans = (trans - int(trans))*60
    seg = trans

end subroutine deg2rad

program DegreetoRadian

    implicit none
    real :: Radiano, Segundos
    integer :: Graus, Minutos

    write(*,*) 'Insira o valor em radianos:'
    read(*,*) Radiano
    call deg2rad(Radiano,Graus,Minutos,Segundos)
    write(*,*) Radiano, 'rad =',Graus,'graus',Minutos,'min',Segundos,'seg.'

end program

