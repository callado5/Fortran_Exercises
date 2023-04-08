program quilometragem
implicit none
real :: L, km,kml !litro, km, km/l
integer :: n !numero de viagens
n = 0
kml = 0.

!write(*,*) 'Em todo percurso:', kml/n
do
    
    n = n+1
    write(*,*) 'Quantos litros? (-1 para terminar)'
    read(*,*) L
    if ((L==-1)) exit 
    write(*,*) 'Quantos quilometros? (-1 para terminar)'
    read(*,*) km
    if ((km==-1)) exit 
    write(*,*) 'no trecho',n,':', km/L
    kml = (kml+(km/L))
end do
write(*,*) 'Em todo percurso:', kml/n,'Km/L'
end program quilometragem
