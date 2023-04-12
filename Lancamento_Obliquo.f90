subroutine quadratica(h,vo,angulo,tempo, dist)
!---------------------------------------------------!
!         Resolve a equação d segundo grau:         !
!                    ax^2+bx+c=0                    !
!         Retornando apenas a raíz positiva         !
!---------------------------------------------------!
implicit none
real, intent(inout) :: h,vo,angulo
real, intent(out) ::  tempo, dist
real, parameter :: g = 9.8
real :: delta,a,b,c
a = g
b = -vo*sin(angulo)
c= -2*h

delta = b**2 - 4*a*c
!Angulo maior que 360
if (abs(angulo) > 360.) then 
    angulo = mod(angulo,360.)
end if

!Input de numeros negativos
if (angulo < 0.) then
    angulo = angulo + 360.
end if

!Redução ao primeiro quadrante
if (angulo > 90. .and. angulo < 180.) then
    angulo = angulo - 90.
else if (angulo>=180. .and. angulo<270.) then
    angulo = angulo - 180.
else if (angulo >= 270. .and. angulo < 360.) then 
    angulo = angulo - 270.
end if

if (delta >= 0.) then !delta positivo retorna raíz positiva
    tempo = (-b + sqrt(delta))/(2*a)
else
    tempo = 0.
end if

dist = vo*cos(angulo)*tempo

end subroutine quadratica
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program Lancamento
implicit none
real:: vo, h, angulo, tempo, distancia !velocidade inicial, altura e angulo do lancamento

write(*,*) 'Velocidade inicial'
read(*,*) vo
write(*,*) 'Altura do lancamento ate o chao'
read(*,*) h
write(*,*) 'Angulo do lancamento'
read(*,*) angulo

call quadratica(vo, h, angulo, tempo, distancia)
write(*,*)'Angulo:',angulo
write(*,*) 'O objeto demorou',tempo,'segundos para cair'
write(*,*) 'atingindo uma distancia de', distancia, 'metros'

end program Lancamento