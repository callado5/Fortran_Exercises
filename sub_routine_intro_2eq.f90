subroutine quadratica(a,b,c,nr,r1,r2)
    !----------------------------------------------------
    !         Resolve a equação d segundo grau:         !
    !                    ax^2+bx+c=0                    !
    !r1: raiz; r2: raiz; nr: numero de raizes da equação!
    !----------------------------------------------------
        implicit none
        real, intent(in) :: a,b,c
        real, intent(out) ::  r1,r2
        integer, intent(out) :: nr
        real :: delta !argumento dentro da raiz quadrada
    
        delta = b**2 - 4*a*c
    
        if (delta<0.) then !se delta for negativo, nãoo há solução nr=0
            nr = 0
        else if (delta >0.) then !delta positivo retorna duas raízes
            nr = 2
            r1 = (-b - sqrt(delta))/(2*a)
            r2 = (-b + sqrt(delta))/(2*a)
        else
            nr=1
            r1 = -b/(2*a)
            r2 = r1
    
        end if
     
    end subroutine quadratica
    
    program segundograu
        implicit none
        real :: c1,c2,c3
        real :: raiz_1, raiz_2
        integer :: num_raizes
    
        write(*,*) 'Coeficientes da Equação ax²+bx+c:'
        write(*,*) 'a,b,c:'
        read(*,*) c1,c2,c3
    
        call quadratica(c1,c2,c3,num_raizes,raiz_1,raiz_2)
    
        if (num_raizes==1) then 
            write(*,*) 'Uma raiz:', raiz_1
        else if (num_raizes==2) then
            write(*,*)'Duas raízes:'
            write(*,*) 'R1:',raiz_1
            write(*,*) 'R2:',raiz_2
        else
            write(*,*) 'Não há raíz real.'
        end if
    end program segundograu