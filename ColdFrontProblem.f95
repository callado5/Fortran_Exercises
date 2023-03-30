program ColdFront
    ! Here we calculate a cold front goind from Atlanta to Miami
    ! Where we have the initial parameters being:
    ! Temperature Atlanta(A) = 12 and Miami(M) = 24
    ! Distance between the two cities is 960km
    ! Cold Front Velocity = 20km/
    ! In this problem we would like to calculate Miami temperatures passing 24hours (dt)
    implicit none

    integer :: n 
    real :: n_hours

    do n = 0,72,3 !temperature 3 to 3 hours during 72 hours
        n_hours = real(n)
        
        write(*,*) 'Temperature of the cold front after', &
         n_hours, 'hours is' , & 
         CF_Temp(12.,24.,20.,960.,n_hours), 'Celcius degrees'
    
    end do
    Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function CF_Temp(tempa,tempm,c,dx,dt) result(res)
        
        real, intent(in) :: tempa, tempm, c, dx, dt

        res = tempm - c*(tempm-tempa) / dx*dt
    
    end function CF_Temp
 
end program ColdFront
