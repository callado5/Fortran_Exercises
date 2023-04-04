program Serie_de_Taylor
    !Taylor Series for sin(x) and cos(x)
    implicit none
    
    real :: x,s,c !Declaring variables
    write(*,*) 'x?'
    read(*,*) x
    s = sint(x) !using the function
    c = cost(x)
    write(*,*) 'sin(x)', s
    write(*,*) 'cos(x)', c
    write(*,*) '',sin(x) !to check if the exponencial is right with the exp(x) intriscec function
    write(*,*) '',cos(x)
    Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function sint(x)

    real,intent(in) :: x
    integer :: n
    real :: t

    sint = x
    t = x
    n= 0.

    do !the do doesnt have a defined loop because thats the number we want in the output
        n = n+1 !here i add the loop to N where he increases 1
        t = t * x**2 * (-1) / (2*n+1) * (2*n) !contador onde a taxa multiplica o fator anterior
        !If the exponencial doesnt make a signifacative diffente exit the function
        if (sint+t == sint) exit
        sint = sint+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
        write(*,*) n, sint
    end do

    write(*,*) 'N:',n !printing the number of loops
end function sint
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function cost(x)

    real,intent(in) :: x
    integer :: n
    real :: t

    cost = 1.
    t = 1.
    n= 0.

    do !the do doesnt have a defined loop because thats the number we want in the output
        n = n+1 !here i add the loop to N where he increases 1
        t = t * x**2 * (-1)/ (2*n) * (2*n-1) !contador onde a taxa multiplica o fator anterior
        !If the exponencial doesnt make a signifacative diffente exit the function
        if (cost+t == cost) exit
        cost = cost+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
        write(*,*) n, cost
    end do

    write(*,*) 'N:',n !printing the number of loops
end function cost