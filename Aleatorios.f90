Integer function lanca_dado() !não possui entrada apenas saída
implicit none
Real:: x 

call random_number(x) !puxa um numero aleatório entre 0 e 1
lanca_dado = INT( 6*x + 1. ) !ajustando o numero aleatório até o [intervalo 1 a 6]

end function lanca_dado
!!!!!!!!!!!!!!!!!!!!!!!!!!
Program aleatorios
implicit none
integer:: lanca_dado !funcao para lancar os dados
integer:: F1, F2, F3, F4, F5, F6 !numero de vezes que caiu a face do dado
integer:: N, i, F !contadores

F1 = 0
F2 = 0
F3 = 0
F4 = 0
F5 = 0
F6 = 0

write(*,*) 'N?'
read(*,*) N !numero de rolagens

Do i = 1, N
    F = lanca_dado() !F retorna o valor da rolagem do dado
    !se a face F=1, quer dizer que o numero 1 foi retirado e entra pra conta, logo, F1= F1+1, e eassim por diante
    If (F == 1) then 
        F1 = F1 + 1
    else If (F == 2) then
        F2 = F2 + 1
    else If (F == 3) then
        F3 = F3 + 1
    else If (F == 4) then
        F4 = F4 + 1
    else If (F == 5) then
        F5 = F5 + 1
    else
        F6 = F6 + 1
    end if
end do

!mostrando a probabilidade em porcentagem de cair uma face 
write(*,*) 'P1:', 100*(F1/real(N))
write(*,*) 'P2:', 100*(F2/real(N))
write(*,*) 'P3:', 100*(F3/real(N))
write(*,*) 'P4:', 100*(F4/real(N))
write(*,*) 'P5:', 100*(F5/real(N))
write(*,*) 'P6:', 100*(F6/real(N))

end program