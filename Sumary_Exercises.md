# FORTRAN
# Aula 03

Exercício: Calcular a distancia entre dois pontos em um plano cartesiano de duas dimensões x,y

```fortran
program distance
!Diference between two points A(x,y) and B(x,y)
	implicit none
	real :: a,b,c,d, distance
	read(*,*) 'Insert A(x):', a
	read(*,*) 'Insert A(y):', b
	read(*,*) 'Insert B(x):', c
	read(*,*) 'Insert B(Y):', d
	distance = sqrt((a-c)**2+((b-d))**2)
	write(*,*) 'The distance between A and B is',distance,'units'
end program distance
```

# Aula 04

Exercício: Calcular a área de um triangulo usando funções

```fortran
Program TriangleArea
    implicit none

    real::Heron
    real::a,b,c
    
    write(*,*)'Please input the triangle measures'
    read(*,*) a,b,c
    write(*,*) 'The Triangle area is:', Heron(a,b,c)

    Contains
end Program TriangleArea

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Real function Heron(a,b,c)
!The Heron formula returns the area of a triangle by the calculating by the... 
!square root of the multiplication of the semiperimter and the diferences between each lateral with it
    real:: s
    real, intent(in) :: a,b,c
    logical :: cond1, cond2
    cond1 = a>0 .and. b>0 .and. c>0
    cond2 = a+b>c .and. a+c>b .and. c+b>a
    s=(a+b+c)/2

  
    if (cond1 .and. cond2) then
        Heron = sqrt(s*(s-a)*(s-b)*(s-c)) 
    else 
        stop 'Not a Triangle'
    end if

end function Heron
```

# Aula 05

Programa para calcular a área de um triangulo usando a formula de Heron

```fortran
Real function heron(a,b,c,error) result(h)
!The Heron formula returns the area of a triangle by the calculating by the... 
!square root of the multiplication of the semiperimter and the diferences between each lateral with it
	implicit none

	real, intent(in) :: a,b,c
  real :: s
  integer, intent(out) :: error 
  logical :: cond1, cond2

  error = 0.
  cond1 = a>0 .and. b>0 .and. c>0
  cond2 = a+b>c .and. a+c>b .and. c+b>a
  s=(a+b+c)/2

  if (cond1 .and. cond2) then
	  h = sqrt(s*(s-a)*(s-b)*(s-c)) 
  else 
	  error=1
  end if

end function Heron
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Program TriangleArea
    implicit none

    real :: a,b,c,heron,area
    integer :: error
    
    write(*,*)'Please input the triangle measures'
    read(*,*) a,b,c

    area = heron(a,b,c,error) !Defining the triangle area

    if (error == 0.) then
        write(*,*) 'The Triangle area is:', area
    else
        write(*,*) error,'Error its not a triangle'
    end if

end Program TriangleArea
```

# Aula 06

Função para calcular o sinc

```fortran
real function sinc(x)
!--------------------------------------------------------------------------
!function to calculate sin
!f(x) = seno(x)/x, where if x is different than 0, you execute the function
!if not just notice that equals 1, 
!--------------------------------------------------------------------------    
    implicit none
        real, intent(in) :: x
    
        if (x /= 0.) then
        sinc = sin(x)/x
        else
            sinc=1.
        end if

end function sinc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program Exercise2 
    implicit none
    
    real :: x, sinc

    write(*,*)"Insert x in degree:"
    read(*,*) x
    write(*,*)"The result of f(x) is:", sinc(x)

end program Exercise2
```

# Aula 07

Aula exemplo do laço do, nesse caso queremos realizar a média de n medidas, onde será printado o valor das medidas a se inserir a cada loop

```fortran
program ex7 
!Exemplo do uso de laço "do"
    implicit none
    real :: x,soma,md
    integer :: n
    integer :: i
    soma=0.
 
    write(*,*) "Quantas medidas"
    read(*,*) n

    do i = 1,n
        write(*,*) "digite o valor da medida", i,":"
        read(*,*) x
        soma = soma+x
    end do
    
    md= soma/n
    write(*,*) 'Média:', md

end program ex7
```

### Série de Taylor

$$

f(x) = f(a) + \frac{f(a)'}{1!}.(x-a) + \frac{f(a)''}{2!}. (x-a)^2+...
+f(a)^{(n)}.(x-a)^n

$$

Expansão em torno de 0

```fortran
program Serie_de_Taylor
    !Taylor Series for exponencial function
    implicit none
    
    real :: x,e !Declaring variables
    write(*,*) 'x?'
    read(*,*) x
    e = exponencial(x) !using the function
    write(*,*) 'Exp(x)', e
    write(*,*) '',exp(x) !to check if the exponencial is right with the exp(x) intriscec function
    Contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real function exponencial(x)

    real,intent(in) :: x
    integer :: n
    real :: t
!Declaring varibles for the first order
    exponencial =1.
    t=1.
    n=0
    do !the do doesnt have a defined loop because thats the number we want in the output
        n = n+1 !here i add the loop to N where he increases 1
        t = t * x/n !the next term in the taylor series
        if (exponencial+t == exponencial) exit !If the exponencial doesnt make a signifacative diffente exit the function
        exponencial = exponencial+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
    end do
    write(*,*) 'N:',n !printing the number of loops
end function exponencial

end program
```

Aqui temos a serie de taylor para o seno e cosseno, onde as derivadas de seno são `seno, seno’= cosseno, seno’’, = - seno, seno’’’ = -cosseno, seno’’’’ = seno.` Vamos fazer a partir daqui a expansão de taylor apenas para seno e cosseno, sendo assim teremos que:

### Seno

$$
\sin(x) = x - \frac{x^3}{3!}+  \frac{x^5}{5!} - \cdots
= \sum_{n=0}^{\infty} \frac{(-1)^n}{(2n+1)!} x^{2n+1}
$$

### Cosseno

$$
\cos(x) = 1 - \frac{x^2}{2!} + \frac{x^4}{4!} + \dots = \sum_{n=0}^\infty \frac{(-1)^n}{2n!}x^{2n}
$$

---

```fortran
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
        t = t * x**2 * (-1) / ((2*n+1) * (2*n)) !contador onde a taxa multiplica o fator anterior
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
        t = t * x**2 * (-1)/ ((2*n) * (2*n-1)) !contador onde a taxa multiplica o fator anterior
        !If the exponencial doesnt make a signifacative diffente exit the function
        if (cost+t == cost) exit
        cost = cost+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
        write(*,*) n, cost
    end do

    write(*,*) 'N:',n !printing the number of loops
end function cost

end program Serie_de_Taylor
```

# Aula 08

Calcular as raízes de uma equação do segundo grau

```fortran
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
```

---

subrotina intreseca `random_number`, retorna um numero aleatório de distribuição uniforme, onde x vai variar de 0 a 1

call random_number(x)

# Aula 09

Rolagem de dados, subrotina intrínseca radom_number()

```fortran
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
```

# Aula 10

Introdução a função character, retorna uma string como resposta.

```fortran
character(len=3) function Conceito(nota) !Em uma função do tipo character tbm é necessário colocar a lenght dele
implicit none
real, intent(in) :: nota

if (nota < 5.) then
    conceito = 'INS'
else if (nota<7.) then
    conceito = 'REG'
else if (nota<9.) then
    conceito = 'BOM'
else
    conceito = 'EXC'
end if
end function Conceito 
!!!!!!!!!!!!!!!!!!!!!!
program Nota_Prova
implicit none
real :: nota
character(len=3) :: Conceito

write(*,*) 'Nota?'
read(*,*) nota
write(*,*) 'Conceito: ', conceito(nota)
end program Nota_Prova
```

# Aula 11

Método de Newton, convergência de uma função ao zero

```fortran
real function fun(x)
real, intent(in) :: x 

!fun = x**2 - 4. !funcionou
fun = x**2 +4
end function fun

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function deriv(x)
real, intent(in) :: x 

!Deriv = 2*x !funcionou
Deriv = 2*x
end function deriv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program Newton
implicit none
real :: x
real,parameter :: eps = 1.e-5
real :: fun
real :: deriv
real :: n 
n = 0

write(*,*) 'Insira o ponto inicial (x0):'
read(*,*) x

do
    x = x - (fun(x)/deriv(x))
    n = n+1
    if (abs(fun(x)) < eps) exit !critério de parada, quando f(x) for proxima de zero
    if (n > 10000) exit !Critério de parada se nao achar o f(x) = 0, ou seja um numero muito elevado
end do

write (*,*) 'Raiz:', x
end program Newton
```

# LISTA DE EXERCÍCIOS 01

- ***Questão 1.***
    
    A função intrínseca INT(x) converte um número x real em inteiro, preservando apenas a parte inteira de x (truncando a parte decimal do número). Já a função NINT(x) converte o número x de real para inteiro arredondando-o para o valor inteiro mais próximo de x. Que valores serão exibidos pelo seguinte programa?
    
    ```fortran
    PROGRAM sample_1
    INTEGER :: i1, i2, i3
    REAL :: a1=2.4, a2
    i1 = a1
    i2 = INT(a1*i1)
    i3 = NINT(a1*i1)
    a2 = a1**i1
    WRITE(*,*) i1, i2, i3, a1, a2
    END PROGRAM sample_1
    ```
    
    A função intrínseca REAL(i) converte o número inteiro i em real. Que valores são armazenados nas variáveis a, b e n após os seguintes comandos ser executados?
    
    - Resposta
        
        2, 4, 5, 2.4, 5,76
        

---

- ***Questão 2.***
    
    ```fortran
    REAL :: a, b
    INTEGER :: n, i, j
    i = 10.
    j = 3
    n = i / j
    a = i / j
    b = REAL(i) / j
    ```
    
    - Resposta
        
        a: 10/3= 3.00000; b:10./3= 3.33333 ; n: 10/3 = 3 
        
    
    ---
    
- ***Questão 3.***
    
    Que valores serão exibidos no terminal pelo seguinte programa?
    
    ```fortran
    PROGRAM quiz_1
    INTEGER :: i
    REAL :: a
    a = 0.05
    i = nint(2.*3.141593/a)
    a = a * (5/3)
    WRITE(*,*) i, a
    END PROGRAM quiz_1
    ```
    
    - Resposta
        
         `126, 5.0e-2`.
        
    
    ---
    

***PROGRAMA SEM SUBPROGRAMAS***

- ***Questão 4.a - Ângulo***
    
    Uma pessoa registrou o consumo de gasolina de seu carro, anotando os valores da quilometragem e da quantidade de litros de gasolina em vários trechos percorridos. Escreva um programa que receba as quantidades de quilômetros e de litros em cada viagem. O programa então deve calcular e exibir o consumo em quilômetros por litro para cada trecho. Depois de processar toda a informação de entrada o programa deve calcular e exibir o quilômetros por litro combinados de todos os tanques cheios. A saída deve ser algo assim:
    
    `Quantos litros? (-1 para terminar)`
    
    `40.5`
    
    `Quantos quilômetros?`
    
    `465.7`
    
    `No trecho 1: 11.4987659 km/l`
    
    `Quantos litros? (-1 para terminar)`
    
    `38.7`
    
    `Quantos quilômetros?`
    
    `425.`
    
    `No trecho 2: 10.9819117 km/l`
    
    `Quantos litros? (-1 para terminar)`
    
    `30.`
    
    `Quantos quilômetros?`
    
    `360.`
    
    `No trecho 3: 12.0000000 km/l`
    
    `Quantos litros? (-1 para terminar)`
    
    `1`
    
    `Em todo o percurso: 11.4532967 km/l`
    
    - Resposta
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 1          !
        !          Marco Callado              !
        !-------------------------------------!
        program quilometragem
        implicit none
        real :: L, km,kml !litro, km, km/l
        integer :: n !numero de viagens
        
        n = 0
        kml = 0.
        
        do
            n = n+1 !numero do percurso
            write(*,*) 'Quantos litros? (-1 para terminar)'
            read(*,*) L
            if ((L==-1)) exit !condição de saída
        	  write(*,*) 'Quantos quilometros? (-1 para terminar)'
            read(*,*) km
            if ((km==-1)) exit !condição de saída
            write(*,*) 'no trecho',n,':', km/L,'km/L'
            kml = (kml+(km/L)) !soma do que foi gasto em km/L
        end do
        
        write(*,*) 'Em todo percurso:', kml/n,'Km/L'!dividindo pelo numero de trechos temos a média
        end program quilometragem
        ```
        
    
    ---
    
- ***Questão 4.b***
    
    Escreva uma subrotina para converter a medida de um ângulo de radianos para graus, minutos e segundos, ou seja, a rotina tem uma única entrada e três saídas. As variáveis de saída graus e minutos devem ser do tipo integer e a variável segundos, do tipo real. O resultado deve ser semelhante ao seguinte exemplo:
    
    `Valor em radianos?`
    
    `1.`
    
    `1.0000000 rad = 57 graus, 17 min e 44.8062471 seg`
    
    - Resposta
        
        ```fortran
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
        ```
        
    
    ---
    
- ***Questão 5. - Logaritmo***
    
    Se conhecemos o logaritmo de um número N na base b, podemos calcular o seu logaritmo na base a através da fórmula:
    
    $$
    \log_a(x) = \frac{\log_b(x)}{\log_b(a)}
    $$
    
    Escreva uma função com dois argumentos de entrada que calcule o logaritmo de um número, x em uma base a, qualquer
    
    - Resposta
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 3          !
        !          Marco Callado              !
        !-------------------------------------!
        real function longa(x,a)
        	
        	implicit none
        	real, intent(in) :: x, a !argumento x na base a
        	longa= log(x)/log(a) !calcula razão entre o log de x e a na base 10
        
        end function longa
        
        program Longaritimo
            
        	implicit none
            real :: x, a, longa
        	write(*,*) "Insira os valor de x"
        	read(*,*) x
        	write(*,*) "Insira os valor da base a"
        	read(*,*) a
        	write(*,*) longa(x,a)
        
        end program Longaritimo
        ```
        
    
    ---
    
- ***Questão 6.a - Triangulo***
    
    Escreva uma function que receba 3 valores reais e determine se eles podem representar os lados de um triângulo.
    
- ***Questão 6.b - Triangulo***
    
    Escreva uma function que receba 3 valores inteiros e determine se eles podem representar os lados de um triângulo retângulo.
    
    ---
    
    > *OBS: Cada `function` nesta questão pode ser do tipo `integer`, `character` ou `logical`. Se a função for do tipo `integer` a saída pode ser 0 ou 1, se ela for do tipo `character` a saída pode ser uma das `strings` SIM ou NAO e se for do tipo `logical` evidentemente a saída deve ser `.true`. ou .false.*
    > 
    - Resposta a & b
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 4          !
        !          Marco Callado              !
        !-------------------------------------!
        !Problema a: Triangulo
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
        !****************************************
        !Problema b. Triangulo retangulo
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
        !****************************************
        program IsItaTriangle
            implicit none
            real:: x,y,z
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
        ```
        
    
    ---
    
- ***Questão 7 - Triangulo***
    
    Qualquer conjunto de três números inteiros que são lados de um triângulo retângulo é chamado de Trio Pitagórico. Encontre todos os trios pitagóricos formados por inteiros não maiores do que 500. Use laços de DO para testar todas as possibilidades. Construa os laços de maneira a não gerar triângulos repetidos. Este é um exemplo de computação de “força bruta”. Em cursos mais avançados você vai aprender que existem vários problema interessantes para os quais não há nenhuma abordagem algorítmica conhecida a não ser usar pura força bruta.
    
    - Resposta
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 5          !
        !          Marco Callado              !
        !-------------------------------------!
        program trianglepossibilities
        implicit none
        integer :: a,b,c !medidas, dado c como hipostenusa
        
        !nesse laço temos a,b,c variando de 1 a 500, quando o laço
        !satisfazer a condição do triangulo pitagorico C^2=A^2+B^2
        !teremos o output dessas medidas.
        do a = 1,500 
            do b = 1,500
                do c = 1,500
                    if (c**2 == b**2 + a**2) then !colocando condição
                        write(*,*) a,b,c !
                    else
                    end if
                end do
            end do
        end do
        
        end program trianglepossibilities
        ```
        
    
    ---
    
- ***Questão 8.a - Numeros Primos***
    
    Um número inteiro é primo se só é divisível por 1 e por si mesmo
    
    Escreva uma função ou uma subrotina que determine se um número N é primo, usando a função intrínseca MOD para testar a divisibilidade. Construa de maneira a realizar o mínimo possível de testes de divisibilidade.
    
    - Resposta
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 5          !
        !           Marco Callado             !
        !  Retorna o valor de um numero primo !
        !-------------------------------------!
        subroutine Primo(x,a)
        implicit none
        integer,intent(in) :: x
        integer :: n,t
        integer, intent(out) :: a !0 n primo, 1 primo
            
        n = 3
        t = mod(x,2) !divisão por dois t = 0 
        do
            if (n**2>x) then
                a =0
            else if (mod(x,n)/=0) then
                a = 1
                n = n+2
            else
                a = 0 
            end if
        end do
          
        end subroutine Primo
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        program NumeroPrimo
            implicit none
            integer :: x
            integer :: a
            
            write(*,*) 'Insira um valor'
            read(*,*) x
            call Primo(x,a)
            
            if (a == 0) then
                write(*,*)'Nao e primo'
            else
                write(*,*)'O numero e primo'
            end if
        
        end program NumeroPrimo
        ```
        
- ***Questão 8.b - Numeros Primos***
    
    Use sua função ou rotina em um programa que determine e exiba todos os números primos menores que 10.000
    
    - Resposta
        
        ```fortran
        !-------------------------------------!
        !        Lista 1. Programa 5          !
        !           Marco Callado             !
        !  Mostra uma lista de numeros primos !
        !-------------------------------------!
        subroutine Primo(x,a)
            implicit none
            integer,intent(in) :: x
            integer :: n,t
            integer, intent(out) :: a
            
            a = 1 !0 n primo, 1 primo
            t = mod(x,2) !divisão por dois t = 0 
        
            if (t==0) then
                a = 0
             else
                 do n = 3,x-1, 2 !sqrt(x)+1
                     t = mod(x,n)
                     if (t==0) then
                        a = 0
                        exit
                     end if
                 end do
             end if
            
        end subroutine Primo
        
        program NumeroPrimo
            implicit none
            integer :: x
            integer :: a
            
        do x = 1,10000
            call Primo(x,a)
            if (a==1) then
                write(*,*) x
            end if
        end do
        end program NumeroPrimo
        ```
        
    
- ***Questão 9. - Lançamento***
    
    Um dispositivo para lançar projéteis (um canhão, ou morteiro) é capaz de lançar uma bala com velocidade de módulo v0 em um ângulo de lançamento θ. Considere que um lançamento é feito do alto de um penhasco de altura h, como ilustrado na figura.
    
    ![Untitled](Untitled.png)
    
    Se ignorarmos a influência do ar e as variações na aceleração da gravidade g com a altura (uma aproximação ruim para velocidades altas) o projétil irá viajar em uma trajetória parabólica e atingir o solo a uma distância horizontal x após um tempo t. Como você pode deduzir facilmente, o tempo para o impacto com o solo é dado pela solução da equação quadrática
    
    $$
    gt^2 - 2v_0 \sin(\theta)t - 2h_0 = 0
    $$
    
    Uma vez tendo o tempo, a distância percorrida na horizontal será simplesmente
    
    $$
    x = v0 \cos(θ)t.
    $$
    
    Escreva um programa que tenha como entrada os dados do modelo e calcule o tempo t e a posição x para o ponto de impacto com o solo, com as seguintes informações:
    
    **Entradas**
    
    Altura *h*;
    
    Velocidade do Lançamento *v0;* 
    
    Angulo do Lançamento *θ.* 
    
    **Saída**
    
    Tempo *t* para o impacto;
    
    Distância até o ponto de impacto *x.*
    
    - Resposta
        
        ```fortran
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
        ```
        
- Questão 10
    
    a) Escreva uma function para calcular a série de Taylor das funções seno e cosseno, com a máxima precisão possível. Teste seu programa para diferentes valores de argumentos das funções. Você irá verificar que seu programa começa a falhar para valores grandes dos argumentos. Explique por que isto acontece. 
    
    - Resposta
        
        Isso ocorre por conta que seno e cosseno são funções ciclicas, ou seja ela repete seu padrão de 0 a 2pi, sendo assim, para valores muito altos a serie vai possuir muitos termos até convergir podendo quebrar o programa, para corrigir isso podemos fazer a redução dos graus para o range -pi e pi, dado pelo resto da divisão de `angulo/pi` assim, podemos utilizar a função `mod(angulo,pi).`
        
        ```fortran
        program Serie_de_Taylor
            !Taylor Series for sin(x) and cos(x)
            implicit none
            real, parameter :: pi =3.1416
            real :: x,s,c 
            write(*,*) 'x?'
            read(*,*) x
            !sin and cos are cyclic functions that varies from -pi to pi so we can make the number fit in the range with mod
            if (x > pi .or. x < -pi) then
                x = mod(x,pi)
            end if
            !using the function
            s = sint(x) 
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
                t = t * x**2 * (-1) / ((2*n+1) * (2*n)) !contador onde a taxa multiplica o fator anterior
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
                t = t * x**2 * (-1) / ((2*n) * (2*n-1)) !contador onde a taxa multiplica o fator anterior
                !If the exponencial doesnt make a signifacative diffente exit the function
                if (cost+t == cost) exit
                cost = cost+t !the first term is equal to 1 because n = 0 and t =1, and we sum the terms after
                write(*,*) n, cost
            end do
        
            write(*,*) 'N:',n !printing the number of loops
        end function cost
        
        end program Serie_de_Taylor
        ```
        
    
    b) Modifique seu programa para que possa calcular os valores com boa precisão, para qualquer valor do argumento. Dica: se o número x dado como argumento da função for menor do que −π ou maior do que π, calcule o número entre −π e π que tenha os mesmos valores de seno e cosseno que o x, e então aplique este número na série de Taylor.
    
    - Resposta
        
        Para obter a maxima precisão possivel paramos a função apenas quando o termo for muito próximo de 0 a ponto que não importe quantos termos calcularmos adiante o resultado continua o mesmo pois a soma desses termos tambem irá tender a zero
        
    
- Questão 11
    
    Escreva uma função para simular o lançamento de um dado, usando a sub-rotina implícita RANDOM_NUMBER. A função, do tipo integer, tem lista de argumentos vazia e sua saída é um número inteiro entre 1 e 6. Teste sua função para verificar se as seis faces tem a mesma probabilidade no lançamento do dado: execute a função muitas vezes e conte as ocorrências de cada número. Se seu dado não é “viciado” cada face tem que aparecer aproximadamente
    1/6 das vezes
    
    - Resposta
        
        ```fortran
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
        ```
        
    
- Questão 12
    
    Neste exercício você vai usar sua function de lançamento de dado para programar uma versão simples do jogo chamado CRAPS.
    
    Craps é um jogo de dados muito comum nos cassinos. Nele um jogador joga contra a “casa”. As regras são as seguintes: O jogador lança dois dados. Se o resultado dos dados somar 7 ou 11 na primeira jogada, o jogador ganha. Se a soma for 2, 3 ou 12 na primeira jogada (“craps”) o jogador perde, ou seja, a casa ganha. Se a soma for 4, 5, 6, 8, 9 ou 10 na primeira jogada, então esse valor se torna o “ponto” do jogador. Neste caso, o jogador deve continuar lançando os dados até que apareça ou seu ponto ou 7. O jogador perde se aparecer um 7 e ganha se aparecer seu ponto novamente.
    
    a) Implemente as regras do jogo em um programa chamado craps, usando sua função de simulação do lançamento do dado. O programa deve terminar exibindo na tela a frase “O jogador ganhou!” ou “O jogador perde.”
    
    - Resposta
        
        ```fortran
                                                !*********!
                                                !  CRAPS  !
                                                !*********!
        Integer function lanca_dado() !não possui entrada apenas saída
        implicit none
        Real:: x 
        
        call random_number(x) !puxa um numero aleatório entre 0 e 1
        lanca_dado = INT( 6*x + 1. ) !ajustando o numero aleatório até o [intervalo 1 a 6]
        
        end function lanca_dado
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        program Craps
        
        implicit none
        integer :: lanca_dado
        integer :: soma, pontos
        pontos = 0
        
        !iniciar jogo
        write(*,*) 'Jogar CRAPS' 
        write(*,*)'(ENTER para lancar os dados)'
        read(*,*)
        soma = lanca_dado() + lanca_dado()
        
        !Primeira rodada
        if (soma == 7 .or. soma ==11) then
            write(*,*) 'Voce tirou', soma
            write(*,*) 'Parabens! Voce venceu :D'
        else if (soma == 2 .or. soma == 3 .or. soma == 12) then
            write(*,*) 'Você tirou', soma
            write(*,*) 'Sinto muito, voce perdeu :('
        !outras rodadas
        else
            do 
                pontos = pontos+1
                write(*,*) 'Voce tirou:', soma
                write(*,*) 'Voce tem:', pontos,'pts'
                write(*,*) '(ENTER para lancar os dados)'
                read(*,*)
                soma = lanca_dado() + lanca_dado()
                if (soma == 7 .or. soma == pontos .or.soma == 2 .or. soma == 3 .or. soma == 12) exit
        
            end do
        
            if (soma == 7 .or. soma == pontos) then
                write(*,*) 'Voce tirou', soma
                write(*,*) 'Parabens! Voce venceu :D'
            else if (soma == 2 .or. soma == 3 .or. soma == 12) then
                write(*,*) 'Voce tirou', soma
                write(*,*) 'Sinto muito, voce perdeu :('
            end if
        end if
        end program Craps
        ```
        
    
    b) Modifique o programa do jogo de dados para incluir apostas. Transforme o programa que joga uma partida de craps em uma function. Inicialize a variável saldo com 1000 reais. Peça ao jogador para fazer uma aposta. Use um laço enquanto para verificar que a aposta é menor que ou igual ao saldo e, se não for, peça ao jogador para apostar outro valor até um valor de aposta válido seja escrito. Feita a aposta, execute o jogo uma vez. Se o jogador ganhar, some ao saldo o valor da aposta e mostre o novo saldo do jogador. Se o jogador perder, subtraia do saldo o valor da aposta e mostre o novo valor do saldo. Ao final de cada rodada verifique se o deposito se tornou zero e, caso isto aconteça, exiba a mensagem “Lamento, você faliu.
    
    - Resposta
        
        ```fortran
        Integer function lanca_dado() !não possui entrada apenas saída
        implicit none
        Real:: x 
        
        call random_number(x) !puxa um numero aleatório entre 0 e 1
        lanca_dado = INT( 6*x + 1. ) !ajustando o numero aleatório até o [intervalo 1 a 6]
        
        end function lanca_dado
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer function Craps()
        
        implicit none
        integer :: lanca_dado
        integer :: soma,soma2, pontos
        
        !iniciar jogo
        write(*,*) 'Jogar CRAPS' 
        write(*,*)'(ENTER para lancar os dados)'
        read(*,*)
        soma = lanca_dado() + lanca_dado()
        soma2= soma
        !Primeira rodada
        if (soma == 7 .or. soma ==11) then
            Craps = 1
            write(*,*) 'Voce tirou', soma
            write(*,*) 'Parabens! Voce venceu :D'
        else if (soma == 2 .or. soma == 3 .or. soma == 12) then
            Craps = 0
            write(*,*) 'Voce tirou', soma
            write(*,*) 'Sinto muito, voce perdeu :('
        !outras rodadas
        else
            do 
                pontos = soma
                write(*,*) 'Voce tirou', soma2
                write(*,*) 'Voce precisa tirar:', pontos
                write(*,*) '(ENTER para lancar os dados)'
                read(*,*)
                soma2 = lanca_dado() + lanca_dado()
                if (soma2 == 7 .or. soma2 == pontos) exit
        
            end do
        
            if (soma2 == pontos) then
                Craps = 1
                write(*,*) 'Voce tirou', soma2
                write(*,*) 'Parabens! Voce venceu :D'
            else if (soma2 == 7) then
                write(*,*) 'Voce tirou', soma2
                write(*,*) 'Sinto muito, voce perdeu :('
                Craps = 0
            end if
        end if
        
        end function Craps
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        program Apostas
        implicit none
        integer :: Craps, jogo
        real :: saldo, aposta
        character(len=3) :: continuar
        
        saldo = 1000.
        jogo = 2
        continuar = 'SIM'
        !inicio do jogo de apostas
        
        do
            write(*,*) 'Seu saldo e de:',saldo,'reais'
            if (jogo == 1 .or. jogo == 0) then
            write(*,*) 'Deseja continuar?'
            write(*,*) 'digite SIM ou NAO'
            read(*,*) continuar
            end if
            if (continuar == 'NAO' .or. continuar == 'Nao' .or. continuar == 'nao' ) exit
            if (continuar == 'SIM' .or. continuar == 'Sim' .or. continuar == 'sim' ) then
                aposta = 0.
            !condição de falencia
                if (saldo < 0) exit
            !enquanto a aposta for invalida mostre a mensagem abaixo
                do while (aposta > saldo .or. aposta == 0.)
                    write(*,*) 'Faca uma aposta, voce tem:',saldo,'reais'
                    write(*,*) '(Se a aposta for invalida a mensagem sera repetida)'
                    read(*,*) aposta
                end do
            !Iniciar craps 1 = venceu, 0 = perdeu
                jogo = Craps()
        
                if (jogo > 0.5 ) then !condição de vitória
                saldo =saldo + aposta
                else !Derrota
                saldo = saldo - aposta
                end if   
            else 
                exit
        end if
        if (saldo <= 0) exit
        end do
        
        if (saldo <= 0) then
            write(*,*) 'Lamento, voce Faliu :('
        else
            write(*,*) 'Voce terminou com um saldo de:', saldo,'reais'
        end if
        end program Apostas
        ```
        

## RESPOSTA PROVA 1.

Problema 1.

```fortran
!**********************************************!
!                   PROVA 1                    !
!      Problema 1: Sequencia de Fibonacci      !
!                Marco Callado                 !
!**********************************************!
program Fibonacci
implicit none
integer :: n1, n2, n !N1, N2 e a soma dos termos
integer :: termo !numero de loops (termos)
integer :: i !Contador

write(*,*) "Digite os dois primeiros termos:"
read(*,*) n1,n2
write(*,*) 'Digite o numero de termos:'
read(*,*) termo
write(*,*) 'A sequencia de Fibonacci é:'

!testando o termo
if (termo <= 0) then !termo = 0
    write(*,*) 'Input de termos inválido'
    
else if (termo == 1) then
    write (*,*) 1,'-',n1

else
    write(*,*) 1,'-',n1
    write(*,*) 2,'-', n2
end if

do i = 3,termo
    n = n1+n2
    n1 = n2
    n2 = n
    write(*,*) i,'-', n
    
end do

end program Fibonacci
```

Problema 2.

```fortran
!**********************************************!
!                   PROVA 1                    !
!  Problema 2: Sequencia de Fibonacci (razão)  !
!                Marco Callado                 !
!**********************************************!

program Fibonacci
implicit none
integer :: n1, n2, n !N1, N2, soma dos termos
integer :: i !Contador (numero de termos)
real ::  r, rdif !razão entre os termos
    
write(*,*) "Digite os dois primeiros termos:"
read(*,*) n1,n2
write(*,*) 'A sequencia de Fibonacci é:'
write(*,*) 1,'-',n1
write(*,*) 2,'-', n2, real(n2)/real(n1)

i = 2 !os dois primeiros termos

do !começa a contar a partir do termo 3
    i = i+1
    rdif = real(n2)/real(n1)
    n = n1+n2
    r = real(n)/real(n2)
    n1 = n2
    n2 = n

    write(*,*) i,'-', n, r
    if (r == rdif) exit !converge quando a razão anterior for igual a posterior    

end do
    
end program Fibonacci
```

# Aula 12

Lendo um arquivo e retornando o maior valor e sua posição

```fortran
program encontra_maior
implicit none
integer :: I_maior, i
integer :: erro
real :: x, Maior
open(55,file='dados.dat',status ='old',action='read')

read(55,*,iostat=erro) x
if (erro == 0) then
    Maior = x
    i = 1
    I_maior = i
    do
        read(55,*,iostat=erro) x
        if (erro==0) then
            i = i + 1
            if (x > Maior) then
                Maior = x
                I_maior = i
            end if
        else
            exit
        end if
    end do
    write(*,*) 'Maior valor:',Maior, 'na posicao', I_maior, 'de',i
else 
    write(*,*) 'Arquivo vazio'
end if

end program encontra_maior
```

# Aula 13

```fortran
subroutine Media_dp(N,x,media,dp)

implicit none
integer, intent(in) :: N
real, intent (in) :: x(N)
real, intent(out) :: media, dp
real :: Soma, Soma2
integer :: i

Soma = 0.
Soma2 = 0.
do i =1,N
    Soma = Soma + x(i)
    Soma2 = Soma2 + x(i)**2
end do

media = Soma / N

dp = sqrt(Soma2/N - media**2)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program dice
implicit none
integer :: i
integer :: N
integer, parameter :: Nmax = 10000
real :: media, dp
real :: x(Nmax)

open(33,file = 'dados.dat',status='old',action='read')

read(33,*) N

do i = 1,N
    read(33,*) x(i)
end do

close(33)

call Media_dp(N, x, media, dp)
write(*,*) media, dp
end program dice
```

# Aula 14

Programa para ordernar os valores do menor ao maior

```fortran
!==============================================!
!   Programa para arrumar de forma crescente   !
!               Adriel Carneiro                !
!==============================================!
subroutine ordem (n, x)
implicit none
integer, intent(in):: n
real, intent(inout):: x(n)
real:: aux
integer:: j, i, saida

do i = 1, n-1
    do j = 1, n-i 
        if (x(j) > x(j+1)) then
            aux = x(j)
            x(j) = x(j+1)
            x(j+1) = aux
            saida = 0
        else
            saida = 1
        end if
    end do
    write(*,*) i
    if (saida ==1) exit
end do

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program ordenar
implicit none 
real, allocatable:: x(:)
integer:: n

open (55, file="dados.dat", status="old", action="read")
read(55,*) n
allocate(x(n))

read(55,*) x

write(*,*)'Valores desordenados:',x 

call ordem (n,x)

write(*,*)'valores ordenados:',x

end program
```

```fortran
program Ascending
!==============================================!
!   Programa para arrumar de forma crescente   !
!                Marco Callado                 !
!==============================================!
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
```
