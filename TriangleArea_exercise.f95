Real function heron(a,b,c,error) result(h) !Declaring the function with a result
!The Heron formula returns the area of a triangle by the calculating by the... 
!square root of the multiplication of the semiperimter and the diferences between each lateral with it
implicit none

real, intent(in) :: a,b,c !inputs
real :: s
integer, intent(out) :: error !putting a error message if the measures dont make a triangle
logical :: cond1, cond2 !conditions to make a triangle

error = 0
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

if (error == 0) then
	write(*,*) 'The Triangle area is:', area
else
	write(*,*) error,'Error its not a triangle'
end if

end Program TriangleArea
