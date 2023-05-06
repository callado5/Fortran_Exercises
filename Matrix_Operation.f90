!==============================================!
!        Module for Operations with matrix     !
!               Marco Callado                  !
!==============================================!
Module Linear_Algebra
    implicit none
    Contains
    
subroutine Matrix_Vet_Mul( i, j, A, B, Prod)
    implicit none
    
    integer,intent(in) :: i,j !matrix row and column
    real,intent(in)    :: A(i,j), B(j)
    real,intent(out)   :: Prod(i)
    integer :: m!,n !contadores
    
    !Rows, dot_product function to calulate the scalar product of two vectors
    do m = 1, i
        Prod(m) = dot_product(A(m,:),B)
    
    end do
    
end subroutine Matrix_Vet_Mul
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine MatMat_mul(i, j, k, A, B, Prod)
    implicit none
    
    integer,intent(in) :: i,j,k !matrix row and column
    real,intent(in)    :: A(j,i), B(i,k)
    real,intent(out)   :: Prod(j,k)
    integer :: m, n !contadores

    do m = 1, j !Linhas
        do n = 1, k !Colunas
            Prod(m,n) = dot_product(A(m,:),B(:,n))
            write(*,*) m,n

        end do
    end do

    end subroutine MatMat_mul

end Module Linear_Algebra
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
program teste_mat
    Use Linear_Algebra
    implicit none
    integer, parameter :: M = 4
    integer, parameter :: N = 4
    integer, parameter :: K = 3
    real               :: A(M,N), x(N,K),y(M,K)
    integer            :: i
    !A values
    A(1,:) = [1.,0.,1.,0.]
    A(2,:) = [2.,1.,0.,1.]
    A(3,:) = [3.,0.,2.,1.]
    A(4,:) = [4.,1.,1.,0.]
    
    !X values
    x(1,:) = [1.,2.,1.] 
    x(2,:) = [1.,1.,0.]
    x(3,:) = [1.,2.,0.]
    x(4,:) = [1.,1.,1.]

    Call MatMat_Mul(M,N,K,A,x,y)
        
    do i = 1,N
        write(*,*) y(i,:)
    end do
end program
