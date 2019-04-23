module naive
    
    implicit none
    public :: naivemultiplication
    private :: naive4, naive8, naive16
    
    interface naivemultiplication
        procedure naive4, naive8, naive16
    end interface
    
    contains
    
    function naive4(A, B) result(C)
        real (kind=4), intent(in), dimension(:,:) :: A,B
        real (kind=4), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: g, h, v, i, j, k
        g=size(A(1,:))
        h=size(A(:,1))
        v=size(B(:,1))
        do i=1, g
            do j=1, h
                do k = 1, v
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end function naive4
    
    function naive8(A, B) result(C)
        real (kind=8), intent(in), dimension(:,:) :: A,B
        real (kind=8), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: g, h, v, i, j, k
        g=size(A(1,:))
        h=size(A(:,1))
        v=size(B(:,1))
        do i=1, g
            do j=1, h
                do k = 1, v
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end function naive8
    
        function naive16(A, B) result(C)
        real (kind=16), intent(in), dimension(:,:) :: A,B
        real (kind=16), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: g, h, v, i, j, k
        g=size(A(1,:))
        h=size(A(:,1))
        v=size(B(:,1))
        do i=1, g
            do j=1, h
                do k = 1, v
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end function naive16
    
end module naive