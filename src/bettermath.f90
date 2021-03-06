module better

    implicit none
    public :: bettermull
    private :: better4, better8, better16
    
    interface bettermull
        procedure better4, better8, better16
    end interface
    
    contains
    
    function better4(A,B) result(C)
        real (kind = 4), intent(in), dimension(:,:)::A,B
        real (kind = 4), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: i, j, k
        do j = 1, size(A(1,:))
            do k = 1, size(A(:,1))
                do i = 1, size(B(:,1))
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
    end function
    
    function better8(A,B) result(C)
        real (kind = 8), intent(in), dimension(:,:)::A,B
        real (kind = 8), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: i, j, k
        do j = 1, size(A(1,:))
            do k = 1, size(A(:,1))
                do i = 1, size(B(:,1))
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
    end function
    
    function better16(A,B) result(C)
        real (kind = 16), intent(in), dimension(:,:)::A,B
        real (kind = 16), dimension(size(A(1,:)), size(B(:,1)))::C
        integer :: i, j, k
        do j = 1, size(A(1,:))
            do k = 1, size(A(:,1))
                do i = 1, size(B(:,1))
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                end do
            end do
        end do
    end function
    
end module better