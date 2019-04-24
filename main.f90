program main
    
    use naive
    use better
    use dot

    
    implicit none
    integer :: i, e, f
    integer :: matrixSizes(8)
    real (kind = 4), allocatable, dimension(:,:) :: A4, B4, C4
    real (kind =8),allocatable,dimension(:,:) :: A8,B8,C8
    real (kind =16),allocatable,dimension(:,:) :: A16,B16,C16
    real (kind = 4) :: rand4
    real (kind = 8) :: rand8
    real (kind = 16) :: rand16
    real (kind = 16) :: timeStart, timeStop, timeNaive
    
    matrixSizes = [10, 20, 40, 80, 160, 320, 640, 1280]
    

    do i=1,8
        
        allocate(A4(matrixSizes(i), matrixSizes(i)))
        allocate(B4(matrixSizes(i), matrixSizes(i)))
        allocate(C4(matrixSizes(i), matrixSizes(i)))
    
        allocate(A8(matrixSizes(i), matrixSizes(i)))
        allocate(B8(matrixSizes(i), matrixSizes(i)))
        allocate(C8(matrixSizes(i), matrixSizes(i)))

        allocate(A16(matrixSizes(i), matrixSizes(i)))
        allocate(B16(matrixSizes(i), matrixSizes(i)))
        allocate(C16(matrixSizes(i), matrixSizes(i)))
        
        !naive

        ! kind = 4
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand4)
                A4(e,f) = rand4
                CALL RANDOM_NUMBER(rand4)
                B4(e,f) = rand4
                
                C4(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C4=naivemultiplication(A4,B4)
        call cpu_time(timeStop)
        write(*,*) i,"4", " ", timeStop - timeStart
        
        ! kind = 8
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand8)
                A8(e,f) = rand8
                CALL RANDOM_NUMBER(rand8)
                B8(e,f) = rand8
                
                C8(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C8=naivemultiplication(A8,B8)
        call cpu_time(timeStop)
        write(*,*) i,"8", " ", timeStop - timeStart
        
        ! kind = 16
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand16)
                A16(e,f) = rand16
                CALL RANDOM_NUMBER(rand16)
                B16(e,f) = rand16
                
                C16(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C16=naivemultiplication(A16,B16)
        call cpu_time(timeStop)
        write(*,*) i,"16", " ", timeStop - timeStart

        !better

        ! kind = 4
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand4)
                A4(e,f) = rand4
                CALL RANDOM_NUMBER(rand4)
                B4(e,f) = rand4
                
                C4(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C4=betterMultiplication(A4,B4)
        call cpu_time(timeStop)
        write(*,*) i, "better","4", " ", timeStop - timeStart
        
        ! kind = 8
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand8)
                A8(e,f) = rand8
                CALL RANDOM_NUMBER(rand8)
                B8(e,f) = rand8
                
                C8(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C8=betterMultiplication(A8,B8)
        call cpu_time(timeStop)
        write(*,*) i, "better","8", " ", timeStop - timeStart
        
        ! kind = 16
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand16)
                A16(e,f) = rand16
                CALL RANDOM_NUMBER(rand16)
                B16(e,f) = rand16
                
                C16(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C16=betterMultiplication(A16,B16)
        call cpu_time(timeStop)
        write(*,*) i, "better", "16", " ", timeStop - timeStart

        !dot

        ! kind = 4
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand4)
                A4(e,f) = rand4
                CALL RANDOM_NUMBER(rand4)
                B4(e,f) = rand4
                
                C4(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C4=dotMultiplication(A4,B4)
        call cpu_time(timeStop)
        write(*,*) i, "dot","4", " ", timeStop - timeStart
        
        ! kind = 8
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand8)
                A8(e,f) = rand8
                CALL RANDOM_NUMBER(rand8)
                B8(e,f) = rand8
                
                C8(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C8=dotMultiplication(A8,B8)
        call cpu_time(timeStop)
        write(*,*) i, "dot","8", " ", timeStop - timeStart
        
        ! kind = 16
        do e = 1, matrixSizes(i)
            do f = 1, matrixSizes(i)
                CALL RANDOM_NUMBER(rand16)
                A16(e,f) = rand16
                CALL RANDOM_NUMBER(rand16)
                B16(e,f) = rand16
                
                C16(e,f) = 0
            end do
        end do
        
        call cpu_time(timeStart)
        C16=dotMultiplication(A16,B16)
        call cpu_time(timeStop)
        write(*,*) i, "dot", "16", " ", timeStop - timeStart
        
        
        
        if (allocated(A4)) deallocate(A4)
        if (allocated(B4)) deallocate(B4)
        if (allocated(C4)) deallocate(C4)

        if (allocated(A8)) deallocate(A8)
        if (allocated(B8)) deallocate(B8)
        if (allocated(C8)) deallocate(C8)

        if (allocated(A16)) deallocate(A16)
        if (allocated(B16)) deallocate(B16)
        if (allocated(C16)) deallocate(C16)
        
    end do
    
end program