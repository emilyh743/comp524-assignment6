! Fortran assignment: (assign6.f03)
! Bubble Sort program from Stack Overflow: https://stackoverflow.com/questions/19550621/fortran-bubble-sort-algorithm

PROGRAM assign6
    implicit none
    integer :: arg1len, arg2len, column, row, i, hold,k
    character(len=:), ALLOCATABLE :: file1name, file2name
    integer, dimension(:,:), ALLOCATABLE :: myMatrix

    ! open input file
    CALL get_command_argument(1, length = arg1len)
    ALLOCATE (CHARACTER(arg1len) :: file1name)
    CALL get_command_argument(1, value = file1name)
    OPEN (unit = 1, file = file1name)

    read(1,*) column, row
    ALLOCATE (myMatrix(column, row))
    DO i=1, column
        read(1,*) myMatrix(i,:)
	call bubble_sort(myMatrix(i,:),row)
    END DO
    CLOSE (1)  

    ! open output file
    CALL get_command_argument(2, length = arg2len)
    ALLOCATE (CHARACTER(arg2len) :: file2name)
    CALL get_command_argument(2, value = file2name)
    OPEN (unit = 2, file = file2name)

    CALL bubble_sort_rows(myMatrix, column, row)
    DO i = 1,column
        write (2,*) myMatrix(i,:)
    END DO

    CLOSE (2)
    DEALLOCATE (myMatrix)
    DEALLOCATE (file1name)
    DEALLOCATE (file2name)
END program assign6

subroutine bubble_sort(vec, row)
  implicit none 
  integer :: temp, bubble, lsup, j,row
  integer vec(row)

  lsup = row !lsup is the size of the array to be used

  DO while (lsup > 1)
    bubble = 0 !bubble in the greatest element out of order
    DO j = 1, (lsup-1)
      if (vec(j) > vec(j+1)) then
        temp = vec(j)
        vec(j) = vec(j+1)
        vec(j+1) = temp
        bubble = j
      END IF 
    END DO
    lsup = bubble   
  END DO
END subroutine bubble_sort

subroutine bubble_sort_rows(myMatrix, column,row)
  implicit none
  integer :: temp, bubble, lsup, j, row, i ,column, k,hold
  integer myMatrix(column, row)

  lsup = column !lsup is the size of the array to be used

  DO while (lsup > 1)
    bubble = 0 !bubble in the greatest element out of order
    DO j = 1, (lsup-1)
      DO k = 1, row
        if (myMatrix(j,k) /= myMatrix(j+1,k)) then
		exit
	end if
      END DO
      if (myMatrix(j,k) > myMatrix(j+1,k)) then
        DO i = 1,row
	 temp = myMatrix(j,i)
	 myMatrix(j,i) = myMatrix(j+1,i)
	 myMatrix(j+1,i) = temp
	END DO
        bubble = j
      END IF
    END DO
    lsup = bubble
  END DO
END subroutine bubble_sort_rows
