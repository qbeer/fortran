program matrix
real A(5,6)
parameter (pi = 3.14159)
do i=1,5
    do j=1,6
        A(i,j) = real(i)*real(j)*pi
    end do
end do

    do i=1,5
        write (*,10) (A(i,j), j=1,6)
    end do
10 format(6f7.3)
end