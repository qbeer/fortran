program avarage
real numbers(100), avg
integer m
print *, "(no more than 100)"
read *, m
do i=1,m
    print *, "Enter next number"
    read *, numbers(i)
end do
print *, "The avarage of inputted numbers is", avg(m, numbers)
end

function avg(size, numbers)
integer size
real numbers(100), avg, sum
sum = 0.
do i=1,size
    sum = sum + numbers(i)
end do
avg = sum / real(size)
end