program sumofsqrs
integer n, sum, sqrSum
print *, "What is n?"
read *, n
sum = sqrSum(n)
print *, "The square sum of the first", n, "integers is", sum
end

function sqrSum(n)
integer n, sqrSum
sqrSum = 0
if (n .le. 0) return
do i=1,n
    sqrSum = sqrSum + i*i
end do
end