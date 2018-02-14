program demofact
integer fact, n 
real k
print *, "What is n?"
read *, n
print *, "The value of n", n, "factorial is", fact(n)
print *, "What is k?"
read *, k
print *, "The value of k", k, "Poisson dist. value is", poisson(n,k)
end program demofact

function fact(n)
integer fact, n, p
p = 1
do i=1,n
    p = p*i
end do
fact = p
end

function poisson(n, t)
real poisson, t
integer n, fact
poisson = (t**n)*exp(-t)/fact(n)
end