! This program shows arithmetic precision differences between
! different type of variable declarations.
program demo
real x
double precision y, z
x = 1.1
y = 1.1
z = 1.1D0
print *, "x=", x, "y=", y, "z=", z
end