FUNCTION crazy_function(x)
IMPLICIT NONE
REAL(KIND=8):: x,crazy_function,s,c
c=DCOS(x)
s=DSIN(x)
crazy_function=DEXP(c+s)
! crazy_function=DEXP(DCOS(x)+DSIN(x))
END FUNCTION crazy_function
