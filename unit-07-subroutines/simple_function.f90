PROGRAM simple_function
IMPLICIT NONE
REAL(KIND=8):: theta_d,theta_r,pi,y
REAL(KIND=8):: crazy_function

pi=DACOS(-1.0D0)
WRITE(*,*) 'Entre com um ângulo em graus:'
READ(*,*) theta_d
theta_r=theta_d*pi/180.0D0
y=crazy_function(theta_r)
WRITE(*,*) 'A função vale:',y
END PROGRAM simple_function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION crazy_function(x)
IMPLICIT NONE
REAL(KIND=8):: x,crazy_function,s,c
c=DCOS(x)
s=DSIN(x)
crazy_function=DEXP(c+s)
! crazy_function=DEXP(DCOS(x)+DSIN(x))
END FUNCTION crazy_function
