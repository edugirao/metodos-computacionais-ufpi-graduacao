PROGRAM simple_function2
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: theta_d,theta_d1,theta_d2
REAL(KIND=8):: theta_r,theta_r1,theta_r2
REAL(KIND=8):: crazy_function,pi,y,dtheta

pi=DACOS(-1.0D0)
WRITE(*,*) 'Entre com um intervalo de ângulos em graus:'
READ(*,*) theta_d1,theta_d2
theta_r1=theta_d1*pi/180.0D0
theta_r2=theta_d2*pi/180.0D0
WRITE(*,*) 'Entre com o número de pontos no intervalo:'
READ(*,*) n

OPEN(UNIT=1,FILE='crazy_function.dat')
dtheta=(theta_r2-theta_r1)/DFLOAT(n-1)
DO i=1,n
  theta_r=theta_r1+dtheta*(i-1)
  y=crazy_function(theta_r)
  WRITE(1,*) theta_r,y
END DO  
WRITE(1,*)
CLOSE(UNIT=1)
END PROGRAM simple_function2

