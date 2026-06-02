PROGRAM rk4
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: a,b,h,f,k1,k2,k3,k4
REAL(KIND=8),ALLOCATABLE:: x(:),y(:)

! Solução analítica
a=0.0D0
b=1.0D0
n=101
h=(b-a)/DFLOAT(n-1)
ALLOCATE(x(n),y(n))
OPEN(UNIT=1,FILE='analitycal.dat')
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h
  y(i)=(4.0D0*x(i)-3.0D0+19.0D0*DEXP(4.0D0*x(i)))/16.0D0
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)
DEALLOCATE(x,y)

! Grid para solução numérica
h=0.0001D0 ! Espaçamento do grid
n=1+(b-a)/h ! Número de pontos
ALLOCATE(x(n))
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h
END DO

! Solução por RK4
ALLOCATE(y(n))
y(1)=1.0D0 ! Valor inicial -> y(1)=y(x(1))=y(x_1)=y_1
! Loop de RK4
DO i=1,n-1
  k1=h*f(x(i),y(i))
  k2=h*f(x(i)+0.5D0*h,y(i)+0.5D0*k1)
  k3=h*f(x(i)+0.5D0*h,y(i)+0.5D0*k2)
  k4=h*f(x(i)+h,y(i)+k3)
  y(i+1)=y(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
END DO

OPEN(UNIT=1,FILE='numerical.dat')
DO i=1,n
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM rk4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,y)
IMPLICIT NONE
REAL(KIND=8):: f,x,y
f=1.0D0-x+4.0D0*y
END FUNCTION f
