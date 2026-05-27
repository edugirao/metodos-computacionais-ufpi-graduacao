PROGRAM euler
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: a,b,h,f,k1
REAL(KIND=8),ALLOCATABLE:: x(:),y(:)

! Solução analítica
a=0.0D0
b=1.0D0
n=10001
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

! Grid para Euler
h=0.0001D0 ! Espaçamento do grid
n=1+(b-a)/h ! Número de pontos
ALLOCATE(x(n))
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h
END DO

! Solução por Euler
ALLOCATE(y(n))
y(1)=1.0D0 ! Valor inicial -> y(1)=y(x(1))=y(x_1)=y_1
! Loop de Euler
DO i=1,n-1
  k1=h*f(x(i),y(i))
  y(i+1)=y(i)+k1
END DO

OPEN(UNIT=1,FILE='numerical.dat')
DO i=1,n
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM euler

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,y)
IMPLICIT NONE
REAL(KIND=8):: f,x,y
f=1.0D0-x+4.0D0*y
END FUNCTION f
