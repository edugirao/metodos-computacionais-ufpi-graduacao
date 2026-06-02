PROGRAM bala
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: h,m,b,v0,k1,k2,f
REAL(KIND=8),ALLOCATABLE:: t(:),v(:)

! Parâmetros do cálculo
WRITE(*,*) 'Entre com a massa do corpo:'
READ(*,*) m
WRITE(*,*) 'Entre com a constante b:'
READ(*,*) b
WRITE(*,*) 'Entre com a velocidade inicial:'
READ(*,*) v0

! Grid no tempo
h=1.0D-3
n=10001
ALLOCATE(t(n))
DO i=1,n
  t(i)=0.0D0+(i-1)*h
END DO

! Condição inicial
ALLOCATE(v(n))
v(1)=v0

! Solução analítica
OPEN(UNIT=1,FILE='analytical.dat')
DO i=1,n-1
  WRITE(1,*) t(i),v0*DEXP(-b*t(i)/m)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

! Solução por Euler
OPEN(UNIT=1,FILE='euler.dat')
DO i=1,n-1
  k1=h*f(t(i),v(i),m,b)
  v(i+1)=v(i)+k1
  WRITE(1,*) t(i),v(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

! Solução por RK2
OPEN(UNIT=1,FILE='rk2.dat')
DO i=1,n-1
  k1=h*f(t(i),v(i),m,b)
  k2=h*f(t(i)+0.5D0*h,v(i)+0.5*k1,m,b)
  v(i+1)=v(i)+k2
  WRITE(1,*) t(i),v(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

! Solução por RK4
OPEN(UNIT=1,FILE='rk4.dat')
DO i=1,n-1
  k1=h*f(t(i),v(i),m,b)
  k2=h*f(t(i)+0.5D0*h,v(i)+0.5*k1,m,b)
  k3=h*f(t(i)+0.5D0*h,v(i)+0.5*k2,m,b)
  k4=h*f(t(i)+h,v(i)+k3,m,b)
  v(i+1)=v(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  WRITE(1,*) t(i),v(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM bala

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(t,v,m,b)
IMPLICIT NONE
REAL(KIND=8):: f,t,v,m,b
f=-b*v/m
END FUNCTION f
