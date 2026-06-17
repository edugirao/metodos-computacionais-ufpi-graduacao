PROGRAM dho
IMPLICIT NONE
INTEGER:: nt,i,contador
REAL(KIND=8):: m,pi,w,k,x0,v0,dt,hdt,fx,fv,e0,ei
REAL(KIND=8):: t1,t2,t3,b,b0,g
REAL(KIND=8):: k1x,k2x,k3x,k4x
REAL(KIND=8):: k1v,k2v,k3v,k4v
REAL(KIND=8),ALLOCATABLE:: t(:),x(:),v(:)

! Parâmetros iniciais
m=1.0D0
pi=DACOS(-1.0D0)
k=4.0D0*pi**2
w=DSQRT(k/m)
WRITE(*,*) 'Período esperado para o SHO:',2.0D0*pi/w
b0=2.0D0*DSQRT(k*m)
b=0.2*b0
g=b/(m+m)
WRITE(*,*) 'w0=',w
WRITE(*,*) 'g==',g
! Leitura das condições iniciais
WRITE(*,*) 'Entre com a posição inicial'
READ(*,*) x0
WRITE(*,*) 'Entre com a velocidade inicial'
READ(*,*) v0

! Grid no tempo
dt=0.001D0
nt=7000
ALLOCATE(t(nt))
DO i=1,nt
  t(i)=DFLOAT(i-1)*dt
END DO

! Inserção das condições iniciais
ALLOCATE(x(nt),v(nt))
x(1)=x0
v(1)=v0

! Integração das equações por RK4
hdt=0.5D0*dt
DO i=1,nt-1
  k1x=dt*fx(t(i),x(i),v(i))
  k1v=dt*fv(t(i),x(i),v(i),w,g)
  k2x=dt*fx(t(i)+hdt,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v)
  k2v=dt*fv(t(i)+hdt,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v,w,g)
  k3x=dt*fx(t(i)+hdt,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v)
  k3v=dt*fv(t(i)+hdt,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v,w,g)
  k4x=dt*fx(t(i)+dt,x(i)+k3x,v(i)+k3v)
  k4v=dt*fv(t(i)+dt,x(i)+k3x,v(i)+k3v,w,g)
  x(i+1)=x(i)+(k1x+2.0D0*k2x+2.0D0*k3x+k4x)/6.0D0
  v(i+1)=v(i)+(k1v+2.0D0*k2v+2.0D0*k3v+k4v)/6.0D0
END DO

OPEN(UNIT=1,FILE='x.dat')
OPEN(UNIT=2,FILE='v.dat')
DO i=1,nt
  WRITE(1,*) t(i),x(i)
  WRITE(2,*) t(i),v(i)
END DO
WRITE(1,*)
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)

! Calculando a energia
OPEN(UNIT=3,FILE='energia.dat')
DO i=1,nt
  ei=0.5D0*m*v(i)**2+0.5D0*k*x(i)**2  
  WRITE(3,*) t(i),ei
END DO
WRITE(3,*)
CLOSE(UNIT=3)

! Verificação do período
t1=-1000
t2=-1000
t3=-1000
contador=0
DO i=1,nt-1
  IF(x(i)*x(i+1).lt.0.0D0)THEN
    contador=contador+1
    t1=t2
    t2=t3
    t3=t(i)
    IF(contador.gt.2)THEN 
      IF(MOD(contador,2).eq.1)THEN
        WRITE(*,*) 'Período=',t3-t1
      END IF
    END IF  
  END IF
END DO


END PROGRAM dho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,x,v,w,g)
IMPLICIT NONE
REAL(KIND=8):: fv,t,x,v,w,g
fv=-w*w*x-2.0D0*g*v
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,v)
IMPLICIT NONE
REAL(KIND=8):: fx,t,x,v
fx=v
END FUNCTION fx
