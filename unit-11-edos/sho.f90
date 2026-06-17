PROGRAM sho
IMPLICIT NONE
INTEGER:: nt,i,contador
REAL(KIND=8):: m,pi,w,k,x0,v0,dt,hdt,fx,fv,e0,ei
REAL(KIND=8):: t1,t2,t3
REAL(KIND=8):: k1x,k2x,k3x,k4x
REAL(KIND=8):: k1v,k2v,k3v,k4v
REAL(KIND=8),ALLOCATABLE:: t(:),x(:),v(:)

! Parâmetros iniciais
m=1.0D0
pi=DACOS(-1.0D0)
k=4.0D0*pi**2
w=DSQRT(k/m)
WRITE(*,*) 'Período esperado para o SHO:',2.0D0*pi/w
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
  k1v=dt*fv(t(i),x(i),v(i),w)
  k2x=dt*fx(t(i)+hdt,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v)
  k2v=dt*fv(t(i)+hdt,x(i)+0.5D0*k1x,v(i)+0.5D0*k1v,w)
  k3x=dt*fx(t(i)+hdt,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v)
  k3v=dt*fv(t(i)+hdt,x(i)+0.5D0*k2x,v(i)+0.5D0*k2v,w)
  k4x=dt*fx(t(i)+dt,x(i)+k3x,v(i)+k3v)
  k4v=dt*fv(t(i)+dt,x(i)+k3x,v(i)+k3v,w)
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

! Monitorando a energia
OPEN(UNIT=3,FILE='d_energia.dat')
e0=0.5D0*m*v(1)**2+0.5D0*k*x(1)**2
DO i=2,nt
  ei=0.5D0*m*v(i)**2+0.5D0*k*x(i)**2  
  WRITE(3,*) t(i),-LOG10(ABS((ei-e0)/e0))
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


END PROGRAM sho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,x,v,w)
IMPLICIT NONE
REAL(KIND=8):: fv,t,x,v,w
fv=-w*w*x
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,v)
IMPLICIT NONE
REAL(KIND=8):: fx,t,x,v
fx=v
END FUNCTION fx
