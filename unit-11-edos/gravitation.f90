PROGRAM gravitation
IMPLICIT NONE
INTEGER:: i,nt
REAL(KIND=8):: GM,dt,x0,y0,vx0,vy0,v0,pi,alpha,r
REAL(KIND=8):: fx,fy,fvx,fvy
REAL(KIND=8):: k1x,k2x,k3x,k4x
REAL(KIND=8):: k1y,k2y,k3y,k4y
REAL(KIND=8):: k1vx,k2vx,k3vx,k4vx
REAL(KIND=8):: k1vy,k2vy,k3vy,k4vy
REAL(KIND=8),ALLOCATABLE:: t(:),x(:),y(:),vx(:),vy(:)


! Parâmetros do cálculo
GM=1.0D0

! Grid no tempo
dt=0.0001D0
nt=10000000
ALLOCATE(t(nt))
DO i=1,nt
  t(i)=DFLOAT(i-1)*dt
END DO

! Condições iniciais
x0=0.5D0
y0=0.0D0
vx0=0.0D0
vy0=2.7D0
ALLOCATE(x(nt),y(nt),vx(nt),vy(nt))
x(1)=x0
y(1)=y0
vx(1)=vx0
vy(1)=vy0

! Integração por RK4
DO i=1,nt-1
  k1x=dt*fx(t(i),x(i),y(i),vx(i),vy(i))
  k1y=dt*fy(t(i),x(i),y(i),vx(i),vy(i))
  k1vx=dt*fvx(t(i),x(i),y(i),vx(i),vy(i),GM)
  k1vy=dt*fvy(t(i),x(i),y(i),vx(i),vy(i),GM)
  
  k2x=dt*fx(t(i)+0.5D0*dt,x(i)+0.5D0*k1x,y(i)+0.5D0*k1y,vx(i)+0.5D0*k1vx,vy(i)+0.5D0*k1vy)
  k2y=dt*fy(t(i)+0.5D0*dt,x(i)+0.5D0*k1x,y(i)+0.5D0*k1y,vx(i)+0.5D0*k1vx,vy(i)+0.5D0*k1vy)
  k2vx=dt*fvx(t(i)+0.5D0*dt,x(i)+0.5D0*k1x,y(i)+0.5D0*k1y,vx(i)+0.5D0*k1vx,vy(i)+0.5D0*k1vy,GM)
  k2vy=dt*fvy(t(i)+0.5D0*dt,x(i)+0.5D0*k1x,y(i)+0.5D0*k1y,vx(i)+0.5D0*k1vx,vy(i)+0.5D0*k1vy,GM)
  
  k3x=dt*fx(t(i)+0.5D0*dt,x(i)+0.5D0*k2x,y(i)+0.5D0*k2y,vx(i)+0.5D0*k2vx,vy(i)+0.5D0*k2vy)
  k3y=dt*fy(t(i)+0.5D0*dt,x(i)+0.5D0*k2x,y(i)+0.5D0*k2y,vx(i)+0.5D0*k2vx,vy(i)+0.5D0*k2vy)
  k3vx=dt*fvx(t(i)+0.5D0*dt,x(i)+0.5D0*k2x,y(i)+0.5D0*k2y,vx(i)+0.5D0*k2vx,vy(i)+0.5D0*k2vy,GM)
  k3vy=dt*fvy(t(i)+0.5D0*dt,x(i)+0.5D0*k2x,y(i)+0.5D0*k2y,vx(i)+0.5D0*k2vx,vy(i)+0.5D0*k2vy,GM)
  
  k4x=dt*fx(t(i)+dt,x(i)+k3x,y(i)+k3y,vx(i)+k3vx,vy(i)+k3vy)
  k4y=dt*fy(t(i)+dt,x(i)+k3x,y(i)+k3y,vx(i)+k3vx,vy(i)+k3vy)
  k4vx=dt*fvx(t(i)+dt,x(i)+k3x,y(i)+k3y,vx(i)+k3vx,vy(i)+k3vy,GM)
  k4vy=dt*fvy(t(i)+dt,x(i)+k3x,y(i)+k3y,vx(i)+k3vx,vy(i)+k3vy,GM)
  
  x(i+1)=x(i)+(k1x+2.0D0*k2x+2.0D0*k3x+k4x)/6.0D0
  y(i+1)=y(i)+(k1y+2.0D0*k2y+2.0D0*k3y+k4y)/6.0D0
  vx(i+1)=vx(i)+(k1vx+2.0D0*k2vx+2.0D0*k3vx+k4vx)/6.0D0
  vy(i+1)=vy(i)+(k1vy+2.0D0*k2vy+2.0D0*k3vy+k4vy)/6.0D0
  
  IF((y(i)*y(i+1).lt.0.0D0).AND.(x(i).gt.0.0D0))THEN
    WRITE(*,'(A,F0.5)') 'Um período em ',t(i)
  END IF
END DO

pi=DACOS(-1.0D0)
r=MAXVAL(ABS(y))*0.05
OPEN(UNIT=1,FILE='trajectory.dat')
DO i=1,nt
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
DO i=0,360
  alpha=i*pi/180.0D0
  WRITE(1,*) r*DCOS(alpha),r*DSIN(alpha)
END DO
WRITE(1,*)

CLOSE(UNIT=1)

END PROGRAM gravitation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fvx(t,x,y,vx,vy,GM)
IMPLICIT NONE
REAL(KIND=8):: fvx,t,x,y,vx,vy,GM,r
r=DSQRT(x**2+y**2)
fvx=-GM*x/r**3
END FUNCTION fvx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fvy(t,x,y,vx,vy,GM)
IMPLICIT NONE
REAL(KIND=8):: fvy,t,x,y,vx,vy,GM,r
r=DSQRT(x**2+y**2)
fvy=-GM*y/r**3
END FUNCTION fvy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,y,vx,vy)
IMPLICIT NONE
REAL(KIND=8):: fx,t,x,y,vx,vy
fx=vx
END FUNCTION fx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fy(t,x,y,vx,vy)
IMPLICIT NONE
REAL(KIND=8):: fy,t,x,y,vx,vy
fy=vy
END FUNCTION fy
