PROGRAM kinematics
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: t,A,w,g,v,xt,vt,at,h,x0,dt,integ,t1,t2
REAL(KIND=8),ALLOCATABLE:: tvec(:),vvec(:),wvec(:)
! Constantes
A=2.0D0
w=2.0D0*DACOS(-1.0D0)/4.0D0
g=2.0D0*DACOS(-1.0D0)/12.0D0

! Solicitando o instante de tempo
WRITE(*,*) 'Entre com a posição inicial:'
READ(*,*) x0

! Solicitando o instante de tempo
WRITE(*,*) 'Entre com o instante de tempo:'
READ(*,*) t

! Velocidade
vt=v(t,A,w,g)

! Aceleração
h=1.0D-5
at=(v(t+h,A,w,g)-v(t-h,A,w,g))/(2.0D0*h)

! Posição
t1=0
t2=t
n=1000
ALLOCATE(tvec(n),vvec(n),wvec(n))
dt=(t2-t1)/(n-1)
DO i=1,n
  tvec(i)=(i-1)*dt
END DO
DO i=1,n
  vvec(i)=v(tvec(i),A,w,g)
END DO
wvec=dt
wvec(1)=dt/2.0D0
wvec(n)=dt/2.0D0
integ=SUM(vvec*wvec)
xt=x0+integ

! Imprimindo os resultados
WRITE(*,*) 'Posição====',xt
WRITE(*,*) 'Velocidade=',vt
WRITE(*,*) 'Aceleração=',at
END PROGRAM kinematics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION v(t,A,w,g)
IMPLICIT NONE
REAL(KIND=8):: v,t,A,w,g
v=A*DEXP(-g*t)*DCOS(w*t)
END FUNCTION v



