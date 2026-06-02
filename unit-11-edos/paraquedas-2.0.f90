PROGRAM paraquedas
IMPLICIT NONE
INTEGER:: n,i
REAL(KIND=8):: m,b,g,h0,dt,fy,fv,tol,variation,vl,tl
REAL(KIND=8):: k1y,k2y,k3y,k4y
REAL(KIND=8):: k1v,k2v,k3v,k4v
REAL(KIND=8),ALLOCATABLE:: t(:),y(:),v(:)
LOGICAL:: fechado,limite

! Parâmetros do cálculo
WRITE(*,*) 'Entre com a massa do paraquedista:'
READ(*,*) m
WRITE(*,*) 'Entre com a constante b:'
READ(*,*) b
WRITE(*,*) 'Entre com o módulo da aceleração da gravidade:'
READ(*,*) g
WRITE(*,*) 'Entre com a altura inicial:'
READ(*,*) h0
fechado=.true.
limite=.false.
! Grid no tempo
dt=0.01
n=100001
ALLOCATE(t(n))
DO i=1,n
  t(i)=DFLOAT(i-1)*dt
END DO

! Condições iniciais
ALLOCATE(y(n),v(n))
y(1)=h0
v(1)=0

! Solução acoplada por RK4
tol=0.0001
tl=10000000
DO i=1,n-1
  ! k1
  k1y=dt*fy(t(i),y(i),v(i))
  k1v=dt*fv(t(i),y(i),v(i),g,b,m)
  ! k2
  k2y=dt*fy(t(i)+0.5D0*dt,y(i)+0.5D0*k1y,v(i)+0.5D0*k1v)
  k2v=dt*fv(t(i)+0.5D0*dt,y(i)+0.5D0*k1y,v(i)+0.5D0*k1v,g,b,m)
  ! k3
  k3y=dt*fy(t(i)+0.5D0*dt,y(i)+0.5D0*k2y,v(i)+0.5D0*k2v)
  k3v=dt*fv(t(i)+0.5D0*dt,y(i)+0.5D0*k2y,v(i)+0.5D0*k2v,g,b,m)
  ! k4
  k4y=dt*fy(t(i)+dt,y(i)+k3y,v(i)+k3v)
  k4v=dt*fv(t(i)+dt,y(i)+k3y,v(i)+k3v,g,b,m)
  ! Recorrência
  y(i+1)=y(i)+(k1y+2.0D0*k2y+2.0D0*k3y+k4y)/6.0D0
  v(i+1)=v(i)+(k1v+2.0D0*k2v+2.0D0*k3v+k4v)/6.0D0
  ! Checando abertura de paraquedas
  variation=100.0*ABS((v(i+1)-v(i))/v(i))
  IF((variation.lt.tol).AND.(.not.limite))THEN
    vl=v(i)
    tl=t(i)
    limite=.true.
  END IF
  IF((t(i).gt.tl+10.0D0).AND.(fechado))THEN
    b=10.0D0*b
    fechado=.false.
  END IF
END DO


! Calculando a velocidade limite

DO i=1,n
  variation=100.0*ABS((v(i+1)-v(i))/v(i))
  IF(variation.lt.tol)THEN
    tl=t(i)
    vl=v(i)
    WRITE(*,'(A,1X,F0.2,A)') 'Velocidade limite alcançada no instante',t(i),'.'
    WRITE(*,'(A,1X,F0.2,A)') 'Velocidade limite =',v(i),'.'
    EXIT
  END IF
END DO

! Escrevendo os dados
OPEN(UNIT=1,FILE='pos.dat')
OPEN(UNIT=2,FILE='vel.dat')
DO i=1,n
  IF(y(i).lt.0.0D0)THEN
    WRITE(*,'(A,1X,F0.2,A)') 'Chegada ao solo no instante',(t(i)+t(i-1))/2.0D0,'.'
    EXIT
  END IF
  WRITE(1,*) t(i),y(i)
  WRITE(2,*) t(i),v(i)
END DO
WRITE(1,*)
WRITE(2,*)
WRITE(2,*) tl,1.1*vl
WRITE(2,*) tl,0
WRITE(2,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)

END PROGRAM paraquedas

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fv(t,y,v,g,b,m)
IMPLICIT NONE
REAL(KIND=8):: fv,t,y,v,g,b,m
fv=-g-b*v/m
END FUNCTION fv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fy(t,y,v)
IMPLICIT NONE
REAL(KIND=8):: fy,t,y,v
fy=v
END FUNCTION fy
