PROGRAM pi_calc
IMPLICIT NONE
INTEGER:: i,nq,nc
REAL(KIND=8):: x,y,pi,dist
! Número de pontos no quadrado
nq=1000000000
! Inicializando o número de pontos no círculo
nc=0
! Gerando pontos aleatórios e calculando
! quantos deles estão dentro do círculo
DO i=1,nq
  ! Gerando um ponto aleatório
  CALL RANDOM_NUMBER(x)
  x=x-0.5D0
  CALL RANDOM_NUMBER(y)
  y=y-0.5D0
  ! Distânca ao centro do círculo
  dist=DSQRT(x**2+y**2)
  ! Verificando se dentro do círculo
  IF(dist.le.0.5D0)THEN
    ! ponto dentro do circulo
    nc=nc+1
  END IF  
END DO
pi=4.0D0*DFLOAT(nc)/DFLOAT(nq)

WRITE(*,'(A,F0.7)') 'pi~',pi

END PROGRAM pi_calc


