PROGRAM reaction
IMPLICIT NONE
INTEGER:: minuto 
REAL(KIND=8):: temp,time,q,k,yield

WRITE(*,*) 'Entre com a temperatura da reação:'
READ(*,*) temp
q=2000.0D0/(temp+273.16D0)
k=DEXP(-q)
minuto=0
DO 
  minuto=minuto+1
  time=minuto*60.0D0
  yield=1.0D0-DEXP(-k*time)
  IF(yield.gt.0.95D0)THEN
    EXIT
  END IF
  WRITE(*,*) 'O rendimento para',minuto,'minutos é',yield,'.'
END DO

END PROGRAM reaction
