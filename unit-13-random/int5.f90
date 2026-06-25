PROGRAM int5
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: fmed,x1,x2,x3,x4,x5,f,integral

! f(x1,x2,x3,x4,x5)=(x1+x2+x3+x4+x5)**2

! Número de quíntuplas ordenadas
n=1000000000
! Valor inicial do valor médio
fmed=0.0D0
DO i=1,n
  CALL RANDOM_NUMBER(x1)
  CALL RANDOM_NUMBER(x2)
  CALL RANDOM_NUMBER(x3)
  CALL RANDOM_NUMBER(x4)
  CALL RANDOM_NUMBER(x5)
  f=(x1+x2+x3+x4+x5)**2
  fmed=fmed+f
END DO
fmed=fmed/DFLOAT(n)
integral=fmed*(1-0)*(1-0)*(1-0)*(1-0)*(1-0)
WRITE(*,'(A,F0.7)') 'Integral==',integral
WRITE(*,'(A,F0.7)') 'Analitico=',20.0D0/3.0D0
END PROGRAM int5


