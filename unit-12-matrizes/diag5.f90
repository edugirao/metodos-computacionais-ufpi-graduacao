PROGRAM diag1
IMPLICIT NONE
INTEGER:: INFO,LWORK,i
REAL(KIND=8),ALLOCATABLE:: WORK(:)
REAL(KIND=8):: A(5,5),eigenvalues(5)


! Matriz A
A(1,1)=4.0D0
A(1,2)=1.0D0
A(1,3)=0.0D0
A(1,4)=0.0D0
A(1,5)=0.0D0
A(2,1)=1.0D0
A(2,2)=3.0D0
A(2,3)=1.0D0
A(2,4)=0.0D0
A(2,5)=0.0D0
A(3,1)=0.0D0
A(3,2)=1.0D0
A(3,3)=2.0D0
A(3,4)=1.0D0
A(3,5)=0.0D0
A(4,1)=0.0D0
A(4,2)=0.0D0
A(4,3)=1.0D0
A(4,4)=3.0D0
A(4,5)=1.0D0
A(5,1)=0.0D0
A(5,2)=0.0D0
A(5,3)=0.0D0
A(5,4)=1.0D0
A(5,5)=4.0D0

! Primeira chamada
ALLOCATE(WORK(1))
CALL DSYEV('V','U',5,A,5,eigenvalues,WORK,-1,INFO)
LWORK=WORK(1)
DEALLOCATE(WORK)
ALLOCATE(WORK(LWORK))
CALL DSYEV('V','U',5,A,5,eigenvalues,WORK,LWORK,INFO)

! Imprimindo os resultados
DO i=1,5
  WRITE(*,'(A)') '****************************************'
  WRITE(*,'(A,I0,A,F0.7)') 'Eigenvalue',i,'=',eigenvalues(i)
  WRITE(*,'(A)') '----------------------------------------'
  WRITE(*,'(A,I0)') 'Eigenvector',i
  WRITE(*,'(F0.5,1x,F0.5,1x,F0.5,1x,F0.5,1X,F0.5)') A(:,i)
END DO
WRITE(*,'(A)') '****************************************'


END PROGRAM diag1


