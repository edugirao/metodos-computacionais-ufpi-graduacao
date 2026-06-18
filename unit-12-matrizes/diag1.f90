PROGRAM diag1
IMPLICIT NONE
INTEGER:: INFO,LWORK,i
REAL(KIND=8),ALLOCATABLE:: WORK(:)
REAL(KIND=8):: A(3,3),eigenvalues(3)


! Matriz A
A(1,1)=3.0D0
A(1,2)=3.0D0
A(1,3)=3.0D0
A(2,1)=3.0D0
A(2,2)=3.0D0
A(2,3)=3.0D0
A(3,1)=3.0D0
A(3,2)=3.0D0
A(3,3)=3.0D0

! Primeira chamada
ALLOCATE(WORK(1))
CALL DSYEV('V','U',3,A,3,eigenvalues,WORK,-1,INFO)
LWORK=WORK(1)
DEALLOCATE(WORK)
ALLOCATE(WORK(LWORK))
CALL DSYEV('V','U',3,A,3,eigenvalues,WORK,LWORK,INFO)

! Imprimindo os resultados
DO i=1,3
  WRITE(*,'(A)') '****************************************'
  WRITE(*,'(A,I0,A,F0.7)') 'Eigenvalue',i,'=',eigenvalues(i)
  WRITE(*,'(A)') '----------------------------------------'
  WRITE(*,'(A,I0)') 'Eigenvector',i
  WRITE(*,'(F0.5,1x,F0.5,1X,F0.5)') A(:,i)
END DO
WRITE(*,'(A)') '****************************************'


END PROGRAM diag1


