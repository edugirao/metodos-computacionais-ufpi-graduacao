PROGRAM p7_7
IMPLICIT NONE
REAL(KIND=8):: media,distinction
REAL(KIND=8),ALLOCATABLE:: nota(:) 
INTEGER:: n,i
INTEGER,ALLOCATABLE:: id(:)

WRITE(*,*) 'Entre com o número de estudantes:'
READ(*,*) n
ALLOCATE(id(n),nota(n))

DO i=1,n
  WRITE(*,*) 'Entre com a ID do aluno (1000-1999):'
  READ(*,*) id(i)
  IF((id(i).lt.1000).OR.(id(i).gt.1999))THEN
    WRITE(*,*) 'Por favor, entre com ID entre 1000 e 1999.'
    STOP
  END IF
  WRITE(*,*) 'Entre com a nota do aluno (0-10):'
  READ(*,*) nota(i)
  IF((nota(i).lt.0.0D0).OR.(nota(i).gt.10.0D0))THEN
    WRITE(*,*) 'Por favor, entre com nota entre 0 e 10.'
    STOP
  END IF
END DO

! DO i=1,n
!   WRITE(*,*) id(i),nota(i)
! END DO

media=SUM(nota)/n

WRITE(*,*) 'Nota média:',media

distinction=1.3D0*media

DO i=1,n
  IF(nota(i).gt.distinction)THEN
    WRITE(*,*) 'Estudante ',id(i),'obteve distinção com nota ',nota(i)
  END IF
END DO


END PROGRAM p7_7

