PROGRAM q5_1
IMPLICIT NONE
INTEGER:: a

WRITE(*,*) 'Por favor, entre com um valor inteiro:'
READ(*,*) a

IF(a.gt.0)THEN
  WRITE(*,*) 'O valor fornecido é positivo.'
ELSE IF(a.lt.0)THEN
  WRITE(*,*) 'O valor fornecido é negativo.'
ELSE
  WRITE(*,*) 'O valor fornecido é nulo.'
END IF

END PROGRAM q5_1
