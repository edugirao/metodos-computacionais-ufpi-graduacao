PROGRAM formatos
IMPLICIT NONE
REAL(KIND=8):: x,y
INTEGER:: i,j

OPEN(UNIT=1,FILE='form.dat')
i=28
j=35
! WRITE(*,'(I0,2X,I0)') i,j

x=14.271D0
! WRITE(*,'(E12.5)') x

WRITE(*,'(A3)') 'casa'
WRITE(*,'(A)') 'casaaaaaa'



CLOSE(UNIT=1)
END PROGRAM formatos
