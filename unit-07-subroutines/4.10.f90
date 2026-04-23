PROGRAM q4_10
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: x(10),media_a,media_g
n=10 ! A princípio, temos 10 números
DO i=1,10
  ! Mensagem ao usuário
  WRITE(*,*) 'Entre com um número positivo'
  WRITE(*,*) 'a ser utilizado no cálculo de'
  WRITE(*,*) 'médias. Caso não queira mais'
  WRITE(*,*) 'incluir números, entre com'
  WRITE(*,*) 'um valor negativo'
  ! Leitura
  READ(*,*) x(i)
  ! Verificando possível saída mais cedo
  IF(x(i).le.0.0D0)THEN
    n=i-1 ! Atualizando a quantidade de números
    EXIT
  END IF
END DO
! Possível mensagem de erro
IF(n.eq.0) STOP 'Entre com pelo menos um número.'

CALL media_a_calc(n,x,media_a)
WRITE(*,*) 'Média aritmética=',media_a

CALL media_g_calc(n,x,media_g)
WRITE(*,*) 'Média geométrica=',media_g
END PROGRAM q4_10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE media_a_calc(n,x,media_a)
IMPLICIT NONE
REAL(KIND=8):: x(10),media_a
INTEGER:: n,i
media_a=0
DO i=1,n
  media_a=media_a+x(i)
END DO 
media_a=media_a/DFLOAT(n)
END SUBROUTINE media_a_calc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE media_g_calc(n,x,media_g)
IMPLICIT NONE
REAL(KIND=8):: x(10),media_g
INTEGER:: n,i
media_g=1.0D0
DO i=1,n
  media_g=media_g*x(i)
END DO 
media_g=media_g**(1.0D0/DFLOAT(n))
END SUBROUTINE media_g_calc

