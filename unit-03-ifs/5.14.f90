PROGRAM q5_14
IMPLICIT NONE
INTEGER:: a,b,c
LOGICAL:: triangle,isosceles,equilat
! Dados de entrada
WRITE(*,*) 'Escreva 3 comprimentos inteiros:'
READ(*,*) a,b,c
! Triângulo?
triangle=.true.
IF(a.ge.b+c)THEN
  triangle=.false.
ELSE IF(b.ge.a+c)THEN
  triangle=.false.
ELSE IF(c.ge.a+b)THEN
  triangle=.false.  
END IF
! Mensagem
IF(triangle)THEN
  WRITE(*,*) 'Os valores podem formar um triângulo.'
ELSE
  WRITE(*,*) 'Os valores não podem formar um triângulo.'
END IF


! Isosceles
isosceles=.false.
IF(triangle)THEN
  IF(a.eq.b)THEN
    IF(a.ne.c)THEN
      isosceles=.true.
    END IF
  END IF
  IF((a.eq.c).AND.(a.ne.b))THEN
    isosceles=.true.
  END IF
  ! IF(a.eq.c)THEN
  !   IF(a.ne.b)THEN
  !     isosceles=.true.
  !   END IF
  ! END IF
  IF(b.eq.c)THEN
    IF(b.ne.a)THEN
      isosceles=.true.
    END IF
  END IF
END IF  
! Mensagem
IF(isosceles)THEN
  WRITE(*,*) 'O triangulo é isósceles'
ELSE
  IF(triangle)THEN
    WRITE(*,*) 'O triangulo não é isósceles'
  END IF  
END IF



! Equilater0
equilat=.false.
IF(triangle)THEN
  IF(a.eq.b)THEN
    IF(a.eq.c)THEN
      equilat=.true.
    END IF
  END IF
  IF((a.eq.c).AND.(a.eq.b))THEN
    equilat=.true.
  END IF
  ! IF(a.eq.c)THEN
  !   IF(a.eq.b)THEN
  !     equilat=.true.
  !   END IF
  ! END IF
  IF(b.eq.c)THEN
    IF(b.eq.a)THEN
      equilat=.true.
    END IF
  END IF
END IF  
! Mensagem
IF(equilat)THEN
  WRITE(*,*) 'O triangulo é equilátero'
ELSE
  IF(triangle)THEN
    WRITE(*,*) 'O triangulo não é equilátero'
  END IF  
END IF







END PROGRAM q5_14
