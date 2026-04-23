PROGRAM q4_2
IMPLICIT NONE
REAL(KIND=8):: x1,y1,x2,y2,d1,d2,d12

WRITE(*,*) 'Entre com dois pontos no plano cartesiano:'
WRITE(*,*) '(x1,y1)'
READ(*,*) x1,y1
WRITE(*,*) '(x2,y2)'
READ(*,*) x2,y2

CALL tri_calc(x1,y1,x2,y2,d1,d2,d12)

WRITE(*,*) 'Dist. 1o ponto à origem=',d1
WRITE(*,*) 'Dist. 2o ponto à origem=',d2
WRITE(*,*) 'Distância entre pontos==',d12
END PROGRAM q4_2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE tri_calc(x1,y1,x2,y2,d1,d2,d12)
IMPLICIT NONE
REAL(KIND=8):: x1,y1,x2,y2,d1,d2,d12,twopoint_d
d1=twopoint_d(x1,y1,0.0D0,0.0D0)
d2=twopoint_d(x2,y2,0.0D0,0.0D0)
d12=twopoint_d(x1,y1,x2,y2)
END SUBROUTINE 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION twopoint_d(x1,y1,x2,y2)
IMPLICIT NONE
REAL(KIND=8):: x1,y1,x2,y2,twopoint_d
twopoint_d=DSQRT((x1-x2)**2+(y1-y2)**2)
END FUNCTION twopoint_d



