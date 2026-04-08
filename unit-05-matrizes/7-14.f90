PROGRAM p7_14
IMPLICIT NONE
REAL(KIND=8):: a(3),b(3),c(3),d,v(3),m
INTEGER:: i,j,k

! Vetores
a(1)=1.0
a(2)=2.0
a(3)=3.0
b(1)=2.0
b(2)=4.0
b(3)=6.0

! Produto interno
! d=a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
! ! c=a*b
! ! d=SUM(c)
d=SUM(a*b)
WRITE(*,*) 'Produto escalar:',d

! Produto vetorial
! c(1)=a(2)*b(3)-a(3)*b(2)
! c(2)=a(3)*b(1)-a(1)*b(3)
! c(3)=a(1)*b(2)-a(2)*b(1)
DO i=1,3
  j=i+1
  IF(j.gt.3) j=j-3
  k=i+2
  IF(k.gt.3) k=k-3
  c(i)=a(j)*b(k)-a(k)*b(j)
END DO

WRITE(*,*) 'Produto vetorial:',c

! Produto misto
v(1)=b(2)*c(3)-b(3)*c(2)
v(2)=b(3)*c(1)-b(1)*c(3)
v(3)=b(1)*c(2)-b(2)*c(1)
m=SUM(a*v)
WRITE(*,*) 'Produto Misto:',m

END PROGRAM p7_14

