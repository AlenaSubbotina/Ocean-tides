program tides

implicit none
 
real :: tetaf, A, delC, delS, delCp, delCm, delSp, delSm, summ, pi
real, allocatable, dimension(:) :: A2, F
integer :: i, j, x, nmax, n, m, A1, tetag
 character(len=2) b  

x = 59462
pi = 3.14159265359
tetag = 1
nmax = 10;

allocate (A2(0:6))
allocate (F(1:6))

open (unit = 1, file = 'coef.txt', action = 'read')
open (unit = 2, file = 'resF.txt', action = 'read')
open (unit = 3, file = 'res.txt', action = 'write')


!-----------------------read F-----------------------------
do i=1, 5
    read(2,*) F(i)
enddo

do i = 1, x
    read(1,*) A, b, n, m, delCp, delCm, delSp, delSm
    
    if (m <= n .and. n <= nmax .and. n >= 2 .and. m >= 0) then
        !------------------------count A2-----------------------------
        A1 = int(A*1000)
        !write(*,*) A1
        do j = 6, 0, -1
            A2(j) = A1 - (A1/10)*10
            A1 = (A1/10)
        enddo
        do j = 1, 5
            A2(j) = A2(j) - 5
        enddo

        !-----------------------count tetaf---------------------------
        summ = 0
        do j = 1, 5   
            summ = summ + A2(j)*F(j)
            !write(*,*) A2(j), F(j), A2(j)*F(j)
            !write(*,*)
        enddo
        
        tetaf = A2(0)*(tetag + pi) - summ
        !write(*,*) tetaf
        !write(*,*) 

        !------------------------count delC, delS-----------------------
        delC = delCp*cos(tetaf) + delSp*sin(tetaf) + delCm*cos(tetaf) + delSm*sin(tetaf)
        delS = delCp*sin(tetaf) - delSp*cos(tetaf) - delCm*sin(tetaf) + delSm*cos(tetaf)
        
        write(3,*) n, m, delC, delS
    end if
enddo

deallocate (A2)
deallocate (F)
 close(1)
 close(2)

end
!  Doodson Darw  l   m    DelC+     DelS+       DelC-     DelS-

