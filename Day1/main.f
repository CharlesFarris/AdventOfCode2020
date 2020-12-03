      program Day1
c        declarations         
         integer count,values(10000),value,sum,product

c        read input file         
         open(1,FILE='input.txt',STATUS='OLD')
         do
            read(1,*,IOSTAT=io) value
            if (io.gt.0) then
               write(*,*) 'Error'
               exit
            else if(io.lt.0) then
               exit
            else
               values(count+1) = value
               count = count+1
            end if
         end do
         close(1)

c        search values to find two values that sum to 2020   
         do 200 i=1,count
            do 300 j=i,count
               sum = values(i)+values(j)
               if(sum.eq.2020) then
c                 compute product
                  product = values(i)*values(j)
c                 output solution
                  write(*,*) i,values(i),j,values(j)
                  write(*,*) 'product: ',product
                  goto 400
               endif
300        continue
200      continue  

c        no solution found
         write(*,*) 'No solution found!'
400   end      