      program Day1
c        declarations         
         integer count,values(10000),value,sum,twoProduct,threeProduct

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
            endif
         enddo
         close(1)

         twoProduct =-1
         threeProduct =-1

c        search values to find two values that sum to 2020   
         do 200 i=1,count
            do 300 j=i+1,count
               sum = values(i)+values(j)
               write(*,*) i,values(i),j,values(j)
               if(sum.eq.2020) then
c                 compute twoProduct
                  twoProduct = values(i)*values(j)
                  goto 400
               endif
300        continue
200      continue  

c        search the values to find three values that sum to 2020
400      do 500 i=1,count
            if (values(i).lt.2020) then
               do 600 j=i+1,count
                  sum = values(i)+values(j)
                  if(sum.lt.2020) then
                     do 700 k=j+1,count
                        sum = values(i)+values(j)+values(k)
                        write(*,*) i,values(i),j,values(j),k,values(k)
                        if(sum.eq.2020) then
c                          compute twoProduct
                           threeProduct = values(i)*values(j)*values(k)
                           goto 900
                        endif
700                 continue
                  endif
600            continue
            endif
500      continue

900      write(*,*) 'Two Product: ',twoProduct
         write(*,*) 'Three Product: ',threeProduct

         end      