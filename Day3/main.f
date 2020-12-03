      program Day3
         character line*1024,token
         integer map(10000,1024),rows,columns(10000),column,row
         integer dr(5),dc(5),sum(5)

c        read input file         
         open(1,FILE='input.txt',STATUS='OLD')
         do
            read(1,*,IOSTAT=io) line
            if (io.gt.0) then
               write(*,*) 'Error'
               exit
            else if(io.lt.0) then
               exit
            else
               rows = rows+1
               do 100 i=1,1024
                  token = line(i:i)
                  if(token.eq.'.') then
                     map(rows,i) = 0
                     columns(rows) = i
                  else if (token.eq.'#') then
                     map(rows,i) = 1
                     columns(rows) = i
                  else
                     exit
                  endif
 100           continue
            endif
         enddo
 200     close(1)

         write(*,*) 'Rows: ',rows

c        set delta rows         
         dr(1) = 1
         dr(2) = 1
         dr(3) = 1
         dr(4) = 1
         dr(5) = 2

c        set delta columns         
         dc(1) = 1
         dc(2) = 3
         dc(3) = 5
         dc(4) = 7
         dc(5) = 1

         do 400 i=1,5
c           iterate down slope
            sum(i) = 0
            column = 1
            do 300 row=1,rows,dr(i)
c              check tree            
               sum(i) = sum(i)+map(row,column)
c              increment             
               column = column+dc(i)
               if (column.gt.columns(i)) then
c                 wrap map
                  column = column-columns(i)
               endif
 300       continue
 400     continue
c        write number of trees
         do 500 i=1,5
            write(*,*) 'Trees: ',sum(i)
 500     continue
         write(*,*) 'Product: ',sum(1)*sum(2)*sum(3)*sum(4)*sum(5)
      end