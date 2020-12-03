      program Day3
         character line*1024,token
         integer map(10000,1024),rows,columns(10000),trees,column,row

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

         write(*,*) 'Rows: ', rows

c        iterate down slope
         column = 1
         do row=1,rows
c           check tree            
            trees = trees+map(row,column)
            write(*,*) row,column,map(row,column),columns(i)
c           increment             
            column = column+3
            if (column.gt.columns(i)) then
c              wrap map
               column = column-columns(i)
            endif
 300     enddo
c        write number of trees
         write(*,*) 'Trees: ', trees
      end