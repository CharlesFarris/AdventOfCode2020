      program Day2
         character tokens(3)*32
         integer validate,valid

         valid = 0

c        read input fil         
         open(1,FILE='input.txt',STATUS='OLD')
         do
            read(1,*,IOSTAT=io) tokens
            if (io.gt.0) then
               write(*,*) 'Error'
               exit
            else if(io.lt.0) then
               exit
            else
               valid = valid + validate(tokens)
            endif
         enddo
         close(1)

         write(*,*) 'Valid: ', valid
      end      

      integer function validate(tokens)
         character tokens(3)*32,c*1
         integer i,min,max,count

         validate = 0
         do 100 i=1,32
            c = tokens(1)(i:i)
            if(c.eq.'-') then
               tokens(1)(i:i) = ' '
            endif
 100     continue
         read(tokens(1), *) min,max            
         c=tokens(2)(1:1)
         do 150 i=1,32
            if(tokens(3)(i:i).eq.c) then
               count = count+1
            endif
 150     continue
         if(count.ge.min.and.count.le.max) then
            validate = 1
         endif
         write(*,*) c,min,max,validate,tokens(3)
         return
      end