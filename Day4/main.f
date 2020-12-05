      program Day4
         implicit none
         character line*256,lines(2048)*256
         character ch*1
         character token*32,tokens(32)*32
         integer rows,count,state,length
         integer i,j,k,io
         integer valid,validate

c        read input file         
         open(1,FILE='input.txt',STATUS='OLD')
         do
            read(1,"(A256)",IOSTAT=io) line
            if (io.gt.0) then
               write(*,*) 'Error'
               exit
            else if(io.lt.0) then
               exit
            else
               rows = rows+1
               lines(rows)=line
            endif
         enddo
 200  close(1)

      write(*,*) 'Rows: ',rows
      rows = rows+1
      lines(rows) = ''
      count = 0
      valid = 0
      do 300 i=1,rows
         line = lines(i)
         if (line.eq.'') then
            write(*,*) 'break'
            write(*,*) 'Tokens:',count,(tokens(k),k=1,count)
            valid = valid+validate(tokens,count)
            write(*,*) 'Valid: ',valid
            count = 0
         else
            state = 0
            do 350 j=1,256
               ch = line(j:j)
               if(state.eq.0) then
                  if(ch.eq.'') then
                  else if(ch.eq.' ') then
                  else
                     state = 1
                     length = 1
                     token(length:length) = ch
                  endif
               else if(state.eq.1) then
                  if(ch.eq.'') then
                     state = 0
                     count = count+1
                     tokens(count) = token(1:length)
                  else if(ch.eq.' ') then
                     state = 0
                     count = count+1
                     tokens(count) = token(1:length)
                  else
                     length = length+1
                     token(length:length) = ch
                  endif
               endif
 350        continue
         endif
 300  continue

      write(*,*) 'Valid: ',valid
      end

      function validate(tokens,count)
         integer validate,count
         character tokens(32)*32,key*3
         integer byr,iyr,eyr,hgt,hcl,ecl,pid,cid
         integer i

         byr = 0
         iyr = 0
         eyr = 0
         hgt = 0
         hcl = 0
         ecl = 0
         pid = 0
         cid = 0
         validate = 0
         do 500 i=1,count
            key = tokens(i)(1:3)
            if(key.eq.'byr') then
               byr = 1
            else if(key.eq.'iyr') then
               iyr = 1
            else if(key.eq.'eyr') then
               eyr = 1
            else if(key.eq.'hgt') then
               hcl = 1
            else if(key.eq.'hcl') then
               hgt = 1
            else if(key.eq.'ecl') then
               ecl = 1
            else if(key.eq.'pid') then
               pid = 1
            else if(key.eq.'cid') then
               cid = 1
            endif
 500     continue
         sum = byr+iyr+eyr+hcl+hgt+ecl+pid
         if (cid.eq.0) then
            sum = sum+1
         else if(cid.eq.1) then
            sum = sum+cid
         endif
         if(sum.eq.8) then
            validate = 1
         else
            validate = 0
         endif
         return
      end
