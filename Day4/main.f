      program Day4
         implicit none
         character line*256,lines(2048)*256
         character ch*1
         character token*32,tokens(32)*32
         integer rows,count,state,length
         integer i,j,k,io
         integer valid,validate,strlen

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
         character tokens(32)*32,key*3,value*32
         integer byr,iyr,eyr,hgt,hcl,ecl,pid,cid
         integer i,strlen
         real temp

         byr = 0
         iyr = 0
         eyr = 0
         hgt = 0
         hcl = 0
         ecl = 0
         pid = 0
         cid = 1
         validate = 0
         do 500 i=1,count
            key = tokens(i)(1:3)
            write(*,*) i,key
            if(key.eq.'byr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.1920.and.temp.le.2002) then
                     byr = 1
                  endif
               endif
            else if(key.eq.'iyr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.2010.and.temp.le.2020) then
                     iyr = 1
                  endif
               endif
            else if(key.eq.'eyr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.2020.and.temp.le.2030) then
                     eyr = 1
                  endif
               endif
            else if(key.eq.'hgt') then
               value = tokens(i)(5:)
               hgt = 1
            else if(key.eq.'ecl') then
               value = tokens(i)(5:)
               if(value.eq.'amb'.or.value.eq.'blu'.or.value.eq.'brn'.or.
     $            value.eq.'gry'.or.value.eq.'grn'.or.value.eq.'hzl'.or.
     $            value.eq.'oth') then
                  ecl = 1
               endif
            else if(key.eq.'hcl') then
               value = tokens(i)(5:)
               if(len(value).eq.7) then
                  if(value(1:1).eq.'#') then
                     hcl = 1
                  endif
               endif
            else if(key.eq.'pid') then
               value = tokens(i)(5:)
               if(len(value).eq.9) then
                  do 475 j=1,9
                     
 475              continue
                  pid = 1
               endif
            else if(key.eq.'cid') then
               cid = 1
            endif
 500     continue
         sum = byr+iyr+eyr+hcl+hgt+ecl+pid+cid
         if(sum.eq.8) then
            validate = 1
         endif
         return
      end

      integer function strlen(st)
         integer i,length
         character st*(*)
         length = len(st)
         do 600 i=1,length
            if (st(i:i).eq.' ') then
               strlen = i-1
               goto 650
            endif
 600     continue            
 650     return
      end      