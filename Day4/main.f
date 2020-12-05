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
            valid = valid+validate(tokens,count)
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

      write(*,*) 'Total: ',valid
      end

      function validate(tokens,count)
         integer validate,count
         character tokens(32)*32,key*3,value*32,units*2
         integer byr,iyr,eyr,hgt,hcl,ecl,pid,cid
         integer i,j,strlen,ishex,sum,length
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
            if(key.eq.'byr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.1920.and.temp.le.2002) then
                     byr = 1
                  endif
               endif
               write(*,*) i,tokens(i),byr,temp
            else if(key.eq.'iyr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.2010.and.temp.le.2020) then
                     iyr = 1
                  endif
               endif
               write(*,*) i,tokens(i),iyr,temp
            else if(key.eq.'eyr') then
               value = tokens(i)(5:)
               if(strlen(value).eq.4) then
                  read(value,*) temp
                  if(temp.ge.2020.and.temp.le.2030) then
                     eyr = 1
                  endif
               endif
               write(*,*) i,tokens(i),eyr,temp
            else if(key.eq.'hgt') then
               value = tokens(i)(5:)
               length = strlen(value)
               units = value(length-1:length)
               if (units.eq.'cm') then
                  read(value(:length-2),*) temp
                  if(temp.ge.150.and.temp.le.193) then
                     hgt = 1
                  endif
               else if(units.eq.'in') then
                  read(value(:length-2),*) temp
                  if(temp.ge.59.and.temp.le.76) then
                     hgt = 1
                  endif
               endif
               write(*,*) i,tokens(i),hgt,temp,units
            else if(key.eq.'ecl') then
               value = tokens(i)(5:)
               if(value.eq.'amb'.or.value.eq.'blu'.or.value.eq.'brn'.or.
     $            value.eq.'gry'.or.value.eq.'grn'.or.value.eq.'hzl'.or.
     $            value.eq.'oth') then
                  ecl = 1
               endif
               write(*,*) i,tokens(i),ecl,value
            else if(key.eq.'hcl') then
               value = tokens(i)(5:)
               if(strlen(value).eq.7) then
                  if(value(1:1).eq.'#') then
                     sum = 0
                     do 485 j=2,7
                        sum = sum + ishex(value(j:j))
 485                 continue
                     if(sum.eq.6) then
                        hcl = 1
                     endif
                  endif
               endif
               write(*,*) i,tokens(i),hcl,value
            else if(key.eq.'pid') then
               value = tokens(i)(5:)
               if(strlen(value).eq.9) then
                  sum = 0
                  do 475 j=1,9
                     if(value(j:j).ge.'0'.and.value(j:j).le.'9') then
                        sum = sum + 1
                     endif
 475              continue
                  if(sum.eq.9) then
                     pid = 1
                  endif
               endif
               write(*,*) i,tokens(i),pid,value,sum
            else if(key.eq.'cid') then
               cid = 1
            endif
 500     continue
         sum = byr+iyr+eyr+hcl+hgt+ecl+pid+cid
         if(sum.eq.8) then
            validate = 1
         endif
         write(*,*) 'Valid: ', validate
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

      integer function ishex(c)
         character c*1
         if(c.ge.'a'.and.c.le.'f') then
            ishex = 1
         else if(c.ge.'0'.and.c.le.'9') then
            ishex = 1
         endif
         return
      end