        program Day5
            character line*12
            integer ids(1024),count,i,decode,filled(1032),seat

            count = 0

c           read input file         
            open(1,FILE='input.txt',STATUS='OLD')
            do
               read(1,*,IOSTAT=io) line
               if (io.gt.0) then
                  write(*,*) 'Error'
                  exit
               else if(io.lt.0) then
                  exit
               else
                    write(*,*) line
                    count = count+1
                    ids(count) = decode(line)
               endif
            enddo
 200        close(1)

c           create filled list
            do 275 i=1,1032
                filled(i) = 0
 275        continue

c           update filled list
            do 300 i=1,count
                write(*,*) ids(i)
                filled(ids(i)) = 1
 300        continue

c           find empty seat 
            seat =-1
            do 325 i=2,1031
                if(filled(i).eq.0) then
                    if (filled(i-1).eq.1.and.filled(i+1).eq.1) then
                        seat = i
                        write(*,*) 'Seat: ',seat
                    endif
                endif
 325        continue

        end

        function decode(line)
            integer decode
            character line*12
            integer s,e,row,column,i,half

            write(*,*) line

c           compute row
            row = 0
            s = 0
            e = 127
            do 100 i=1,7
                half =(e-s+1)/2
                if(i.eq.7) then
                    if(line(i:i).eq.'F') then
                        row = s
                    else if (line(i:i).eq.'B') then
                        row = e
                    endif
                else
                    if(line(i:i).eq.'F') then
                        e =s+half-1
                    else if (line(i:i).eq.'B') then
                        s = e-half+1
                    endif
                endif
                write(*,*) i,line(i:i),s,e,row
 100        continue
            write(*,*) s,e,row

c           compute column
            column = 0
            s = 0
            e = 7
            do 150 i=8,10
                half =(e-s+1)/2
                if(i.eq.10) then
                    if(line(i:i).eq.'L') then
                        column = s
                    else if (line(i:i).eq.'R') then
                        column = e
                    endif
                else
                    if(line(i:i).eq.'L') then
                        e =s+half-1
                    else if (line(i:i).eq.'R') then
                        s = e-half+1
                    endif
                endif
                write(*,*) i,line(i:i),s,e,column
 150        continue
            write(*,*) s,e,column
            decode = row * 8+column
            write(*,*) decode
            return
        end
