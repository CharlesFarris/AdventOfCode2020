        program Day5
            character line*12
            integer max,id,decode

            max = 0

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
                    id = decode(line)
                    if(id.gt.max) then
                        max = id
                    endif
               endif
            enddo
 200        close(1)


            write(*,*) 'Max: ',max
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
