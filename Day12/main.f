        program Day11
            implicit none

            integer count,io
            character instructions(1024)*1,instruction*1
            integer values(1024),value,i,j
            integer x,y,heading
            integer wx,wy,c,s,tx,ty

c           read instructions            
            count = 0
            open(1,FILE='input.txt',STATUS='OLD')
            do
                read(1,"(A1,I4)",IOSTAT=io) instruction,value
                if (io.gt.0) then
                    write(*,*) 'Error'
                    exit
                else if(io.lt.0) then
                    exit
                else
                    count = count+1
                    instructions(count) = instruction
                    values(count) = value
                endif
            enddo

c           execute instructions (part 1)
            x = 0
            y = 0
            heading = 90
            do i=1,count
                write(*,*) i,instructions(i),values(i)
                write(*,*) x,y,heading
                if (instructions(i).eq.'R') then
                    heading = heading+values(i)
                    if(heading.ge.360) then
                        heading = heading-360
                    endif
                else if(instructions(i).eq.'L') then
                    heading = heading-values(i)
                    if(heading.lt.0) then
                        heading = heading+360
                    endif
                else if(instructions(i).eq.'N') then
                    y = y+values(i)
                else if (instructions(i).eq.'E') then
                    x = x+values(i)
                else if(instructions(i).eq.'S') then
                    y = y-values(i)
                else if(instructions(i).eq.'W') then
                    x = x-values(i)
                else if(instructions(i).eq.'F') then
                    if(heading.eq.0) then
                        y = y+values(i)
                    else if(heading.eq.90) then
                        x = x+values(i)
                    else if(heading.eq.180) then
                        y = y-values(i)
                    else if(heading.eq.270) then
                        x = x-values(i)
                    endif
                endif
                write(*,*) x,y,heading
            enddo

            write(*,*) 'Part 1:'
            write(*,*) 'Manhattan Distance: ',abs(x)+abs(y)
            write(*,*) ''


c           execute instructions (part 2)
            x = 0
            y = 0
            wx = 10
            wy = 1
            do i=1,count
                write(*,*) i,instructions(i),values(i)
                write(*,*) x,y,wx,wy
                if (instructions(i).eq.'R') then
                    if(values(i).eq.90) then
                        c = 0
                        s = -1
                    else if(values(i).eq.180) then
                        c = -1
                        s = 0
                    else if(values(i).eq.270) then
                        c = 0
                        s = 1
                    endif
                    tx = c*wx - s*wy
                    ty = s*wx + c*wy
                    wx = tx
                    wy = ty
                else if(instructions(i).eq.'L') then
                    if(values(i).eq.90) then
                        c = 0
                        s = 1
                    else if(values(i).eq.180) then
                        c = -1
                        s = 0
                    else if(values(i).eq.270) then
                        c = 0
                        s = -1
                    endif
                    tx = c*wx - s*wy
                    ty = s*wx + c*wy
                    wx = tx
                    wy = ty
                else if(instructions(i).eq.'N') then
                    wy = wy+values(i)
                else if (instructions(i).eq.'E') then
                    wx = wx+values(i)
                else if(instructions(i).eq.'S') then
                    wy = wy-values(i)
                else if(instructions(i).eq.'W') then
                    wx = wx-values(i)
                else if(instructions(i).eq.'F') then
                    x = x+wx*values(i)
                    y = y+wy*values(i)
                endif
                write(*,*) x,y,wx,wy
            enddo

            write(*,*) 'Part 2:'
            write(*,*) 'Manhattan Distance: ',abs(x)+abs(y)
            write(*,*) ''

        end

