        program Day11
            implicit none

            integer count,io
            character instructions(1024)*1,instruction*1
            integer values(1024),value,i
            integer x,y,heading

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

c           execute instructions            
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

        end

