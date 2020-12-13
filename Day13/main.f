        program Day13
            implicit none

c           functions            
            integer replace

c           locals            
            integer io,i,value
            integer timestamp,buses(128),count,f,c
            integer mintime,minbus
            character line*256
            real x,y

            open(1,FILE='input.txt',STATUS='OLD')
            read(1,"(I10)",IOSTAT=io) timestamp
            read(1,"(A256)",IOSTAT=io) line
            close(1)

c           process bus data
            do i=1,len(line)
                if(line(i:i).eq.'x') then
                    line(i:i) = '0'
                endif
            enddo
            line(256:256) = ','

c           parse bus data            
            count = 0
            i = index(line,',')
 100        if(i.gt.0) then
                if(i.gt.0) then
                    read(line(:i-1),*) value
                    if(value.gt.0) then
                        count = count+1
                        buses(count) = value
                    endif
                    if(i.lt.256) then
                        line = line(i+1:)
                        i=index(line,',')
                    else
                        i=0
                    endif
                endif
                goto 100
            endif

            minbus = 0
            mintime = 0
            x = timestamp
            do i=1,count
                y = buses(i)
                c = ceiling(x/y)*buses(i)
                f = floor(x/y)*buses(i)
                write(*,*) timestamp,buses(i),f,c
                if(i.eq.1.or.c.lt.mintime) then
                    minbus = buses(i)
                    mintime = c
                endif
            enddo

            write(*,*) timestamp
            write(*,*) (buses(i),i=1,count)
            write(*,*) minbus,mintime
            write(*,*) 'Part 1'
            write(*,*) 'Product: ', (mintime-timestamp)*minbus
        end