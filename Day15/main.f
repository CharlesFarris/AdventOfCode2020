        program Day11
            implicit none

            integer io,value
            integer starting(16) /16*-1/,count
            integer i,findprevious
            integer turns(2048),current,previous
            integer index

c           read starting numbers       
            open(1,FILE='input.txt',STATUS='OLD')
            read(1,*,IOSTAT=io) (starting,i=1,16)
            write(*,*) 'IOSTAT :',io
            if(io.gt.0) then
                write(*,*) 'Error 1'
                goto 999
            endif

c           count starting numbers            
            count =-1
            do i=1,16
                if(starting(i).eq.-1) then
                    count = i-1
                    goto 100
                endif
            enddo
            write(*,*) 'Error 2'
            goto 999
 100        continue

c           write starting numbers
            write(*,*) 'Starting:'
            write(*,*) (starting(i),i=1,count)

c           initialize turns            
            current = 1
            do i=1,count
                turns(current) = starting(i)
                current = current+1
                write(*,*) i,turns(i)
            enddo

c           iterate
 200        if(current.le.2020) then
                previous = turns(current-1)
c               find previous index                
                index = findprevious(turns,current-1)
                if(index.eq.-1) then
                    turns(current) = 0
                else
                    turns(current) = current-1-index
                endif
                write(*,*) current,turns(current)
                current = current+1

                goto 200
            endif            

 999        continue            
        end

c       finds the previous index of the value in
c       the turns array at the supplied index
        integer function findprevious(turns,index)
            implicit none

            integer turns(*),index,value,i

            value = turns(index)
            findprevious =-1
            do i=index-1,1,-1
                if(turns(i).eq.value) then
                    findprevious = i
                    goto 999
                endif
            enddo
 999        continue        
            return
        end


