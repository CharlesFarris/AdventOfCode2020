        program Day10
            implicit none

c           locals            
            integer adapters(1024),count,value
            integer joltage,differences(1024),match,matchindex
            integer difference,chain(1024),i,j,io
            integer counts(3)

c           read input file         
            count = 0
            open(1,FILE='input.txt',STATUS='OLD')
            do
                read(1,*,IOSTAT=io) value
                if (io.gt.0) then
                    write(*,*) 'Error'
                exit
            else if(io.lt.0) then
                exit
            else
                count = count+1
                adapters(count) = value
                endif
            enddo
            close(1)

c           build chain
            joltage = 0
            do i=1,count
                differences(i) = 0

c               find minimum valid adapter
                match =-1
                do j=1,count

c                   check adapater exists                    
                    if(adapters(j).ne.0) then

c                       check difference                        
                        difference = adapters(j)-joltage
                        write(*,*) j,adapters(j),difference
                        if (difference.ge.1.and.difference.le.3) then
                            if (match.eq.-
     $                         1.or.adapters(j).lt.match) then
c                               match found
                                match = adapters(j)
                                matchindex = j
                            endif
                        endif
                    endif
                enddo

c               update adapter chain                
                chain(i) = match
                differences(i) = match-joltage
                adapters(matchindex) = 0
                write(*,*) 'Match',match,matchindex,match-joltage
                write(*,*) ''
                joltage = chain(i)
            enddo

c           sum counts
            counts(1) = 0
            counts(2) = 0
            counts(3) = 1
            do i=1,count
                counts(differences(i)) = counts(differences(i))+1
                write(*,*) i,chain(i),differences(i)
            enddo
            write(*,*) 'Counts: ',counts
            write(*,*) 'Product: ',counts(1)*counts(3)


        end
