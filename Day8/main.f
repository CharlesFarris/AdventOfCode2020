        program Day8
            implicit none

c           functions            
            integer readlines

c           locals            
            character lines(2048)*256,instructions(2048)*3
            integer values(2048),visits(2048)
            integer count,accumulator,index
            integer i

            count = readlines('input.txt',lines)
            write(*,*) count

c           parse instructionts
            do i=1,count
                read(lines(i),'(a3,i6)') instructions(i),values(i)
                visits(i) = 0
                write(*,*) instructions(i),values(i),visits(i)
            enddo

c           execute program
            index = 1
            accumulator = 0
 100        if(visits(index).lt.1) then
c               update visists    
                write(*,*) index,accumulator,visits(index)
                visits(index) = visits(index)+1
                
c               nop instruction                
                if(instructions(index).eq.'nop') then
                    index = index+1

c               acc instruction                    
                else if(instructions(index).eq.'acc') then
                    accumulator = accumulator+values(index)
                    index = index+1

c               jmp instruction                    
                else if(instructions(index).eq.'jmp') then
                    index = index+values(index)
                endif
                goto 100
            endif

            write(*,*) 'Accumulator:',accumulator
        end

