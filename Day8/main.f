        program Day8
            implicit none

c           functions            
            integer readlines

c           locals            
            character lines(2048)*256,instructions(2048)*3
            character copy(2048)*3
            integer values(2048),visits(2048)
            integer count,accumulator,index
            integer i,j,k

            count = readlines('input.txt',lines)
            write(*,*) count

c           parse instructionts
            do i=1,count
                read(lines(i),'(a3,i6)') instructions(i),values(i)
                visits(i) = 0
                write(*,*) instructions(i),values(i),visits(i)
            enddo

c           part 1            
c           execute program
            index = 1
            accumulator = 0
 100        if(index.le.count.and.visits(index).lt.1) then
c               update visists    
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
            write(*,*) 'Part 1'
            write(*,*) 'Accumulator:',accumulator
            write(*,*) ''

c           part 2
            do j=1,count
                if(instructions(j).eq.'nop'.or.instructions(j).eq.'jmp')
     $              then
                    write(*,*) 'Flip ',j

c                   copy instructions                    
                    do k=1,count
                        copy(k) = instructions(k)
                        visits(k) = 0
                    enddo

c                   flip operations                    
                    if(copy(j).eq.'nop') then
                        copy(j) = 'jmp'
                    else if(copy(j).eq.'jmp') then
                        copy(j) = 'nop'
                    endif

c                   execute program
                    index = 1
                    accumulator = 0
 300                if(index.le.count.and.visits(index).lt.1) then
c                       update visists    
                        visits(index) = visits(index)+1
                
c                       nop instruction                
                        if(copy(index).eq.'nop') then
                            index = index+1

c                       acc instruction                    
                        else if(copy(index).eq.'acc') then
                            accumulator = accumulator+values(index)
                            index = index+1

c                      jmp instruction                    
                       else if(copy(index).eq.'jmp') then
                            index = index+values(index)
                        endif
                        goto 300
                    endif
                    if(index.gt.count) then
                        write(*,*) 'Part 2'
                        write(*,*) 'Accumulator:',accumulator
                        goto 200
                    endif
                endif
            enddo
 200        continue            


        end

