        program Day16
            implicit none

c           functions
            integer readlines,getfields,strlen
            integer getticket

c           locals
            character lines(512)*256,fields(256)*64
            character line*256
            integer fieldranges(256,4)
            integer count,i,fieldcount,l,j,k
            integer yourticket(32),yourticketcount
            integer ticket(32),ticketcount
            integer rate,value,valid

c           read input file         
            count = readlines('input.txt',lines)

            do i=1,count
                l =strlen(lines(i))
                write(*,*) lines(i)(:l)
            enddo

c           get fields            
            fieldcount = getfields(lines,count,fields,fieldranges)
            write(*,*) 'Fields:'
            write(*,*) fieldcount
            do i=1,fieldcount
                write(*,*) fields(i),(fieldranges(i,j),j=1,4)
            enddo

c           get your ticket            
            do i=1,count
                if(lines(i).eq.'your ticket:') then
                    yourticketcount = getticket(lines,i+1,yourticket)
                    goto 100
                endif
            enddo
 100        continue
            write(*,*) 'Your Ticket:'
            write(*,*) (yourticket(i),i=1,yourticketcount)

c           process tickets            
            rate = 0
            do i=1,count
                if(lines(i).eq.'nearby tickets:') then
                    do j=i+1,count
                        ticketcount = getticket(lines,j,ticket)
                        write(*,*) (ticket(k),k=1,ticketcount)
                        do k=1,ticketcount
                            value = ticket(k)
                            valid = 0
                            do l=1,fieldcount
                                if(value.ge.fieldranges(l,
     $                             1).and.value.le.fieldranges(l,
     $                             2)) then
                                    valid = 1 
                                else if(value.ge.fieldranges(l,
     $                             3).and.value.le.fieldranges(l,
     $                             4)) then
                                    valid = 1
                                endif
                            enddo
                            if(valid.eq.0) then
                                rate = rate+value
                            endif
                        enddo
                    enddo
                endif
            enddo
            write(*,*) 'Error Rate: ',rate
        end

c       gets the fields and ranges        
        integer function getfields(lines,count,fields,fieldranges)
            implicit none

            integer strlen

            character lines(512)*256,fields(256)*64
            character line*256
            integer fieldranges(256,4)
            integer count,i,j

            getfields = 0
            do i=1,count
                line = lines(i)
                if(line.eq.'') then
                    getfields = i-1
                    goto 999
                endif
                j = index(line,':')
                fields(i) = line(:j-1)

                line = line(j+1:)
                j = index(line,'-')
                read(line(:j-1),*) fieldranges(i,1)

                line = line(j+1:)
                j = index(line,' ')
                read(line(:j-1),*) fieldranges(i,2)

                line = line(j+4:)
                j = index(line,'-')
                read(line(:j-1),*) fieldranges(i,3)

                line = line(j+1:)
                read(line,*) fieldranges(i,4)

            enddo
 999        continue     
            return
        end

c       gets the ticket values
        integer function getticket(lines,k,values)
            implicit none

            integer strlen

            character lines(512)*256,line*256
            integer values(32),i,k,l
            integer value,j

            getticket = 0
            line = lines(k)
            l = strlen(line)
            line(l+1:l+1) = ','
            i = 0
            j = index(line,',')
 100        if (j.ne.0) then
                read(line(:j-1),*) value
                i = i+1
                values(i) = value
                getticket = i
                line=line(j+1:)
                j = index(line,',')
                goto 100
            endif     
 999        continue        
            return
        end