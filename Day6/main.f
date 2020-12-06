        program Day6
            character line*32,c*1
            integer questions(1024,26),groups,length,i,j,index,total
            integer sums(1024),people(1024)

            groups = 1
            people(1) = 0

c           read input file         
            open(1,FILE='input.txt',STATUS='OLD')
            do
               read(1,"(A32)",IOSTAT=io) line
               if (io.gt.0) then
                  write(*,*) 'Error'
                  exit
               else if(io.lt.0) then
                  exit
               else
                    if(line.eq.'') then
                        groups = groups+1
                        people(groups) = 0
                        write(*,*) line
                    else
                        people(groups) = people(groups)+1
                        do 100 i=1,32
                            c = line(i:i)
                            if(c.ne.' ') then
                                index = ichar(c)-96
                                questions(groups,
     $                             index) = questions(groups,index)+1
                                write(*,*) c,index,questions(groups,
     $                             index)
                            endif
 100                    continue
                    endif
               endif
            enddo
 200        close(1)

c           sum groups
            total = 0
            do 300 i=1,groups
                sums(i) = 0
                do 350 j=1,26
                    if(questions(i,j).eq.people(i)) then
                        sums(i) = sums(i)+1
                    endif
 350            continue
                write(*,*) 'Group: ',i,sums(i),people(i)
                total = total+sums(i)
 300        continue
            write(*,*) 'Total: ',total
        end
