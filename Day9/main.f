        program Day9
            implicit none

c           locals            
            double precision values(1024)
            double precision valid,sum,value
            double precision validate,weakness

            integer i,j,io,count,preamble


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
                values(count) = value
                endif
            enddo
            close(1)

c           preamble for `input.txt`            
            preamble = 25

c           preamble for `test.txt`
c           preamble = 5            

c           iterate over the values
            do i=preamble+1,count

c               check invalid
                valid = validate(values,values(i),i-preamble, i-1)
                if (valid.eq.0) then
                    write(*,*) 'Part 1'
                    write(*,*) 'Invalid: ',values(i)

                    write(*,*) 'Part 2'
                    sum = weakness(values,count,values(i))
                    write(*,*) 'Weakness: ',sum
                endif
            enddo                
        end

c       validates the value by finding a matching sum in the 
c       preamble values
        double precision function validate(values,value,sindex,eindex)
            implicit none

            double precision values(*),value,sum
            integer sindex,eindex,i,j

            validate = 0
            do i=sindex,eindex-1
                do j=i+1,eindex
                    sum=values(i) + values(j)
c                    write(*,*) i,j,values(i),values(j),sum
                    if(sum.eq.value) then
                        validate = 1
                        goto 200
                    endif
                enddo
            enddo
 200        continue
c           write(*,*) ''
            return
        end

c       finds the encryption weakness by returning the sum
c       of the minimum and maximum values in the preamble values
        double precision function weakness(values,count,value)
            implicit none

            double precision values(*),value,sum
            double precision minimum,maximum
            integer count,i,j

c           iterate over values
            weakness = -1
            do i=1,count-1
c               initial minimum, maximum, and sum
                minimum = values(i)
                maximum = values(j)
                sum = values(i)

c               build continguous range
                do j=i+1,count

c                   update minimum
                    if(values(j).lt.minimum) then
                        minimum = values(j)
                    endif

c                   update maximum
                    if(values(j).gt.maximum) then
                        maximum = values(j)
                    endif

c                   update sum                    
                    sum = sum+values(j)

c                   check sum                    
                    if(sum.eq.value) then                        
                        weakness = minimum + maximum
                        goto 300
                    else if(sum.gt.value) then
                        goto 325
                    endif
                enddo
 325            continue
            enddo
 300        continue            
            return
        end
