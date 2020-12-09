        program Day9
            implicit none

c           locals            
            double precision values(1024)
            double precision valid,sum,value
            double precision validate

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

c           iterate over the values
            preamble = 25
            do i=preamble+1,count

c               check invalid
                valid = validate(values,values(i),i-preamble, i-1)
                if (valid.eq.0) then
                    write(*,*) 'Invalid: ',values(i)
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
