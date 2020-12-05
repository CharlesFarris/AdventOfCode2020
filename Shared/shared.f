c       returns the actual length of the string
        integer function strlen(value)
            implicit none
            integer i,length
            character value*(*)
            length = len(value)
            do 600 i=length,1,-1
                if (value(i:i).ne.' ') then
                    strlen = i
                    goto 650
                endif
 600        continue            
 650        return
        end 

c       replaces all instances of the character c
c       with the character r
        integer function replace(value,c,r)
            implicit none
            integer i,length,count
            character value*(*),c*1,r*1
            replace = 0
            length = len(value)
            count = 0
            do 700 i=1,length
                if(value(i:i).eq.c) then
                    count = count + 1
                    value(i:i) = r
                endif
 700        continue
            replace = count
            return
        end


            
