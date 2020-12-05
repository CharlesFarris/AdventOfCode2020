        integer function strlen(st)
            integer i,length
            character st*(*)
            length = len(st)
            do 600 i=1,length
                if (st(i:i).eq.' ') then
                    strlen = i-1
                    goto 650
                endif
 600        continue            
 650    return
        end 