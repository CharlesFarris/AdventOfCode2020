        program Day7
            implicit none
         
c           shared functions
            integer readlines,strlen
                     
c           locals         
            character lines(2048)*256,bags(2048)*64
            character line*256,bag*64
            integer rules(4096,3),rulecount
            integer rows,count,findbag,childbag
            integer i,j,k,l,findparentbags
            integer stack(1024),height
            integer multiplier,bagcount
            
c           read input file
            rows = readlines('input.txt',lines)
            do 125 i=1,rows
                j = index(lines(i),'contain')
                bags(i) = lines(i)(1:j-3)
                write(*,*) i,bags(i)
 125        continue

            rulecount = 0
            do 150 i=1,rows
                j = index(lines(i),'contain')
                line = lines(i)(j+8:)
                do 165 l=1,10
                    write(*,*) line
                    j = index(line,',')
                    if (j.gt.1) then
                        read(line(1:2),*) count
                        if(line(j-1:j-1).eq.'s') then
                            bag = line(3:j-2)
                        else
                            bag = line(3:j-1)
                        endif
                        rulecount = rulecount+1
                        rules(rulecount,1) = i
                        rules(rulecount,2) = findbag(bag,bags,rows)
                        rules(rulecount,3) = count
                        write(*,*) count,bag
                        line = line(j+2:)
                    else 
                        if(line.ne.'no other bags.') then
                            j = index(line,'.')
                            read(line(1:2),*) count
                            if(line(j-1:j-1).eq.'s') then
                                bag = line(3:j-2)
                            else
                                bag = line(3:j-1)
                            endif
                            rulecount = rulecount+1
                            rules(rulecount,1) = i
                            rules(rulecount,2) = findbag(bag,bags,rows)
                            rules(rulecount,3) = count
                            write(*,*) count,bag
                        endif
                        goto 175
                    endif
 165            continue
 175            write(*,*) '-------------'
 150        continue

            do 190 i=1,rulecount
                write(*,*) rules(i,1),rules(i,2),rules(i,3)
 190        continue

            height = 1
            bag = 'shiny gold bag'
            bagcount = -1
            stack(height) = findbag(bag,bags,rows)
            do 195 i = 1,32768
                if(height.eq.0) then
                    goto 196
                endif
                childbag = stack(height)
                bagcount = bagcount + 1
                height = height-1
                do 197 j=1,rulecount
                    if(rules(j,1).eq.childbag) then
                        do 198 k=1,rules(j,3)
                            height = height+1
                            stack(height) = rules(j,2)
 198                    continue
                    endif
 197            continue           
 195        continue
 196        write(*,*) 'bags: ',bagcount
        end

        integer function findparentbags(id,parentbags,parentbagscount)
            implicit none
            integer id,parentbags(4096),parentbagscount,i
            findparentbags = 0
            do 201 i=1,parentbagscount
                if(parentbags(i).eq.id) then
                    findparentbags = 1
                endif
 201        continue
            return
        end

        integer function findbag(bag,bags,count)
            implicit none
            character bag*64,bags(2048)*64
            integer count,i

            findbag =-1
            do 200 i=1,count
                if(bags(i).eq.bag) then
                    findbag = i
                    goto 210
                endif
 200        continue
 210        return
        end

