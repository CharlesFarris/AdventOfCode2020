        program Day11
            implicit none

c           functions
            integer strlen,readlines,adjacent,areequal

c           locals
            character lines(256)*256,c*1
            integer count,rows,columns
            integer grid(128,128),i,j
            integer copy(128,128),state
            integer iteration,occupied

c           read input file         
            rows = readlines('input.txt',lines)

c           create grid
            columns = strlen(lines(1))
            do i=1,rows
                do j=1,columns
                    c = lines(i)(j:j)
                    if (c.eq.'.') then
                        grid(i,j) = 0
                        copy(i,j) = 0
                    else if (c.eq.'L') then
                        grid(i,j) = 1
                        copy(i,j) = 0
                    endif
                enddo
            enddo

c           iterate over rules
            state = 0
            iteration = 1
100         if(state.eq.0) then
                write(*,*) 'Iteration: ',iteration
                call printgrid(grid,rows,columns)
                call update(grid,copy,rows,columns)
                call printgrid(copy,rows,columns)
                state = areequal(grid,copy,rows,columns)
                call swap(grid,copy)
                iteration = iteration+1
                goto 100
            endif

            count = occupied(grid,rows,columns)
            write(*,*) 'Part 1'
            write(*,*) 'Occupied: ',count
        end

c       prints the grid
        subroutine printgrid(grid,rows,columns)
            implicit none

            integer grid(128,128),rows,columns,i,j
            character line*128

            write(*,*) rows,columns
            do j=1,128
                line(j:j) = ' '
            enddo
            do i=1,rows
                do j=1,columns
                    if(grid(i,j).eq.0) then
                        line(j:j) = '.'
                    else if (grid(i,j).eq.1) then
                        line(j:j) = 'L'
                    else if (grid(i,j).eq.2) then
                        line(j:j) = '#'
                    endif
                enddo
                write(*,*) line
            enddo
            write(*,*) ''
        end

c       swaps the grids
        subroutine swap(source,target)
            implicit none

            integer source(128,128),target(128,128)
            integer i,j,tmp

            do i=1,128
                do j=1,128
                    tmp = source(i,j)
                    source(i,j) = target(i,j)
                    target(i,j) = tmp
                enddo
            enddo
        end

c       counts the number of occupied adjacent seats
        integer function adjacent(grid,rows,columns,row,column)
            implicit none

            integer grid(128,128),rows,columns,row,column
            integer i,j
            
            adjacent = 0
            do i=row-1,row+1
                do j=column-1,column+1
                    if (i.ge.1.and.i.le.rows.and.j.ge.1.and.j.le.columns
     $                 ) then
                        if(i.eq.row.and.j.eq.column) then
                        else
                            if(grid(i,j).eq.2) then
                                adjacent = adjacent+1
                            endif
                        endif
                    endif
                enddo
            enddo
        end

c       compares two grids
        integer function areequal(source,target,rows,columns)
            implicit none

            integer source(128,128),target(128,128),rows,columns
            integer i,j

            areequal = 1
            do i=1,rows
                do j=1,columns
                    if(source(i,j).ne.target(i,j)) then
                        areequal = 0
                        goto 100
                    endif
                enddo
            enddo
 100        continue
            return
        end            

c       counts the occupied seats
        integer function occupied(source,rows,columns)
            implicit none

            integer source(128,128),target(128,128),rows,columns
            integer i,j

            occupied = 0
            do i=1,rows
                do j=1,columns
                    if(source(i,j).eq.2) then
                        occupied = occupied+1
                    endif
                enddo
            enddo
 100        continue
            return
        end            


c       applies the seat rules        
        subroutine update(source,target,rows,columns)
            implicit none

            integer source(128,128),target(128,128),rows,columns
            integer i,j,adjacent,count

            do i=1,rows
                do j=1,columns
                    target(i,j) = source(i,j)
                    if(source(i,j).eq.1) then
                        count = adjacent(source,rows,columns,i,j)
                        if(count.eq.0) then
                            target(i,j) = 2
                        endif
                    else if (source(i,j).eq.2) then
                        count = adjacent(source,rows,columns,i,j)
                        if(count.ge.4) then
                            target(i,j) = 1
                        endif
                    endif
                enddo
            enddo
        end
