c       test program
        program Test
            integer strlen,length
            character line*32

            test_strlen = 1

            line = ''
            length = strlen(line)
            if(length.ne.0) then
                write(*,*) 'Length = 0 failed',length
            endif

            line = ' '
            length = strlen(line)
            if (length.ne.0) then
                write(*,*) 'Length = 0 failed',length
            endif

            line = 'abc'
            length = strlen(line)
            if (length.ne.3) then
                write(*,*) 'Length = 3 failed',length
            endif

            line = 'abc def'
            length = strlen(line)
            if (length.ne.7) then
                write(*,*) 'Length = 7 failed',length
            endif

        end