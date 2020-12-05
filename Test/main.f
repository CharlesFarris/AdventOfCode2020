c       tests for the functions in Shared\shared.f
        program Test
            implicit none

c           functions            
            integer strlen,replace,readlines
            
c           locals
            integer length,count
            character source*32,lines(2048)*256

c           test strlen()            
            source = ''
            length = strlen(source)
            if(length.ne.0) then
                write(*,*) 'strlen: Length = 0 failed',length
            endif

            source = ' '
            length = strlen(source)
            if (length.ne.0) then
                write(*,*) 'strlen: Length = 0 failed',length
            endif

            source = 'abc'
            length = strlen(source)
            if (length.ne.3) then
                write(*,*) 'strlen: Length = 3 failed',length
            endif

            source = 'abc def'
            length = strlen(source)
            if (length.ne.7) then
                write(*,*) 'strlen: Length = 7 failed',source,length
            endif

c           test replace()            
            source = 'abcdef'
            count = replace(source,'a','z')
            if(count.ne.1.or.source.ne.'zbcdef') then
                write(*,*) 'replace: failed',count,source
            endif

            source ='aaa aaa'
            count = replace(source,'a','z')
            if(count.ne.6.or.source.ne.'zzz zzz') then
                write(*,*) 'replace: failed',count,source
            endif

c           test readlines()            
            count = readlines('test.txt',lines)
            if(count.ne.6.or.lines(1).ne.'aaa'.or.lines(2).ne.'bbbbb'
     $       .or.lines(3).ne.''.or.lines(4).ne.'ccc 123'
     $       .or.lines(5).ne.'ddd 456 789'
     $       .or.lines(6).ne.'e 1 2 3') then
                write(*,*) 'readlines: failed',count
            endif
        end