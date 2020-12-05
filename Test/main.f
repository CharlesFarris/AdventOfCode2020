c       tests for the functions in Shared\shared.f
        program Test
            implicit none

c           functions            
            integer strlen,replace
            
c           locals
            integer length,count
            character source*32

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
        end