open(newunit=unit, file='dos.ini', access='stream', form='unformatted', status='replace')
write(unit) '[grid]'//achar(13)//achar(10)//'cells = 64'//achar(13)//achar(10)
close(unit)
call dos%load(filename='dos.ini')
call dos%get(section_name='grid', option_name='cells', val=cells)
print '(A,I0)', 'cells from a CRLF file: ', cells
