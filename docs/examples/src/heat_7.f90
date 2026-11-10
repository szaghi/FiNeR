program heat
!< Tutorial, chapter 7: other INI dialects.
use finer
use penf, only : I4P
implicit none
type(file_ini)                :: legacy, dos
integer(I4P)                  :: levels, cells, unit, s
character(len=:), allocatable :: author

!region separator
call legacy%load(filename='legacy.cfg', separator=':')
call legacy%get(section_name='mesh.refinement', option_name='levels', val=levels)
call legacy%get_string(section_name='', option_name='author', val=author)
print '(A,I0,A)', 'levels = ', levels, ', author = '//author
!endregion separator
!region sections
do s=1, legacy%sections_number()
  print '(A)', 'section "'//legacy%section(s)//'"'
enddo
!endregion sections
!region comments
call legacy%print(unit=6, retain_comments=.true.)
!endregion comments
!region crlf
open(newunit=unit, file='dos.ini', access='stream', form='unformatted', status='replace')
write(unit) '[grid]'//achar(13)//achar(10)//'cells = 64'//achar(13)//achar(10)
close(unit)
call dos%load(filename='dos.ini')
call dos%get(section_name='grid', option_name='cells', val=cells)
print '(A,I0)', 'cells from a CRLF file: ', cells
!endregion crlf
endprogram heat
!as heat
!run heat_7 heat
