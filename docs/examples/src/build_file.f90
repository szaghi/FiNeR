program build_file
!< Cookbook: build a file in code and save it.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini) :: ini
integer(I4P)   :: iostat

call ini%add(section_name='grid', option_name='cells', val=128_I4P)
call ini%add(section_name='grid', option_name='length', val=1.0_R8P)
call ini%add(section_name='grid', option_name='periodic', val=.false.)
call ini%add(section_name='output', option_name='format', val='vtk')
call ini%save(filename='built.ini', iostat=iostat)
if (iostat /= 0) stop 'cannot save built.ini'
endprogram build_file
!run build_file build_file
!run build_file_out cat built.ini
