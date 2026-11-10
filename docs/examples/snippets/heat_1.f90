program heat
!< Tutorial, chapter 1: a first file.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini)    :: case_file
integer(I4P)      :: cells
real(R8P)         :: length
character(len=32) :: material

call case_file%load(source='[grid]'//new_line('a')// &
                           'cells  = 64'//new_line('a')// &
                           'length = 0.5'//new_line('a')// &
                           '[material]'//new_line('a')// &
                           'name = copper')
call case_file%get(section_name='grid', option_name='cells', val=cells)
call case_file%get(section_name='grid', option_name='length', val=length)
call case_file%get(section_name='material', option_name='name', val=material)
print '(A,I0,A,F5.2,A)', 'a rod of ', cells, ' cells, ', length, ' m long, made of '//trim(material)
call case_file%print(unit=6)
endprogram heat
