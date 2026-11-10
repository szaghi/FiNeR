program copy_file
!< Cookbook: copy a file_ini, then change the copy only.
use finer
use penf, only : I4P
implicit none
type(file_ini) :: base, variant
integer(I4P)   :: a, b

call base%load(filename='rod.ini')
variant = base
call variant%add(section_name='grid', option_name='cells', val=256_I4P)
call base%get(section_name='grid', option_name='cells', val=a)
call variant%get(section_name='grid', option_name='cells', val=b)
print '(A,I0,A,I0)', 'base: ', a, ', variant: ', b
endprogram copy_file
!run copy_file copy_file
