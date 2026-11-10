program heat
!< Tutorial, chapter 8: robust input.
use finer
use penf, only : I1P, I4P, R8P
implicit none
type :: probe
  real(R8P) :: x
endtype probe
type(file_ini)    :: case_file, empty
integer(I4P)      :: error, iostat
integer(I4P)      :: cells
integer(I1P)      :: small
real(R8P)         :: origin(2)
type(probe)       :: p
character(len=64) :: iomsg

call case_file%load(filename='hat.ini', error=error)
print '(A,L1)', 'load of a missing file: error /= 0 is ', error /= 0
call case_file%load(filename='heat.ini')
cells = -1
call case_file%get(section_name='grids', option_name='cells', val=cells, error=error)
print '(A,I0,A,I0)', 'missing section:     error ', error, ', cells kept: ', cells
call case_file%get(section_name='material', option_name='name', val=cells, error=error)
print '(A,I0,A,I0)', 'not an integer:      error ', error, ', cells kept: ', cells
small = -1_I1P
call case_file%get(section_name='material', option_name='density', val=small, error=error)
print '(A,I0,A,I0)', 'out of range (I1P):  error ', error, ', small kept: ', small
origin = -1._R8P
call case_file%get(section_name='grid', option_name='origin', val=origin, error=error)
print '(A,I0,A,2F5.1)', 'array too small:     error ', error, ', origin kept:', origin
call case_file%get(section_name='grid', option_name='length', val=p, error=error)
print '(A,I0)', 'unsupported type:    error ', error
call case_file%get(section_name='material', option_name='name', val=cells, error=error)
if (error == err_option_vals) print '(A)', 'error is err_option_vals'
call empty%save(iostat=iostat, iomsg=iomsg)
print '(A,I0,A)', 'save without a file name: iostat ', iostat, ', "'//trim(iomsg)//'"'
call case_file%save(filename='no-such-dir/heat.ini', iostat=iostat)
print '(A,L1)', 'save into a missing directory: iostat /= 0 is ', iostat /= 0
endprogram heat
