program read_number
!< Cookbook: read an integer or a real of any kind.
use finer
use penf, only : I1P, I2P, I4P, I8P, R4P, R8P
implicit none
type(file_ini) :: ini
integer(I1P)   :: i1
integer(I8P)   :: i8
real(R4P)      :: r4
real(R8P)      :: r8

call ini%load(filename='heat.ini')
call ini%get(section_name='grid', option_name='cells', val=i1)
call ini%get(section_name='time', option_name='steps', val=i8)
call ini%get(section_name='time', option_name='dt', val=r4)
call ini%get(section_name='time', option_name='dt', val=r8)
print '(A,I0,A,I0)', 'cells (I1P) = ', i1, ', steps (I8P) = ', i8
print '(A,ES14.7,A,ES22.15)', 'dt (R4P) = ', r4, ', dt (R8P) = ', r8
endprogram read_number
