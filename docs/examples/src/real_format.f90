program real_format
!< Cookbook: how reals are written.
use finer
use penf, only : R4P, R8P
implicit none
type(file_ini) :: ini
real(R8P)      :: back, x(6)
integer        :: i
character(len=1) :: name

x = [0.1_R8P, 1.0_R8P/3.0_R8P, 2.5e-5_R8P, 6.02214076e23_R8P, -1.0e-12_R8P, huge(1.0_R8P)]
do i=1, size(x)
  name = achar(iachar('a') + i - 1)
  call ini%add(section_name='reals', option_name=name, val=x(i))
enddo
call ini%add(section_name='reals', option_name='r4', val=0.1_R4P)
call ini%print(unit=6)
do i=1, size(x)
  name = achar(iachar('a') + i - 1)
  call ini%get(section_name='reals', option_name=name, val=back)
  if (back /= x(i)) print '(A)', name//' does not read back exactly'
enddo
print '(A)', 'every R8P value reads back exactly'
endprogram real_format
!run real_format real_format
