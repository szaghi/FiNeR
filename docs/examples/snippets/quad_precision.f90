program quad_precision
!< Cookbook: quadruple precision reals.
use finer
use penf, only : R16P
implicit none
type(file_ini) :: ini
real(R16P)     :: third, back

third = 1.0_R16P / 3.0_R16P
call ini%add(section_name='q', option_name='third', val=third)
call ini%print(unit=6)
call ini%get(section_name='q', option_name='third', val=back)
print '(A,L1)', 'reads back exactly: ', back == third
endprogram quad_precision
