program update_value
!< Cookbook: change a value and save the file.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini) :: ini

call ini%load(filename='rod.ini')
call ini%add(section_name='time', option_name='dt', val=5.0e-5_R8P)
call ini%add(section_name='time', option_name='steps', val=4000_I4P)
call ini%save(filename='rod-fine.ini')
endprogram update_value
!run update_value update_value
!run update_value_out cat rod-fine.ini
