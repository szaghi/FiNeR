!< FiNeR test: update existing option.

program finer_test_update_option
!< FiNeR test: update existing option.
!<
!<### Usage
!<```bash
!< ./finer_test_update_option
!<```
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
use finer, only :  file_ini
use penf, only : I4P, R4P, str

implicit none
type(file_ini)                :: fini           !< INI file handler.
character(len=:), allocatable :: source         !< Testing string.
character(len=:), allocatable :: string         !< String option.
integer(I4P)                  :: error          !< Error code.
logical                       :: test_passed(3) !< List of passed tests.

call fini%load(filename='./src/tests/update_option.ini')
call fini%print(unit=stdout)

string = repeat(' ', 999)
call fini%get(section_name='sec-foo', option_name='bar2', val=string, error=error)
test_passed(1) = ((error==0).and.(trim(string)=='1.102325'))

call fini%add(section_name='sec-foo', option_name='bar2',val='0.99')
call fini%print(unit=stdout)

call fini%get(section_name='sec-foo', option_name='bar2', val=string, error=error)
test_passed(2) = ((error==0).and.(trim(string)=='0.99'))

call fini%get(section_name='sec-foo', option_name='bar', val=string, error=error)
test_passed(3) = ((error==0).and.(trim(string)=='-0.583000E+02'))

print "(A,3L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram finer_test_update_option
