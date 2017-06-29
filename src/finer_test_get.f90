!< FiNeR test: basic get.
program finer_test_get
!< FiNeR test: basic get.
!<
!<### Usage
!<```bash
!< ./finer_test_get
!<```
use, intrinsic :: iso_fortran_env, only : output_unit
use finer, only :  file_ini
use penf, only : I4P, R4P, str

implicit none
type(file_ini)                :: fini           !< INI file handler.
character(len=:), allocatable :: source         !< Testing string.
character(len=:), allocatable :: string         !< String option.
integer(I4P)                  :: error          !< Error code.
logical                       :: test_passed(2) !< List of passed tests.

source = '[section-1]'//new_line('a')//                                &
         'option-1 = one ; this is an inline comment'//new_line('a')// &
         'option-2 = bar'//new_line('a')

print "(A)", 'Source input'//new_line('a')//new_line('a')//source//new_line('a')//new_line('a')//'Parse results'//new_line('a')

call fini%load(source=source, error=error)

string = repeat(' ', 999)
call fini%get(section_name='section-1', option_name='option-1', val=string, error=error)
test_passed(1) = ((error==0).and.(trim(string)=='one'))
print "(A,L1)", '[section-1].(option-1) = "'//trim(string)//'", is correct? ', test_passed(1)

call fini%get(section_name='section-1', option_name='option-3', val=string, error=error)
test_passed(2) = (error/=0)
print "(A,L1)", '[section-1].(option-3) dos not exist, is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram finer_test_get
