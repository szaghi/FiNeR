!< FiNeR test: basic parse.
program finer_test_parse
!< FiNeR test: basic parse.
!<
!<### Usage
!<```bash
!< ./finer_test_parse
!<```
use finer, only :  file_ini
use penf, only : I4P, R4P, str

implicit none
type(file_ini)                :: fini           !< INI file handler.
character(len=:), allocatable :: source         !< Testing string.
character(len=:), allocatable :: string         !< String option.
real(R4P), allocatable        :: array(:)       !< Array option.
integer(I4P)                  :: error          !< Error code.
logical                       :: test_passed(2) !< List of passed tests.

source = '[section-1]'//new_line('a')//                                &
         '; option-1 = two ; commented line'//new_line('a')//          &
         '# option-1 = three ; commented line'//new_line('a')//        &
         'option-1 = one ; this is an inline comment'//new_line('a')// &
         '! option-1 = four ; commented line'//new_line('a')//         &
         'option-2 = 2.'//new_line('a')//                              &
         '           3. ; continued line'//new_line('a')//             &
         'option-3 = bar'//new_line('a')//                             &
         '[section-2]'//new_line('a')//                                &
         'option-1 = foo'//new_line('a')//                             &
         '[section-3]'//new_line('a')//                                &
         'option-1 = foo'//new_line('a')//                             &
         'option-2 = bar'//new_line('a')

print "(A)", 'Source input'//new_line('a')//new_line('a')//source//new_line('a')//new_line('a')//'Parse results'//new_line('a')

call fini%load(source=source)

string = repeat(' ', 999)
call fini%get(section_name='section-1', option_name='option-1', val=string, error=error)
test_passed(1) = ((error==0).and.(trim(string)=='one'))
print "(A,L1)", '[section-1].(option-1) = "'//trim(string)//'", is correct? ', test_passed(1)

allocate(array(1:fini%count_values(section_name='section-1', option_name='option-2')))
call fini%get(section_name='section-1', option_name='option-2', val=array, error=error)
test_passed(2) = ((error==0).and.(array(1)==2._R4P).and.(array(2)==3._R4P))
print "(A,L1)", '[section-1].(option-2) = "'//trim(str(array))//'", is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
endprogram finer_test_parse
