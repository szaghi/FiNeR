!< FiNeR test: add / del operations.
program finer_test_update_option
!< Covers: update existing option value, add option to existing section,
!<         add section, del option, del section, section count tracking.
use finer, only: file_ini
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, val, slist(:)
integer                       :: error, passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_update_option'

source = '[sec-a]'//new_line('A')//       &
         'key = initial'//new_line('A')// &
         '[sec-b]'//new_line('A')//       &
         'val = 42'

call fini%load(source=source)

val = repeat(' ', 64)
call fini%get(section_name='sec-a', option_name='key', val=val, error=error)
call check('initial value',                  trim(val) == 'initial')

call fini%add(section_name='sec-a', option_name='key', val='updated')
val = repeat(' ', 64)
call fini%get(section_name='sec-a', option_name='key', val=val, error=error)
call check('updated value',                  trim(val) == 'updated')

call fini%add(section_name='sec-a', option_name='new-key', val='new-val')
call check('new option exists',              fini%has_option(option_name='new-key'))

call fini%add(section_name='sec-c')
call fini%get_sections_list(slist)
call check('3 sections after add',           size(slist) == 3)

call fini%del(section_name='sec-b', option_name='val')
call check('del option: no longer present',  .not. fini%has_option(option_name='val'))

call fini%del(section_name='sec-b')
call check('del section: no longer present', .not. fini%has_section(section_name='sec-b'))
call fini%get_sections_list(slist)
call check('2 sections after del',           size(slist) == 2)

call summary
contains

  subroutine check(label, ok)
  character(*), intent(in) :: label
  logical,      intent(in) :: ok
  total = total + 1
  if (ok) passed = passed + 1
  if (ok) then
    write(*, '("  [PASS] ", A)') label
  else
    write(*, '("  [FAIL] ", A)') label
  end if
  end subroutine check

  subroutine summary
  write(*, '(/, "--- ", I0, "/", I0, " passed")') passed, total
  write(*, '(A, L1)') 'Are all tests passed? ', passed == total
  if (passed /= total) stop 1
  end subroutine summary

end program finer_test_update_option
