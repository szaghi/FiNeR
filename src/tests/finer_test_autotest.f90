!< FiNeR test: introspection methods.
program finer_test_autotest
!< Covers: has_section, has_option (with section_name output), index,
!<         section(i), get_sections_list, loop, get_items.
use finer, only: file_ini
implicit none

type(file_ini)                :: fini
character(len=:), allocatable :: source, sec_name, pair(:), items(:,:), slist(:)
integer                       :: count, passed, total
logical                       :: pres

passed = 0 ; total = 0
print '(A)', 'finer_test_autotest'

source = '[alpha]'//new_line('A')//      &
         'key1 = val1'//new_line('A')//  &
         'key2 = val2'//new_line('A')//  &
         '[beta]'//new_line('A')//       &
         'key3 = val3'

call fini%load(source=source)
call fini%get_sections_list(slist)

call check('section count == 2',           size(slist) == 2)
call check('section list(1) == alpha',     trim(slist(1)) == 'alpha')
call check('section list(2) == beta',      trim(slist(2)) == 'beta')
call check('has_section: existing',        fini%has_section(section_name='alpha'))
call check('has_section: missing',         .not. fini%has_section(section_name='gamma'))
call check('has_option: existing',         fini%has_option(option_name='key1'))
call check('has_option: missing',          .not. fini%has_option(option_name='missing'))

sec_name = repeat(' ', 32)
pres = fini%has_option(option_name='key3', section_name=sec_name)
call check('has_option: section_name out', pres .and. trim(sec_name) == 'beta')

call check('index section: first == 1',   fini%index(section_name='alpha') == 1)
call check('index section: missing == 0', fini%index(section_name='gamma') == 0)
call check('index option: first == 1',    fini%index(section_name='alpha', option_name='key1') == 1)
call check('index option: missing == 0',  fini%index(section_name='alpha', option_name='x') == 0)

count = 0
do while (fini%loop(section_name='alpha', option_pairs=pair))
  count = count + 1
end do
call check('loop over alpha: 2 pairs',    count == 2)

call fini%get_items(items)
call check('get_items: 3 total options',  size(items, dim=1) == 3)

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

end program finer_test_autotest
