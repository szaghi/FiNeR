!< FiNeR test: global (unnamed) section.
program finer_test_global_section
!< Covers: options defined before the first section (with comments and multi-line continuation), file without sections,
!<         global section handling by get/add/del/inquiries, save without header and reload, explicit empty header
!<         (that is the global section too).
use finer, only: file_ini
use penf,  only: I4P
implicit none

character(*), parameter       :: filename = 'finer_test_global_section.ini'
type(file_ini)                :: fini
character(len=:), allocatable :: source, sval, slist(:)
character(64)                 :: sec_name, line
integer(I4P)                  :: ival, error
integer                       :: unit, passed, total
logical                       :: pres

passed = 0 ; total = 0
print '(A)', 'finer_test_global_section'

! options before the first section
source = '; leading comment'//new_line('A')//  &
         'foo = bar'//new_line('A')//          &
         'num = 3'//new_line('A')//            &
         '# another comment'//new_line('A')//  &
         'arr = 1'//new_line('A')//            &
         '      2'//new_line('A')//            &
         '[first]'//new_line('A')//            &
         'baz = 1'
call fini%load(source=source, error=error)
call check('load: no error',                     error == 0)
call fini%get_sections_list(slist)
call check('2 sections',                         size(slist) == 2)
if (size(slist) == 2) then
  call check('global section is the first',      len_trim(slist(1)) == 0)
  call check('named section is the second',      trim(slist(2)) == 'first')
endif
call check('has_section global',                 fini%has_section(section_name=''))
call check('index of global section == 1',       fini%index(section_name='') == 1)
call fini%get_string(section_name='', option_name='foo', val=sval, error=error)
call check('global string option',               error == 0 .and. sval == 'bar')
call fini%get(section_name='', option_name='num', val=ival, error=error)
call check('global integer option',              error == 0 .and. ival == 3_I4P)
call check('global multi-line option',           fini%count_values(section_name='', option_name='arr') == 2)
call fini%get(section_name='first', option_name='baz', val=ival, error=error)
call check('named section option',               error == 0 .and. ival == 1_I4P)
call fini%get(section_name='first', option_name='foo', val=ival, error=error)
call check('global option not in named section', error /= 0)
sec_name = 'unset'
pres = fini%has_option(option_name='num', section_name=sec_name)
call check('has_option: global section name',    pres .and. len_trim(sec_name) == 0)

! save without header and reload
call fini%save(filename=filename)
open(newunit=unit, file=filename, action='read')
read(unit, '(A)') line
close(unit)
call check('save: no header for global section', trim(line) == 'foo = bar')
call fini%free
call fini%load(filename=filename, error=error)
call fini%get_sections_list(slist)
call check('reload: no error',                   error == 0)
call check('reload: 2 sections',                 size(slist) == 2)
call fini%get_string(section_name='', option_name='foo', val=sval, error=error)
call check('reload: global option',              error == 0 .and. sval == 'bar')
call fini%get(section_name='first', option_name='baz', val=ival, error=error)
call check('reload: named section option',       error == 0 .and. ival == 1_I4P)

! file without sections
call fini%free
call fini%load(source='alpha = 1'//new_line('A')//'beta = 2', error=error)
call check('no sections: no error',              error == 0)
call fini%get(section_name='', option_name='beta', val=ival, error=error)
call check('no sections: option',                error == 0 .and. ival == 2_I4P)

! lines that are not options before the first section do not create the global section
call fini%free
call fini%load(source='garbage'//new_line('A')//'[only]'//new_line('A')//'key = 1', error=error)
call fini%get_sections_list(slist)
call check('garbage before section: 1 section',  size(slist) == 1)
call check('garbage before section: no global',  .not. fini%has_section(section_name=''))
call fini%get(section_name='only', option_name='key', val=ival, error=error)
call check('garbage before section: option',     error == 0 .and. ival == 1_I4P)

! global section added to a file having only named sections
call fini%add(section_name='', option_name='top', val='level', error=error)
call check('add global option: no error',        error == 0)
call fini%get_sections_list(slist)
call check('add global option: 2 sections',      size(slist) == 2)
call check('add global option: global is first', fini%index(section_name='') == 1 .and. fini%index(section_name='only') == 2)
call fini%save(filename=filename)
open(newunit=unit, file=filename, action='read')
read(unit, '(A)') line
close(unit)
call check('add global option: saved first',     trim(line) == 'top = level')
call fini%del(section_name='', option_name='top')
call check('del global option',                  .not. fini%has_option(option_name='top'))
call fini%del(section_name='')
call check('del global section',                 .not. fini%has_section(section_name=''))
call check('del global section: named kept',     fini%has_section(section_name='only'))

! a section with an explicit empty header is the global section
call fini%free
call fini%load(source='[a]'//new_line('A')//'x = 1'//new_line('A')//'[]'//new_line('A')//'y = 2', error=error)
call fini%get(section_name='', option_name='y', val=ival, error=error)
call check('explicit empty header: option',      error == 0 .and. ival == 2_I4P)
call fini%save(filename=filename)
call fini%free
call fini%load(filename=filename, error=error)
call fini%get(section_name='', option_name='y', val=ival, error=error)
call check('explicit empty header: reload',      error == 0 .and. ival == 2_I4P)
call fini%get(section_name='a', option_name='x', val=ival, error=error)
call check('explicit empty header: a kept',      error == 0 .and. ival == 1_I4P)
call check('explicit empty header: y not in a',  fini%index(section_name='a', option_name='y') == 0)

call fini%get_sections_list(slist)
call check('explicit empty header: is first',    size(slist) == 2 .and. fini%index(section_name='') == 1)

! options before the first section and under explicit empty headers belong to the same (global) section
call fini%free
source = 'g = 1'//new_line('A')//  &
         '[]'//new_line('A')//     &
         'h = 2'//new_line('A')//  &
         '[a]'//new_line('A')//    &
         'x = 3'//new_line('A')//  &
         '[ ]'//new_line('A')//    &
         'k = 4'
call fini%load(source=source, error=error)
call fini%get_sections_list(slist)
call check('merged global: 2 sections',          size(slist) == 2)
call fini%get(section_name='', option_name='g', val=ival, error=error)
call check('merged global: leading option',      error == 0 .and. ival == 1_I4P)
call fini%get(section_name='', option_name='h', val=ival, error=error)
call check('merged global: [] option',           error == 0 .and. ival == 2_I4P)
call fini%get(section_name='', option_name='k', val=ival, error=error)
call check('merged global: [ ] option',          error == 0 .and. ival == 4_I4P)
call fini%get(section_name='a', option_name='x', val=ival, error=error)
call check('merged global: named section',       error == 0 .and. ival == 3_I4P)
call check('merged global: k not in a',          fini%index(section_name='a', option_name='k') == 0)
call fini%free
call fini%load(source='[]'//new_line('A')//'[b]'//new_line('A')//'x = 1', error=error)
call check('empty [] section is kept',           fini%has_section(section_name='') .and. fini%has_section(section_name='b'))

open(newunit=unit, file=filename) ; close(unit=unit, status='DELETE')

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

end program finer_test_global_section
