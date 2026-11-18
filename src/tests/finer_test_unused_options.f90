!< FiNeR test: unused_options, the options never read through get since the file was loaded.
program finer_test_unused_options
!< Covers: nothing read (every option listed, file order); scalar get, array get, get_string and a get whose value does not
!<         convert all count as read; a missing option or section adds nothing; a repeated read is listed once; the global
!<         section; a duplicated section matched by name, its own options unreachable by get and so unused; the log deep-copied by assignment (reads on the copy do not
!<         touch the original); free and a reload reset it; a file built with add (never loaded) has no log, so every
!<         option is unused; no unused option gives a zero-size list.
use finer, only: file_ini
use penf,  only: I4P, R8P
implicit none

type(file_ini)                :: fini, copy, built
character(len=:), allocatable :: source, sval, items(:,:)
character(8)                  :: cval
real(R8P)                     :: rarr(3), rval
integer(I4P)                  :: ival
integer(I4P)                  :: error
integer                       :: passed, total

passed = 0 ; total = 0
print '(A)', 'finer_test_unused_options'

source = 'top = 1'//new_line('A')//          &
         '[grid]'//new_line('A')//           &
         'ni   = 64'//new_line('A')//        &
         'emin = 0. 0. 0.'//new_line('A')//  &
         'name = box'//new_line('A')//       &
         'bad  = abc'//new_line('A')//       &
         'CLF  = 0.5'//new_line('A')//       &
         '[io]'//new_line('A')//             &
         'out  = run'//new_line('A')//       &
         '[grid]'//new_line('A')//           &
         'ni   = 32'//new_line('A')//        &
         'nj   = 8'
call fini%load(source=source)

call fini%unused_options(items=items)
call check('nothing read: every option listed', size(items, dim=1) == 9)
call check('nothing read: file order, global first', has(items, 1, '', 'top') .and. has(items, 2, 'grid', 'ni'))
call check('nothing read: section/option pairs', has(items, 7, 'io', 'out') .and. has(items, 9, 'grid', 'nj'))

call fini%get(section_name='grid', option_name='ni', val=ival, error=error)
call fini%get(section_name='grid', option_name='ni', val=ival, error=error)
call fini%get(section_name='grid', option_name='emin', val=rarr, error=error)
call fini%get_string(section_name='grid', option_name='name', val=sval, error=error)
call fini%get(section_name='grid', option_name='bad', val=rval, error=error)
call check('a value that does not convert: error', error /= 0)
call fini%get(section_name='grid', option_name='missing', val=ival, error=error)
call fini%get(section_name='nowhere', option_name='ni', val=ival, error=error)
call fini%get(section_name='', option_name='top', val=cval, error=error)
call fini%unused_options(items=items)
call check('after reads: CLF, out and nj left', size(items, dim=1) == 3)
call check('after reads: the typo CLF',         has(items, 1, 'grid', 'CLF'))
call check('after reads: io out',               has(items, 2, 'io', 'out'))
call check('duplicated section: nj, not ni',    has(items, 3, 'grid', 'nj'))

copy = fini
call copy%get(section_name='io', option_name='out', val=cval, error=error)
call copy%unused_options(items=items)
call check('copy: its own read recorded',       size(items, dim=1) == 2)
call fini%unused_options(items=items)
call check('copy: the original unchanged',      size(items, dim=1) == 3)

call fini%get(section_name='grid', option_name='CLF', val=rval, error=error)
call fini%get(section_name='io', option_name='out', val=cval, error=error)
call fini%get(section_name='grid', option_name='nj', val=ival, error=error)
call check('duplicated section: nj unreachable', error /= 0)
call fini%unused_options(items=items)
call check('duplicated section: nj stays unused', size(items, dim=1) == 1 .and. has(items, 1, 'grid', 'nj'))

call copy%load(source='[s]'//new_line('A')//'a = 1'//new_line('A')//'b = 2')
call copy%get(section_name='s', option_name='a', val=ival, error=error)
call copy%get(section_name='s', option_name='b', val=ival, error=error)
call copy%unused_options(items=items)
call check('everything read: zero-size list',   size(items, dim=1) == 0 .and. size(items, dim=2) == 2)

call fini%load(source=source)
call fini%unused_options(items=items)
call check('reload: the log is reset',          size(items, dim=1) == 9)
call fini%free
call fini%unused_options(items=items)
call check('free: nothing left',                size(items, dim=1) == 0)

call built%add(section_name='s', option_name='a', val=1)
call built%get(section_name='s', option_name='a', val=ival, error=error)
call built%unused_options(items=items)
call check('built with add, never loaded: no log, listed', size(items, dim=1) == 1)

call summary
contains

  logical function has(items, row, sec, opt)
  character(*), intent(in) :: items(:,:)
  integer,      intent(in) :: row
  character(*), intent(in) :: sec, opt
  has = .false.
  if (row > size(items, dim=1)) return
  has = trim(items(row, 1)) == sec .and. trim(items(row, 2)) == opt
  end function has

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

end program finer_test_unused_options
