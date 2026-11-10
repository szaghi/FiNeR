program report_bad_values
!< Cookbook: check every setting and report all the bad ones.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini) :: ini
integer(I4P)   :: error, cells, bad
real(R8P)      :: dt, length

call ini%load(source='[grid]'//new_line('a')//'cells = 64.5'//new_line('a')//'length = 1m'//new_line('a')// &
                     '[time]'//new_line('a')//'dt = 1e-4')
bad = 0
call ini%get(section_name='grid', option_name='cells', val=cells, error=error)
call report('grid', 'cells')
call ini%get(section_name='grid', option_name='length', val=length, error=error)
call report('grid', 'length')
call ini%get(section_name='time', option_name='dt', val=dt, error=error)
call report('time', 'dt')
call ini%get(section_name='time', option_name='steps', val=cells, error=error)
call report('time', 'steps')
print '(I0,A)', bad, ' bad settings'
contains
  subroutine report(section, option)
  character(*), intent(in) :: section, option
  if (error == err_option_vals) then
    bad = bad + 1 ; print '(A)', '['//section//'] '//option//': not a value of that type'
  elseif (error /= 0) then
    bad = bad + 1 ; print '(A)', '['//section//'] '//option//': missing'
  endif
  endsubroutine report
endprogram report_bad_values
!run report_bad_values report_bad_values
