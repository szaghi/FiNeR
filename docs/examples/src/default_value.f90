program default_value
!< Cookbook: give a setting a default.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini)                :: ini
real(R8P)                     :: cfl, weights(3)
integer(I4P)                  :: error
character(len=:), allocatable :: scheme

call ini%load(filename='heat.ini')
call ini%get(section_name='time', option_name='cfl', val=cfl, default=0.9_R8P, error=error)
print '(A,F4.2,A,I0)', 'cfl = ', cfl, ' (default), error ', error
call ini%get(section_name='time', option_name='weights', val=weights, default=[0.25_R8P, 0.5_R8P, 0.25_R8P])
print '(A,3F5.2)', 'weights =', weights
call ini%get_string(section_name='time', option_name='scheme', val=scheme, default='euler')
print '(A)', 'scheme = '//scheme
endprogram default_value
!run default_value default_value
