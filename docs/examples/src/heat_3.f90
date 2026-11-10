program heat
!< Tutorial, chapter 3: optional settings and defaults.
use finer
use penf, only : I4P, R8P
implicit none
type(file_ini)                :: case_file
integer(I4P)                  :: error
real(R8P)                     :: cfl
integer(I4P)                  :: every
character(len=:), allocatable :: fields, format, where

call case_file%load(filename='heat.ini')
!region default
call case_file%get(section_name='time', option_name='cfl', val=cfl, error=error, default=0.9_R8P)
print '(A,F4.2,A,I0)', 'cfl = ', cfl, ', error ', error
call case_file%get(section_name='output', option_name='every', val=every, default=100_I4P)
print '(A,I0)', 'every = ', every
!endregion default
!region string
call case_file%get_string(section_name='output', option_name='fields', val=fields)
print '(A)', 'fields = "'//fields//'"'
call case_file%get_string(section_name='output', option_name='compression', val=format, default='none')
print '(A)', 'compression = "'//format//'"'
!endregion string
!region presence
print '(A,L1)', 'has [restart]: ', case_file%has_section(section_name='restart')
where = repeat(' ', 32)
if (case_file%has_option(option_name='density', section_name=where)) print '(A)', 'density is in ['//trim(where)//']'
!endregion presence
endprogram heat
!as heat
!run heat_3 heat
