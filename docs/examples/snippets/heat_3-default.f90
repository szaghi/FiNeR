call case_file%get(section_name='time', option_name='cfl', val=cfl, error=error, default=0.9_R8P)
print '(A,F4.2,A,I0)', 'cfl = ', cfl, ', error ', error
call case_file%get(section_name='output', option_name='every', val=every, default=100_I4P)
print '(A,I0)', 'every = ', every
