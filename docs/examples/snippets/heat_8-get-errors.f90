cells = -1
call case_file%get(section_name='grids', option_name='cells', val=cells, error=error)
print '(A,I0,A,I0)', 'missing section:     error ', error, ', cells kept: ', cells
call case_file%get(section_name='material', option_name='name', val=cells, error=error)
print '(A,I0,A,I0)', 'not an integer:      error ', error, ', cells kept: ', cells
small = -1_I1P
call case_file%get(section_name='material', option_name='density', val=small, error=error)
print '(A,I0,A,I0)', 'out of range (I1P):  error ', error, ', small kept: ', small
origin = -1._R8P
call case_file%get(section_name='grid', option_name='origin', val=origin, error=error)
print '(A,I0,A,2F5.1)', 'array too small:     error ', error, ', origin kept:', origin
call case_file%get(section_name='grid', option_name='length', val=p, error=error)
print '(A,I0)', 'unsupported type:    error ', error
