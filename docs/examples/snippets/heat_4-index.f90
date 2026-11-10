print '(A,I0)', 'index of [time]: ', case_file%index(section_name='time')
print '(A,I0)', 'index of steps in [time]: ', case_file%index(section_name='time', option_name='steps')
print '(A,I0)', 'values of fields: ', case_file%count_values(section_name='output', option_name='fields')
