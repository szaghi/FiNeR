call restart%load(filename='restart.ini', error=error)
call restart%get(section_name='time', option_name='dt', val=dt)
print '(A,ES10.3,A,L1)', 'dt read back: ', dt, ', exact: ', dt == 5.0e-5_R8P
