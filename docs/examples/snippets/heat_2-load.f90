call case_file%load(filename='heat.ini', error=error)
if (error /= 0) then
  print '(A,I0)', 'cannot read heat.ini, error ', error
  stop 1
endif
