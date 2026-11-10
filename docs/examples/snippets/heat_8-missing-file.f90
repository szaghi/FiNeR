call case_file%load(filename='hat.ini', error=error)
print '(A,L1)', 'load of a missing file: error /= 0 is ', error /= 0
