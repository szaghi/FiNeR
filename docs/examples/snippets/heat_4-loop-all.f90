i = 0
do while (case_file%loop(option_pairs=pair))
  i = i + 1
enddo
print '(A,I0,A)', 'the whole file has ', i, ' options'
