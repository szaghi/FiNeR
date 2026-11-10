print '(A,I0,A)', 'heat.ini has ', case_file%sections_number(), ' sections:'
do s=1, case_file%sections_number()
  print '(A,I0,A)', '  ', s, ': "'//case_file%section(s)//'"'
enddo
call case_file%get_sections_list(names)
print '(A,I0,A)', 'the list holds ', size(names), ' names'
