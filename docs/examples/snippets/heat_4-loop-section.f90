print '(A)', 'options of [material]:'
do while (case_file%loop(section_name='material', option_pairs=pair))
  print '(A)', '  '//trim(pair(1))//' -> '//trim(pair(2))
enddo
