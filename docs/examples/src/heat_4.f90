program heat
!< Tutorial, chapter 4: walking the file.
use finer
use penf, only : I4P
implicit none
type(file_ini)                :: case_file
character(len=:), allocatable :: names(:)
character(len=:), allocatable :: pair(:)
character(len=:), allocatable :: items(:,:)
integer(I4P)                  :: s, i

call case_file%load(filename='heat.ini')
!region sections
print '(A,I0,A)', 'heat.ini has ', case_file%sections_number(), ' sections:'
do s=1, case_file%sections_number()
  print '(A,I0,A)', '  ', s, ': "'//case_file%section(s)//'"'
enddo
call case_file%get_sections_list(names)
print '(A,I0,A)', 'the list holds ', size(names), ' names'
!endregion sections
!region loop-section
print '(A)', 'options of [material]:'
do while (case_file%loop(section_name='material', option_pairs=pair))
  print '(A)', '  '//trim(pair(1))//' -> '//trim(pair(2))
enddo
!endregion loop-section
!region loop-all
i = 0
do while (case_file%loop(option_pairs=pair))
  i = i + 1
enddo
print '(A,I0,A)', 'the whole file has ', i, ' options'
!endregion loop-all
!region items
call case_file%get_items(items)
print '(A,I0,A)', 'get_items: ', size(items, dim=1), ' pairs, the last is '// &
                  trim(items(size(items, dim=1), 1))//' = '//trim(items(size(items, dim=1), 2))
!endregion items
!region index
print '(A,I0)', 'index of [time]: ', case_file%index(section_name='time')
print '(A,I0)', 'index of steps in [time]: ', case_file%index(section_name='time', option_name='steps')
print '(A,I0)', 'values of fields: ', case_file%count_values(section_name='output', option_name='fields')
!endregion index
endprogram heat
!as heat
!run heat_4 heat
