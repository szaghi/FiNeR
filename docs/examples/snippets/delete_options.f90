program delete_options
!< Cookbook: delete options and sections, or empty them.
use finer
implicit none
type(file_ini) :: ini

call ini%load(filename='heat.ini')
call ini%del(section_name='time', option_name='adaptive')   ! one option
call ini%del(section_name='output')                         ! a section and its options
call ini%free_options(section_name='boundaries')            ! the options of a section, the section stays
call ini%free_options(section_name='grid', option_name='origin')
print '(A,I0,A,L1)', 'sections: ', ini%sections_number(), ', [boundaries] still there: ', &
                     ini%has_section(section_name='boundaries')
call ini%print(unit=6)
call ini%free_options()                                     ! every option, the sections stay
print '(A,I0,A,L1)', 'after free_options(): ', ini%sections_number(), ' sections, any option: ', &
                     ini%has_option(option_name='cells')
call ini%free()                                             ! everything
print '(A,I0)', 'after free(): ', ini%sections_number()
endprogram delete_options
