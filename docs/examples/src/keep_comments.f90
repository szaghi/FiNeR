program keep_comments
!< Cookbook: keep the inline comments when saving.
use finer
implicit none
type(file_ini) :: ini

call ini%load(filename='heat.ini')
call ini%del(section_name='time')
call ini%del(section_name='boundaries')
call ini%del(section_name='output')
call ini%save(filename='plain.ini')
call ini%save(filename='commented.ini', retain_comments=.true.)
endprogram keep_comments
!run keep_comments keep_comments
!run keep_comments_out cat plain.ini commented.ini
