program get_items
!< Cookbook: get every option name and value at once.
use finer
implicit none
type(file_ini)                :: ini
character(len=:), allocatable :: items(:,:)
integer                       :: i

call ini%load(filename='rod.ini')
call ini%get_items(items)
do i=1, size(items, dim=1)
  print '(A)', trim(items(i,1))//' = '//trim(items(i,2))
enddo
endprogram get_items
