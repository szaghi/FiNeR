!< INI file class definition.
module finer_file_ini_t
!< INI file class definition.
use finer_backend
use finer_option_t, only : option
use finer_section_t, only : section
use penf
use stringifor
use, intrinsic :: iso_fortran_env, only : stdout => output_unit

implicit none
private
public :: file_ini
public :: file_ini_autotest

type :: file_ini
  !< INI file class.
  private
  character(len=:), allocatable, public :: filename              !< File name
  integer(I4P)                          :: Ns = 0                !< Number of sections.
  character(1)                          :: opt_sep = DEF_OPT_SEP !< Separator character of option name/value.
  type(section), allocatable            :: sections(:)           !< Sections.
  contains
    ! public methods
    generic               :: add          => add_section, &             !< Add a section.
                                             add_option,  &             !< Add an option to a section (scalar).
                                             add_a_option               !< Add an option to a section (array).
    procedure, pass(self) :: count_values                               !< Count option value(s).
    generic               :: del          => free_option_of_section, &  !< Remove (freeing) an option of a section.
                                             free_section               !< Remove (freeing) a section.
    procedure, pass(self) :: free                                       !< Free dynamic memory destroyng file data.
    generic               :: free_options => free_options_all,        & !< Free all options.
                                             free_options_of_section, & !< Free all options of a section.
                                             free_option_of_section     !< Free an option of a section.
    generic               :: get          => get_option, &              !< Get option value (scalar).
                                             get_a_option               !< Get option value (array).
    procedure, pass(self) :: get_items                                  !< Get list of pairs option name/value.
    procedure, pass(self) :: get_sections_list                          !< Get sections names list.
    procedure, pass(self) :: initialize                                 !< Initialize file.
    procedure, pass(self) :: has_option                                 !< Inquire the presence of an option.
    procedure, pass(self) :: has_section                                !< Inquire the presence of a section.
    generic               :: index        => index_section, &           !< Return the index of a section.
                                             index_option               !< Return the index of an option.
    procedure, pass(self) :: load                                       !< Load file data.
    generic               :: loop         => loop_options_section, &    !< Loop over options of a section.
                                             loop_options               !< Loop over all options.
    procedure, pass(self) :: print        => print_file_ini             !< Pretty printing data.
    procedure, pass(self) :: save         => save_file_ini              !< Save data.
    procedure, pass(self) :: section      => section_file_ini           !< Get section name once provided an index.
    ! operators overloading
    generic :: assignment(=) => assign_file_ini !< Procedure for section assignment overloading.
    ! private methods
    procedure, private, pass(self) :: add_a_option            !< Add an option to a section (array).
    procedure, private, pass(self) :: add_option              !< Add an option to a section (scalar).
    procedure, private, pass(self) :: add_section             !< Add a section.
    procedure, private, pass(self) :: free_options_all        !< Free all options of all sections.
    procedure, private, pass(self) :: free_options_of_section !< Free all options of a section.
    procedure, private, pass(self) :: free_option_of_section  !< Free an option of a section.
    procedure, private, pass(self) :: free_section            !< Free a section.
    procedure, private, pass(self) :: get_a_option            !< Get option value (array).
    procedure, private, pass(self) :: get_option              !< Get option value (scalar).
    procedure, private, pass(self) :: index_option            !< Return the index of an option.
    procedure, private, pass(self) :: index_section           !< Return the index of a section.
    procedure, private, pass(self) :: loop_options            !< Loop over all options.
    procedure, private, pass(self) :: loop_options_section    !< Loop over options of a section.
    procedure, private, pass(self) :: parse                   !< Parse file data.
    ! assignments
    procedure, private, pass(lhs) :: assign_file_ini !< Assignment overloading.
endtype file_ini

contains
  ! public methods
  elemental function count_values(self, delimiter, section_name, option_name) result(Nv)
  !< Get the number of values of option into section data.
  class(file_ini),        intent(in) :: self         !< File data.
  character(*), optional, intent(in) :: delimiter    !< Delimiter used for separating values.
  character(*),           intent(in) :: section_name !< Section name.
  character(*),           intent(in) :: option_name  !< Option name.
  integer(I4P)                       :: Nv           !< Number of values.
  character(len=:), allocatable      :: dlm          !< Dummy string for delimiter handling.
  integer(I4P)                       :: s            !< Counter.

  if (allocated(self%sections)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == trim(adjustl(section_name))) then
        Nv = self%sections(s)%count_values(delimiter=dlm, option_name=option_name)
        exit
      endif
    enddo
  endif
  endfunction count_values

  elemental subroutine free(self)
  !< Free dynamic memory.
  class(file_ini), intent(inout) :: self !< File data.

  if (allocated(self%filename)) deallocate(self%filename)
  if (allocated(self%sections)) then
    call self%sections%free
    deallocate(self%sections)
  endif
  self%Ns = 0
  self%opt_sep = def_opt_sep
  endsubroutine free

  pure subroutine get_items(self, items)
  !< Get list of pairs option name/value.
  class(file_ini),               intent(in)  :: self       !< File data.
  character(len=:), allocatable, intent(out) :: items(:,:) !< Items, list of pairs option name/value for all options [1:No,1:2].
  character(len=:), allocatable              :: pairs(:)   !< Option name/values pairs.
  integer(I4P)                               :: mx_chars   !< Maximum number of chars into name/value within all options.
  integer(I4P)                               :: o          !< Counter.
  integer(I4P)                               :: s          !< Counter.
  integer(I4P)                               :: No         !< Counter.

  mx_chars = MinI4P
  if (allocated(self%sections)) then
    No = 0
    do s=1, size(self%sections, dim=1)
      if (self%sections(s)%has_options()) then
        mx_chars = max(mx_chars, self%sections(s)%max_chars_len())
        No = No + self%sections(s)%options_number()
      endif
    enddo
    if ((mx_chars > 0).and.(No > 0)) then
      allocate(character(mx_chars):: items(1:No, 1:2))
      No = 0
      do s=1, size(self%sections, dim=1)
        if (self%sections(s)%has_options()) then
          do o=1, self%sections(s)%options_number()
            No = No + 1
            call self%sections(s)%option_pairs(option_index=o, pairs=pairs)
            items(No, 1) = pairs(1)
            items(No, 2) = pairs(2)
          enddo
        endif
      enddo
    endif
  endif
  endsubroutine get_items

  pure subroutine get_sections_list(self, list)
  !< Get sections names list.
  class(file_ini),               intent(in)  :: self    !< File data.
  character(len=:), allocatable, intent(out) :: list(:) !< Sections names list.
  integer                                    :: max_len !< Max length of section name.
  integer                                    :: s       !< Counter.

  if (allocated(self%sections)) then
    max_len = MinI_P
    do s=1, self%Ns
      max_len = max(max_len, len(self%sections(s)%name()))
    enddo
    if (max_len>0) then
      allocate(character(len=max_len) :: list(1:self%Ns))
      do s=1, self%Ns
        list(s) = self%sections(s)%name()
      enddo
    endif
  endif
  endsubroutine get_sections_list

  function has_option(self, option_name, section_name) result(pres)
  !< Inquire the presence of (at least one) option with the name passed.
  !<
  !< Optional, the first matching section name is returned.
  !<
  !< @note All sections are searched and the first occurence is returned.
  class(file_ini),        intent(in)    :: self         !< File data.
  character(*),           intent(in)    :: option_name  !< Option name.
  character(*), optional, intent(inout) :: section_name !< Section name.
  logical                               :: pres         !< Inquiring flag.
  integer(I4P)                          :: s            !< Counter.

  pres = .false.
  if (allocated(self%sections)) then
    do s=1, size(self%sections, dim=1)
      pres = (self%sections(s)%index(option_name=option_name)>0)
      if (pres) then
        if (present(section_name)) section_name = self%sections(s)%name()
        exit
      endif
    enddo
  endif
  endfunction has_option

  elemental function has_section(self, section_name) result(pres)
  !< Inquire the presence of (at least one) section with the name passed.
  class(file_ini), intent(in) :: self         !< File data.
  character(*),    intent(in) :: section_name !< Section name.
  logical                     :: pres         !< Inquiring flag.

  pres = (self%index(section_name=section_name)>0)
  endfunction has_section

  elemental subroutine initialize(self, filename)
  !< Initialize file.
  class(file_ini), intent(inout)         :: self     !< File data.
  character(*),    intent(in),  optional :: filename !< File name.

  call self%free
  if (present(filename)) self%filename = trim(adjustl(filename))
  endsubroutine initialize

  subroutine load(self, separator, filename, source, error)
  !< Get file data from a file or a source string.
  !<
  !<### Usage
  !<
  !<##### Loading from a file
  !<```bash
  !<type(file_ini):: fini
  !<call fini%load(filename='path_to_my_file.ini')
  !<```
  !<
  !<##### Loading from a source string
  !<```bash
  !<type(file_ini):: fini
  !<call fini%load(source='[section-1] option-1=one [section-2] option-2=due')
  !<```
  class(file_ini), intent(inout)         :: self      !< File data.
  character(1),    intent(in),  optional :: separator !< Separator of options name/value.
  character(*),    intent(in),  optional :: filename  !< File name.
  character(*),    intent(in),  optional :: source    !< File source contents.
  integer(I4P),    intent(out), optional :: error     !< Error code.
  integer(I4P)                           :: errd      !< Error code.
  type(string)                           :: source_   !< File source contents, local variable.

  errd = ERR_SOURCE_MISSING
  if (present(separator)) self%opt_sep = separator
  if (present(filename)) then
    self%filename = trim(adjustl(filename))
    call source_%read_file(file=self%filename, iostat=errd)
  elseif (present(source)) then
    source_ = source
    errd = 0
  elseif (allocated(self%filename)) then
    call source_%read_file(file=self%filename, iostat=errd)
  endif
  if (errd <= 0) call self%parse(source=source_, error=errd)
  if (present(error)) error = errd
  endsubroutine load

  subroutine print_file_ini(self, unit, pref, retain_comments, iostat, iomsg)
  !< Print data with a pretty format.
  class(file_ini),        intent(in)  :: self            !< File data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  character(*), optional, intent(in)  :: pref            !< Prefixing string.
  logical,      optional, intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  character(len=:), allocatable       :: prefd           !< Prefixing string.
  logical                             :: rt_comm         !< Flag for retaining eventual comments.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  integer(I4P)                        :: s               !< Counter.

  prefd = '' ; if (present(pref)) prefd = pref
  rt_comm = .false. ; if (present(retain_comments)) rt_comm = retain_comments
  if (allocated(self%sections)) then
    do s=1, size(self%sections, dim=1)
      call self%sections(s)%print(pref=prefd, iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=rt_comm)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  endsubroutine print_file_ini

  subroutine save_file_ini(self, retain_comments, iostat, iomsg, filename)
  !< Save data.
  class(file_ini),        intent(inout) :: self            !< File data.
  logical,      optional, intent(in)    :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(out)   :: iostat          !< IO error.
  character(*), optional, intent(out)   :: iomsg           !< IO error message.
  character(*), optional, intent(in)    :: filename        !< File name.
  logical                               :: rt_comm         !< Flag for retaining eventual comments.
  integer(I4P)                          :: unit            !< Logic unit.
  integer(I4P)                          :: iostatd         !< IO error.
  character(500)                        :: iomsgd          !< Temporary variable for IO error message.
  integer(I4P)                          :: s               !< Counter.

  rt_comm = .false. ; if (present(retain_comments)) rt_comm = retain_comments
  if (present(filename)) self%filename = filename
  if (allocated(self%filename).and.allocated(self%sections)) then
    open(newunit=unit, file=self%filename, action='WRITE', iostat=iostatd, iomsg=iomsgd)
    do s=1, size(self%sections, dim=1)
      call self%sections(s)%save(iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=rt_comm)
    enddo
    close(unit=unit, iostat=iostatd, iomsg=iomsgd)
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  endsubroutine save_file_ini

  pure function section_file_ini(self, section_index) result(sname)
  !< Get section name once an index (valid) is provided.
  class(file_ini), intent(in)   :: self          !< File data.
  integer(I4P),    intent(in)   :: section_index !< Section index.
  character(len=:), allocatable :: sname         !< Section name.

  if (allocated(self%sections)) then
    if ((section_index >= lbound(self%sections, dim=1)).and.(section_index <= ubound(self%sections, dim=1))) then
      sname = self%sections(section_index)%name()
    endif
  endif
  endfunction section_file_ini

  ! private methods
  pure subroutine add_a_option(self, error, section_name, option_name, val)
  !< Add an option (with array value).
  !<
  !< If the option already exists, its value is updated.
  class(file_ini),        intent(inout) :: self         !< File data.
  integer(I4P), optional, intent(out)   :: error        !< Error code.
  character(*),           intent(in)    :: section_name !< Section name.
  character(*),           intent(in)    :: option_name  !< Option name.
  class(*),               intent(in)    :: val(1:)      !< Option value.
  integer(I4P)                          :: errd         !< Error code.
  integer(I4P)                          :: s            !< Counter.

  errd = err_section_options
  call self%add(section_name=section_name, error=errd)
  if (errd==0) then
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == section_name) then
        call self%sections(s)%add(error=errd, option_name=option_name, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine add_a_option

  pure subroutine add_option(self, error, section_name, option_name, val)
  !< Add an option (with scalar value).
  !<
  !< If the option already exists, its value is updated.
  class(file_ini),        intent(inout) :: self         !< File data.
  integer(I4P), optional, intent(out)   :: error        !< Error code.
  character(*),           intent(in)    :: section_name !< Section name.
  character(*),           intent(in)    :: option_name  !< Option name.
  class(*),               intent(in)    :: val          !< Option value.
  integer(I4P)                          :: errd         !< Error code.
  integer(I4P)                          :: s            !< Counter.

  errd = err_section_options
  call self%add(section_name=section_name, error=errd)
  if (errd==0) then
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == section_name) then
        call self%sections(s)%add(error=errd, option_name=option_name, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine add_option

  pure subroutine add_section(self, error, section_name)
  !< Add a section.
  !<
  !< If the section already exists, it is left unchanged.
  class(file_ini),        intent(inout) :: self         !< File data.
  integer(I4P), optional, intent(out)   :: error        !< Error code.
  character(*),           intent(in)    :: section_name !< Section name.
  type(section), allocatable            :: sections(:)  !< Temporary sections array.
  integer(I4P)                          :: errd         !< Error code.

  errd = err_section
  if (allocated(self%sections)) then
    if (self%index(section_name=section_name)==0) then
      ! section not present
      allocate(sections(1:size(self%sections, dim=1)+1))
      sections(1:size(self%sections, dim=1)) = self%sections
      sections(size(self%sections, dim=1)+1) = section(section_name=trim(adjustl(section_name)))
      call move_alloc(sections, self%sections)
      self%Ns = self%Ns + 1
    endif
  else
    allocate(self%sections(1:1))
    self%sections(1) = section(section_name=section_name)
    self%Ns = self%Ns + 1
  endif
  if (self%index(section_name=section_name)>0) errd = 0
  if (present(error)) error = errd
  endsubroutine add_section

  elemental subroutine free_options_all(self)
  !< Free all options of all sections.
  class(file_ini), intent(inout):: self !< File data.

  if (allocated(self%sections)) call self%sections%free_options
  endsubroutine free_options_all

  elemental subroutine free_option_of_section(self, section_name, option_name)
  !< Free all options of a section.
  class(file_ini), intent(inout) :: self         !< File data.
  character(*),    intent(in)    :: section_name !< Section name.
  character(*),    intent(in)    :: option_name  !< Option  name.
  integer(I4P)                   :: s            !< Counter.

  s = self%index(section_name=section_name)
  if (s>0) call self%sections(s)%free_option(option_name=option_name)
  endsubroutine free_option_of_section

  elemental subroutine free_options_of_section(self, section_name)
  !< Free all options of a section.
  class(file_ini), intent(inout) :: self         !< File data.
  character(*),    intent(in)    :: section_name !< Section name.
  integer(I4P)                   :: s            !< Counter.

  if (allocated(self%sections)) then
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == section_name) then
        call self%sections(s)%free_options
        exit
      endif
    enddo
  endif
  endsubroutine free_options_of_section

  elemental subroutine free_section(self, section_name)
  !< Free all options of a section.
  class(file_ini), intent(inout) :: self         !< File data.
  character(*),    intent(in)    :: section_name !< Section name.
  type(section), allocatable     :: sections(:)  !< Temporary sections array.
  integer(I4P)                   :: s            !< Counter.

  s = self%index(section_name=section_name)
  if (s>0) then
    allocate(sections(1:size(self%sections, dim=1)-1))
    if (s==1) then
      sections = self%sections(2:)
    elseif (s==size(self%sections, dim=1)) then
      sections = self%sections(:s-1)
    else
      sections(:s-1) = self%sections(:s-1)
      sections(s:  ) = self%sections(s+1:)
    endif
    call move_alloc(sections, self%sections)
    self%Ns = self%Ns - 1
  endif
  endsubroutine free_section

  subroutine get_a_option(self, section_name, option_name, val, delimiter, error)
  !< Get option value (array)
  class(file_ini), intent(in)            :: self         !< File data.
  character(*),    intent(in)            :: section_name !< Section name.
  character(*),    intent(in)            :: option_name  !< Option name.
  class(*),        intent(inout)         :: val(1:)      !< Value.
  character(*),    intent(in),  optional :: delimiter    !< Delimiter used for separating values.
  integer(I4P),    intent(out), optional :: error        !< Error code.
  character(len=:), allocatable          :: dlm          !< Dummy string for delimiter handling.
  integer(I4P)                           :: errd         !< Error code.
  integer(I4P)                           :: s            !< Counter.

  errd = ERR_OPTION
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%sections)) then
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == trim(adjustl(section_name))) then
        call self%sections(s)%get(delimiter=dlm, error=errd, option_name=option_name, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine get_a_option

  subroutine get_option(self, section_name, option_name, val, error)
  !< Get option value (scalar).
  class(file_ini), intent(in)            :: self         !< File data.
  character(*),    intent(in)            :: section_name !< Section name.
  character(*),    intent(in)            :: option_name  !< Option name.
  class(*),        intent(inout)         :: val          !< Value.
  integer(I4P),    intent(out), optional :: error        !< Error code.
  integer(I4P)                           :: errd         !< Error code.
  integer(I4P)                           :: s            !< Counter.

  errd = ERR_OPTION
  if (allocated(self%sections)) then
    do s=1, size(self%sections, dim=1)
      if (self%sections(s) == trim(adjustl(section_name))) then
        call self%sections(s)%get(error=errd, option_name=option_name, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine get_option

  elemental function index_option(self, back, section_name, option_name) result(ind)
  !< Return the index of the option (inside a  section) matching the name(s) passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  class(file_ini),   intent(in) :: self         !< File data.
  logical, optional, intent(in) :: back         !< If back appears with the value true, the last matching index is returned.
  character(*),      intent(in) :: option_name  !< Option  name.
  character(*),      intent(in) :: section_name !< Section name.
  integer(I4P)                  :: ind          !< Index of searched section.
  logical                       :: backd        !< Dummy back flag.
  integer(I4P)                  :: s            !< Counter.

  ind = 0
  if (allocated(self%sections)) then
    backd = .false. ; if (present(back)) backd = back
    s = self%index(section_name=section_name, back=backd)
    if (s>0) then
      ind = self%sections(s)%index(option_name=option_name, back=backd)
    endif
  endif
  endfunction index_option

  elemental function index_section(self, back, section_name) result(ind)
  !< Return the index of the section matching the name passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  class(file_ini),   intent(IN) :: self         !< File data.
  logical, optional, intent(IN) :: back         !< If back appears with the value true, the last matching index is returned.
  character(*),      intent(IN) :: section_name !< Section name.
  integer(I4P)                  :: ind          !< Index of searched section.
  logical                       :: backd        !< Dummy back flag.
  integer(I4P)                  :: s            !< Counter.

  ind = 0
  if (allocated(self%sections)) then
    backd = .false. ; if (present(back)) backd = back
    if (backd) then
      do s=size(self%sections, dim=1), 1,-1
        if (self%sections(s) == trim(adjustl(section_name))) then
          ind = s
          exit
        endif
      enddo
    else
      do s=1, size(self%sections, dim=1)
        if (self%sections(s) == trim(adjustl(section_name))) then
          ind = s
          exit
        endif
      enddo
    endif
  endif
  endfunction index_section

  function loop_options_section(self, section_name, option_pairs) result(again)
  !< Loop returning option name/value defined into section.
  class(file_ini),               intent(in)  :: self            !< File data.
  character(*),                  intent(in)  :: section_name    !< Section name.
  character(len=:), allocatable, intent(out) :: option_pairs(:) !< Pairs option name/value [1:2].
  logical                                    :: again           !< Flag continuing the loop.
  integer(I4P)                               :: s               !< Counter.

  again = .false.
  s = self%index(section_name=section_name)
  if (s>0) then
    again = self%sections(s)%loop(option_pairs=option_pairs)
  endif
  endfunction loop_options_section

  recursive function loop_options(self, option_pairs) result(again)
  !< Loop returning option name/value defined into all sections.
  class(file_ini),               intent(IN)  :: self            !< File data.
  character(len=:), allocatable, intent(OUT) :: option_pairs(:) !< Pairs option name/value [1:2].
  logical                                    :: again           !< Flag continuing the loop.
  logical,      save                         :: againO=.false.  !< Flag continuing the loop.
  integer(I4P), save                         :: s=0             !< Counter.

  again = .false.
  if (allocated(self%sections)) then
    if (s==0) then
      s = lbound(self%sections, dim=1)
      againO = self%loop(section_name=self%sections(s)%name(), option_pairs=option_pairs)
      again = .true.
    elseif (s<ubound(self%sections, dim=1)) then
      if (.not.againO) s = s + 1
      againO = self%loop(section_name=self%sections(s)%name(), option_pairs=option_pairs)
      if (.not.againO) then
        again = self%loop(option_pairs=option_pairs)
      else
        again = .true.
      endif
    else
      s = 0
      againO = .false.
      again = .false.
    endif
  endif
  endfunction loop_options

  subroutine parse(self, source, error)
  !< Parse file either from the self source data or from a source string.
  class(file_ini),        intent(inout)   :: self      !< File data.
  type(string),           intent(in)      :: source    !< String source.
  integer(I4P), optional, intent(out)     :: error     !< Error code.
  integer(I4P)                            :: errd      !< Error code.
  type(string), allocatable               :: tokens(:) !< Options strings tokenized.
  type(string)                            :: dummy     !< Dummy string for parsing sections.
  integer(I4P)                            :: Ns        !< Counter.
  integer(I4P)                            :: s         !< Counter.
  integer(I4P)                            :: ss        !< Counter.

  errd = err_source_missing
  call source%split(tokens=tokens, sep=new_line('a'))

  Ns = 0
  s = 0
  do while (s+1<=size(tokens, dim=1))
    s = s + 1
    if (scan(adjustl(tokens(s)), comments) == 1) cycle
    if (index(trim(adjustl(tokens(s))), "[") == 1) then
      Ns = Ns + 1
      dummy = trim(adjustl(tokens(s)))//new_line('a')
      ss = s
      do while (ss+1<=size(tokens, dim=1))
        ss = ss + 1
        if (index(trim(adjustl(tokens(ss))), "[") == 1) then
          ! new section... go back
          exit
        else
          ! continuation of current section
          dummy = trim(adjustl(dummy))//new_line('a')//trim(adjustl(tokens(ss)))
          tokens(ss) = comments ! forcing skip this in the following scan
        endif
      enddo
      tokens(s) = trim(adjustl(dummy))
    endif
  enddo

  if (Ns>0) then
    if (allocated(self%sections)) deallocate(self%sections) ; allocate(self%sections(1:Ns))
    s = 0
    ss = 0
    do while (s+1<=size(tokens, dim=1))
      s = s + 1
      if (scan(adjustl(tokens(s)), comments) == 1) cycle
      if (index(trim(adjustl(tokens(s))), "[") == 1) then
        ss = ss + 1
        call self%sections(ss)%parse(sep=self%opt_sep, source=tokens(s), error=errd)
      endif
    enddo
  endif
  self%Ns = Ns

  if (present(error)) error = errd
  endsubroutine parse

  ! assignments
  elemental subroutine assign_file_ini(lhs, rhs)
  !< Assignment between two INI files.
  class(file_ini), intent(inout) :: lhs !< Left hand side.
  type(file_ini),  intent(in)    :: rhs !< Rigth hand side.

  if (allocated(rhs%filename)) lhs%filename = rhs%filename
  if (allocated(rhs%sections)) then
    if (allocated(lhs%sections)) deallocate(lhs%sections) ; allocate(lhs%sections(1:size(rhs%sections, dim=1)))
    lhs%sections = rhs%sections
  endif
  lhs%Ns = rhs%Ns
  endsubroutine assign_file_ini

  ! non TBP methods
  subroutine file_ini_autotest()
  !< Autotest the library functionalities.
  type(file_ini)                :: fini       !< INI File.
  character(len=:), allocatable :: source     !< Testing string.
  character(len=:), allocatable :: string     !< String option.
  real(R4P), allocatable        :: array(:)   !< Array option.
  integer(I4P)                  :: error      !< Error code.
  character(len=:), allocatable :: items(:,:) !< List of all options name/value pairs.
  character(len=:), allocatable :: item(:)    !< Option name/value couple.
  character(len=:), allocatable :: list(:)    !< Sections names list.
  integer(I4P)                  :: i          !< Counter.
  integer(I4P)                  :: s          !< Counter.

  source='[section-1]'//new_line('A')//   &
         'option-1 = one'//new_line('A')//&
         'option-2 = 2.'//new_line('A')// &
         '           3. ; this is an inline comment'//new_line('A')// &
         'option-3 = bar ; this is an inline comment'//new_line('A')//&
         '[section-2]'//new_line('A')//   &
         'option-1 = foo'
  print "(A)", ''
  print "(A)", "Testing parsing procedures"
  print "(A)", ''
  print "(A)", "Source to be parsed:"
  print "(A)", source
  call fini%load(source=source)
  print "(A)", ''
  print "(A)", "Result of parsing:"
  string = '   '
  call fini%get(section_name='section-1', option_name='option-1', val=string, error=error)
  if (error==0) print "(A,A)", '  option-1 of section-1 has values: ', string
  allocate(array(1:fini%count_values(section_name='section-1', option_name='option-2')))
  call fini%get(section_name='section-1', option_name='option-2', val=array, error=error)
  if (error==0) print "(A,3(F4.1,1X))", '  option-2 of section-1 has values: ', array
  call fini%get(section_name='section-1', option_name='option-3', val=string, error=error)
  if (error==0) print "(A,A)", '  option-3 of section-1 has values: ', string
  call fini%get(section_name='section-2',option_name='option-1', val=string, error=error)
  if (error==0) print "(A,A)", '  option-1 of section-2 has values: ', string
  print "(A)", ''

  print "(A)", "Parsed data will be saved as (having retained inline comments that are trimmed out by default):"
  call fini%print(pref='  ', unit=stdout, retain_comments=.true.)
  call fini%save(filename='foo.ini', retain_comments=.true.)
  call fini%free
  print "(A)", ''
  print "(A)", "Testing generating procedures"
  call fini%add(section_name='sec-foo')
  call fini%add(section_name='sec-foo', option_name='bar', val=-32.1_R8P)
  call fini%add(section_name='sec-foo', option_name='baz', val=' hello FiNeR! ')
  call fini%add(section_name='sec-foo', option_name='array', val=[1, 2, 3, 4])
  call fini%add(section_name='sec-bar')
  call fini%add(section_name='sec-bar', option_name='bools', val=[.true.,.false.,.false.])
  call fini%add(section_name='sec-bartolomeo')
  call fini%add(section_name='sec-bartolomeo', option_name='help', val='I am Bartolomeo')
  print "(A)", "The autogenerated INI file will be saved as:"
  call fini%print(pref='  ', unit=stdout)
  print "(A)", ''
  print "(A)", "Testing removing option baz"
  call fini%del(section_name='sec-foo', option_name='baz')
  call fini%print(pref='  ', unit=stdout)
  print "(A)", ''
  print "(A)", "Testing removing section sec-bar"
  call fini%del(section_name='sec-bar')
  call fini%print(pref='  ', unit=stdout)
  print "(A)", ''
  print "(A)", "Testing introspective methods"
  print "(A,L1)", "Is there option bar? ", fini%has_option(option_name='bar')
  print "(A,L1)", "Is there option baz? ", fini%has_option(option_name='baz')
  print "(A,L1)", "Is there section sec-bar? ", fini%has_section(section_name='sec-bar')
  print "(A,L1)", "Is there section sec-foo? ", fini%has_section(section_name='sec-foo')
  print "(A)", ''
  print "(A)", "What are all options name/values pairs? Can I have a list? Yes, you can:"
  call fini%get_items(items=items)
  do i=1, size(items, dim=1)
    print "(A)", trim(items(i, 1))//' = '//trim(items(i, 2))
  enddo
  print "(A)", ''
  print "(A)", "Testing loop method over options of a section:"
  do s=1, fini%Ns
    print "(A)", fini%section(s)
    do while(fini%loop(section_name=fini%section(s), option_pairs=item))
      print "(A)", '  '//trim(item(1))//' = '//trim(item(2))
    enddo
  enddo
  print "(A)", "Testing sections names list inquire:"
  call fini%get_sections_list(list)
  do s=1, fini%Ns
    print "(A)", 'Sec. '//trim(str(s, .true.))//': '//trim(list(s))
  enddo
  print "(A)", ''
  print "(A)", "Testing loop method over all options:"
  do while(fini%loop(option_pairs=item))
    print "(A)", '  '//trim(item(1))//' = '//trim(item(2))
  enddo
  print "(A)", ''
  print "(A)", "Testing custom separator of option name/value:, use ':' instead of '='"
  source='[section-1]'//new_line('A')//   &
         'option-1 : one'//new_line('A')//&
         'option-2 : 2.'//new_line('A')// &
         '           3.'//new_line('A')// &
         'option-3 : bar'//new_line('A')//&
         '[section-2]'//new_line('A')//   &
         'option-1 : foo'
  print "(A)", ''
  print "(A)", "Source to be parsed:"
  print "(A)", source
  call fini%free
  call fini%load(separator=':', source=source)
  print "(A)", ''
  print "(A)", "Result of parsing:"
  call fini%print(pref='  ', unit=stdout)
  ! remove "foo.ini"
  open(newunit=i, file='foo.ini') ; close(unit=i, status='DELETE')
  endsubroutine file_ini_autotest
endmodule finer_file_ini_t
