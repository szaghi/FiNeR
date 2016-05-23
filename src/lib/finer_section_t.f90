!< Section class definition.
module finer_section_t
!-----------------------------------------------------------------------------------------------------------------------------------
!< Section class definition.
!-----------------------------------------------------------------------------------------------------------------------------------
use finer_backend
use finer_option_t, only : option
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: section
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: section
  !< Section data of file INI.
  character(len=:), allocatable :: sname      !< Section name.
  type(option),     allocatable :: options(:) !< Section options.
  contains
    procedure :: free         => free_section                !< Free dynamic memory.
    procedure :: free_options => free_options_section        !< Free all options.
    procedure :: free_option  => free_option_section         !< Free a option.
    procedure :: parse        => parse_section               !< Parse section data.
    procedure :: index        => index_option_section        !< Return the index of an option.
    procedure :: count_values => count_values_option_section !< Count option value(s).
    generic ::   set          => set_option_section, &       !< Set option value (scalar).
                                 set_a_option_section        !< Set option value (array).
    generic ::   add          => add_option_section, &       !< Add an option (scalar).
                                 add_a_option_section        !< Add an option (array).
    generic ::   get          => get_option_section, &       !< Get option value (scalar).
                                 get_a_option_section        !< Get option value (array).
    procedure :: loop         => loop_options_section        !< Loop over options.
    procedure :: print        => print_section               !< Pretty print data.
    procedure :: save         => save_section                !< Save data.
    ! operators overloading
    generic :: assignment(=) => assign_section !< Assignment overloading.
    ! private procedures
    procedure, private            :: parse_name    => parse_section_name    !< Get section name.
    procedure, private            :: parse_options => parse_options_section !< Get section options.
    procedure, private            :: set_option_section                     !< Set option value (scalar).
    procedure, private            :: set_a_option_section                   !< Set option value (array).
    procedure, private            :: add_option_section                     !< Add an option (scalar).
    procedure, private            :: add_a_option_section                   !< Add an option (array).
    procedure, private            :: get_option_section                     !< Get option value (scalar).
    procedure, private            :: get_a_option_section                   !< Get option value (array).
    procedure, private, pass(lhs) :: assign_section                         !< Assignment overloading.
endtype section
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine free_section(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout) :: self !< Section data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sname)) deallocate(self%sname)
  if (allocated(self%options)) then
    call self%options%free
    deallocate(self%options)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_section

  elemental subroutine free_options_section(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free all options.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout) :: self !< Section data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    call self%options%free
    deallocate(self%options)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_options_section

  elemental subroutine free_option_section(self, option_name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free an option.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout) :: self        !< Section data.
  character(*),   intent(in)    :: option_name !< Option name.
  type(option), allocatable     :: options(:)  !< Temporary options array.
  integer(I4P)                  :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    o = self%index(option_name=option_name)
    if (o>0) then
      allocate(options(1:size(self%options, dim=1)-1))
      if (o==1) then
        options = self%options(2:)
      elseif (o==size(self%options, dim=1)) then
        options = self%options(:o-1)
      else
        options(:o-1) = self%options(:o-1)
        options(o:  ) = self%options(o+1:)
      endif
      call move_alloc(options, self%options)
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_option_section

  elemental subroutine parse_section_name(self, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get section name from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout) :: self     !< Section data.
  character(*),   intent(in)    :: source   !< String containing section data.
  integer(I4P),   intent(out)   :: error    !< Error code.
  integer(I4P)                  :: pos(1:2) !< Characters counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = err_section_name
  pos(1) = index(source, "[")
  pos(2) = index(source, "]")
  if (all(pos > 0)) then
    self%sname = trim(adjustl(source(pos(1)+1:pos(2)-1)))
    error = 0
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_section_name

  elemental subroutine parse_options_section(self, sep, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get section options from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout)       :: self       !< Section data.
  character(*),   intent(in)          :: sep        !< Separator of option name/value.
  character(*),   intent(in)          :: source     !< String containing section data.
  integer(I4P),   intent(out)         :: error      !< Error code.
  character(len(source))              :: osource    !< String containing options data.
  character(len(source)), allocatable :: options(:) !< Options strings tokenized.
  type(string), allocatable           :: tokens(:)  !< Options strings tokenized.
  character(len(source))              :: dummy      !< Dummy string for parsing options.
  type(string)                        :: dummy_str  !< Dummy string.
  integer(I4P)                        :: No         !< Counter.
  integer(I4P)                        :: o          !< Counter.
  integer(I4P)                        :: oo         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error = 0
  osource = trim(adjustl(source(index(source, "]")+1:)))
  ! to remove after StringiFor adoption
  dummy_str = osource
  call dummy_str%split(tokens=tokens, sep=new_line('a'))
  allocate(options(1:size(tokens, dim=1)))
  do o =1, size(tokens, dim=1)
    options(o) = tokens(o)%chars()
  enddo
  ! call tokenize(strin=osource, delimiter=new_line('A'), toks=options)
  ! to remove after StringiFor adoption
  No = 0
  o = 0
  do while (o+1<=size(options, dim=1))
    o = o + 1
    if (scan(adjustl(options(o)), comments) == 1) cycle
    if (index(options(o), sep)>0) then
      No = No + 1
      dummy = options(o)
      oo = o
      do while (oo+1<=size(options, dim=1))
        oo = oo + 1
        if (index(options(oo), sep)>0) then
          ! new option... go back
          exit
        else
          ! continuation of current option
          dummy = trim(adjustl(dummy))//" "//trim(adjustl(options(oo)))
          options(oo) = comments ! forcing skip this in the following scan
        endif
      enddo
      options(o) = trim(adjustl(dummy))
    endif
  enddo
  if (No>0) then
    if (allocated(self%options)) deallocate(self%options) ; allocate(self%options(1:No))
    o = 0
    oo = 0
    do while (o+1<=size(options, dim=1))
      o = o + 1
      if (scan(adjustl(options(o)), comments) == 1) cycle
      if (index(options(o), sep)>0) then
        oo = oo + 1
        call self%options(oo)%parse(sep=sep, source=options(o), error=error)
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_options_section

  elemental subroutine parse_section(self, sep, source, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Gett section data from a source string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section), intent(inout) :: self   !< Section data.
  character(*),   intent(in)    :: sep    !< Separator of option name/value.
  character(*),   intent(in)    :: source !< String containing section data.
  integer(I4P),   intent(out)   :: error  !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%parse_name(source=source, error=error)
  call self%parse_options(sep=sep, source=source, error=error)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_section

  elemental function count_values_option_section(self, option_name, delimiter) result(Nv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the number of values of option into section data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(in) :: self        !< Section data.
  character(*),           intent(in) :: option_name !< Option name.
  character(*), optional, intent(in) :: delimiter   !< Delimiter used for separating values.
  integer(I4P)                       :: Nv          !< Number of values.
  character(len=:), allocatable      :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                       :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    do o=1, size(self%options, dim=1)
      if (self%options(o)%oname == trim(adjustl(option_name))) then
        Nv = self%options(o)%count_values(delimiter=dlm)
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction count_values_option_section

  elemental function index_option_section(self, option_name, back) result(ind)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the index of the option matching the name passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),    intent(in) :: self        !< Section data.
  character(*),      intent(in) :: option_name !< Option name.
  logical, optional, intent(in) :: back        !< If back appears with the value true, the last matching index is returned.
  integer(I4P)                  :: ind         !< Index of searched section.
  logical                       :: backd       !< Dummy back flag.
  integer(I4P)                  :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ind = 0
  if (allocated(self%options)) then
    backd = .false. ; if (present(back)) backd = back
    if (backd) then
      do o=size(self%options, dim=1), 1,-1
        if (self%options(o)%oname == trim(adjustl(option_name))) then
          ind = o
          exit
        endif
      enddo
    else
      do o=1, size(self%options, dim=1)
        if (self%options(o)%oname == trim(adjustl(option_name))) then
          ind = o
          exit
        endif
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction index_option_section

  subroutine set_option_section(self, option_name, val, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set option value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val         !< Value.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o)%oname == trim(adjustl(option_name))) then
        call self%options(o)%set(val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_option_section

  subroutine set_a_option_section(self, option_name, val, delimiter, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set option value (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val(:)      !< Value.
  character(*), optional, intent(in)    :: delimiter   !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                          :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o)%oname == trim(adjustl(option_name))) then
        call self%options(o)%set(delimiter=dlm, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_a_option_section

  subroutine add_option_section(self, option_name, val, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add an option (with scalar value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val         !< Option value.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  type(option), allocatable             :: options(:)  !< Temporary options array.
  integer(I4P)                          :: errd        !< Error code.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(error=errd, option_name=option_name, val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options, dim=1)+1))
      options(1:size(self%options, dim=1)  ) = self%options
      options(  size(self%options, dim=1)+1) = option(oname=option_name)
      call move_alloc(options, self%options)
      call self%set(error=errd, option_name=option_name, val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1)%oname = option_name
    call self%set(error=errd, option_name=option_name, val=val)
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_option_section

  subroutine add_a_option_section(self, option_name, val, delimiter, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add an option (with array value).
  !<
  !< If the option already exists, its value is updated.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val(:)      !< Option value.
  character(*), optional, intent(in)    :: delimiter   !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  type(option), allocatable             :: options(:)  !< Temporary options array.
  integer(I4P)                          :: errd        !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(delimiter=dlm, error=errd, option_name=option_name, val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options, dim=1)+1))
      options(1:size(self%options, dim=1)  ) = self%options
      options(  size(self%options, dim=1)+1) = option(oname=option_name)
      call move_alloc(options, self%options)
      call self%set(error=errd, option_name=option_name, val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1)%oname = option_name
    call self%set(delimiter=dlm, error=errd, option_name=option_name, val=val)
  endif
  if (present(error)) error = errd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_a_option_section

  subroutine get_option_section(self, option_name, val, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get option value (scalar).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(in)    :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(inout) :: val         !< Value.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o)%oname == trim(adjustl(option_name))) then
        call self%options(o)%get(error=errd, val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_option_section

  subroutine get_a_option_section(self, option_name, val, delimiter, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting option value (array).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(in)    :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(inout) :: val(1:)     !< Value.
  character(*), optional, intent(in)    :: delimiter   !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o)%oname == trim(adjustl(option_name))) then
        call self%options(o)%get(delimiter=dlm, error=errd, val=val)
        if (present(error)) error = errd
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_a_option_section

  function loop_options_section(self, option_pairs) result(again)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Loop returning option name/value defined into section.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),                intent(in)  :: self            !< Section data.
  character(len=:), allocatable, intent(out) :: option_pairs(:) !< Couples option name/value [1:2].
  logical                                    :: again           !< Flag continuing the loop.
  integer(I4P), save                         :: o=0             !< Counter.
  integer(I4P)                               :: Nc              !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  again = .false.
  if (allocated(self%options)) then
    if (o==0) then
      o = lbound(self%options, dim=1)
      Nc = max(len(self%options(o)%oname), len(self%options(o)%ovals))
      allocate(character(Nc):: option_pairs(1:2))
      option_pairs(1) = self%options(o)%oname
      option_pairs(2) = self%options(o)%ovals
      again = .true.
    elseif (o<ubound(self%options, dim=1)) then
      o = o + 1
      Nc = max(len(self%options(o)%oname), len(self%options(o)%ovals))
      allocate(character(Nc):: option_pairs(1:2))
      option_pairs(1) = self%options(o)%oname
      option_pairs(2) = self%options(o)%ovals
      again = .true.
    else
      o = 0
      again = .false.
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction loop_options_section

  subroutine print_section(self, unit, retain_comments, pref, iostat, iomsg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print data with a pretty format.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(in)  :: self            !< Section data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  logical,                intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  character(*), optional, intent(in)  :: pref            !< Prefixing string.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  character(len=:), allocatable       :: prefd           !< Prefixing string.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  integer(I4P)                        :: o               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (allocated(self%sname)) write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      call self%options(o)%print(pref=prefd//'  ', iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_section

  subroutine save_section(self, unit, retain_comments, iostat, iomsg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Save data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(section),         intent(in)  :: self            !< Section data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  logical,                intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  integer(I4P)                        :: o               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%sname)) write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      call self%options(o)%save(iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_section

  elemental subroutine assign_section(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Assignment between two sections.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(section), intent(INOUT):: lhs !< Left hand side.
  type(section),  intent(IN)::    rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(rhs%sname)) lhs%sname = rhs%sname
  if (allocated(rhs%options)) then
    if (allocated(lhs%options)) deallocate(lhs%options) ; allocate(lhs%options(1:size(rhs%options, dim=1)))
    lhs%options = rhs%options
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_section
endmodule finer_section_t
