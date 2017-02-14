!< Section class definition.
module finer_section_t
!< Section class definition.
use finer_backend
use finer_option_t, only : option
use penf
use stringifor

implicit none
private
public :: section

type :: section
  !< Section data of file INI.
  private
  character(len=:), allocatable :: sname      !< Section name.
  type(option),     allocatable :: options(:) !< Section options.
  contains
    ! public methods
    generic               :: add => add_option, &   !< Add an option (scalar).
                                    add_a_option    !< Add an option (array).
    procedure, pass(self) :: count_values           !< Count option value(s).
    procedure, pass(self) :: free                   !< Free dynamic memory.
    procedure, pass(self) :: free_options           !< Free all options.
    procedure, pass(self) :: free_option            !< Free a option.
    generic               :: get => get_option, &   !< Get option value (scalar).
                                    get_a_option    !< Get option value (array).
    procedure, pass(self) :: has_options            !< Inquire if section has options.
    procedure, pass(self) :: index => index_option  !< Return the index of an option.
    procedure, pass(self) :: loop                   !< Loop over options.
    procedure, pass(self) :: max_chars_len          !< Return max len of option-name/values on all options.
    procedure, pass(self) :: name                   !< Return section name.
    procedure, pass(self) :: options_number         !< Return the options number.
    procedure, pass(self) :: option_pairs           !< Return an option pairs.
    procedure, pass(self) :: parse                  !< Parse section data.
    procedure, pass(self) :: print => print_section !< Pretty print data.
    generic               :: set => set_option, &   !< Set option value (scalar).
                                    set_a_option    !< Set option value (array).
    procedure, pass(self) :: save  => save_section  !< Save data.
    ! operators overloading
    generic :: assignment(=) => assign_section !< Assignment overloading.
    generic :: operator(==) => section_eq_string, &
                               section_eq_character !< Equal operator overloading.
    ! private methods
    procedure, private, pass(self) :: add_option      !< Add an option (scalar).
    procedure, private, pass(self) :: add_a_option    !< Add an option (array).
    procedure, private, pass(self) :: get_option      !< Get option value (scalar).
    procedure, private, pass(self) :: get_a_option    !< Get option value (array).
    procedure, private, pass(self) :: parse_name      !< Get section name.
    procedure, private, pass(self) :: parse_options   !< Get section options.
    procedure, private, nopass     :: sanitize_source !< Sanitize source.
    procedure, private, pass(self) :: set_option      !< Set option value (scalar).
    procedure, private, pass(self) :: set_a_option    !< Set option value (array).
    ! assignments
    procedure, private, pass(lhs) :: assign_section !< Assignment overloading.
    ! logical operators
    procedure, private, pass(lhs) :: section_eq_string    !< Equal to string logical operator.
    procedure, private, pass(lhs) :: section_eq_character !< Equal to character logical operator.
endtype section

interface section
  !< Overload `section` name with a function returning a new (itiliazed) section instance.
  module procedure new_section
endinterface section

contains
  ! public methods
  elemental function count_values(self, option_name, delimiter) result(Nv)
  !< Get the number of values of option into section data.
  class(section),         intent(in) :: self        !< Section data.
  character(*),           intent(in) :: option_name !< Option name.
  character(*), optional, intent(in) :: delimiter   !< Delimiter used for separating values.
  integer(I4P)                       :: Nv          !< Number of values.
  character(len=:), allocatable      :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                       :: o           !< Counter.

  if (allocated(self%options)) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    do o=1, size(self%options, dim=1)
      if (self%options(o) == trim(adjustl(option_name))) then
        Nv = self%options(o)%count_values(delimiter=dlm)
        exit
      endif
    enddo
  endif
  endfunction count_values

  elemental subroutine free(self)
  !< Free dynamic memory.
  class(section), intent(inout) :: self !< Section data.

  if (allocated(self%sname)) deallocate(self%sname)
  if (allocated(self%options)) then
    call self%options%free
    deallocate(self%options)
  endif
  endsubroutine free

  elemental subroutine free_options(self)
  !< Free all options.
  class(section), intent(inout) :: self !< Section data.

  if (allocated(self%options)) then
    call self%options%free
    deallocate(self%options)
  endif
  endsubroutine free_options

  elemental subroutine free_option(self, option_name)
  !< Free an option.
  class(section), intent(inout) :: self        !< Section data.
  character(*),   intent(in)    :: option_name !< Option name.
  type(option), allocatable     :: options(:)  !< Temporary options array.
  integer(I4P)                  :: o           !< Counter.

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
  endsubroutine free_option

  elemental function has_options(self)
  !< Inquire is section has options (at least one).
  class(section), intent(in) :: self        !< Section data.
  logical                    :: has_options !< Inquire result.

  has_options = allocated(self%options)
  endfunction has_options

  elemental function index_option(self, option_name, back) result(ind)
  !< Return the index of the option matching the name passed.
  !<
  !< @note The matching index returned is the first found if *back* is not passed or if *back=.false.*. On the contrary the last
  !< found is returned if *back=.true.*.
  class(section),    intent(in) :: self        !< Section data.
  character(*),      intent(in) :: option_name !< Option name.
  logical, optional, intent(in) :: back        !< If back appears with the value true, the last matching index is returned.
  integer(I4P)                  :: ind         !< Index of searched section.
  logical                       :: backd       !< Dummy back flag.
  integer(I4P)                  :: o           !< Counter.

  ind = 0
  if (allocated(self%options)) then
    backd = .false. ; if (present(back)) backd = back
    if (backd) then
      do o=size(self%options, dim=1), 1,-1
        if (self%options(o) == trim(adjustl(option_name))) then
          ind = o
          exit
        endif
      enddo
    else
      do o=1, size(self%options, dim=1)
        if (self%options(o) == trim(adjustl(option_name))) then
          ind = o
          exit
        endif
      enddo
    endif
  endif
  endfunction index_option

  function loop(self, option_pairs) result(again)
  !< Loop returning option name/value defined into section.
  class(section),                intent(in)  :: self            !< Section data.
  character(len=:), allocatable, intent(out) :: option_pairs(:) !< Couples option name/value [1:2].
  logical                                    :: again           !< Flag continuing the loop.
  integer(I4P), save                         :: o=0             !< Counter.

  again = .false.
  if (allocated(self%options)) then
    if (o==0) then
      o = lbound(self%options, dim=1)
      call self%options(o)%get_pairs(pairs=option_pairs)
      again = .true.
    elseif (o<ubound(self%options, dim=1)) then
      o = o + 1
      call self%options(o)%get_pairs(pairs=option_pairs)
      again = .true.
    else
      o = 0
      again = .false.
    endif
  endif
  endfunction loop

  elemental function max_chars_len(self)
  !< Return the maximum number of characters between option-name/option-values on all options.
  class(section), intent(in) :: self          !< Section data.
  integer                    :: max_chars_len !< Inquire result.
  integer(I4P)               :: o             !< Counter.

  max_chars_len = MinI4P
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      max_chars_len = max(max_chars_len, self%options(o)%name_len(), self%options(o)%values_len())
    enddo
  endif
  endfunction max_chars_len

  pure function name(self)
  !< Return section name.
  class(section), intent(in)     :: self !< Section data.
  character(len=len(self%sname)) :: name !< Section data.

  if (allocated(self%sname)) name = self%sname
  endfunction name

  pure subroutine option_pairs(self, option_index, pairs)
  !< Return an option pairs.
  class(section),                intent(in)  :: self         !< Option data.
  integer,                       intent(in)  :: option_index !< Option index.
  character(len=:), allocatable, intent(out) :: pairs(:)     !< Option name/values pairs.

  call self%options(option_index)%get_pairs(pairs=pairs)
  endsubroutine option_pairs

  elemental function options_number(self)
  !< Return the options number.
  class(section), intent(in) :: self           !< Section data.
  integer                    :: options_number !< Options number.

  if (allocated(self%options)) options_number = size(self%options, dim=1)
  endfunction options_number

  elemental subroutine parse(self, sep, source, error)
  !< Gett section data from a source string.
  class(section), intent(inout) :: self   !< Section data.
  character(*),   intent(in)    :: sep    !< Separator of option name/value.
  type(string),   intent(inout) :: source !< String containing section data.
  integer(I4P),   intent(out)   :: error  !< Error code.

  call self%sanitize_source(sep=sep, source=source, error=error)
  call self%parse_name(source=source, error=error)
  call self%parse_options(sep=sep, source=source, error=error)
  endsubroutine parse

  subroutine print_section(self, unit, retain_comments, pref, iostat, iomsg)
  !< Print data with a pretty format.
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

  prefd = '' ; if (present(pref)) prefd = pref
  if (allocated(self%sname)) write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      call self%options(o)%print(pref=prefd//'  ', iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  endsubroutine print_section

  subroutine save_section(self, unit, retain_comments, iostat, iomsg)
  !< Save data.
  class(section),         intent(in)  :: self            !< Section data.
  integer(I4P),           intent(in)  :: unit            !< Logic unit.
  logical,                intent(in)  :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P), optional, intent(out) :: iostat          !< IO error.
  character(*), optional, intent(out) :: iomsg           !< IO error message.
  integer(I4P)                        :: iostatd         !< IO error.
  character(500)                      :: iomsgd          !< Temporary variable for IO error message.
  integer(I4P)                        :: o               !< Counter.

  if (allocated(self%sname)) write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)'['//self%sname//']'
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      call self%options(o)%save(iostat=iostatd, iomsg=iomsgd, unit=unit, retain_comments=retain_comments)
    enddo
  endif
  if (present(iostat)) iostat = iostatd
  if (present(iomsg))  iomsg  = iomsgd
  endsubroutine save_section

  ! private methods
  pure subroutine add_option(self, option_name, val, error)
  !< Add an option (with scalar value).
  !<
  !< If the option already exists, its value is updated.
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val         !< Option value.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  type(option), allocatable             :: options(:)  !< Temporary options array.
  integer(I4P)                          :: errd        !< Error code.

  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(error=errd, option_name=option_name, val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options, dim=1)+1))
      options(1:size(self%options, dim=1)  ) = self%options
      options(  size(self%options, dim=1)+1) = option(option_name=option_name)
      call move_alloc(options, self%options)
      call self%set(error=errd, option_name=option_name, val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1) = option(option_name=option_name)
    call self%set(error=errd, option_name=option_name, val=val)
  endif
  if (present(error)) error = errd
  endsubroutine add_option

  pure subroutine add_a_option(self, option_name, val, delimiter, error)
  !< Add an option (with array value).
  !<
  !< If the option already exists, its value is updated.
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val(:)      !< Option value.
  character(*), optional, intent(in)    :: delimiter   !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  type(option), allocatable             :: options(:)  !< Temporary options array.
  integer(I4P)                          :: errd        !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.

  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    call self%set(delimiter=dlm, error=errd, option_name=option_name, val=val)
    if (errd/=0) then ! the option does not exist
      allocate(options(1:size(self%options, dim=1)+1))
      options(1:size(self%options, dim=1)  ) = self%options
      options(  size(self%options, dim=1)+1) = option(option_name=option_name)
      call move_alloc(options, self%options)
      call self%set(error=errd, option_name=option_name, val=val)
    endif
  else
    allocate(self%options(1:1))
    self%options(1) = option(option_name=option_name)
    call self%set(delimiter=dlm, error=errd, option_name=option_name, val=val)
  endif
  if (present(error)) error = errd
  endsubroutine add_a_option

  subroutine get_option(self, option_name, val, error)
  !< Get option value (scalar).
  class(section), intent(in)            :: self        !< Section data.
  character(*),   intent(in)            :: option_name !< Option name.
  class(*),       intent(inout)         :: val         !< Value.
  integer(I4P),   intent(out), optional :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.

  errd = ERR_OPTION
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o) == trim(adjustl(option_name))) then
        call self%options(o)%get(error=errd, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine get_option

  subroutine get_a_option(self, option_name, val, delimiter, error)
  !< Procedure for getting option value (array).
  class(section), intent(in)            :: self        !< Section data.
  character(*),   intent(in)            :: option_name !< Option name.
  class(*),       intent(inout)         :: val(1:)     !< Value.
  character(*),   intent(in),  optional :: delimiter   !< Delimiter used for separating values.
  integer(I4P),   intent(out), optional :: error       !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.

  errd = ERR_OPTION
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o) == trim(adjustl(option_name))) then
        call self%options(o)%get(delimiter=dlm, error=errd, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine get_a_option

  elemental subroutine parse_name(self, source, error)
  !< Get section name from a source string.
  class(section), intent(inout) :: self     !< Section data.
  type(string),   intent(in)    :: source   !< String containing section data.
  integer(I4P),   intent(out)   :: error    !< Error code.
  integer(I4P)                  :: pos(1:2) !< Characters counter.

  error = err_section_name
  pos(1) = index(source, "[")
  pos(2) = index(source, "]")
  if (all(pos > 0)) then
    self%sname = trim(adjustl(source%slice(pos(1)+1, pos(2)-1)))
    error = 0
  endif
  endsubroutine parse_name

  elemental subroutine parse_options(self, sep, source, error)
  !< Get section options from a source string.
  class(section), intent(inout) :: self      !< Section data.
  character(*),   intent(in)    :: sep       !< Separator of option name/value.
  type(string),   intent(inout) :: source    !< String containing section data.
  integer(I4P),   intent(out)   :: error     !< Error code.
  type(string), allocatable     :: tokens(:) !< Options strings tokenized.
  type(string)                  :: dummy     !< Dummy string for parsing options.
  integer(I4P)                  :: No        !< Counter.
  integer(I4P)                  :: o         !< Counter.
  integer(I4P)                  :: oo        !< Counter.

  error = 0
  source = trim(adjustl(source%slice(index(source, "]")+1, len(source))))
  No = source%count(substring=sep)
  if (No>0) then
    call source%split(tokens=tokens, sep=new_line('a'))
    if (allocated(self%options)) deallocate(self%options) ; allocate(self%options(1:No))
    o = 0
    oo = 0
    do while (o+1<=size(tokens, dim=1))
      o = o + 1
      if (index(tokens(o), sep)>0) then
        oo = oo + 1
        call self%options(oo)%parse(sep=sep, source=tokens(o), error=error)
      endif
    enddo
  endif
  endsubroutine parse_options

  elemental subroutine sanitize_source(sep, source, error)
  !< Sanitize source.
  !<
  !<+ Join splitted options;
  character(*),  intent(in)    :: sep       !< Separator of option name/value.
  type(string),  intent(inout) :: source    !< String containing option data.
  integer(I4P),  intent(out)   :: error     !< Error code.
  type(string),  allocatable   :: tokens(:) !< Source tokens.
  integer(I4P)                 :: o         !< Counter.

  call source%split(tokens=tokens, sep=new_line('a'))
  if (size(tokens, dim=1) > 1) then
    do o=2, size(tokens, dim=1)
      if (tokens(o)%index(substring=sep) == 0) tokens(o-1) = tokens(o-1)//' '//tokens(o)
    enddo
  endif
  source = ''
  do o=1, size(tokens, dim=1)
    if ((tokens(o)%index(substring=sep) > 0).or.&
        (tokens(o)%index(substring='[') > 0).or.&
        (tokens(o)%index(substring=']') > 0)) source = source//tokens(o)//new_line('a')
  enddo
  source = source%slice(1, len(source)-1)
  error = 0
  endsubroutine sanitize_source

  pure subroutine set_option(self, option_name, val, error)
  !< Set option value (scalar).
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val         !< Value.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  integer(I4P)                          :: o           !< Counter.

  errd = err_section_options
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o) == trim(adjustl(option_name))) then
        call self%options(o)%set(val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine set_option

  pure subroutine set_a_option(self, option_name, val, delimiter, error)
  !< Set option value (array).
  class(section),         intent(inout) :: self        !< Section data.
  character(*),           intent(in)    :: option_name !< Option name.
  class(*),               intent(in)    :: val(:)      !< Value.
  character(*), optional, intent(in)    :: delimiter   !< Delimiter used for separating values.
  integer(I4P), optional, intent(out)   :: error       !< Error code.
  integer(I4P)                          :: errd        !< Error code.
  character(len=:), allocatable         :: dlm         !< Dummy string for delimiter handling.
  integer(I4P)                          :: o           !< Counter.

  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = err_section_options
  if (allocated(self%options)) then
    do o=1, size(self%options, dim=1)
      if (self%options(o) == trim(adjustl(option_name))) then
        call self%options(o)%set(delimiter=dlm, val=val)
        exit
      endif
    enddo
  endif
  if (present(error)) error = errd
  endsubroutine set_a_option

  ! assignments
  elemental subroutine assign_section(lhs, rhs)
  !< Assignment between two sections.
  class(section), intent(INOUT):: lhs !< Left hand side.
  type(section),  intent(IN)::    rhs !< Rigth hand side.

  if (allocated(rhs%sname)) lhs%sname = rhs%sname
  if (allocated(rhs%options)) then
    if (allocated(lhs%options)) deallocate(lhs%options) ; allocate(lhs%options(1:size(rhs%options, dim=1)))
    lhs%options = rhs%options
  endif
  endsubroutine assign_section

  ! logical operators
  elemental function section_eq_string(lhs, rhs) result(is_it)
  !< Equal to string logical operator.
  class(section), intent(in) :: lhs   !< Left hand side.
  type(string),   intent(in) :: rhs   !< Right hand side.
  logical                    :: is_it !< Opreator test result.

  is_it = lhs%sname == rhs
  endfunction section_eq_string

  elemental function section_eq_character(lhs, rhs) result(is_it)
  !< Equal to character logical operator.
  class(section),            intent(in) :: lhs   !< Left hand side.
  character(kind=CK, len=*), intent(in) :: rhs   !< Right hand side.
  logical                               :: is_it !< Opreator test result.

  is_it = lhs%sname == rhs
  endfunction section_eq_character

  ! non TBP methods
  elemental function new_section(section_name)
  !< Return a new (initiliazed) section instance.
  character(*), intent(in), optional  :: section_name !< Option name.
  type(section)                       :: new_section  !< New (initiliazed) section instance.

  if (present(section_name)) new_section%sname = section_name
  endfunction new_section
endmodule finer_section_t
