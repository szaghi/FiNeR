!< Option class definition.
module finer_option_t
!< Option class definition.
use finer_backend
use penf
use stringifor, only : adjustl, index, scan, string

implicit none
private
public :: option
public :: assign_default

type :: option
  !< Option data of sections.
  private
  type(string) :: oname !< Option name.
  type(string) :: ovals !< Option values.
  type(string) :: ocomm !< Eventual option inline comment.
  contains
    ! public methods
    procedure, pass(self) :: count_values          !< Counting option value(s).
    procedure, pass(self) :: free                  !< Free dynamic memory.
    generic               :: get => get_option, &  !< Get option value (scalar).
                                    get_a_option   !< Get option value (array).
    procedure, pass(self) :: get_pairs             !< Return option name/values pairs.
    procedure, pass(self) :: get_string            !< Get option value as an allocatable string.
    procedure, pass(self) :: name_len              !< Return option name length.
    procedure, pass(self) :: parse                 !< Parse option data.
    procedure, pass(self) :: print => print_option !< Pretty print data.
    procedure, pass(self) :: save  => save_option  !< Save data.
    generic               :: set => set_option, &  !< Set option value (scalar).
                                    set_a_option   !< Set option value (array).
    procedure, pass(self) :: values_len            !< Return option values length.
    ! operators overloading
    generic :: assignment(=) => assign_option      !< Assignment overloading.
    generic :: operator(==) => option_eq_string, &
                               option_eq_character !< Equal operator overloading.
    ! private methods
    procedure, private, pass(self) :: get_option      !< Get option value (scalar).
    procedure, private, pass(self) :: get_a_option    !< Get option value (array).
    procedure, private, pass(self) :: parse_comment   !< Parse option inline comment.
    procedure, private, pass(self) :: parse_name      !< Parse option name.
    procedure, private, pass(self) :: parse_value     !< Parse option values.
    procedure, private, pass(self) :: set_option      !< Set option value (scalar).
    procedure, private, pass(self) :: set_a_option    !< Set option value (array).
    ! assignments
    procedure, private, pass(lhs) :: assign_option !< Assignment overloading.
    ! logical operators
    procedure, private, pass(lhs) :: option_eq_string    !< Equal to string logical operator.
    procedure, private, pass(lhs) :: option_eq_character !< Equal to character logical operator.
endtype option

interface integer_string
  !< Return the string representing an integer number, without the plus sign of positive numbers.
  module procedure integer_string_I8P, integer_string_I4P, integer_string_I2P, integer_string_I1P
endinterface integer_string

interface real_string
  !< Return the shortest string representing a real number that is read back exactly.
#ifdef _R16P
  module procedure real_string_R16P
#endif
  module procedure real_string_R8P, real_string_R4P
endinterface real_string

interface option
  !< Overload `option` name with a function returning a new (initiliazed) option instance.
  module procedure new_option
endinterface option

contains
  ! public methods
  elemental function count_values(self, delimiter) result(Nv)
  !< Get the number of values of option data.
  class(option), intent(in)           :: self      !< Option data.
  character(*),  intent(in), optional :: delimiter !< Delimiter used for separating values.
  character(len=:), allocatable       :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                        :: Nv        !< Number of values.

  if (self%ovals%is_allocated()) then
    dlm = ' ' ; if (present(delimiter)) dlm = delimiter
    if (is_complex_list(self%ovals%chars())) then
      Nv = self%ovals%count('(') ! complex values, e.g. (1.0, 2.0) (3.0, 4.0): the delimiter can be also inside them
    else
      Nv = self%ovals%count(dlm) + 1
    endif
  else
    Nv = 0
  endif
  endfunction count_values

  elemental subroutine free(self)
  !< Free dynamic memory.
  class(option), intent(inout) :: self !< Option data.

  call self%oname%free
  call self%ovals%free
  call self%ocomm%free
  endsubroutine free

  pure subroutine get_pairs(self, pairs)
  !< Return option name/values pairs.
  class(option),                 intent(in)  :: self     !< Option data.
  character(len=:), allocatable, intent(out) :: pairs(:) !< Option name/values pairs.
  integer(I4P)                               :: Nc       !< Counter.

  if (self%oname%is_allocated()) then
    Nc = max(self%oname%len(), self%ovals%len())
    allocate(character(Nc) :: pairs(1:2))
    pairs(1) = self%oname%chars()
    pairs(2) = self%ovals%chars()
  endif
  endsubroutine get_pairs

  subroutine get_string(self, val, error)
  !< Get option value as an allocatable string.
  !<
  !< `val` is (re)allocated with the length of the option value. If the option has no value, `val` is left unchanged and an
  !< error is returned.
  class(option),                 intent(in)            :: self  !< Option data.
  character(len=:), allocatable, intent(inout)         :: val   !< Value.
  integer(I4P),                  intent(out), optional :: error !< Error code.
  integer(I4P)                                         :: errd  !< Error code.

  errd = ERR_OPTION_VALS
  if (self%ovals%is_allocated()) then
    val = self%ovals%chars()
    errd = 0
  endif
  if (present(error)) error = errd
  endsubroutine get_string

  elemental function name_len(self) result(length)
  !< Return option name length.
  class(option), intent(in) :: self   !< Option data.
  integer                   :: length !< Option name length.

  length = 0
  if (self%oname%is_allocated()) length = self%oname%len()
  endfunction name_len

  elemental function values_len(self) result(length)
  !< Return option values length.
  class(option), intent(in) :: self   !< Option data.
  integer                   :: length !< Option values length.

  length = 0
  if (self%ovals%is_allocated()) length = self%ovals%len()
  endfunction values_len

  elemental subroutine parse(self, sep, source, error)
  !< Parse option data from a source string.
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  type(string),  intent(inout) :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.

  error = ERR_OPTION
  if (scan(adjustl(source), comments) == 1) return
  call self%parse_name(sep=sep, source=source, error=error)
  call self%parse_value(sep=sep, source=source, error=error)
  call self%parse_comment
  endsubroutine parse

  ! private methods
  subroutine get_option(self, val, error)
  !< for getting option data value (scalar).
  !<
  !< If the option value cannot be converted to the type of `val`, `val` is left unchanged and an error is returned.
  class(option), intent(in)            :: self   !< Option data.
  class(*),      intent(inout)         :: val    !< Value.
  integer(I4P),  intent(out), optional :: error  !< Error code.
  integer(I4P)                         :: errd   !< Error code.

  errd = ERR_OPTION_VALS
  if (self%ovals%is_allocated()) call convert(source=self%ovals%chars(), val=val, error=errd)
  if (present(error)) error = errd
  endsubroutine get_option

  subroutine get_a_option(self, val, delimiter, error)
  !< Get option data values (array).
  !<
  !< If `val` cannot hold all the values, or a value cannot be converted to the type of `val`, `val` is left unchanged and
  !< an error is returned.
  class(option), intent(in)            :: self      !< Option data.
  class(*),      intent(inout)         :: val(1:)   !< Value.
  character(*),  intent(in),  optional :: delimiter !< Delimiter used for separating values.
  integer(I4P),  intent(out), optional :: error     !< Error code.
  character(len=:), allocatable        :: dlm       !< Dummy string for delimiter handling.
  integer(I4P)                         :: Nv        !< Number of values.
  type(string), allocatable            :: valsV(:)  !< String array of values.
  integer(I4P)                         :: errd      !< Error code.
  integer(I4P)                         :: v         !< Counter.

  errd = ERR_OPTION_VALS
  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  if (self%ovals%is_allocated()) then
    if (is_complex(val)) then
      call split_complex(source=self%ovals%chars(), tokens=valsV)
    else
      call self%ovals%split(tokens=valsV, sep=dlm)
    endif
    Nv = size(valsV, dim=1)
    if (Nv > size(val, dim=1) .or. Nv == 0) then ! val cannot hold all values (or there are none): leave it untouched
      if (present(error)) error = errd
      return
    endif
    errd = 0
    do v=1, Nv ! check all values before modifying val
      call convert(source=valsV(v)%chars(), val=val(v), error=errd, check_only=.true.)
      if (errd /= 0) exit
    enddo
    if (errd == 0) then
      do v=1, Nv
        call convert(source=valsV(v)%chars(), val=val(v), error=errd)
      enddo
    endif
  endif
  if (present(error)) error = errd
  endsubroutine get_a_option

  elemental subroutine parse_comment(self)
  !< Parse option inline comment trimming it out from pure value string.
  class(option), intent(inout) :: self !< Option data.
  integer(I4P)                 :: pos  !< Characters counter.

  if (self%ovals%is_allocated()) then
    pos = self%ovals%index(INLINE_COMMENT)
    if (pos>0) then
      if (pos < self%ovals%len()) self%ocomm = trim(adjustl(self%ovals%slice(pos+1, self%ovals%len())))
      self%ovals = trim(adjustl(self%ovals%slice(1, pos-1)))
    endif
  endif
  endsubroutine parse_comment

  elemental subroutine parse_name(self, sep, source, error)
  !< Parse option name from a source string.
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  type(string),  intent(in)    :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.
  integer(I4P)                 :: pos    !< Characters counter.

  error = ERR_OPTION_NAME
  pos = index(source, sep)
  if (pos > 0) then
    self%oname = trim(adjustl(source%slice(1, pos-1)))
    error = 0
  endif
  endsubroutine parse_name

  elemental subroutine parse_value(self, sep, source, error)
  !< Parse option value from a source string.
  class(option), intent(inout) :: self   !< Option data.
  character(*),  intent(in)    :: sep    !< Separator of option name/value.
  type(string),  intent(in)    :: source !< String containing option data.
  integer(I4P),  intent(out)   :: error  !< Error code.
  integer(I4P)                 :: pos    !< Characters counter.

  error = ERR_OPTION_VALS
  pos = index(source, sep)
  if (pos > 0) then
    if (pos<source%len()) self%ovals = trim(adjustl(source%slice(pos+1, source%len())))
    error = 0
  endif
  endsubroutine parse_value

  subroutine print_option(self, unit, retain_comments, pref, iostat, iomsg)
  !< Print data with a pretty format.
  class(option), intent(in)            :: self            !< Option data.
  integer(I4P),  intent(in)            :: unit            !< Logic unit.
  logical,       intent(in)            :: retain_comments !< Flag for retaining eventual comments.
  character(*),  intent(in),  optional :: pref            !< Prefixing string.
  integer(I4P),  intent(out), optional :: iostat          !< IO error.
  character(*),  intent(out), optional :: iomsg           !< IO error message.
  character(len=:), allocatable        :: prefd           !< Prefixing string.
  integer(I4P)                         :: iostatd         !< IO error.
  character(500)                       :: iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable        :: comment         !< Eventual option comments.

  if (self%oname%is_allocated()) then
    prefd = '' ; if (present(pref)) prefd = pref
    comment = '' ; if (self%ocomm%is_allocated().and.retain_comments) comment = ' ; '//self%ocomm
    if (self%ovals%is_allocated()) then
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//self%oname//' = '//self%ovals//comment
    else
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)prefd//self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  endsubroutine print_option

  pure subroutine set_option(self, val, error)
  !< Set option data value (scalar).
  !<
  !< If the type of `val` is not supported the option value is left unchanged and an error is returned.
  class(option), intent(inout)         :: self  !< Option data.
  class(*),      intent(in)            :: val   !< Value.
  integer(I4P),  intent(out), optional :: error !< Error code.
  integer(I4P)                         :: errd  !< Error code.

  errd = 0
  select type(val)
#ifdef _R16P
  type is(real(R16P))
    self%ovals = real_string(val)
#endif
  type is(real(R8P))
    self%ovals = real_string(val)
  type is(real(R4P))
    self%ovals = real_string(val)
#ifdef _R16P
  type is(complex(R16P))
    self%ovals = '('//real_string(real(val))//','//real_string(aimag(val))//')'
#endif
  type is(complex(R8P))
    self%ovals = '('//real_string(real(val))//','//real_string(aimag(val))//')'
  type is(complex(R4P))
    self%ovals = '('//real_string(real(val))//','//real_string(aimag(val))//')'
  type is(integer(I8P))
    self%ovals = integer_string(val)
  type is(integer(I4P))
    self%ovals = integer_string(val)
  type is(integer(I2P))
    self%ovals = integer_string(val)
  type is(integer(I1P))
    self%ovals = integer_string(val)
  type is(logical)
    self%ovals = trim(str(n=val))
  type is(character(*))
    self%ovals = val
  class default
    errd = ERR_OPTION_VALS ! unsupported type
  endselect
  if (present(error)) error = errd
  endsubroutine set_option

  pure subroutine set_a_option(self, val, delimiter, error)
  !< Set option data value (array).
  !<
  !< If the type of `val` is not supported the option value is left unchanged and an error is returned.
  class(option), intent(inout)         :: self      !< Option data.
  class(*),      intent(in)            :: val(1:)   !< Value.
  character(*),  intent(in),  optional :: delimiter !< Delimiter used for separating values.
  integer(I4P),  intent(out), optional :: error     !< Error code.
  character(len=:), allocatable        :: dlm       !< Dummy string for delimiter handling.
  type(string)                         :: ovals     !< New option values.
  integer(I4P)                         :: errd      !< Error code.
  integer(I4P)                         :: v         !< Counter.

  dlm = ' ' ; if (present(delimiter)) dlm = delimiter
  errd = 0
  ovals = ''
  select type(val)
#ifdef _R16P
  type is(real(R16P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//real_string(val(v))
    enddo
    ovals = ovals%strip()
#endif
  type is(real(R8P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//real_string(val(v))
    enddo
    ovals = ovals%strip()
  type is(real(R4P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//real_string(val(v))
    enddo
    ovals = ovals%strip()
#ifdef _R16P
  type is(complex(R16P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//'('//real_string(real(val(v)))//','//real_string(aimag(val(v)))//')'
    enddo
    ovals = ovals%strip()
#endif
  type is(complex(R8P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//'('//real_string(real(val(v)))//','//real_string(aimag(val(v)))//')'
    enddo
    ovals = ovals%strip()
  type is(complex(R4P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//'('//real_string(real(val(v)))//','//real_string(aimag(val(v)))//')'
    enddo
    ovals = ovals%strip()
  type is(integer(I8P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//integer_string(val(v))
    enddo
    ovals = ovals%strip()
  type is(integer(I4P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//integer_string(val(v))
    enddo
    ovals = ovals%strip()
  type is(integer(I2P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//integer_string(val(v))
    enddo
    ovals = ovals%strip()
  type is(integer(I1P))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//integer_string(val(v))
    enddo
    ovals = ovals%strip()
  type is(logical)
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//trim(str(n=val(v)))
    enddo
    ovals = ovals%strip()
  type is(character(*))
    do v=1, size(val, dim=1)
      ovals = ovals//dlm//trim(val(v))
    enddo
    ovals = ovals%strip()
  class default
    errd = ERR_OPTION_VALS ! unsupported type
  endselect
  if (errd == 0) self%ovals = ovals
  if (present(error)) error = errd
  endsubroutine set_a_option

  subroutine save_option(self, unit, retain_comments, iostat, iomsg)
  !< Save data.
  class(option), intent(in)            :: self            !< Option data.
  integer(I4P),  intent(in)            :: unit            !< Logic unit.
  logical,       intent(in)            :: retain_comments !< Flag for retaining eventual comments.
  integer(I4P),  intent(out), optional :: iostat          !< IO error.
  character(*),  intent(out), optional :: iomsg           !< IO error message.
  integer(I4P)                         :: iostatd         !< IO error.
  character(500)                       :: iomsgd          !< Temporary variable for IO error message.
  character(len=:), allocatable        :: comment         !< Eventual option comments.

  if (self%oname%is_allocated()) then
    comment = '' ; if (self%ocomm%is_allocated().and.retain_comments) comment = ' ; '//self%ocomm
    if (self%ovals%is_allocated()) then
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)self%oname//' = '//self%ovals//comment
    else
      write(unit=unit, fmt='(A)', iostat=iostatd, iomsg=iomsgd)self%oname//' = '//comment
    endif
    if (present(iostat)) iostat = iostatd
    if (present(iomsg))  iomsg  = iomsgd
  endif
  endsubroutine save_option

  ! assignments
  elemental subroutine assign_option(lhs, rhs)
  !< Assignment between two options.
  class(option), intent(inout) :: lhs !< Left hand side.
  type(option),  intent(in)    :: rhs !< Rigth hand side.

  call lhs%free
  if (rhs%oname%is_allocated()) lhs%oname = rhs%oname
  if (rhs%ovals%is_allocated()) lhs%ovals = rhs%ovals
  if (rhs%ocomm%is_allocated()) lhs%ocomm = rhs%ocomm
  endsubroutine assign_option

  ! logical operators
  elemental function option_eq_string(lhs, rhs) result(is_it)
  !< Equal to string logical operator.
  class(option), intent(in) :: lhs   !< Left hand side.
  type(string),  intent(in) :: rhs   !< Right hand side.
  logical                   :: is_it !< Opreator test result.

  is_it = lhs%oname == rhs
  endfunction option_eq_string

  elemental function option_eq_character(lhs, rhs) result(is_it)
  !< Equal to character logical operator.
  class(option),             intent(in) :: lhs   !< Left hand side.
  character(kind=CK, len=*), intent(in) :: rhs   !< Right hand side.
  logical                               :: is_it !< Opreator test result.

  is_it = lhs%oname == rhs
  endfunction option_eq_character

  ! non TBP methods
  subroutine assign_default(val, default, error)
  !< Assign a default value to a value of one of the supported types.
  !<
  !< A complex `val` accepts a complex, real or integer default of any supported kind.
  !< A real `val` accepts a real or integer default of any supported kind, an integer `val` accepts an integer default of any
  !< supported kind (that it can represent), a logical or character `val` accepts a default of the same type. For any other
  !< combination `val` is left unchanged and an error is returned.
  class(*),     intent(inout) :: val     !< Value.
  class(*),     intent(in)    :: default !< Default value.
  integer(I4P), intent(out)   :: error   !< Error code.
#ifdef _R16P
  integer, parameter          :: RKP=R16P !< Kind of the widest supported real.
#else
  integer, parameter          :: RKP=R8P  !< Kind of the widest supported real.
#endif
  integer, parameter          :: IS_NONE=0, IS_INTEGER=1, IS_REAL=2, IS_LOGICAL=3, IS_COMPLEX=4 !< Default value types.
  integer                     :: dtype   !< Type of the default value.
  integer(I8P)                :: di      !< Integer default value.
  real(RKP)                   :: dr      !< Real default value.
  logical                     :: dl      !< Logical default value.
  complex(RKP)                :: dz      !< Complex default value.

  dtype = IS_NONE
  select type(default)
#ifdef _R16P
  type is(complex(R16P))
    dz = default ; dtype = IS_COMPLEX
#endif
  type is(complex(R8P))
    dz = default ; dtype = IS_COMPLEX
  type is(complex(R4P))
    dz = default ; dtype = IS_COMPLEX
#ifdef _R16P
  type is(real(R16P))
    dr = default ; dtype = IS_REAL
#endif
  type is(real(R8P))
    dr = default ; dtype = IS_REAL
  type is(real(R4P))
    dr = default ; dtype = IS_REAL
  type is(integer(I8P))
    di = default ; dtype = IS_INTEGER
  type is(integer(I4P))
    di = default ; dtype = IS_INTEGER
#ifndef _NVF
  type is(integer(I2P))
    di = default ; dtype = IS_INTEGER
#endif
  type is(integer(I1P))
    di = default ; dtype = IS_INTEGER
  type is(logical)
    dl = default ; dtype = IS_LOGICAL
  endselect
  if (dtype == IS_INTEGER) dr = real(di, kind=RKP)
  if (dtype == IS_REAL .or. dtype == IS_INTEGER) dz = cmplx(dr, 0._RKP, kind=RKP)

  error = ERR_OPTION_VALS
  select type(val)
#ifdef _R16P
  type is(complex(R16P))
    if (dtype == IS_COMPLEX .or. dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = dz ; error = 0
    endif
#endif
  type is(complex(R8P))
    if (dtype == IS_COMPLEX .or. dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = cmplx(dz, kind=R8P) ; error = 0
    endif
  type is(complex(R4P))
    if (dtype == IS_COMPLEX .or. dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = cmplx(dz, kind=R4P) ; error = 0
    endif
#ifdef _R16P
  type is(real(R16P))
    if (dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = dr ; error = 0
    endif
#endif
  type is(real(R8P))
    if (dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = real(dr, kind=R8P) ; error = 0
    endif
  type is(real(R4P))
    if (dtype == IS_REAL .or. dtype == IS_INTEGER) then
      val = real(dr, kind=R4P) ; error = 0
    endif
  type is(integer(I8P))
    if (dtype == IS_INTEGER) then
      val = di ; error = 0
    endif
  type is(integer(I4P))
    if (dtype == IS_INTEGER) then
      if (abs(di) <= int(huge(val), kind=I8P)) then
        val = int(di, kind=I4P) ; error = 0
      endif
    endif
#ifndef _NVF
  type is(integer(I2P))
    if (dtype == IS_INTEGER) then
      if (abs(di) <= int(huge(val), kind=I8P)) then
        val = int(di, kind=I2P) ; error = 0
      endif
    endif
#endif
  type is(integer(I1P))
    if (dtype == IS_INTEGER) then
      if (abs(di) <= int(huge(val), kind=I8P)) then
        val = int(di, kind=I1P) ; error = 0
      endif
    endif
  type is(logical)
    if (dtype == IS_LOGICAL) then
      val = dl ; error = 0
    endif
  type is(character(*))
    select type(default)
    type is(character(*))
      val = default ; error = 0
    endselect
  endselect
  endsubroutine assign_default

  subroutine convert(source, val, error, check_only)
  !< Convert a string into a value of one of the supported types.
  !<
  !< If the string cannot be converted, or the type of `val` is not supported, `val` is left unchanged and an error is returned.
  character(*), intent(in)           :: source     !< String to be converted.
  class(*),     intent(inout)        :: val        !< Value.
  integer(I4P), intent(out)          :: error      !< Error code.
  logical,      intent(in), optional :: check_only !< Check the conversion without modifying `val`.
  logical                            :: assign     !< Flag for assigning the converted value to `val`.
  integer                            :: ios        !< IO status of the conversion.
#ifdef _R16P
  real(R16P)                         :: r16        !< Converted value.
#endif
  real(R8P)                          :: r8         !< Converted value.
  real(R4P)                          :: r4         !< Converted value.
  integer(I8P)                       :: i8         !< Converted value.
  integer(I4P)                       :: i4         !< Converted value.
#ifndef _NVF
  integer(I2P)                       :: i2         !< Converted value.
#endif
  integer(I1P)                       :: i1         !< Converted value.
  logical                            :: l          !< Converted value.
#ifdef _R16P
  real(R16P)                         :: r16i       !< Converted value, imaginary part.
#endif
  real(R8P)                          :: r8i        !< Converted value, imaginary part.
  real(R4P)                          :: r4i        !< Converted value, imaginary part.
  character(len=:), allocatable      :: re         !< Real part of a complex value.
  character(len=:), allocatable      :: im         !< Imaginary part of a complex value.

  assign = .true. ; if (present(check_only)) assign = .not.check_only
  ios = 1
  select type(val)
#ifdef _R16P
  type is(real(R16P))
    if (is_numeric(source)) read(source, *, iostat=ios) r16
    if (ios == 0 .and. assign) val = r16
#endif
  type is(real(R8P))
    if (is_numeric(source)) read(source, *, iostat=ios) r8
    if (ios == 0 .and. assign) val = r8
  type is(real(R4P))
    if (is_numeric(source)) read(source, *, iostat=ios) r4
    if (ios == 0 .and. assign) val = r4
  type is(integer(I8P))
    if (is_numeric(source)) read(source, *, iostat=ios) i8
    if (ios == 0 .and. assign) val = i8
  type is(integer(I4P))
    if (is_numeric(source)) read(source, *, iostat=ios) i4
    if (ios == 0 .and. assign) val = i4
#ifndef _NVF
  type is(integer(I2P))
    if (is_numeric(source)) read(source, *, iostat=ios) i2
    if (ios == 0 .and. assign) val = i2
#endif
  type is(integer(I1P))
    if (is_numeric(source)) read(source, *, iostat=ios) i1
    if (ios == 0 .and. assign) val = i1
#ifdef _R16P
  type is(complex(R16P))
    if (split_parts(source)) read(re, *, iostat=ios) r16
    if (ios == 0) read(im, *, iostat=ios) r16i
    if (ios == 0 .and. assign) val = cmplx(r16, r16i, kind=R16P)
#endif
  type is(complex(R8P))
    if (split_parts(source)) read(re, *, iostat=ios) r8
    if (ios == 0) read(im, *, iostat=ios) r8i
    if (ios == 0 .and. assign) val = cmplx(r8, r8i, kind=R8P)
  type is(complex(R4P))
    if (split_parts(source)) read(re, *, iostat=ios) r4
    if (ios == 0) read(im, *, iostat=ios) r4i
    if (ios == 0 .and. assign) val = cmplx(r4, r4i, kind=R4P)
  type is(logical)
    if (verify(source(1:min(1, len(source))), '.tTfF') == 0) read(source, *, iostat=ios) l
    if (ios == 0 .and. assign) val = l
  type is(character(*))
    ios = 0
    if (assign) val = source
  endselect
  error = 0 ; if (ios /= 0) error = ERR_OPTION_VALS
  contains
    pure function is_numeric(string)
    !< Return true if the string contains only characters allowed in numbers.
    !<
    !< The list-directed read accepts (without error) also separators, null values and repeat counts, e.g. `/`, `,` and `2*3`,
    !< that must not be considered valid numbers.
    character(*), intent(in) :: string     !< String to be checked.
    logical                  :: is_numeric !< Check result.

    is_numeric = len_trim(string) > 0 .and. verify(string, ' 0123456789+-.eEdDqQ') == 0
    endfunction is_numeric

    function split_parts(string)
    !< Split a complex value in the Fortran notation, `(re,im)`, into its real and imaginary parts.
    !<
    !< Return false, leaving the parts undefined, if the string is not a complex value.
    character(*), intent(in)      :: string      !< String to be split.
    logical                       :: split_parts !< True if the string is a complex value.
    character(len=:), allocatable :: buffer      !< String without leading and trailing blanks.
    integer                       :: c           !< Position of the comma.
    integer                       :: n           !< Length of the string.

    split_parts = .false.
    buffer = trim(adjustl(string))
    n = len(buffer)
    if (n < 5) return
    if (buffer(1:1) /= '(' .or. buffer(n:n) /= ')') return
    c = index(buffer, ',')
    if (c < 3 .or. c > n-2 .or. index(buffer, ',', back=.true.) /= c) return
    re = buffer(2:c-1)
    im = buffer(c+1:n-1)
    split_parts = is_numeric(re) .and. is_numeric(im)
    endfunction split_parts
  endsubroutine convert

  pure function integer_string_I8P(n) result(string)
  !< Return the string representing an integer number (I8P), without the plus sign of positive numbers.
  integer(I8P), intent(in)     :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  character(24)                 :: buffer !< Buffer for the conversion.

  write(buffer, '(I0)') n
  string = trim(buffer)
  endfunction integer_string_I8P

  pure function integer_string_I4P(n) result(string)
  !< Return the string representing an integer number (I4P), without the plus sign of positive numbers.
  integer(I4P), intent(in)     :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  character(24)                 :: buffer !< Buffer for the conversion.

  write(buffer, '(I0)') n
  string = trim(buffer)
  endfunction integer_string_I4P

  pure function integer_string_I2P(n) result(string)
  !< Return the string representing an integer number (I2P), without the plus sign of positive numbers.
  integer(I2P), intent(in)     :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  character(24)                 :: buffer !< Buffer for the conversion.

  write(buffer, '(I0)') n
  string = trim(buffer)
  endfunction integer_string_I2P

  pure function integer_string_I1P(n) result(string)
  !< Return the string representing an integer number (I1P), without the plus sign of positive numbers.
  integer(I1P), intent(in)     :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  character(24)                 :: buffer !< Buffer for the conversion.

  write(buffer, '(I0)') n
  string = trim(buffer)
  endfunction integer_string_I1P

  pure function is_complex(val)
  !< Return true if the values are of complex type.
  class(*), intent(in) :: val(1:)    !< Values.
  logical              :: is_complex !< Check result.

  is_complex = .false.
  select type(val)
#ifdef _R16P
  type is(complex(R16P))
    is_complex = .true.
#endif
  type is(complex(R8P))
    is_complex = .true.
  type is(complex(R4P))
    is_complex = .true.
  endselect
  endfunction is_complex

  pure function is_complex_list(source)
  !< Return true if the string is a list of complex values in the Fortran notation, e.g. `(1.0, 2.0) (3.0, 4.0)`.
  character(*), intent(in) :: source          !< String to be checked.
  logical                  :: is_complex_list !< Check result.
  integer                  :: first           !< Position of the first non blank character.
  integer                  :: last            !< Position of the last non blank character.

  first = verify(source, ' ')
  last = len_trim(source)
  is_complex_list = .false.
  if (first > 0 .and. last > first) is_complex_list = source(first:first) == '(' .and. source(last:last) == ')'
  endfunction is_complex_list

  pure subroutine split_complex(source, tokens)
  !< Split a list of complex values in the Fortran notation, e.g. `(1.0, 2.0) (3.0, 4.0)`, into its values.
  !<
  !< Whatever is between the values is ignored: the delimiter can be also inside the values.
  character(*),              intent(in)  :: source    !< String to be split.
  type(string), allocatable, intent(out) :: tokens(:) !< Complex values.
  integer                                :: Nt        !< Number of values.
  integer                                :: pass      !< Passes counter: the first counts, the second stores.
  integer                                :: b         !< Position of the beginning of a value.
  integer                                :: e         !< Position of the end of a value.

  do pass=1, 2
    Nt = 0
    e = 0
    do
      b = index(source(e+1:), '(')
      if (b == 0) exit
      b = b + e
      e = index(source(b:), ')')
      if (e == 0) exit
      e = e + b - 1
      Nt = Nt + 1
      if (pass == 2) tokens(Nt) = source(b:e)
    enddo
    if (pass == 1) allocate(tokens(1:Nt))
  enddo
  endsubroutine split_complex

#ifdef _R16P
  pure function real_string_R16P(n) result(string)
  !< Return the shortest string representing a real number (R16P) that is read back exactly.
  real(R16P), intent(in)        :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  integer, parameter            :: MAX_DIGITS = 36 !< Significant digits always sufficient for an exact read back.
  character(MAX_DIGITS+8)       :: buffer !< Buffer for the conversions.
  character(16)                 :: frm    !< Format of the conversion.
  real(R16P)                    :: check  !< Number read back.
  integer                       :: d      !< Significant digits counter.
  integer                       :: ios    !< IO status.

  do d=1, MAX_DIGITS
    write(frm, '(A,I0,A,I0,A)') '(ES', d+8, '.', d-1, 'E4)'
    write(buffer, frm) n
    read(buffer, *, iostat=ios) check
    if (ios == 0 .and. check == n) exit
  enddo
  string = tidy_real_string(trim(adjustl(buffer)))
  endfunction real_string_R16P

#endif
  pure function real_string_R8P(n) result(string)
  !< Return the shortest string representing a real number (R8P) that is read back exactly.
  real(R8P), intent(in)        :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  integer, parameter            :: MAX_DIGITS = 17 !< Significant digits always sufficient for an exact read back.
  character(MAX_DIGITS+8)       :: buffer !< Buffer for the conversions.
  character(16)                 :: frm    !< Format of the conversion.
  real(R8P)                    :: check  !< Number read back.
  integer                       :: d      !< Significant digits counter.
  integer                       :: ios    !< IO status.

  do d=1, MAX_DIGITS
    write(frm, '(A,I0,A,I0,A)') '(ES', d+8, '.', d-1, 'E4)'
    write(buffer, frm) n
    read(buffer, *, iostat=ios) check
    if (ios == 0 .and. check == n) exit
  enddo
  string = tidy_real_string(trim(adjustl(buffer)))
  endfunction real_string_R8P

  pure function real_string_R4P(n) result(string)
  !< Return the shortest string representing a real number (R4P) that is read back exactly.
  real(R4P), intent(in)        :: n      !< Number.
  character(len=:), allocatable :: string !< String representing the number.
  integer, parameter            :: MAX_DIGITS = 9 !< Significant digits always sufficient for an exact read back.
  character(MAX_DIGITS+8)       :: buffer !< Buffer for the conversions.
  character(16)                 :: frm    !< Format of the conversion.
  real(R4P)                    :: check  !< Number read back.
  integer                       :: d      !< Significant digits counter.
  integer                       :: ios    !< IO status.

  do d=1, MAX_DIGITS
    write(frm, '(A,I0,A,I0,A)') '(ES', d+8, '.', d-1, 'E4)'
    write(buffer, frm) n
    read(buffer, *, iostat=ios) check
    if (ios == 0 .and. check == n) exit
  enddo
  string = tidy_real_string(trim(adjustl(buffer)))
  endfunction real_string_R4P

  pure function tidy_real_string(source) result(string)
  !< Tidy a string representing a real number in scientific notation, e.g. `-3.21E+0001` becomes `-32.1`.
  !<
  !< The plain decimal notation is used for decimal exponents in [-5, 15], the scientific one otherwise, e.g. `1.0E+20`.
  !< Not finite numbers (NaN, Infinity) are left unchanged.
  character(*), intent(in)      :: source   !< String representing the number in scientific notation.
  character(len=:), allocatable :: string   !< Tidy string.
  character(len=:), allocatable :: digits   !< Significant digits.
  character(len=:), allocatable :: sgn      !< Sign.
  character(8)                  :: buffer   !< Buffer for the exponent conversion.
  integer                       :: epos     !< Position of the exponent.
  integer                       :: expnt    !< Decimal exponent.
  integer                       :: nd       !< Number of significant digits.
  integer                       :: ios      !< IO status.

  string = source
  epos = scan(source, 'E')
  if (epos < 2 .or. verify(source, '+-.0123456789E') /= 0) return ! not a finite number
  read(source(epos+1:), *, iostat=ios) expnt
  if (ios /= 0) return
  sgn = '' ; if (source(1:1) == '-') sgn = '-'
  digits = source(verify(source, '+-'):epos-1)
  digits = digits(1:1)//digits(3:)                   ! remove the decimal point
  nd = max(1, verify(digits, '0', back=.true.))      ! remove the trailing zeros
  digits = digits(1:nd)
  if (expnt >= -5 .and. expnt <= 15) then
    if (expnt < 0) then
      string = sgn//'0.'//repeat('0', -expnt-1)//digits
    elseif (expnt >= nd-1) then
      string = sgn//digits//repeat('0', expnt-nd+1)//'.0'
    else
      string = sgn//digits(1:expnt+1)//'.'//digits(expnt+2:)
    endif
  else
    if (nd == 1) digits = digits//'0'
    write(buffer, '(SP,I0)') expnt
    string = sgn//digits(1:1)//'.'//digits(2:)//'E'//trim(buffer)
  endif
  endfunction tidy_real_string

  elemental function new_option(option_name, option_values, option_comment)
  !< Return a new (initiliazed) option instance.
  character(*), intent(in), optional :: option_name    !< Option name.
  character(*), intent(in), optional :: option_values  !< Option values.
  character(*), intent(in), optional :: option_comment !< Option comment.
  type(option)                       :: new_option     !< New (initiliazed) option instance.

  if (present(option_name   )) new_option%oname = option_name
  if (present(option_values )) new_option%ovals = option_values
  if (present(option_comment)) new_option%ocomm = option_comment
  endfunction new_option
endmodule finer_option_t
