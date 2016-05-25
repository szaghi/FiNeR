!< FiNeR, Fortran INI ParseR and generator.
module finer
!-----------------------------------------------------------------------------------------------------------------------------------
!< FiNeR, Fortran INI ParseR and generator.
!-----------------------------------------------------------------------------------------------------------------------------------
use finer_backend
use finer_file_ini_t
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: err_option_name
public :: err_option_vals
public :: err_option
public :: err_section_name
public :: err_section_options
public :: err_section
public :: err_source_missing
public :: file_ini
public :: file_ini_autotest
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule finer
