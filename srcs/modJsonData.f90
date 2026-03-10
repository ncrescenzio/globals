module JsonData

   use Globals
   use json_module

   implicit none

   private

   type, public :: json_type
      private
      type(json_file) :: data
   contains
      procedure, private :: init_core
      procedure, private :: throw_error

      generic, public :: init => init_empty, init_string
      procedure, private :: init_empty
      procedure, private :: init_string

      generic, public :: load_file => load_file_name
      procedure, private :: load_file_name

      generic, public :: write => write_to_console, write_to_unit, write_to_file
      procedure, private :: write_to_console
      procedure, private :: write_to_unit
      procedure, private :: write_to_file

      generic, public :: clone => assign_json_type
      procedure, private :: assign_json_type

      generic, public :: has => check_path_is_valid
      procedure, private :: check_path_is_valid

      generic, public :: rename => rename_path
      procedure, private :: rename_path

      generic, public :: remove => remove_path
      procedure, private :: remove_path

      generic, public :: set_null => set_null_value, set_vec_null_value
      procedure, private :: set_null_value
      procedure, private :: set_vec_null_value

      generic, public :: set => set_logical, set_integer, set_real, set_string, set_object
      procedure, private :: set_object
      procedure, private :: set_logical
      procedure, private :: set_integer
      procedure, private :: set_real
      procedure, private :: set_string

      generic, public :: set => set_vec_logical, set_vec_integer, set_vec_real
      procedure, private :: set_vec_logical
      procedure, private :: set_vec_integer
      procedure, private :: set_vec_real

      generic, public :: get => get_integer, get_real, get_logical, get_string, get_object
      procedure, private :: get_object
      procedure, private :: get_integer
      procedure, private :: get_real
      procedure, private :: get_logical
      procedure, private :: get_string

      generic, public :: get => get_vec_integer, get_vec_real, get_vec_logical
      procedure, private :: get_vec_integer
      procedure, private :: get_vec_real
      procedure, private :: get_vec_logical

      generic, public :: assignment(=) => assign_json_type, init_string

      procedure, public, pass :: kill
      final :: finalize_json_type
   end type json_type

contains

   subroutine init_core(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%data%initialize(&
         compact_reals=.true.,&
         verbose=.false.,& ! if true, a lot of warnings are printed
         strict_type_checking=.true.,&
         compress_vectors=.true.,&
         stop_on_error=.false.,& ! error handling inside this module
         allow_duplicate_keys=.false.&
         )

   end subroutine init_core

   subroutine throw_error(this)
      implicit none
      class(json_type), intent(inout) :: this
      logical :: json_status
      character(len=:), allocatable :: error_msg

      call this%data%check_for_errors(json_status, error_msg)
      if (.not.json_status) error stop error_msg

   end subroutine throw_error

   subroutine init_empty(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%kill()
      call this%init_core()

   end subroutine init_empty

   subroutine init_string(this,str)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: str

      call this%init_empty()
      call this%data%deserialize(str)
      call this%throw_error()

   end subroutine init_string

   subroutine kill(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%data%destroy(.true.)
      call this%throw_error()

   end subroutine kill

   subroutine finalize_json_type(this)
      implicit none
      type(json_type), intent(inout) :: this

      call this%kill()

   end subroutine finalize_json_type

   subroutine write_to_console(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%data%print()
      call this%throw_error()

   end subroutine write_to_console

   subroutine write_to_unit(this, iunit)
      implicit none
      class(json_type), intent(inout) :: this
      integer, intent(in) :: iunit

      call this%data%print(iunit)
      call this%throw_error()

   end subroutine write_to_unit

   subroutine write_to_file(this, filename)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: filename

      call this%data%print(filename)
      call this%throw_error()

   end subroutine

   subroutine load_file_name(this, filename)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: filename

      call this%data%load_file(filename, destroy_pointer=.true.)
      call this%throw_error()

   end subroutine load_file_name

   function check_path_is_valid(this, path) result(found)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical :: found

      found = this%data%valid_path(path)

   end function check_path_is_valid

   subroutine rename_path(this, path, new_name)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: new_name
      logical :: found
      character(len=256) :: error_msg

      call this%data%rename(path, new_name, found)
      call this%throw_error()
      if (.not.found) then
         write(error_msg,*) "Key '", path, "' not found!"
         ERROR STOP error_msg
      end if

   end subroutine rename_path

   subroutine remove_path(this, path)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path

      call this%data%remove(path)
      call this%throw_error()

   end subroutine remove_path

   subroutine set_object(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      type(json_type), intent(inout) :: value
      type(json_value), pointer :: p, pclone
      type(json_core) :: core

      call this%data%get_core(core)
      call value%data%get(p)
      call core%clone(p,pclone)
      call this%data%add(path, pclone)
      call this%throw_error()

   end subroutine set_object

   subroutine set_logical(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_logical

   subroutine set_integer(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_integer

   subroutine set_real(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_real

   subroutine set_string(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: value

      call this%data%add(path, value, trim_str=.true., adjustl_str=.true.)
      call this%throw_error()

   end subroutine set_string

   subroutine set_vec_logical(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_logical

   subroutine set_vec_integer(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_integer

   subroutine set_vec_real(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_real

   subroutine get_object(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      type(json_type), intent(inout) :: value
      type(json_value), pointer :: p, pclone
      type(json_core) :: core

      call this%data%get(path, p)
      call this%data%get_core(core)
      call core%clone(p, pclone)
      call value%init_empty()
      call value%data%add(pclone, destroy_original=.false.)

   end subroutine get_object

   subroutine get_integer(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(out) :: value
      integer, intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_integer

   subroutine get_real(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), intent(out) :: value
      real(kind=double), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_real

   subroutine get_logical(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, intent(out) :: value
      logical, intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_logical

   subroutine get_string(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: value
      character(len=*), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_string

   subroutine get_vec_integer(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, dimension(:), allocatable, intent(out) :: value
      integer, dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_integer

   subroutine get_vec_real(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), dimension(:), allocatable, intent(out) :: value
      real(kind=double), dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_real

   subroutine get_vec_logical(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, dimension(:), allocatable, intent(out) :: value
      logical, dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_logical

   subroutine set_null_value(this, path)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path

      call this%data%add_null(path)
      call this%throw_error()

   end subroutine set_null_value

   subroutine set_vec_null_value(this, path, nvalues)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(in) :: nvalues

      call this%data%add_null(path, nvalues)
      call this%throw_error()

   end subroutine set_vec_null_value

   subroutine assign_json_type(this, other)
      implicit none
      class(json_type), intent(out) :: this
      type(json_type), intent(in) :: other

      call this%init_empty()
      this%data = other%data

   end subroutine assign_json_type

end module JsonData
