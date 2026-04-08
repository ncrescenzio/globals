module JsonData

   use Globals
   use json_module

   implicit none

   private

   !>-------------------------------------------------------------
   !> @brief Derived data type to handle JSON data
   !> @details This type acts as a wrapper around json-fortran's `json_file` type.
   !> It simplifies the API and provides automatic error handling,
   !> initialization, and type-bound procedures for get/set operations.
   !<-------------------------------------------------------------
   type, public :: json_type
      private
      !> Internal json-fortran object representing the JSON database.
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

      generic, public :: kill => kill_json_type
      procedure, private :: kill_json_type
      final :: finalize_json_type
   end type json_type

contains

   !>--------------------------------------------------------------------
   !> @brief Static constructor for internal JSON core settings
   !> @details Sets the preferred configuration for json-fortran's core.
   !<---------------------------------------------------------------------
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

   !>--------------------------------------------------------------------
   !> @brief Checks for parsing/runtime errors and raises Fortran exception
   !> @details If an error has been registered within the `json_file` instance,
   !> this subroutine throws a standard Fortran `error stop` with the message.
   !<---------------------------------------------------------------------
   subroutine throw_error(this)
      implicit none
      class(json_type), intent(inout) :: this
      logical :: json_status
      character(len=:), allocatable :: error_msg

      call this%data%check_for_errors(json_status, error_msg)
      if (.not.json_status) error stop error_msg

   end subroutine throw_error

   !>--------------------------------------------------------------------
   !> @brief Initialize the object as an empty JSON file/database
   !<---------------------------------------------------------------------
   subroutine init_empty(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%kill()
      call this%init_core()

   end subroutine init_empty

   !>--------------------------------------------------------------------
   !> @brief Initialize the object from a JSON string payload
   !>
   !> @param[in   ] str: character string payload in JSON format
   !<---------------------------------------------------------------------
   subroutine init_string(this,str)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: str

      call this%init_empty()
      call this%data%deserialize(str)
      call this%throw_error()

   end subroutine init_string

   !>--------------------------------------------------------------------
   !> @brief Destroys the JSON object and deallocates internal variables
   !<---------------------------------------------------------------------
   subroutine kill_json_type(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%data%destroy(.true.)
      call this%throw_error()

   end subroutine kill_json_type

   !>--------------------------------------------------------------------
   !> @brief Finalization routine triggered implicitly when object goes
   !> out of scope or is destroyed
   !<---------------------------------------------------------------------
   subroutine finalize_json_type(this)
      implicit none
      type(json_type), intent(inout) :: this

      call this%kill()

   end subroutine finalize_json_type

   !>--------------------------------------------------------------------
   !> @brief Print the JSON object representation to standard output
   !<---------------------------------------------------------------------
   subroutine write_to_console(this)
      implicit none
      class(json_type), intent(inout) :: this

      call this%data%print()
      call this%throw_error()

   end subroutine write_to_console

   !>--------------------------------------------------------------------
   !> @brief Print the JSON object representation to a specific
   !> formatted Fortran unit
   !>
   !> @param[in   ] iunit: integer, connected Fortran file unit
   !<---------------------------------------------------------------------
   subroutine write_to_unit(this, iunit)
      implicit none
      class(json_type), intent(inout) :: this
      integer, intent(in) :: iunit

      call this%data%print(iunit)
      call this%throw_error()

   end subroutine write_to_unit

   !>--------------------------------------------------------------------
   !> @brief Save the JSON object representation directly to a file
   !>
   !> @param[in   ] filename: name of the output JSON file
   !<---------------------------------------------------------------------
   subroutine write_to_file(this, filename)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: filename

      call this%data%print(filename)
      call this%throw_error()

   end subroutine

   !>--------------------------------------------------------------------
   !> @brief Construct a JSON structure from a JSON file path
   !>
   !> @param[in   ] filename: path to the JSON file to parse
   !<---------------------------------------------------------------------
   subroutine load_file_name(this, filename)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: filename

      call this%data%load_file(filename, destroy_pointer=.true.)
      call this%throw_error()

   end subroutine load_file_name

   !>--------------------------------------------------------------------
   !> @brief Verify if a specific path exists within the JSON tree
   !>
   !> @param[in   ] path: valid JSONPath style or key path
   !> @return found: logical, true if path exists, false otherwise
   !<---------------------------------------------------------------------
   function check_path_is_valid(this, path) result(found)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical :: found

      found = this%data%valid_path(path)

   end function check_path_is_valid

   !>--------------------------------------------------------------------
   !> @brief Rename a node located at the specified path
   !>
   !> @param[in   ] path: path to the node to rename
   !> @param[in   ] new_name: new string identifier for the node
   !<---------------------------------------------------------------------
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

   !>--------------------------------------------------------------------
   !> @brief Remove a node located at the specified path entirely
   !>
   !> @param[in   ] path: path to the node to remove
   !<---------------------------------------------------------------------
   subroutine remove_path(this, path)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path

      call this%data%remove(path)
      call this%throw_error()

   end subroutine remove_path

   !>--------------------------------------------------------------------
   !> @brief Append a deeply copied instance of another JSON object at path
   !>
   !> @param[in   ] path: path where the new object will be inserted
   !> @param[inout] value: standard json_type object (content will be copied)
   !<---------------------------------------------------------------------
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

   !>--------------------------------------------------------------------
   !> @brief Setup a logical boolean value inside the JSON tree
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: target scalar array to map
   !<---------------------------------------------------------------------
   subroutine set_logical(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_logical

   !>--------------------------------------------------------------------
   !> @brief Setup an integer value inside the JSON tree
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: target scalar to map
   !<---------------------------------------------------------------------
   subroutine set_integer(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_integer

   !>--------------------------------------------------------------------
   !> @brief Setup a float literal (double precision) value into JSON tree
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: target scalar to map
   !<---------------------------------------------------------------------
   subroutine set_real(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_real

   !>--------------------------------------------------------------------
   !> @brief Setup a JSON string scalar property
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: character string to add
   !<---------------------------------------------------------------------
   subroutine set_string(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      character(len=*), intent(in) :: value

      call this%data%add(path, value, trim_str=.true., adjustl_str=.true.)
      call this%throw_error()

   end subroutine set_string

   !>--------------------------------------------------------------------
   !> @brief Adds an array dimension of logic variables linearly
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: logical dimension 1 vector mapping
   !<---------------------------------------------------------------------
   subroutine set_vec_logical(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_logical

   !>--------------------------------------------------------------------
   !> @brief Adds an array block size containing integers to current root
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: integer target dimensions to add
   !<---------------------------------------------------------------------
   subroutine set_vec_integer(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_integer

   !>--------------------------------------------------------------------
   !> @brief Adds an array literal values consisting of double precisions
   !>
   !> @param[in   ] path: target property path
   !> @param[in   ] value: sequence representing float point doubles
   !<---------------------------------------------------------------------
   subroutine set_vec_real(this, path, value)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), dimension(:), intent(in) :: value

      call this%data%add(path, value)
      call this%throw_error()

   end subroutine set_vec_real

   !>--------------------------------------------------------------------
   !> @brief Parses a subtree and clones its content into an alternative
   !> object variable
   !>
   !> @param[in   ] path: json path identifier to load the object hierarchy
   !> @param[inout] value: distinct `json_type` object constructed from
   !> that path copy
   !<---------------------------------------------------------------------
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
      call this%throw_error()

   end subroutine get_object

   !>--------------------------------------------------------------------
   !> @brief Retrive integer primitive from loaded property tree
   !>
   !> @param[in   ] path: integer json path target to retrieve
   !> @param[out  ] value: output mapped variable memory target
   !> @param[in   ] default: integer default backup variable if path
   !> does not exist
   !<---------------------------------------------------------------------
   subroutine get_integer(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(out) :: value
      integer, intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_integer

   !>--------------------------------------------------------------------
   !> @brief Retrieve floating point double sequence type variable
   !>
   !> @param[in   ] path: target search index representing object floating node
   !> @param[out  ] value: output double float mapped variable target
   !> @param[in   ] default: double precision backup variable definition
   !<---------------------------------------------------------------------
   subroutine get_real(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), intent(out) :: value
      real(kind=double), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_real

   !>--------------------------------------------------------------------
   !> @brief Get a logical expression defined inside properties list mapping
   !>
   !> @param[in   ] path: structural map defining where local var resides
   !> @param[out  ] value: mapping output variable allocation logic
   !> @param[in   ] default: literal substitution default sequence expression
   !<---------------------------------------------------------------------
   subroutine get_logical(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, intent(out) :: value
      logical, intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_logical

   !>--------------------------------------------------------------------
   !> @brief Retrieve string character definitions loaded dynamically via
   !> parsed map length format
   !>
   !> @param[in   ] path: mapping representation block identifier parameter
   !> @param[out  ] value: explicit allocation string containing sequence
   !> variable loaded
   !> @param[in   ] default: base case default string text returned as
   !> alternative definition structure
   !<---------------------------------------------------------------------
   subroutine get_string(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      character(len=:), allocatable, intent(out) :: value
      character(len=*), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_string

   !>--------------------------------------------------------------------
   !> @brief Parses target dynamically evaluated integer matrix structure
   !> hierarchy sequences
   !>
   !> @param[in   ] path: json-fortran query pointer representing valid
   !> location definitions array
   !> @param[out  ] value: parsed output list of dynamically typed integers map
   !> @param[in   ] default: alternative list sequence to bypass failing
   !> load tree elements logic structure
   !<---------------------------------------------------------------------
   subroutine get_vec_integer(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, dimension(:), allocatable, intent(out) :: value
      integer, dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_integer

   !>--------------------------------------------------------------------
   !> @brief Retrives continuous unmapped values sequence containing
   !> decimal arrays
   !>
   !> @param[in   ] path: standard map search component path lookup
   !> identifier map sequence
   !> @param[out  ] value: mapped data payload holding dynamic floating
   !> sequences size allocation
   !> @param[in   ] default: definition array fallback defaults representation
   !<---------------------------------------------------------------------
   subroutine get_vec_real(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      real(kind=double), dimension(:), allocatable, intent(out) :: value
      real(kind=double), dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_real

   !>--------------------------------------------------------------------
   !> @brief Returns memory structure block sequences representing
   !> dynamically stored logic array data
   !>
   !> @param[in   ] path: base location directory to find boolean maps vector
   !> @param[out  ] value: memory structure to assign found array boolean
   !> literals map dynamically
   !> @param[in   ] default: precomputed defaults mapping fallback sequence
   !> vector structure list
   !<---------------------------------------------------------------------
   subroutine get_vec_logical(this, path, value, default)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      logical, dimension(:), allocatable, intent(out) :: value
      logical, dimension(:), intent(in), optional :: default

      call this%data%get(path, value, default=default)
      call this%throw_error()

   end subroutine get_vec_logical

   !>--------------------------------------------------------------------
   !> @brief Modifies payload hierarchy to explicitly associate a JSON
   !> null block
   !>
   !> @param[in   ] path: JSON property where null literal representation
   !> is added mapping identifier sequence
   !<---------------------------------------------------------------------
   subroutine set_null_value(this, path)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path

      call this%data%add_null(path)
      call this%throw_error()

   end subroutine set_null_value

   !>--------------------------------------------------------------------
   !> @brief Evaluates an explicitly formatted sized array mapped block
   !> of JSON null components
   !>
   !> @param[in   ] path: directory sequence pointer map parameter
   !> reference string identifier
   !> @param[in   ] nvalues: amount of mapped identical logical NULL array
   !> sequence size representation vector
   !<---------------------------------------------------------------------
   subroutine set_vec_null_value(this, path, nvalues)
      implicit none
      class(json_type), intent(inout) :: this
      character(len=*), intent(in) :: path
      integer, intent(in) :: nvalues

      call this%data%add_null(path, nvalues)
      call this%throw_error()

   end subroutine set_vec_null_value

   !>--------------------------------------------------------------------
   !> @brief Prepares native assignments logic deeply copies mapping JSON
   !> representation states
   !>
   !> @param[in   ] other: representation data map wrapper struct source
   !> variable reference structure target
   !<---------------------------------------------------------------------
   subroutine assign_json_type(this, other)
      implicit none
      class(json_type), intent(out) :: this
      type(json_type), intent(in) :: other

      this%data = other%data

   end subroutine assign_json_type

end module JsonData
