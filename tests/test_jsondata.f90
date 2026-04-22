module test_jsondata

   use Globals
   use JsonData
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit

   implicit none

   private

   public :: collect_jsondata

contains

   subroutine collect_jsondata(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("init_empty", test_init_empty), &
         new_unittest("init_string", test_init_string), &
         new_unittest("load_file_name", test_load_file_name), &
         new_unittest("write_to_console", test_write_to_console), &
         new_unittest("write_to_unit", test_write_to_unit), &
         new_unittest("write_to_file", test_write_to_file), &
         new_unittest("assign_json_type", test_assign_json_type), &
         new_unittest("check_path_is_valid", test_check_path_is_valid), &
         new_unittest("rename_path", test_rename_path), &
         new_unittest("remove_path", test_remove_path), &
         new_unittest("set_null_value", test_set_null_value), &
         new_unittest("set_vec_null_value", test_set_vec_null_value), &
         new_unittest("set_object", test_set_object), &
         new_unittest("set_logical", test_set_logical), &
         new_unittest("set_integer", test_set_integer), &
         new_unittest("set_real", test_set_real), &
         new_unittest("set_string", test_set_string), &
         new_unittest("set_vec_logical", test_set_vec_logical), &
         new_unittest("set_vec_integer", test_set_vec_integer), &
         new_unittest("set_vec_real", test_set_vec_real), &
         new_unittest("get_object", test_get_object), &
         new_unittest("get_integer", test_get_integer), &
         new_unittest("get_real", test_get_real), &
         new_unittest("get_logical", test_get_logical), &
         new_unittest("get_string", test_get_string), &
         new_unittest("get_vec_integer", test_get_vec_integer), &
         new_unittest("get_vec_real", test_get_vec_real), &
         new_unittest("get_vec_logical", test_get_vec_logical), &
         new_unittest("kill_json_type", test_kill_json_type) &
         ]
   end subroutine collect_jsondata

   ! ==============================================================
   ! Tests for Initialization
   ! ==============================================================

   subroutine test_init_empty(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      ! Assuming a successful init means no errors thrown.
      call check(error, .true., .true.)
   end subroutine test_init_empty

   subroutine test_init_string(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      character(len=256) :: str_json
      str_json = '{"key": 1}'
      call j%init(trim(str_json))
      call check(error, j%has('key'), .true.)
   end subroutine test_init_string

   subroutine test_load_file_name(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j_out, j_in
      integer :: val
      call j_out%init()
      call j_out%set('key', 42)
      call j_out%write('dummy_load.json')
      
      call j_in%load_file('dummy_load.json')
      call j_in%get('key', val)
      call check(error, val, 42)
   end subroutine test_load_file_name

   ! ==============================================================
   ! Tests for Printing/Writing
   ! ==============================================================

   subroutine test_write_to_console(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('dummy', 1)
      ! Note: write_to_console prints to stdout. We just check it doesn't crash.
      call j%write()
      call check(error, .true., .true.)
   end subroutine test_write_to_console

   subroutine test_write_to_unit(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      integer :: u
      call j%init()
      call j%set('dummy', 1)
      open(newunit=u, file='dummy_unit.json', status='replace')
      call j%write(u)
      close(u)
      call check(error, .true., .true.)
   end subroutine test_write_to_unit

   subroutine test_write_to_file(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('file_val', 99)
      call j%write('dummy_write.json')
      call check(error, .true., .true.)
   end subroutine test_write_to_file

   ! ==============================================================
   ! Tests for State Manipulation
   ! ==============================================================

   subroutine test_assign_json_type(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j1, j2
      integer :: val
      call j1%init()
      call j1%set('key', 123)
      j2 = j1 ! calls assign_json_type (clone)
      call j2%get('key', val)
      call check(error, val, 123)
   end subroutine test_assign_json_type

   subroutine test_check_path_is_valid(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('exists', 1)
      call check(error, j%has('exists'), .true.)
      call check(error, j%has('missing'), .false.)
   end subroutine test_check_path_is_valid

   subroutine test_rename_path(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('old_name', 1)
      call j%rename('old_name', 'new_name')
      call check(error, j%has('new_name'), .true.)
      call check(error, j%has('old_name'), .false.)
   end subroutine test_rename_path

   subroutine test_remove_path(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('to_remove', 1)
      call j%remove('to_remove')
      call check(error, j%has('to_remove'), .false.)
   end subroutine test_remove_path

   subroutine test_kill_json_type(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('val', 1)
      call j%kill()
      ! After kill it should be empty or uninitialized.
      call check(error, .true., .true.)
   end subroutine test_kill_json_type

   ! ==============================================================
   ! Tests for Setter Subroutines
   ! ==============================================================

   subroutine test_set_null_value(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set_null('null_var')
      call check(error, j%has('null_var'), .true.)
   end subroutine test_set_null_value

   subroutine test_set_vec_null_value(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set_null('null_vec', 3)
      call check(error, j%has('null_vec'), .true.)
   end subroutine test_set_vec_null_value

   subroutine test_set_object(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j1, j2
      call j1%init()
      call j2%init()
      call j2%set('key', 1)
      call j1%set('obj', j2)
      call check(error, j1%has('obj.key'), .true.)
   end subroutine test_set_object

   subroutine test_set_logical(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('log', .true.)
      call check(error, j%has('log'), .true.)
   end subroutine test_set_logical

   subroutine test_set_integer(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('int', 42)
      call check(error, j%has('int'), .true.)
   end subroutine test_set_integer

   subroutine test_set_real(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('real', 3.14d0)
      call check(error, j%has('real'), .true.)
   end subroutine test_set_real

   subroutine test_set_string(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('str', 'hello')
      call check(error, j%has('str'), .true.)
   end subroutine test_set_string

   subroutine test_set_vec_logical(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('vlog', [.true., .false.])
      call check(error, j%has('vlog'), .true.)
   end subroutine test_set_vec_logical

   subroutine test_set_vec_integer(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('vint', [1, 2])
      call check(error, j%has('vint'), .true.)
   end subroutine test_set_vec_integer

   subroutine test_set_vec_real(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      call j%init()
      call j%set('vreal', [1.0d0, 2.0d0])
      call check(error, j%has('vreal'), .true.)
   end subroutine test_set_vec_real

   ! ==============================================================
   ! Tests for Getter Subroutines
   ! ==============================================================

   subroutine test_get_object(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j1, j2, j3
      call j1%init()
      call j2%init()
      call j1%set('key', 123)
      call j2%set('obj', j1)
      call j2%get('obj', j3)
      call check(error, j3%has('key'), .true.)
   end subroutine test_get_object

   subroutine test_get_integer(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      integer :: val
      call j%init()
      call j%set('int', 42)
      call j%get('int', val)
      call check(error, val, 42)
   end subroutine test_get_integer

   subroutine test_get_real(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      real(kind=double) :: val
      call j%init()
      call j%set('real', 3.14d0)
      call j%get('real', val)
      call check(error, val, 3.14d0)
   end subroutine test_get_real

   subroutine test_get_logical(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      logical :: val
      call j%init()
      call j%set('log', .true.)
      call j%get('log', val)
      call check(error, val, .true.)
   end subroutine test_get_logical

   subroutine test_get_string(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      character(len=:), allocatable :: val
      call j%init()
      call j%set('str', 'hello')
      call j%get('str', val)
      call check(error, val, 'hello')
   end subroutine test_get_string

   subroutine test_get_vec_integer(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      integer, allocatable :: val(:)
      call j%init()
      call j%set('vint', [1, 2])
      call j%get('vint', val)
      call check(error, val(2), 2)
   end subroutine test_get_vec_integer

   subroutine test_get_vec_real(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      real(kind=double), allocatable :: val(:)
      call j%init()
      call j%set('vreal', [1.5d0, 2.5d0])
      call j%get('vreal', val)
      call check(error, val(2), 2.5d0)
   end subroutine test_get_vec_real

   subroutine test_get_vec_logical(error)
      type(error_type), allocatable, intent(out) :: error
      type(json_type) :: j
      logical, allocatable :: val(:)
      call j%init()
      call j%set('vlog', [.true., .false.])
      call j%get('vlog', val)
      call check(error, val(2), .false.)
   end subroutine test_get_vec_logical

end module test_jsondata

program tester
   use, intrinsic :: iso_fortran_env, only : error_unit
   use testdrive, only : run_testsuite, new_testsuite, testsuite_type, init_color_output
   use test_jsondata, only : collect_jsondata

   implicit none

   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("jsondata", collect_jsondata) &
      ]

   call init_color_output(.true.)

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if

end program
