module test_kinddeclaration

   use KindDeclaration
   use testdrive, only : new_unittest, unittest_type, error_type, check

   implicit none

   private

   public :: collect_kinddeclaration

contains

   ! Export test suit, i.e. a collection of unit tests
   subroutine collect_kinddeclaration(testsuite)

      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("variable_small", test_variable_small) &
         ]

   end subroutine collect_kinddeclaration

   subroutine test_variable_small(error)

      type(error_type), allocatable, intent(out) :: error

      call check(error, one, 1.0d0)

   end subroutine test_variable_small

end module test_kinddeclaration

program tester

   use, intrinsic :: iso_fortran_env, only : error_unit
   use testdrive, only : run_testsuite, new_testsuite, testsuite_type
   use test_kinddeclaration, only : collect_kinddeclaration

   implicit none

   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("kinddeclaration", collect_kinddeclaration) &
      ]

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if

end program
