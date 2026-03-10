program tester

   use, intrinsic :: iso_fortran_env, only : error_unit, output_unit, dp => real64
   use jsondata

   implicit none

   type(json_type) :: json, other
   integer :: int_variable
   real(dp) :: r_variable
   logical :: l_variable
   character(len=:), allocatable :: c_variable
   integer, dimension(:), allocatable :: i_vec_variable
   logical, dimension(:), allocatable :: l_vec_variable
   real(dp), dimension(:), allocatable :: r_vec_variable

   ! initialize empty
   call json%init()
   ! initialize from text
   call json%init('{ "foo" : 2, "bar" : "baz", "key" : [true,"foo",3] }')
   ! call json%write()

   ! load from file
   call json%load_file('jsontest.json')

   ! write to console
   ! call json%write()
   ! write to unit
   ! open(file="testoutput.json",unit=999)
   ! call json%write(999)
   ! close(999)
   ! write to file
   ! call json%write("outputjson.json")

   ! write(*,*) "Json contains 'foo'?", json%has("foo")
   ! write(*,*) "Json contains 'foo'?", json%has("key")
   ! write(*,*) "Json contains 'foo'?", json%has("bar.baz")

   call json%set("ping.pong", .false.)
   call json%set("ping.pung", [.true.,.true.,.false.])
   call json%set("int", 2)
   call json%set("ping.pung(1)", 1.5d0)
   call json%set("ping.pung(2)", "foo")
   call json%set("ping.pung(6)", "prova")
   call json%rename("ping.pung","newname")
   call json%set("logicalvec", [.true.,.true.,.true.])
   call json%set("realvec", [0.0d0, 1.0d0, 2.0d0, 3.0d0])
   ! call json%write()

   call json%get("ping.pong", int_variable, default=1) ! without "default", error is printed
   call json%get("ping.newname(1)", r_variable, default=0.0_dp) ! without "default", error is printed
   call json%get("ping.pong", l_variable, default=.true.)
   call json%get("ping.newname(2)", c_variable)
   call json%get("foovector", i_vec_variable, default=[0,0,0,0,0])
   call json%get("logicalvec", l_vec_variable, default=[.true.,.false.])
   call json%get("realvec", r_vec_variable, default=[0.10d0, 1.0d0])

   ! write(*,*) "real variable: ", r_variable
   ! write(*,*) "logical variable: ", l_variable
   ! write(*,*) "string variable: ", c_variable
   ! write(*,*) "integer vector variable: ", i_vec_variable
   ! write(*,*) "logical vector variable: ", l_vec_variable

   other = json
   ! call other%write()

   other = '{ "int" : 1, "real" : 2.0, "logical" : true, "array" : [1, true, null] }'
   ! call other%write()

   call json%set('newkeyobject',other)
   ! call json%write()

   call json%get("ping", other)
   ! call other%write()
   ! call json%write()

   ! call json%kill()
   ! call other%write()
   ! call other%kill()
   ! call json%write()

end program
