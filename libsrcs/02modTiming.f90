!> @brief Defines vars and procs for cpu timing of code sections
module Timing

   use Globals

   implicit none

   private

   type, public :: tim
      !> Actual value of wall-clock-time
      real(kind=double) :: wct = zero
      !> Actual value of user (cpu) time (summed across threads)
      real(kind=double) :: usr = zero
      !> Cumulated value of wall-clock-time
      real(kind=double) :: CUMwct = zero
      !> Cumulated value of user (cpu) time (summed across threads)
      real(kind=double) :: CUMusr = zero
      !> Status variable: both command and stutus
      !> can be: `start`, `stop`, `stac`, `accumulate` and `reset`.
      !> Can be abbreviated using the first 3 characters of the word.
      character(len=15) :: status
   contains
      !> Static constructor (public for type `tim`)
      procedure, public, pass :: init => tim_construct
      !> Static destructor (public for type `tim`)
      procedure, public, pass :: kill => tim_destroy
      !> Outputs the content of the variable (public for type `tim`)
      procedure, public, pass :: info => tim_print
      !> Sets the current `tim` vars (public for type `tim`)
      procedure, public, pass :: set => tim_set
   end type tim

contains

   !>-------------------------------------------------------------
   !> @brief Static constructor (procedure public for type `tim`)
   !> @details Instantiate (allocate if necessary) and initilize
   !> (by also reading from input file) variable of type `tim`.
   !>
   !> @param[out] this Object of type `tim` calling the constructor
   !<-------------------------------------------------------------
   subroutine tim_construct(this)
      implicit none
      class(tim), intent(out) :: this

      this%wct = zero
      this%usr = zero
      this%status = "start"
      this%CUMusr=zero
      this%CUMwct=zero

   end subroutine tim_construct

   !>-------------------------------------------------------------
   !> @brief Static destructor (procedure public for type `tim`).
   !<-----------------------------------------------------------
   subroutine Tim_destroy(this)
      implicit none
      class(tim), intent(inout) :: this

      this%wct = zero
      this%usr = zero
      this%CUMusr=zero
      this%CUMwct=zero

   end subroutine Tim_destroy

   !>-------------------------------------------------------------
   !> @brief Info procedure (public procedure for type `tim`).
   !> @details Prints content of a variable of type `tim`.
   !>
   !> @param[in] lun: integer. output unit
   !> @param[in] add_msg: character, optional.
   !>            additional message to be printed.
   !> @param[in] add_msg1: character, optional.
   !>            additional message to be printed.
   !> @param[in] add_msg2: character, optional.
   !>            additional message to be printed.
   !<-------------------------------------------------------------
   subroutine Tim_print(this, lun, add_msg, add_msg1, add_msg2)
      implicit none
      class(Tim), intent(in) :: this
      integer, intent(in) :: lun
      character(len=*), optional, intent(in) :: add_msg, add_msg1, add_msg2
      !local vars
      character(len=256) :: msg,msg1,msg2

      if (present(add_msg)) then
         msg=add_msg
      else
         msg=''
      end if

      if (present(add_msg1)) then
         msg1=add_msg1
      else
         msg1=''
      end if

      if (present(add_msg2)) then
         msg2=add_msg2
      else
         msg2=''
      end if

      write(lun,'(a,f10.2,a,f10.2,a)') trim(msg)//' WCT: ',this%CUMwct,&
         ' (s) '//trim(msg1)//'; USR: ',this%CUMusr,' (s) '//trim(msg2)

   end subroutine Tim_print

   !>-------------------------------------------------------------
   !> @brief Set procedure (public procedure for type `tim`).
   !> Sets the values of var of type `tim`.
   !>
   !> @param[in] code: character, optional.
   !>  Commands to start/stop/reset/accumulate stopwatch.
   !>  If the variable is not given, the value of `status` will be used.
   !>  Admissible values for code are two up to now are:
   !>   1. `start`: starts stopwatch.
   !>   2. `stop`: stops stopwatch and accumulates measured timing.
   !<-------------------------------------------------------------
   subroutine tim_set(this, code)
      implicit none
      class(Tim), intent(inout) :: this
      character(len=*), optional, intent(in) :: code
      ! local vars
      real(kind=double) :: time
      integer :: itime, tick_per_sec
      character(len=3) :: com

      call system_clock(itime,tick_per_sec)

      if (present(code)) then
         com=code(1:3)
      else
         select case (this%status(1:3))
         case ('sta')
            com='sta'
         case ('sto')
            com='sto'
         case default
            com='non'
         end select
      end if
      select case (com)
      case ('sta')
         !  initial setting
         call cpu_time(this%usr)
         call system_clock(itime)
         this%wct=dble(itime)/dble(tick_per_sec)
         this%status='sto'
      case ('sto')
         !  periodic measure
         call cpu_time(time)
         this%usr = time - this%usr
         call system_clock(itime)
         this%wct=dble(itime)/dble(tick_per_sec)-this%wct
         this%CUMusr=this%CUMusr+this%usr
         this%CUMwct=this%CUMwct+this%wct
         this%status='sta'
      case default
         this%usr=-999
         this%wct=-999
      end select

   end subroutine Tim_set

end module Timing
