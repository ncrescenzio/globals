!> Defines vars and procs for cpu timing of code sections
module Timing
  use Globals
  implicit none

  private
  type, public :: Tim 
     !> actual value of wall-clock-time
     real(kind=double) :: wct = zero
     !> actual value of user (cpu) time (summed across threads)
     real(kind=double) :: usr = zero
     !> cumulated value of wall-clock-time
     real(kind=double) :: CUMwct = zero
     !> cumulated value of user (cpu) time (summed across threads)
     real(kind=double) :: CUMusr = zero
     !> status variable: both command and stutus
     !> can be: "start" "stop" "stac" "accumulate" "reset"
     !> can be abbreviated using the first 3 characters of the word
     character(len=15) :: status
   contains
     !> static constructor
     !> (public for type Tim)
     procedure, public, pass :: init => Tim_construct
     !> static destructor
     !> (public for type Tim)
     procedure, public, pass :: kill => Tim_destroy
     !> outputs the content of the variable
     !> (public for type Tim)
     procedure, public, pass :: info => Tim_print
     !> sets the current Tim vars
     !> (public for type Tim)
     procedure, public, pass :: set => Tim_set
  end type Tim
contains

  !>-------------------------------------------------------------
  !> Static constructor.
  !> (procedure public for type Tim)
  !> Instantiate (allocate if necessary)
  !> and initilize (by also reading from input file)
  !> variable of type Tim
  !>
  !> usage:
  !>     call 'var'%init()
  !>
  !<-------------------------------------------------------------
  subroutine Tim_construct(this)
    use Globals
    implicit none
    !vars
    class(Tim), intent(out) :: this
    ! local vars

    this%wct = zero
    this%usr = zero
    this%status = "start"
    this%CUMusr=zero
    this%CUMwct=zero

  end subroutine Tim_construct

  !>-------------------------------------------------------------
  !> Static destructor.
  !> (procedure public for type Tim)
  !> only scalars, does nothing
  !>
  !> usage:
  !>     call 'var'%kill(lun)
  !>
  !> where:
  !> \param[in] lun -> integer. I/O unit for error message output
  !<-----------------------------------------------------------
  subroutine Tim_destroy(this)
    use Globals
    implicit none
    class(Tim), intent(inout) :: this

    this%wct = zero
    this%usr = zero
    this%CUMusr=zero
    this%CUMwct=zero
    
  end subroutine Tim_destroy

  !>-------------------------------------------------------------
  !> Info procedure.
  !> (public procedure for type Tim)
  !> Prints content of a variable of type Tim
  !> 
  !> usage:
  !>     call 'var'%info(lun [, add_msg])
  !>
  !> where:
  !> \param[in] lun: integer. output unit
  !> \param[in] add_msg: character, optional.
  !>            additional message to be printed.
  !>
  !<-------------------------------------------------------------
  subroutine Tim_print(this, lun, add_msg, add_msg1, add_msg2)
    use Globals
    implicit none
    ! vars
    class(Tim), intent(in) :: this
    integer, intent(in) :: lun
    character(len=*), optional, intent(in) :: add_msg,add_msg1,add_msg2
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
  !> Set procedure.
  !> (public procedure for type Tim)
  !> sets the values of var of type Tim
  !> if 
  !> 
  !> usage:
  !>     call 'var'%set([code])
  !>
  !> where:
  !> \param[in] code: character, optional.
  !>            commands to start/stop/reset/accumulate stopwatch
  !>
  !> admissible values for code are two up to now::
  !>
  !> 1) 'start' : starts stopwatch
  !> 2) 'stop'  : stops stopwatch and accumulates measured timing
  !>
  !> if code is not given, we assume code=start/stop alternatively
  !>
  !> all commands are abbreviated at the first 3 characters
  !> (the remaining characters are ignored)
  !>
  !<-------------------------------------------------------------
  subroutine Tim_set(this, code)
    use Globals
    implicit none
    
    Class(Tim), intent(inout) :: this
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
