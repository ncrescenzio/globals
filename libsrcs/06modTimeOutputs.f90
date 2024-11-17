module TimeOutputs

   use Globals

   implicit none

   private

   !> structure variable containing vars for variable in time input data
   !> (e.g., forcing functions, exponents, k(x,t)...)
   !> up to no, only forcing function input
   type, public :: TDOut
      !> O-1 flag if is initialized
      logical :: built
      !> Dimension of real array
      integer :: dimdata
      !> Dimension of real array
      integer :: ndata
      !> Dimension (Ndata)
      !> Actual time values
      real(kind=double), allocatable :: TDactual(:,:)
      !> Time of evaluated TDactual
      real(kind=double) :: time
      !> SteadyTD=.true. : boundary conditions do not change during time
      !> SteadyTD=.false. : boundary conditions do    change during time
      logical :: steadyTD
      !> steadyTD_written=.true. : boundary conditions do not change during time
      !> SteadyTD_written=.false. : boundary conditions do    change during time
      logical :: steadyTD_written=.false.
   contains
      !> static constructor
      !> (procedure public for type TDOut)
      procedure, public, pass :: init => init_TD
      !> static destructor
      !> (procedure public for type TDOut)
      procedure, public, pass :: kill => kill_TD
      !> Info procedure.
      !> (public procedure for type TDOut)
      procedure, public, pass :: info => info_TD
      !> Writing procedure.
      !> (public procedure for type TDOut)
      procedure, public, pass :: write2dat => write_TD
      !> Writing procedure.
      !> (public procedure for type TDOut)
      procedure, public, pass :: write_end_time
      !!$     !> Set (read if necessary) boundary values
      !!$     procedure, public, pass :: set => set_TD
      !> Set number of non zero element
      procedure, public, pass :: eval_ninput
   end type TDOut

contains

   !>-------------------------------------------------------------
   !> Static constructor.
   !> (procedure public for type TDOut)
   !> Instantiate (allocate)
   !> and initilize (by also reading from input file)
   !> variable of type TDOut
   !>
   !> usage:
   !>     call 'var'%init(IOfiles, InputFdescr, Ndata)
   !>
   !> where:
   !> \param[in] IOfiles -> type(IOfdescr) I/O file information
   !> \param[in] InputFdescr -> type(file) I/O file information for
   !>                           input variable
   !> \param[in] Ndata -> number of data to be read in
   !>
   !<-------------------------------------------------------------------
   subroutine init_TD(this, stderr, dimdata, Ndata)
      implicit none
      !vars
      class(TDOut), intent(out) :: this
      integer,      intent(in ) :: stderr
      integer,      intent(in ) :: dimdata
      integer,      intent(in ) :: Ndata
      ! local vars
      integer :: res
      logical :: rc

      this%built=.true.

      this%dimdata  = dimdata
      this%ndata    = ndata

      allocate(this%TDactual(dimdata,Ndata),stat=res)
      if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
         '  type TDOut member TDactual (array)',res)
      this%TDactual=zero

      this%steadyTD=.false.

   end subroutine init_TD

   !>-------------------------------------------------------------
   !> Static destructor.
   !> (procedure public for type TDOut)
   !> deallocate all arrays for a var of type TDOut
   !>
   !> usage:
   !>     call 'var'%kill(lun)
   !>
   !> where:
   !> \param[in] lun -> integer. I/O unit for error message output.
   !<-----------------------------------------------------------
   subroutine kill_TD(this, lun)
      implicit none
      ! vars
      class(TDOut), intent(inout):: this
      integer, intent(in) :: lun
      ! local vars
      integer :: res
      logical :: rc

      this%built=.false.
      deallocate(this%TDactual)
      if (res.ne.0) rc=IOerr(lun, err_dealloc, 'kill_TD', &
         'type TDOut var TDactual')

   end subroutine kill_TD

   !>-------------------------------------------------------------
   !> Info procedure.
   !> (public procedure for type TDOut)
   !> Prints content of a variable of type TDOut
   !>
   !> usage:
   !>     call 'var'%info(lun,nsample)
   !>
   !> where:
   !> \param[in] lun -> integer. I/O unit for error message output.
   !> \param[in] lun -> integer. Number of first no zero values
   !<-------------------------------------------------------------
   subroutine info_TD(this, lun, nsample)
      implicit none
      ! vars
      class(TDOut), intent(in) :: this
      integer,         intent(in) :: lun
      integer,         intent(in) :: nsample

      ! local vars
      integer :: i,j,k
      real(kind=double) :: dnrm2

      write(lun,*) ' '
      write(lun,*) ' Info: TDOut structure definition:'

      write(lun,*) 'dimension data', this%dimdata
      write(lun,*) 'ndata         ', this%ndata
      if( this%steadyTD       ) write(lun,*) ' Steady state'
      if( .not. this%steadyTD ) write(lun,*) ' Not in steady state'
      write(lun,*) ' Actual val '
      i=0
      j=0
      do while ( (i .lt. this%ndata) .and. (j .lt. nsample) )
      i=i+1
      if( dnrm2(this%dimdata,this%TDactual(:,i),1).ne.zero ) then
         j=j+1
         write(lun,'(5(i5,e11.3))') i,(this%TDactual(k,i),k=1,this%dimdata)
      end if
      end do

   end subroutine info_TD

   subroutine  write_TD(this,lun)
      implicit none
      class(TDOut),   intent(inout) :: this
      integer,        intent(in   ) :: lun
      !local
      integer :: i,k,ninput
      real(kind=double) :: dnrm2

      !> if the steady steady is already written
      !> do nothing
      if ( .not. this%steadyTD_written ) then
         ninput = this%eval_ninput()
         write(lun,'(a4,1pe15.6)') 'time', this%time
         write(lun,*)  ninput
         do i = 1, this%NData
         if ( dnrm2(this%dimdata,this%TDactual(:,i),1) > small ) then
            write(lun,*) i, (this%TDactual(k,i), k=1,this%dimdata)
         end if
         end do
         !> if the steady steady is reached
         !> write closing sequence and change the flag
         if ( this%steadyTD ) then
            write(lun,'(a4,1pe15.6)') 'time', huge
            this%steadyTD_written = .true.
         end if
      end if

   end subroutine write_TD

   subroutine  write_end_time(this,lun)
      implicit none
      class(TDOut),   intent(inout) :: this
      integer,        intent(in   ) :: lun
      !local
      integer :: i,k,ninput
      real(kind=double) :: dnrm2

      write(lun,'(a4,1pe15.6)') 'time', this%time

   end subroutine write_end_time

   function eval_ninput(this) result(ninput)
      implicit none
      class(TDOut), intent(in) :: this
      integer :: ninput
      !local
      integer :: i
      real(kind=double) :: dnrm2
      ninput= 0
      do i = 1, this%NData
      if ( dnrm2(this%dimdata,this%TDactual(:,i),1) > small ) then
         ninput=ninput+1
      end if
      end do

   end function eval_ninput

end module TimeOutputs
