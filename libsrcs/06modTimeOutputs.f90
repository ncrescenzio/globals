module TimeOutputs

   use Globals

   implicit none

   private

   !> @brief Data structure containing values of a variable
   !> possibly depending on time (e.g., forcing function)
   !> @details This data structure can be used, for example, for
   !> storing the values of a forcing function. Each colum of
   !> `tdout::tdactual` represent the value of the forcing function
   !> in a different point of the domain. All the values are
   !> associated to a specific time value.
   type, public :: TDOut
      !> Logical flag to check if object is initialized
      logical :: built
      !> Dimension of real data array
      integer :: dimdata
      !> Number of data arrays
      integer :: ndata
      !> Dimension (`::dimdata`,`::ndata`)
      real(kind=double), allocatable :: TDactual(:,:)
      !> Time associated to data stored in `::tdactual`
      real(kind=double) :: time
      !> `.true.` if data doeas not depend on time
      !> `.false.` if data depends on time
      logical :: steadyTD
      !> `.true.` if steady state data has already
      !> been written `.false.` otherwise
      logical :: steadyTD_written = .false.
   contains
      !> Static constructor for `timeoutputs::tdout`
      procedure, public, pass :: init => init_TD
      !> Static destructor for `timeoutputs::tdout`
      procedure, public, pass :: kill => kill_TD
      !> Info procedure for `timeoutputs::tdout`
      procedure, public, pass :: info => info_TD
      !> Writing procedure for `timeoutputs::tdout`
      procedure, public, pass :: write2dat => write_TD
      !> Write variable `::time`
      procedure, public, pass :: write_end_time
      !> Compute number of data arrays (columns of
      !> `tdout::tdactual`) having non-zero norm
      procedure, public, pass :: eval_ninput
   end type TDOut

contains

   !>-------------------------------------------------------------
   !> @brief Static constructor for `timeoutputs::tdout`
   !> @details Initialize variables `tdout::dimdata` and `tdout::ndata`
   !> and allocate variable `tdout::tdactual`. Set `tdout::built = .true.`
   !> and `tdout::steadytd = .false.`
   !>
   !> @param[in] stderr: unit number for error message
   !> @param[in] dimdata: value to be assigned to `tdout::dimdata`
   !> @param[in] ndata: value to be assigned to `tdout::ndata`
   !<-------------------------------------------------------------------
   subroutine init_TD(this, stderr, dimdata, Ndata)
      implicit none
      class(TDOut), intent(out) :: this
      integer,      intent(in ) :: stderr
      integer,      intent(in ) :: dimdata
      integer,      intent(in ) :: Ndata
      ! local vars
      integer :: res
      logical :: rc

      this%built = .true.

      this%dimdata = dimdata
      this%ndata   = ndata

      allocate(this%TDactual(dimdata,Ndata),stat=res)
      if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
         '  type TDOut member TDactual (array)',res)
      this%TDactual = zero

      this%steadyTD = .false.

   end subroutine init_TD

   !>-------------------------------------------------------------
   !> @brief Static destructor for `timeoutputs::tdout`
   !> @details Deallocate array `tdout::tdactual` and set
   !> `tdout::built = .false.`
   !>
   !> @param[in] lun: unit number for error message
   !<-----------------------------------------------------------
   subroutine kill_TD(this, lun)
      implicit none
      class(TDOut), intent(inout) :: this
      integer,      intent(in   ) :: lun
      ! local vars
      integer :: res
      logical :: rc

      this%built = .false.
      deallocate(this%TDactual)
      if (res.ne.0) rc=IOerr(lun, err_dealloc, 'kill_TD', &
         'type TDOut var TDactual')

   end subroutine kill_TD

   !>-------------------------------------------------------------
   !> @brief Info procedure for `timeoutputs::tdout`
   !> @details Write non-zero content of variable `tdout::tdactual`
   !>
   !> @param[in] lun: unit number for output message
   !> @param[in] nsample: number of first non zero columns of
   !> `tdout::tdactual` to print
   !<-------------------------------------------------------------
   subroutine info_TD(this, lun, nsample)
      implicit none
      class(TDOut), intent(in) :: this
      integer,      intent(in) :: lun
      integer,      intent(in) :: nsample
      ! local vars
      integer :: i,j,k
      real(kind=double) :: dnrm2

      write(lun,*) ' '
      write(lun,*) ' Info: TDOut structure definition:'
      write(lun,*) 'dimension data', this%dimdata
      write(lun,*) 'ndata         ', this%ndata

      if (this%steadyTD     ) write(lun,*) ' Steady state'
      if (.not.this%steadyTD) write(lun,*) ' Not in steady state'

      write(lun,*) ' Actual val '
      i=0
      j=0
      do while ((i.lt.this%ndata) .and. (j.lt.nsample))
         i=i+1
         if (dnrm2(this%dimdata,this%TDactual(:,i),1).ne.zero) then
            j=j+1
            write(lun,'(5(i5,e11.3))') i,(this%TDactual(k,i),k=1,this%dimdata)
         end if
      end do

   end subroutine info_TD

   !>-------------------------------------------------------------
   !> @brief Write non-zero content of `tdout::tdactual` for
   !> non steady state data
   !> @details If `tdout::steadytd_written = .false.` this
   !> procedure writes the columns of `tdout::tdactual`
   !> having non-zero norm, otherwise it does nothin.
   !> Then, if `tdout::steadytd = .true.`
   !> set `tdout::steadytd_written = .true.`.
   !>
   !> @param[in] lun: unit number for output
   !<-------------------------------------------------------------
   subroutine  write_TD(this,lun)
      implicit none
      class(TDOut),   intent(inout) :: this
      integer,        intent(in   ) :: lun
      !local
      integer :: i,k,ninput
      real(kind=double) :: dnrm2

      !> if the steady steady is already written
      !> do nothing
      if (.not.this%steadyTD_written) then
         ninput = this%eval_ninput()
         write(lun,'(a4,1pe15.6)') 'time', this%time
         write(lun,*)  ninput
         do i = 1, this%NData
            if (dnrm2(this%dimdata,this%TDactual(:,i),1) > small) then
               write(lun,*) i, (this%TDactual(k,i), k=1,this%dimdata)
            end if
         end do
         !> if the steady steady is reached
         !> write closing sequence and change the flag
         if (this%steadyTD) then
            write(lun,'(a4,1pe15.6)') 'time', huge
            this%steadyTD_written = .true.
         end if
      end if

   end subroutine write_TD

   !>-------------------------------------------------------------
   !> @brief Write variable `tdout::time`
   !>
   !> @param[in] lun: unit number for output message
   !<-------------------------------------------------------------
   subroutine  write_end_time(this,lun)
      implicit none
      class(TDOut),   intent(inout) :: this
      integer,        intent(in   ) :: lun
      !local
      integer :: i,k,ninput
      real(kind=double) :: dnrm2

      write(lun,'(a4,1pe15.6)') 'time', this%time

   end subroutine write_end_time

   !>-------------------------------------------------------------
   !> @brief Compute number of data arrays (columns of
   !> `tdout::tdactual`) having non-zero norm
   !>
   !> @return (integer) number of data arrays (columns of
   !> `tdout::tdactual`) having non-zero norm
   !<-------------------------------------------------------------
   function eval_ninput(this) result(ninput)
      implicit none
      class(TDOut), intent(in) :: this
      integer                  :: ninput
      !local
      integer :: i
      real(kind=double) :: dnrm2

      ninput = 0

      do i = 1,this%NData
         if (dnrm2(this%dimdata,this%TDactual(:,i),1) > small) then
            ninput = ninput+1
         end if
      end do

   end function eval_ninput

end module TimeOutputs
