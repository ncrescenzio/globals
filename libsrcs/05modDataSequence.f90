module DataSequence

   use Globals

   implicit none

   private

   !>-------------------------------------------------------------
   !> @brief Data structure to store time-sequence of real data
   !> @details Used to generate linear Lagrangian interpolations.
   !> Data are stored cyclicaly.
   !>
   !> **Example:**
   !> Assume `nsequence = 3`. After init `istored = 0`.
   !> Using `::fill` procedure we pass the data to be stored.
   !>
   !> ```
   !> data1, time1 -> this%datas(:,1), this%time(1) istored = 1
   !> data2, time2 -> this%datas(:,2), this%time(2) istored = 2
   !> data3, time3 -> this%datas(:,3), this%time(3) istored = 3
   !> data4, time4 -> this%datas(:,1), this%time(1) istored = 1
   !> data5, time5 -> this%datas(:,2), this%time(2) istored = 2
   !> data6, time6 -> this%datas(:,3), this%time(3) istored = 3
   !> ```
   !>
   !> The integer `::istored` says which is the last slot were
   !> data has been stored.
   !<-------------------------------------------------------------
   type , public :: dataseq
      !> Data size
      integer ::  ndata
      !> Number of time instants
      integer ::  nsequence
      !> Dimension (`::ndata`,`::nsequence`)
      !> Data stored
      real(kind=double), allocatable  :: datas(:,:)
      !> Dimension (`::nsequence`)
      !> Times for which data is stored
      real(kind=double), allocatable  :: times(:)
      !> Column index where last data has been stored
      integer :: istored
      !> Number of non-empty columns of real data
      integer :: nstored
   contains
      !> Static constructor for `datasequence::dataseq`
      procedure, public , pass :: init => init_dataseq
      !> Static destructor for `datasequence::dataseq`
      procedure, public , pass :: kill => kill_dataseq
      !> Add new data to the database
      procedure, public , pass :: fill => fill_dataseq
      !> Lagrange interpolation in time of stored data
      procedure, public , pass :: lagrange_interpolation
   end type dataseq

contains

   !>--------------------------------------------------------------------
   !> @brief Static constructor for `datasequence::dataseq`
   !> @details Set variables `dataseq::ndata` and `dataseq::nsequence`
   !> and allocate variables `dataseq::datas` and `dataseq::times`.
   !>
   !> @param[in   ] lun_err: integer, unit number for error message
   !> @param[in   ] ndata: integer, dimension data to store
   !> @param[in   ] nsequence: integer, number of time instants
   !<---------------------------------------------------------------------
   subroutine init_dataseq(this, lun_err, ndata, nsequence)
      implicit none
      class(dataseq), intent(inout) :: this
      integer,        intent(in   ) :: lun_err
      integer,        intent(in   ) :: ndata
      integer,        intent(in   ) :: nsequence
      !local
      integer :: res
      logical :: rc

      this%ndata     = ndata
      this%nsequence = nsequence
      allocate(&
         this%datas(ndata,nsequence),&
         this%times(nsequence),&
         stat=res)
      if (res.ne.0) rc = IOerr(lun_err, err_alloc, 'init_dataseq', &
         ' type dataseq member datas times')
      !
      !  this%istored = 0 so at the first data is stored in this%data(:,1)
      !
      this%istored = 0
      this%nstored = 0

   end subroutine init_dataseq

   !>--------------------------------------------------------------------
   !> @brief Static destructor for `datasequence::dataseq`
   !> @details Deallocate variables `dataseq::datas` and `dataseq::times`
   !>
   !> @param[in   ] lun_err: integer, unit number for error message
   !<---------------------------------------------------------------------
   subroutine kill_dataseq(this, lun_err)
      implicit none
      class(dataseq), intent(inout) :: this
      integer, intent(in   )        :: lun_err
      !local
      integer :: res
      logical :: rc

      deallocate(&
         this%datas,&
         stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc, 'init_dataseq', &
         ' type dataseq member datas')
      deallocate(&
         this%times,&
         stat=res)
      if (res.ne.0) rc = IOerr(lun_err, err_dealloc, 'init_dataseq', &
         ' type dataseq membertimes')

   end subroutine kill_dataseq

   !>--------------------------------------------------------------------
   !> @brief Add new data to the database
   !>
   !> @param[in   ] data: data to add to database `dataseq::datas`
   !> @param[in   ] time: time associated to new data
   !<---------------------------------------------------------------------
   subroutine fill_dataseq(this, data, time)
      implicit none
      class(dataseq),    intent(inout) :: this
      real(kind=double), intent(in   ) :: data(this%ndata)
      real(kind=double), intent(in   ) :: time
      !local
      integer :: res
      logical :: rc

      ! count number of data stored
      this%nstored = this%nstored + 1
      this%nstored = min(this%nstored, this%nsequence)

      ! select slot for next data
      this%istored = this%istored + 1
      if (this%istored.eq.(this%nsequence+1))  this%istored = 1

      ! store data in istored slot
      this%datas(:,this%istored) = data(:)
      this%times(this%istored)   = time

   end subroutine fill_dataseq

   !>--------------------------------------------------------------------
   !> @brief Lagrange interpolation in time of stored data
   !>
   !> @param[in   ] time: time at which we want to compute new data
   !> @param[inout] interpolation: vector of data at time `time`computed
   !> using Lagrange interpolation in time of data stored in `dataseq::datas`
   !> @param[inout] info: `-1`: database is not full and interpolation not
   !> computed, `0` otherwise
   !<---------------------------------------------------------------------
   subroutine lagrange_interpolation(this, time, interpolation, info)
      implicit none
      class(dataseq), intent(in)       :: this
      real(kind=double), intent(in)    :: time
      real(kind=double), intent(inout) :: interpolation(this%ndata)
      integer, intent(inout)           :: info
      !local
      logical :: rc
      integer :: res
      integer :: i, idata, nsequence
      real(kind=double) :: coeff_langrange

      info = 0
      if (this%nstored.ne.this%nsequence) then
         info = -1
         return
      end if

      nsequence = this%nsequence

      interpolation(:) = zero
      do i = 1,nsequence
         ! find data
         idata = this%istored + i - 1
         if( idata > nsequence ) then
            idata = idata - nsequence
         end if
         write(*,*) i, idata

         ! eval i-th lagrnagian coefficeint
         coeff_langrange = eval_coeff(idata, nsequence, this%times, time)
         ! add  i-th data
         interpolation (:) = interpolation (:) + &
            coeff_langrange * this%datas(:, idata)
      end do

   contains

      function eval_coeff(idata, ntimes, times, time) result(out)
         implicit none
         integer, intent(in )             :: idata
         integer, intent(in )             :: ntimes
         ! this times are not sorted but the algorithm works anyway
         real(kind=double), intent(in   ) :: times(ntimes)
         real(kind=double), intent(in   ) :: time
         real(kind=double) :: out
         ! local
         integer :: i

         out = one
         do i = 1, ntimes
            if ( i .ne. idata ) then
               out = out * ( time - times(i) ) / ( times(idata) - times(i) )
            end if
         end do

      end function eval_coeff

   end subroutine lagrange_interpolation

end module DataSequence
