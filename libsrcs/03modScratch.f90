!>-------------------------------------------------------------
!> @brief Auxiliary module to store an integer and real array
!> used (typically as optional argument) as scratch in different code.
!<-------------------------------------------------------------
module Scratch

   use Globals

   implicit none

   private

   type,  public :: scrt
      !> Flag for initialization
      !> Used to check if arrays are allocated
      logical :: is_initialized = .false.
      !> Size of integer array `::iaux`
      integer :: niaux = 0
      !> Size of real array `::raux`
      integer :: nraux = 0
      !> Dimension (`::niaux`)
      !> Integer scratch array
      integer,  allocatable :: iaux(:)
      !> Dimension (`::niaux`)
      !> Real scratch array
      real(kind=double), allocatable :: raux(:)
   contains
      !> Static constructor for `scratch::scrt`
      procedure, public, pass :: init => init_scrt
      !> Static destructor for `scratch::scrt`
      procedure, public, pass :: kill => kill_scrt
      !> Info procedure for `scratch::scrt`
      procedure, public, pass :: info => info_scrt
      !> Check is scratch arrays are big enoguh
      procedure, public, pass :: check => check_scrt
      !> Check is scratch arrays are big enoguh
      procedure, public, pass :: is_enough => check_scrt
      !> Define `ibegin`, `iend` for assign portion of arrays
      procedure, public, nopass :: range
   end type scrt

contains

   !>-------------------------------------------------------------
   !> @brief Procedure to define portion of member `scrt::iaux`
   !>  or `scrt::raux`
   !>
   !> **Example**: partioning `raux` in two portion of length
   !> `n1` and `n2`, assign it to two two real pointer `v1` and `v2`.
   !>
   !>  ```fortran
   !>  iend = 0
   !>  call aux%range(n1, ibegin, iend)
   !>  v1 => aux%raux(ibegin:iend)
   !>  call aux%range(n2, ibegin, iend)
   !>  v2 => aux%raux(ibegin:iend)
   !>  ```
   !>
   !> @param[in] size    integer. Size of array required
   !> @param[out] ibegin integer. Start position of array portion
   !> @param[inout] iend integer. End position of array portion.
   !>                       It cointains the end position of previous
   !>                       array or it must begin initialize to zero.
   !<----------------------------------------------------------------
   subroutine range(size,ibegin,iend)
      implicit none
      integer, intent(in)    :: size
      integer, intent(out)   :: ibegin
      integer, intent(inout) :: iend

      ibegin = iend  + 1
      iend   = ibegin + size - 1

   end subroutine range

   !>-------------------------------------------------------------
   !> @brief Static constructor for `scratch::scrt`
   !> @details Set `scrt::niaux` and `scrt::nraux` and allocate
   !> arrays `scrt::iaux` and `scrt::raux`.
   !>
   !> @param[in] lun_err integer, unit for error message output
   !> @param[in] niaux   integer, length integer array
   !> @param[in] nraux   integer, length real array
   !<-------------------------------------------------------------
   subroutine init_scrt(this, lun_err, niaux, nraux)
      implicit none
      class(scrt), intent(inout) :: this
      integer,     intent(in   ) :: lun_err
      integer,     intent(in   ) :: niaux
      integer,     intent(in   ) :: nraux
      !local
      integer :: res
      logical :: rc

      this%is_initialized = .true.
      this%niaux = niaux
      this%nraux = nraux
      allocate(this%iaux(this%niaux),this%raux(this%nraux),stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc, 'init_aux', &
         ' array iaux raux',res)

   end subroutine init_scrt

   !>-------------------------------------------------------------
   !> @brief Destructor for `scratch::scrt`.
   !> @details Deallocate variables `scrt::iaux` and `scrt::raux`.
   !>
   !> @param[in] lun: integer, unit number for error message
   !<-----------------------------------------------------------
   subroutine kill_scrt(this, lun)
      implicit none
      class(scrt), intent(inout) :: this
      integer,     intent(in   ) :: lun
      ! local vars
      integer :: res
      logical :: rc

      if (this%is_initialized) then
         deallocate(this%iaux,this%raux,stat=res)
         if (res.ne.0) rc=IOerr(lun, err_dealloc, 'kill_scrt', &
            'dealloc fail for type scrt member iaux raux',res)
      end if

      this%is_initialized = .false.
      this%niaux=0
      this%nraux=0

   end subroutine kill_scrt

   !>-------------------------------------------------------------
   !> @brief Info procedure for `scratch::scrt`
   !> @details Prints content of a variable of type `scratch::scrt`
   !>
   !> @param[in] lun: integer, unit number for output message
   !<-------------------------------------------------------------
   subroutine info_scrt(this, lun)
      implicit none
      class(scrt), intent(in) :: this
      integer    , intent(in) :: lun

      if ( this%is_initialized ) then
         write(lun,*) 'niaux = ', this%niaux, 'nraux = ', this%nraux
      else
         write(lun,*) 'Scratch type not initialized'
      end if

   end subroutine info_scrt

   !>-------------------------------------------------------------
   !> @brief Check is scratch arrays are big enoguh
   !> @details Return false if a variable of type `scratch::scrt`
   !> is not initialized, or it contains integer or real array that
   !> are too small.
   !>
   !> @param[in] niaux   integer. Number of integer required
   !> @param[in] nraux   integer. Number of real    required
   !> @return (logical) `True`: type initialized and big enough,
   !>                   `False` type not initialized or not big enough.
   !<-------------------------------------------------------------
   function check_scrt(this,niaux,nraux) result (test)
      implicit none
      class(scrt), intent(in) :: this
      integer,     intent(in) :: niaux
      integer,     intent(in) :: nraux
      logical :: test

      test = .true.
      if ( niaux .gt. this%niaux .or. &
         nraux .gt. this%nraux .or. &
         .not. this%is_initialized) then
         test = .false.
      end if

   end function check_scrt

end module Scratch
