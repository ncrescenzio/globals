!>-------------------------------------------------------------
!> Module auxiliary to store an integer and real array
!> use das scratch in different code. Tipically used
!> as optional argument.
!<-------------------------------------------------------------
module Scratch
  use Globals
  implicit none
  private
  type,  public :: scrt
     !> Flag for initialization 
     !> Used to check if arrays are allocated
     logical :: is_initialized=.false.
     !> Size of integer array iaux 
     integer :: niaux=0
     !> Size of real array raux
     integer :: nraux=0
     !> Dimension(niaux)
     !> Integer scratch array
     integer,  allocatable :: iaux(:)
     !> Dimension(niaux)
     !> Real scratch array
     real(kind=double), allocatable :: raux(:)  
   contains 
     !> Static constructor
     !> (procedure public for type scrt)
     procedure, public, pass :: init => init_scrt
     !> Static destructor
     !> (procedure public for type scrt)
     procedure, public, pass :: kill => kill_scrt
     !> Info procedure.
     !> (public procedure for type scrt)
     procedure, public, pass :: info => info_scrt
     !> Check is scratch arrays are big enoguh
     !> (public procedure for type scrt)
     procedure, public, pass :: check => check_scrt
     !> Check is scratch arrays are big enoguh
     !> (public procedure for type scrt)
     procedure, public, pass :: is_enough => check_scrt
     !> Defined ibegin, iend for assign portion of arrays
     !> (public procedure for type scrt)
     procedure, public, nopass :: range
  end type scrt
contains
  !>-------------------------------------------------------------
  !> Static constructor.
  !> (procedure public for type manip)
  !> Instantiate variables of type scrt
  !> 
  !> usage call 'var'%init(lun_err,niaux,nraux)
  !> where:
  !> \param[in] lun_err  -> integer. I/O unit for error message output
  !> \param[in] niaux    -> integer. Len. Integer array
  !> \param[in] nraux    -> integer. Len. Real array
  !<-------------------------------------------------------------

  subroutine init_scrt(this,&
       lun_err,&
       niaux,nraux)
    use Globals
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
  !> Static destructor.
  !> (procedure public for type scrt)
  !> deallocate all arrays for a var of type scrt
  !>
  !> usage:
  !>     call 'var'%kill(lun)
  !>
  !> where:
  !> \param[in] lun -> integer. I/O unit for error message output
  !<-----------------------------------------------------------
  subroutine kill_scrt(this, lun)
    implicit none
    ! vars
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
  !> Info procedure.
  !> (public procedure for type scrt)
  !> Prints content of a variable of type scrt
  !> 
  !> usage:
  !>     call 'var'%info(lun)
  !>
  !> where:
  !> \param[in] lun -> integer. I/O unit for error message output
  !>
  !<-------------------------------------------------------------
  subroutine info_scrt(this, lun)
    implicit none
    ! vars
    class(scrt), intent(in) :: this
    integer    , intent(in) :: lun

    if ( this%is_initialized ) then
       write(lun,*) 'niaux = ', this%niaux, 'nraux = ', this%nraux
    else
       write(lun,*) 'Scratch type not initialized'
    end if
       
  end subroutine info_scrt

  
  !>-------------------------------------------------------------
  !> Info procedure.
  !> (public procedure for type scrt)
  !> Return false if a variable of type scrt
  !> is not initialized, or it contains integer or real array that are
  !> too small
  !> 
  !> usage:
  !>     call 'var'%check()
  !>
  !> where:
  !> \param[in] niaux   -> integer. Number of integer required
  !> \param[in] nraux   -> integer. Number of real    required
  !> \param[out] resutl -> logical. True  ( Type initialized and big enough)
  !>                                False ( Type not initialized or 
  !>                                        not big enough)
  !<-------------------------------------------------------------
  function check_scrt(this,niaux,nraux) result (test)
    implicit none
    ! vars
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

  !>-------------------------------------------------------------
  !> Procedure to define portion of member iaux or raux
  !> (procedure public for type scrt)
  !> EXAMPLE partioning raux in two portion of length
  !> n1 and n2, assing it to two two real pointer v1,v2
  !>
  !>  iend=0
  !>  call aux%range(n1,ibegin,iend)
  !>  v1 => aux%raux(ibegin:iend)
  !>  call aux%range(n2,ibegin,iend)
  !>  v2 => aux%raux(ibegin:iend)
  !> 
  !> usage call 'var'%range(size,ibegin,iend)
  !> where:
  !> \param[in] size    -> integer. Size of array required
  !> \param[out] ibegin  -> integer. Start position of array portion
  !> \param[inout] iend -> integer. End position of array portion
  !>                       It cointains the end position of previous
  !>                       array or it must begin initialize to zero.
  !<----------------------------------------------------------------
  subroutine range(size,ibegin,iend)
    use Globals
    implicit none
    integer, intent(in)    :: size
    integer, intent(out)   :: ibegin
    integer, intent(inout) :: iend
    
    ibegin = iend  + 1
    iend   = ibegin + size - 1

  end subroutine range
end module Scratch
