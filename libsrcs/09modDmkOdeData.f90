module DmkOdeData
  use Globals
  implicit none
  private  
  !>----------------------------------------------------------------
  !> Structure variable containg the time varing quantities 
  !> (pflux, pmass, decay, kappa, rhs_integrated)
  !> and fix in time quanties 
  !> (tdens0) the ODE  
  !> \begin{gather}
  !>    \Div(\Tdens \Grad \Pot) = \Forcing 
  !>    \quad
  !>    -\Tdens \Grad \Pot \cdot \n_{\partial \Domain} = \Bdflux
  !> \\
  !> \Tdens'=|\Tdens \Grad \Pot(\Tdens)|^\Pflux-kappa*decay*\Tdens^{\Pmass}
  !> \\
  !> \Tdens(tzero) = \Tdens0
  !> \end{gather}
  !> It may contains the optional quantities 
  !> forcing, boundary_flux, optdens(fix initialized and used
  !> only if the correspondet input files exis
  !>------------------------------------------------------------------
  type, public :: OdeData
     !> Identifier of ODE
     !> 1 : PP  (Physarum Policephalum dynamic)
     !> 2 : GF  (Gradient Flow dynamic in Tdens variable)
     !> 3 : GF2 (Gradient Flow dynamic in Gfvar variable)
     integer :: id_ode
     !> Number of time frames
     integer :: ntimes
     !> time frames
     real(kind=double), allocatable :: times(:)
     !>-------------------------------------------------------------
     !> Mandatory Input data
     !>-------------------------------------------------------------
     !> Nmber of variables for Tdens-variables 
     integer :: ntdens
     !> Nmber of variables for Pot-variables
     integer :: npot
     !>--------------------------------------------------------------
     !> True/False flag for existence of optimal potential array
     logical :: dirichlet_exists= .False.
     !> Dirichelet quantities
     !> Number of Dirichlet nodes 
     integer :: ndir=0
     !> Dimension= ndir
     !> Dirichlet node indeces ( in subgrid ) 
     integer, allocatable :: dirichlet_nodes(:,:)
     !> Dimension = ndir
     !> Dirichlet values at nodes ( in subgrid )
     real(kind=double), allocatable :: dirichlet_values(:,:)
     !>------------------------------------------------------------
     !> Lift of tdens in elliptic equation
     !> -div(\tdens + lasso \grad \pot) = rhs         ( continouos )
     !> ( A ( diag(\tdens) + lambda  Id )A pot  ) \pot = rhs ( discrete )
     real(kind=double), allocatable :: lambda(:)
     !> Lasso relaxation 
     !> (stiff(tdens) + lasso mass_matrix)  pot = rhs         ( continouos )
     !> (stiff(tdens) + lasso Id         )  pot = rhs         ( discrete )
     real(kind=double), allocatable :: lasso(:)
     !>--------------------------------------------------------------
     !> Initial data for transport dnsity
     !> Dimension (ntdens=grid%ncell)
     real(kind=double), allocatable :: tdens0(:)
     !> Initial data for transport dnsity
     !> Dimension (npot=grid_pot%nnode)
     real(kind=double), allocatable :: pot0(:)     
     !> Pflux power
     !> Contiains 1 real with pflux power
     real(kind=double), allocatable :: pflux(:)
     !> Time decay
     !> Contiains 1 real with pflux power
     real(kind=double), allocatable :: decay(:)
     !> Pmass power
     !> Contiains 1 real with pflux power
     real(kind=double), allocatable :: pmass(:)
     !> ODE expoent for norm of grad
     !> pflux : PP  (Physarum Policephalum dynamic)
     !> 2.0   : GF  (Gradient Flow dynamic in Tdens variable)
     !> 2.0   : GF2 (Gradient Flow dynamic in Gfvar variable)
     real(kind=double), allocatable :: pode(:)
     !>-----------------------------------------------------
     !> Penalization
     !>-----------------------------------------------------
     !> True/False flag for existence penalization
     logical :: penalty_exists = .False.
     !> Scaling factor for penalty
     !> Contiains 1 real with pflux power
     real(kind=double), allocatable :: penalty_factor(:)
     !> Optional weight $\Wpenalty$ of penalty decay
     !> Give by adding penalty 
     !> $\Wpenalty\|\Tdens -\Penalty\|^2
     real(kind=double), allocatable :: penalty_weight(:,:)
     !> Dimension = ntdens
     !> Optional penalty $\geq 0$ given by adding
     !> $\Wpenalty\|\Tdens -\Penalty\|^2
     !> in the minimization problem.
     real(kind=double), allocatable :: penalty(:,:)
     !>-----------------------------------------------------
     !> Dimension = ntdens
     !> Spatial decay 
     !> Contiains ntria-array with spatial decay
     real(kind=double), allocatable :: kappa(:,:)
     !> Dimension = npot
     !> Rhs in the linear system
     !> arising from equation 
     !> $-\div(\Tdens \Grad \Pot ) = \Forcing$
     !> Completed with proper boundary condition
     real(kind=double), allocatable :: rhs_integrated(:,:)
     !>-----------------------------------------------------
     !> Reference solution to compute errors
     !>-----------------------------------------------------
     !> True/False flag for existence of optdens array
     logical :: opt_tdens_exists= .False.
     !> Reference solution for Tdens
     !> Dimension (ntdens=grid%ncell)
     real(kind=double), allocatable :: opt_tdens(:,:)
     !> True/False flag for existence of optdens array
     logical :: opt_pot_exists= .False.
     !> Reference solution for Tdens
     !> Dimension (ntdens=grid%ncell)
     real(kind=double), allocatable :: opt_pot(:,:)
   contains
     !> Static constructor
     !> (public for type OdeData)
     procedure, public, pass :: init => OdeData_init
     !> Static destructor
     !> (public for type OdeData)
     procedure, public, pass :: kill => OdeData_kill
     !> Info procedure
     !> (public for type OdeData)
     procedure, public, pass :: info => OdeData_info
     !> Shift second slot into the second
     !> (public for type OdeData)
     procedure, public, pass :: shift => OdeData_shift
  end type OdeData
  
contains
    !>------------------------------------------------------------
    !> Static constructor.
    !> (procedure public for type OdeInp)
    !> Instantiate (allocate if necessary)
    !> and initialize variables of type OdeInp
    !>
    !> usage:
    !>     call 'var'%init(lun_err,ntdens,npot)
    !>
    !> where:
    !> \param[in] lun_err -> integer. Logical unit for error msg.
    !> \param[in] ntdens  -> integer. Dimension of Tdens-variables
    !> \param[in] npot    -> integer. Dimension of Pot-variables
    !<-------------------------------------------------------------
    subroutine OdeData_init(this,&
         lun_err,&
         ntdens,npot)
      use Globals
      implicit none
      class(OdeData),     intent(inout) :: this
      integer,     intent(in   ) :: lun_err
      integer,     intent(in   ) :: ntdens
      integer,     intent(in   ) :: npot
      !local
      logical :: rc
      integer :: res


      this%ntdens = ntdens
      this%npot   = npot

      allocate( &
            this%times(2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member times')

      allocate( &
            this%dirichlet_nodes(this%npot,2),&
            this%dirichlet_values(this%npot,2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member dirichlet_nodes, dirichlet_values')

      allocate( &
           this%tdens0(ntdens),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member tdens0')

      allocate( &
           this%lambda(2),&
           this%penalty_factor(2),&
           this%penalty(ntdens,2),&
           this%penalty_weight(ntdens,2),&
           this%kappa(ntdens,2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member penalty factor penalty penalty_weight kappa')

      allocate( &
           this%pflux(2),&
           this%pode(2),&
           this%pmass(2),&
           this%decay(2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member pflux pmass decay')

      allocate( &
           this%rhs_integrated(npot,2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member rhs_integrated')

      allocate( &
           this%opt_tdens(ntdens,2),&
           this%opt_pot(npot,2),&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc , 'OdeData_init', &
           ' type odedata member opt_tdens, opt_pot')

    end subroutine OdeData_init


    !>------------------------------------------------------------
    !> Static desconstructor.
    !> (procedure public for type OdeInp)
    !> Free memory
    !>
    !> usage:
    !>     call 'var'%kill(lun_err)
    !>
    !> where:
    !> \param[in] lun_err -> integer. I/O logical unit
    !<-------------------------------------------------------------
    subroutine OdeData_kill(this,lun_err )
      use Globals
      implicit none
      class(OdeData),     intent(inout) :: this
      integer,     intent(in   ) :: lun_err
      !local
      logical :: rc
      integer ::res

      this%ntdens = 0
      this%npot   = 0
      this%ndir   = 0


      deallocate( &
            this%dirichlet_nodes,&
            this%dirichlet_values,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member dirichlet_nodes, dirichlet_values')

      deallocate( &
           this%tdens0,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member tdens0')

      deallocate( &
           this%lambda,&
           this%penalty_factor,&
           this%penalty,&
           this%penalty_weight,&
           this%kappa,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member penalty penalty_weight kappa')

      deallocate( &
           this%pflux,&
           this%pmass,&
           this%decay,&
           this%pode,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member pflux pmass decay')

      deallocate( &
           this%rhs_integrated,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member rhs_integrated')

      deallocate( &
           this%opt_tdens,&
           this%opt_pot,&
           stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc , 'OdeData_init', &
           ' type odedata member opt_tdens, opt_pot')



    end subroutine OdeData_kill

    !>------------------------------------------------------------
    !> Information procedure
    !> (procedure public for type OdeData)
    !>
    !> usage:
    !>     call 'var'%info(lun_out)
    !>
    !> where:
    !> \param[in] lun_out -> integer. I/O logical unit
    !<-------------------------------------------------------------
    subroutine OdeData_info(this,lun_out )
      use Globals
      implicit none
      class(OdeData),     intent(inout) :: this
      integer,     intent(in   ) :: lun_out

      write(lun_out,*) 'Tdens     dimensions =', this%ntdens 
      write(lun_out,*) 'Potential dimensions =', this%npot
      write(lun_out,*) 'ODE type             =', this%id_ode

    end subroutine OdeData_info

    !>------------------------------------------------------------
    !> Shift data from second to first time
    !<-------------------------------------------------------------
    subroutine OdeData_shift(this)
      class(odedata), intent(inout) :: this

      this%times(1) = this%times(2)
      this%pflux(1) = this%pflux(2)
      this%pmass(1) = this%pmass(2)
      this%pode(1)  = this%pode(2)
      this%decay(1) = this%decay(2)

      this%kappa(:,1) =  this%kappa(:,2)

      this%lambda(1) = this%lambda(2)

      !
      ! rhs
      !
      this%rhs_integrated(:,1) =     this%rhs_integrated(:,2)

      !
      ! tdens penalization 
      !
      this%penalty_factor(1) = this%penalty_factor(2) 
      this%penalty(:,1)        = this%penalty(:,2)        
      this%penalty_weight(:,1) = this%penalty_weight(:,2)        

      ! 
      this%opt_tdens(:,1) = this%opt_tdens(:,2) 
      this%opt_pot(:,1) = this%opt_pot(:,2) 

    end subroutine OdeData_shift

    
  end module DmkOdeData
