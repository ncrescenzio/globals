module Norms

   use Globals

   implicit none

   private

   !>-----------------------------------------------------------
   !> @brief Abstract type for the definition of the
   !> procedure for the evaluation of the norm of a vector
   !>
   !> @details General definition of norm of a vector function
   !>
   !> **Example:**
   !> * \f$\| v \| = \sqrt{\sum_{i} v_i^2}\f$
   !> * \f$\| v \| = \sqrt{\sum_{i \ni D}  v_i^2}\f$, where \f$D\f$
   !>   is a set of indices
   !> * \f$\| v \| = \sqrt{ v^{\top} G v }\f$, for a metric \f$G\f$
   type, public, abstract :: abs_norm
   contains
      !> Abstract procedure for the evaluation of the norm
      procedure(compute_norm), public, deferred :: eval_norm
   end type abs_norm

   abstract interface
      !>-----------------------------------------------------------
      !> @brief Abstract interface for the computation of the norm
      !> @param[inout] this: Object calling the function
      !> @param[in]    nvec: Integer, length of the vector
      !> @param[in]    vec:  Real, input vector
      !> @param[inout] scr:  optional
      !> @param[out] result: Norm of the input vector
      !>----------------------------------------------------------
      function compute_norm(this,nvec,vec,scr) result (norm)
         use Globals
         import abs_norm
         implicit none
         class(abs_norm),             intent(inout) :: this
         integer,                     intent(in   ) :: nvec
         real(kind=double),           intent(in   ) :: vec(nvec)
         real(kind=double), optional, intent(inout) :: scr(nvec)
         ! output result
         real(kind=double) :: norm
      end function compute_norm
   end interface

   !> @brief Euclidean norm
   type, public, extends(abs_norm) :: euc_norm
   contains
      !> Evaluation of the Euclidean norm
      procedure, public :: eval_norm => eval_euc_norm
   end type euc_norm

   !> @brief Weighted version of Euclian norm.
   !> @details The entries on the Dirichlet node are set to zero
   !> \f$G_{i,j} = \delta_{i,j} (1 - \delta_{i,k})\f$
   !> for \f$k\f$ in `noddir`.
   type, public, extends(abs_norm) :: dir_norm
      !> Number of Dirichlet nodes
      integer :: ndir
      !> Indeces of the Dirichlet Nodes
      integer, allocatable :: noddir(:)
   contains
      !> Constructor for type `dir_norm`
      procedure, public :: init => init_dir_norm
      !> Evaluation of the weighted Euclidean norm
      procedure, public :: eval_norm => eval_dir_norm
   end type dir_norm

contains

   !> TODO COMMENT
   function eval_euc_norm(this,nvec,vec,scr) result (resnorm)
      implicit none
      class(euc_norm),             intent(inout) :: this
      integer,                     intent(in   ) :: nvec
      real(kind=double),           intent(in   ) :: vec(nvec)
      real(kind=double), optional, intent(inout) :: scr(nvec)
      ! output result
      real(kind=double) :: resnorm
      !local
      real(kind=double) :: dnrm2

      resnorm = dnrm2(nvec,vec,1)

   end function eval_euc_norm

   !> TODO COMMENT
   subroutine init_dir_norm(this,lun_err,ndir,noddir)
      implicit none
      class(dir_norm), intent(inout) :: this
      integer,         intent(in   ) :: lun_err
      integer,         intent(in   ) :: ndir
      integer,         intent(in   ) :: noddir(ndir)
      !local
      logical :: rc
      integer :: res

      this%ndir = ndir
      if (.not. allocated(this%noddir) ) then
         allocate ( this%noddir(ndir),stat=res)
         rc = IOerr(lun_err, err_alloc , 'init_dirichlet_resnorm', &
            'type dir_resnorm member noddir ', res )
      end if
      this%noddir = noddir

   end subroutine init_dir_norm

   function eval_dir_norm(this,nvec,vec,scr) result (resnorm)
      use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
      implicit none
      class(dir_norm),             intent(inout) :: this
      integer,                     intent(in   ) :: nvec
      real(kind=double),           intent(in   ) :: vec(nvec)
      real(kind=double), optional, intent(inout) :: scr(nvec)

      ! output result
      real(kind=double)  :: resnorm
      !local
      logical :: rc
      integer :: i
      real(kind=double) :: dnrm2

      if ( .not. present(scr) ) then
         rc = IOerr(stderr, err_val, 'eval_dir_norm', &
            'scratch array not passed' )
      else
         scr = vec
         do i=1,this%ndir
            scr(this%noddir(i)) = zero
         end do
         resnorm = dnrm2(nvec,scr,1)
      end if

   end function eval_dir_norm

end module Norms
