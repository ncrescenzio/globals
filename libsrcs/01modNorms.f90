module Norms

   use Globals

   implicit none

   private

   public :: abs_norm, compute_norm, euc_norm, dir_norm

   !>-----------------------------------------------------------
   !> Abstract type for definition of the
   !> evaluation procedure of the residum
   !>----------------------------------------------------------
   type, abstract :: abs_norm
      !> General definition of norm of a vector
      !> fucntion from R^n to [0,+infty[
      !> Example:
      !>   \| v \| = sqrt(\sum_{i} (v_i)^2)
      !>   \| v \| = sqrt(\sum{i \ni Dirichelt} m (v_i)^2)
      !>   \| v \| = sqrt( V^t G v ) for a metric G
   contains
      !> Abstract Procedure for the evaluation of the norm
      procedure(compute_norm), deferred :: eval_norm
   end type abs_norm

   abstract interface
   !>-------------------------------------------------------------
   !> Genreal norm interface
   !<-------------------------------------------------------------
   function compute_norm(this,nvec,vec,scr) result (norm)
      use Globals
      import abs_norm
      implicit none
      class(abs_norm),             intent(inout) :: this
      integer,                     intent(in   ) :: nvec
      real(kind=double),           intent(in   ) :: vec(nvec)
      ! TODO remove this optinal arguement and defind the scratch
      ! vectors in the concrete type???
      real(kind=double), optional, intent(inout) :: scr(nvec)
      ! output result
      real(kind=double) :: norm
   end function compute_norm
   end interface


   type, extends(abs_norm) :: euc_norm
      !> Use the classical eucliadian norm
   contains
      !>
      !> Procedure for \|res\|
      procedure, public :: eval_norm => eval_euc_norm
   end type euc_norm

   type, extends(abs_norm) :: dir_norm
      !> Use a weighted version of euclian norm
      !> where the entries on the dirichlet node are set to zero
      !> Number of Dirichlet Nodes
      !> G(i,j) = \delta_{i,j} (1 - \delta{i,idir})
      !> for idir in noddir
      integer :: ndir
      !> Indeces of the Dirichlet Nodes
      integer, allocatable :: noddir(:)
   contains
      !> Evaluation procedure
      procedure, public :: init => init_dir_norm
      !> Evaluation procedure
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
