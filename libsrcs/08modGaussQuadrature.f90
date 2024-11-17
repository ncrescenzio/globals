!> @brief Module for computation of 1d and tensorized
!> 2d Gauss points
module GaussQuadrature

   use Globals

   implicit none

   private

   type, public :: gaussq
      !> Number of 1d Gauss points
      integer :: ngauss
      !> Dimension (`::ngauss`)
      !> Coefficients of 1d Gauss points on \f$[-1,+1]\f$.
      real(kind = double) , allocatable :: coeff1d(:)
      !> Dimension (`::ngauss`)
      !> Cordinate of Gauss points on interval \f$[a,b]\f$.
      !> Only allocated, it will be
      !> gauss = (b-a)/2 coeff + (a+b) /2.
      real(kind = double) , allocatable :: coord_ab(:)
      !> Dimension (`::ngauss`).
      !> Weight of 1d Gauss points on \f$[-1,+1]\f$.
      real(kind = double) , allocatable :: weight1d(:)
      !> Dimension (`::ngauss`).
      !> Weight of 1d Gauss points on \f$[a,b]\f$.
      real(kind = double) , allocatable :: weight_ab(:)
      !> Dimension (`::ngauss`,2).
      !> Cordinate of 2d gauss points (first column
      !> \f$x\f$, second column \f$y\f$)
      real(kind = double) , allocatable :: coord_cell(:,:)
      !> Dimension (`::ngauss`,2).
      !> Weights of 2d Gauss points (first column
      !> \f$x\f$, second column \f$y\f$)
      real(kind = double) , allocatable :: weight_cell(:,:)
      !> Dimension (`::ngauss`**2)
      !> Tensioral production of 1d Gauss weights
      real(kind = double) , allocatable :: weight2d(:)
   contains
      !> Static constructor for `gaussquadrature::gaussq`
      procedure, public, pass :: init => init_gaussq
      !> Static destructor for `gaussquadrature::gaussq`
      procedure, public, pass :: kill => kill_gaussq
      !> Info procedure for `gaussquadrature::gaussq`
      procedure, public, pass :: info => info_gaussq
      !> Procedure to compute Gauss points of \f$[a,b]\f$
      procedure, public, pass :: on_interval
      !> Procedure to compute Gauss points of \f$[a_x,b_x] \times [a_y,b_y]\f$
      procedure, public, pass :: on_cell
   end type gaussq

contains

   !>-------------------------------------------------------------
   !> @brief Static constructor for `gaussquadrature::gaussq`
   !> @details Allocate all the variables. Define variable
   !> `gaussq::coeff1d` and `gaussq::weight1d`.
   !>
   !> @param[in   ] lun_err: unit number for error messages
   !> @param[in   ] ngauss: Number of Gauss points
   !<-------------------------------------------------------------
   subroutine init_gaussq(this,lun_err,ngauss)

      implicit none
      class(gaussq), intent(inout) :: this
      integer, intent(in) :: lun_err
      integer, intent(in) :: ngauss
      !local
      logical :: rc
      integer :: res
      integer :: i,j,m

      this%ngauss = ngauss
      allocate (&
         this%coeff1d(ngauss),&
         this%coord_ab(ngauss),&
         this%weight1d(ngauss),&
         this%weight_ab(ngauss),&
         this%coord_cell(ngauss,2),&
         this%weight_cell(ngauss,2),&
         this%weight2d(ngauss**2),&
         stat=res)
      if(res .ne. 0) rc = IOerr(lun_err, err_alloc, 'init_gaussq', &
         ' member coeff1d, points1d, weight1d,  weight2d ',res)

      call gaussquad(lun_err, ngauss, this%coeff1d, this%weight1d)

      m=0
      do i = 1, ngauss
         do j= 1, ngauss
            m=m+1
            this%weight2d(m) = this%weight1d(i) * this%weight1d(j)
         end do
      end do

   contains

      subroutine gaussquad(lun_err,ngauss,coefcoord,weight)
         implicit none
         integer,           intent(in ) :: lun_err
         integer,           intent(in ) :: ngauss
         real(kind=double), intent(out) :: coefcoord(ngauss)
         real(kind=double), intent(out) :: weight(ngauss)
         ! local
         integer :: info,lwork,i,j
         logical :: rc
         integer :: res
         real(kind=double), allocatable :: u(:)
         real(kind=double), allocatable :: work(:)
         real(kind=double), allocatable :: A(:,:)

         lwork=20*ngauss
         allocate(&
            u(ngauss-1),&
            work(20*ngauss),&
            A(ngauss,ngauss),&
            stat=res)
         if(res .ne. 0) rc = IOerr(lun_err, err_alloc, 'gaussquad', &
            ' local array u, work, A',res)

         A=0.d0
         do j=1,ngauss-1
            A(j,j+1) = j / sqrt(4.0d0*j**2-one)
         end do

         call dsyev('V', 'U',ngauss,A,ngauss,coefcoord,work,lwork,info)
         weight(:)=2*A(1,:)**2
         deallocate(u, work, A, stat=res)
         if(res .ne. 0) rc = IOerr(lun_err, err_dealloc, 'gaussquad', &
            ' local array u, work, A',res)

      end subroutine gaussquad

   end subroutine init_gaussq

   !>-------------------------------------------------------------
   !> @brief Static destructor for `gaussquadrature::gaussq`
   !> @details Deallocate all the variables.
   !>
   !> @param[in   ] lun_err: unit number for error messagges
   !<-------------------------------------------------------------
   subroutine kill_gaussq(this,lun_err)
      implicit none
      class(gaussq), intent(inout) :: this
      integer, intent(in) :: lun_err
      !local
      logical :: rc
      integer :: res

      this%ngauss = 0
      deallocate (&
         this%coeff1d,&
         this%coord_ab,&
         this%weight1d,&
         this%weight_ab,&
         this%coord_cell,&
         this%weight_cell,&
         this%weight2d,&
         stat=res)
      if (res.ne.0) rc = IOerr(lun_err, err_dealloc, 'kill_gaussq', &
         ' member coeff1d, weight1d, weight2d',res)

   end subroutine kill_gaussq

   !>-------------------------------------------------------------
   !> @brief Info procedure for `gaussquadrature::gaussq`
   !>
   !> @param[in   ] lun_out: unit number for output messagges
   !<-------------------------------------------------------------
   subroutine info_gaussq(this,lun_out)
      implicit none
      class(gaussq), intent(in   ) :: this
      integer,       intent(in   ) :: lun_out
      !local
      integer :: i

      write(lun_out,*) 'Nmb of Gauss points = ', this%ngauss
      do i=1,this%ngauss
         write(lun_out,*) 'Gauss coeff1d, weight1d  = ',&
            this%coeff1d(i), this%weight1d(i)
      end do

   end subroutine info_gaussq

   !>-------------------------------------------------------------
   !> @brief Compute Gauss points on the interval \f$[a,b]\f$.
   !> @details Define variables `gaussq::coord_ab` and
   !>  `gaussq::weight_ab`.
   !>
   !> @param[in   ] a: left endpoint of interval
   !> @param[in   ] b: right endpoint of interval
   !<-------------------------------------------------------------
   subroutine on_interval(this,a,b)
      implicit none
      class(gaussq), intent(inout) :: this
      real(double),  intent(in   ) :: a, b

      this%coord_ab(:)  = onehalf * ((b-a) * this%coeff1d(:) + (a+b))
      this%weight_ab(:) = onehalf * (b-a) * this%weight1d(:)

   end subroutine on_interval

   !>-------------------------------------------------------------
   !> @brief Compute Gauss points of \f$[a_x,b_x] \times [a_y,b_y]\f$
   !> @details Gauss points in \f$\mathbb{R}^{2}\f$ are computed by
   !> tensorization of 1d Gauss points. Define variables
   !> `gaussq::coord_cell` and `gaussq::weight_cell`.
   !>
   !> @param[in   ] ax: left endpoint interval variable \f$x\f$.
   !> @param[in   ] bx: right endpoint interval variable \f$x\f$.
   !> @param[in   ] ay: left endpoint interval variable \f$y\f$.
   !> @param[in   ] by: right endpoint interval variable y.
   !<-------------------------------------------------------------
   subroutine on_cell(this,ax,bx,ay,by)
      implicit none
      class(gaussq), intent(inout) :: this
      real(double),  intent(in   ) :: ax, bx, ay, by

      this%coord_cell(:,1)  = onehalf * ((bx-ax) * this%coeff1d(:) + (ax+bx))
      this%coord_cell(:,2)  = onehalf * ((by-ay) * this%coeff1d(:) + (ay+by))

      this%weight_cell(:,1) = onehalf * (bx-ax) * this%weight1d(:)
      this%weight_cell(:,2) = onehalf * (by-ay) * this%weight1d(:)

   end subroutine on_cell

end module GaussQuadrature
