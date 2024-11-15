module GaussQuadrature

   use Globals

   implicit none

   public

   type, public :: gaussq
      !> Nmb of gauss 1d points
      integer :: ngauss
      !> Dimension (ngauss)
      !> Coeff. of 1d gauss points
      !> gauss = (b-a)/2 coeff + (a+b) /2
      real(kind = double) , allocatable :: coeff1d(:)
      !> Dimension (ngauss)
      !> Cordinate of gauss point on interval
      !> Only allocated, it will be
      !> gauss = (b-a)/2 coeff + (a+b) /2
      real(kind = double) , allocatable :: coord_ab(:)
      !> Dimension (ngauss)
      !> Weight of 1d gauss points
      real(kind = double) , allocatable :: weight1d(:)
      !> Dimension (ngauss)
      !> Weight of 1d gauss points
      real(kind = double) , allocatable :: weight_ab(:)
      !> Dimension (ngauss,2)
      !> Cordinate of gauss point on interval
      !> Only allocated, it will be
      !> gauss = (b-a)/2 coeff + (a+b) /2
      real(kind = double) , allocatable :: coord_cell(:,:)
      !> Dimension (ngauss,2)
      !> Weight of 1d gauss points
      real(kind = double) , allocatable :: weight_cell(:,:)
      !> Dimension (ngauss**2)
      !> Tensioral production of 1dgauss weight
      real(kind = double) , allocatable :: weight2d(:)
   contains
      !> static constructor
      !> (procedure public for type gaussq)
      procedure, public, pass :: init => init_gaussq
      !> static destructor
      !> (procedure public for type gaussq)
      procedure, public, pass :: kill => kill_gaussq
      !> Info procedure.
      !> (public procedure for type gaussq)
      procedure, public, pass :: info => info_gaussq
      !> Procedure for building coord1d
      !> (public procedure for type gaussq)
      procedure, public, pass :: on_interval
      !> Procedure for building coord1d
      !> (public procedure for type gaussq)
      procedure, public, pass :: on_cell
   end type gaussq

contains

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
      if(res .ne. 0) rc = IOerr(lun_err, err_dealloc, 'kill_gaussq', &
         ' member coeff1d, weight1d, weight2d',res)

   end subroutine kill_gaussq

   subroutine info_gaussq(this,lun_out)
      implicit none
      class(gaussq), intent(inout) :: this
      integer,       intent(in   ) :: lun_out
      !local
      integer :: i

      write(lun_out,*) 'Nmb of Gauss points =', this%ngauss
      do i=1,this%ngauss
      write(lun_out,*) 'Gauss coeff1d, weigth1d  =',&
         this%coeff1d(i), this%weight1d(i)
      end do

   end subroutine info_gaussq

   subroutine on_interval(this,a,b)
      class(gaussq), intent(inout) :: this
      real(double),  intent(in   ) :: a, b
      this%coord_ab(:)  = onehalf *( (b-a)* this%coeff1d(:) + ( a + b ) )
      this%weight_ab(:) = onehalf * (b-a) * this%weight1d(:)

   end subroutine on_interval

   subroutine on_cell(this,ax,bx,ay,by)
      class(gaussq), intent(inout) :: this
      real(double),  intent(in   ) :: ax, bx, ay, by
      this%coord_cell(:,1)  = onehalf *( (bx-ax)* this%coeff1d(:) + ( ax + bx ) )
      this%coord_cell(:,2)  = onehalf *( (by-ay)* this%coeff1d(:) + ( ay + by ) )
      this%weight_cell(:,1) = onehalf * (bx-ax) * this%weight1d(:)
      this%weight_cell(:,2) = onehalf * (by-ay) * this%weight1d(:)

   end subroutine on_cell

end module GaussQuadrature
