!>-------------------------------------------------------------
!> Define global parameter values
!>
!<------------------------------------------------------------- 
module KindDeclaration
  implicit none
  !> single precision parameter for real vars
  integer, parameter ::  single = kind(1.0e0)
  !> double precision parameter for real vars
  integer, parameter ::  double = kind(1.0d0)
  
  !> double parameters for useful constants
  real(kind=double), parameter  :: zero=0.0d0
  real(kind=double), parameter  :: one=1.0d0
  real(kind=double), parameter  :: two=2.0d0
  real(kind=double), parameter  :: three=3.0d0
  real(kind=double), parameter  :: four=4.0d0
  real(kind=double), parameter  :: onehalf=0.5d0
  real(kind=double), parameter  :: onethird=1.0d0/3.0d0
  real(kind=double), parameter  :: onefourth=1.0d0/4.0d0
  real(kind=double), parameter  :: onesixth=1.0d0/6.0d0
  real(kind=double), parameter  :: verysmall=1.0d-40
  real(kind=double), parameter  :: small=1.0d-15  
  real(kind=double), parameter  :: large=1.0d10
  real(kind=double), parameter  :: huge=1.0d30
  real(kind=double), parameter  :: pigreco=4.0d0*atan(one)
  !
  !> double parameters of physical constants
  real(kind=double), parameter  :: grav = 9.806d0
  real(kind=double), parameter  :: zerodepth = 1.0d-9
  real(kind=double), parameter  :: noflux = 1.0d-10 

contains

  !> the following function is useless but needed to avoid a warning
  !> message when running "ar"
  function empty_fun() result(a)
    implicit none
    ! vars
    integer :: a

    a=0
  end function empty_fun
  
end module KindDeclaration
