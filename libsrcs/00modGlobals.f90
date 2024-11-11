!>-------------------------------------------------------------
!> Define global vars
!>
!> err numbers:
!> errno = [0:1][01-10] I/O errors (file existence etc...)
!>       = [0:1][11-20] input errors (reads)
!>       = [0:1][21-30] output errors (writes)
!>       = [0:1][31-40] alloc errors 
!>       = [0:1][41-50] dealloc errors
!>       = [0:1][51-60] vtk libray errors
!<------------------------------------------------------------- 
module Globals
  use KindDeclaration
  implicit none
  !> single precision parameter for real vars
  !integer, parameter ::  single = kind(1.0e0)
  !> double precision parameter for real vars
  !integer, parameter ::  double = kind(1.0d0)
  !> error codes 
  integer, parameter :: wrn_IO=1,wrn_read=11,wrn_write=21
  integer, parameter :: wrn_val=3,wrn_inp=13,wrn_out=23
  integer, parameter :: err_IO=101,err_inp=111,err_out=121
  integer, parameter :: err_read=161,err_write=171
  integer, parameter :: err_alloc=131,err_dealloc=141
  integer, parameter :: err_vtk=151
  integer, parameter :: err_val=201
  !> double parameters for useful constants
!!$  real(kind=double), parameter  :: zero=0.0d0
!!$  real(kind=double), parameter  :: one=1.0d0
!!$  real(kind=double), parameter  :: two=2.0d0
!!$  real(kind=double), parameter  :: three=3.0d0
!!$  real(kind=double), parameter  :: four=4.0d0
!!$  real(kind=double), parameter  :: onehalf=0.5d0
!!$  real(kind=double), parameter  :: onethird=1.0d0/3.0d0
!!$  real(kind=double), parameter  :: onefourth=1.0d0/4.0d0
!!$  real(kind=double), parameter  :: onesixth=1.0d0/6.0d0
!!$  real(kind=double), parameter  :: verysmall=1.0d-40
!!$  real(kind=double), parameter  :: small=1.0d-15  
!!$  real(kind=double), parameter  :: large=1.0d10
!!$  real(kind=double), parameter  :: huge=1.0d30
!!$  real(kind=double), parameter  :: pigreco=4.0d0*atan(one)

  public :: newton_raphson_reverse

  type, public :: file
     logical :: exist=.False.     !< Logical of existence
     integer :: lun              !< I/O unit number
     character (len=256) :: fn    !< I/O file name or directory name
     character (len=256) :: fnloc !< I/O file name or directory name, local
     !  integer(hid_t)  :: hdf5_id=0 !< ID numbe for corresponding dataset in h5df
   contains
     !> init,kill,info methods
     !> (public for type file)
     procedure, public, pass :: init => fn_init
     procedure, public, pass :: kill => fn_kill
     procedure, public, pass :: info => fn_print
     procedure, public, nopass :: get_dirname
  end type file

  type, public :: newton_input
     !> Maximum number of iterations 
     integer :: max_nonlinear_iterations=30
     !> Incremental stept
     real(kind=double) :: alpha=one
     !> Tolerance in solving non-linear equation
     real(kind=double) :: tolerance_nonlinear=1e-12
     real(kind=double) :: relative_tolerance=0.0
     real(kind=double) :: absolute_tolerance=0.0
     !> Tolerance solving linear J x =-s
     real(kind=double) :: tolerance_linear=1e-5
     real(kind=double) :: max_tolerance_linear=1e-3
     real(kind=double) :: gamma=0.999d0
     real(kind=double) :: safeguard=1.0d-1

     
     !> User passed norm of non-linear equation 
     real(kind=double) :: fnewton_norm
     !> Absolute and relative grown of the residuum 
     real(kind=double) :: absolute_growth_factor=1.0d6
     real(kind=double) :: relative_growth_factor=1.0d4
     real(kind=double) :: line_search_contraction_rate=1.05d0
     real(kind=double) :: alpha_limit=5.0d-2
  end type newton_input

  type, public :: newton_output
     integer :: current_newton_step=0
     real(kind=double) :: initial_fnewton_norm=huge
     real(kind=double) :: previuous_fnewton_norm=huge
     real(kind=double) :: previuous_linear_tolerance=zero
     real(kind=double) :: inexact_linear_tolerance=1.0d-5
  end type newton_output

contains

  !>-------------------------------------------------------------
  !> Handle and write alert I/O errors.
  !> (public global procedure)
  !>
  !> usage:
  !>    rc= IOerr(lun, errno, call_proc [, add_msg] [, add_int])
  !> where:
  !> \param[in] lun -> integer. I/O unit for output of error messages
  !> \param[in] errno -> integer. error number
  !> \param[in] call_proc -> character. name of the procedure where
  !>                          the error occured
  !> \param[in] (optional) add_msg -> character.
  !>                                   additional message to be printed
  !> \param[in] (optional) add_int -> integer.
  !>                                   integer to be output as string
  !>                                   in the message
  !>
  !> \return rc -> logical. true if execution continues
  !>                (obviously no return otherwise as execution stops)
  !>
  !> errno convention:
  !> 0<errno<=100 -> warning: execution continues
  !> errno > 100  -> severe: execution terminates 
  !>
  !>
  !> contains:
  !> private function errmsg storing the message values
  !> 
  !<-------------------------------------------------------------
  function IOerr(lun, errno, call_proc, add_msg, add_int) result(rc)
    implicit none
    integer, intent(in) :: lun,errno
    integer, optional, intent(in) :: add_int
    logical :: rc
    character(len=*), intent(in) :: call_proc
    character(len=*), optional, intent(in) :: add_msg
    ! local vars
    character(len=256) :: msg
    integer :: int

    if (present(add_msg)) then
       msg=trim(add_msg)
       if(present(add_int)) then
          int=add_int
       else
          int = 0
       end if
    else
       msg=''
       int = 0
    end if
    write(lun,*) ''
    write(lun,*) ' *************'
    if (errno .gt. 100) then
       write(lun,fmt=100) '  SEVERE ERROR:'
       write(lun,fmt=101) ' in procedure: ',trim(call_proc)
       write(lun,fmt=101) trim(errmsg(errno, trim(msg), int))
       stop 'EXECUTION TERMINATED IN PROC IOerr'
    else if (errno .gt. 0) then
       write(lun,fmt=100) ' WARNING ERROR:'
       write(lun,fmt=101) 'in procedure: ',trim(call_proc)
       write(lun,fmt=101) etb((errmsg(errno, trim(msg), int)))
       rc=.true.
    else
       write(lun,fmt=100) ' IO ERROR NUMBER not found'
       stop 'EXECUTION TERMINATED IN PROC IOerr'
    end if

100 format(1x,a,1x,i5)
101 format(5(1x,a))
  contains

    !>-------------------------------------------------------------
    !> return msg value indexed by a number
    !> (private procedure of IOerr function)
    !>
    !> usage:
    !>     msg = errmsg(errno, addmsg, int)
    !>
    !> where:
    !> \param[in] errno -> integer, error number
    !> \param[in] addmsg -> character, additional message to be printed
    !> \param[in] int -> integer, to be included in msg if nonzero
    !> \return msg -> character, error message
    !>
    !<-------------------------------------------------------------
    function errmsg(errno, addmsg, addint) result(msg)
      implicit none
      integer, intent(in) :: errno
      character(len=*), intent(in) :: addmsg
      integer, intent(in) :: addint
      character(len=256) :: msg
      ! local vars
      character(len=5) :: rdwr

      if(addint.eq.0) then
         rdwr=''
      else
         write(rdwr,'(i5)') addint
      end if

      select case (errno)
      case (wrn_IO) ! file/dir not found but continues execution
         msg='File or Directory '//trim(addmsg)//' (unit '//rdwr//') does not exist.'
      case (wrn_read) ! error in reading file
         msg='Error Reading File '//trim(addmsg)//' (unit '//rdwr//')'
      case (wrn_write) ! error in writing file
         msg='Error Writing File '//trim(addmsg)//' (unit '//rdwr//')'
      case (wrn_val) ! error in inputs data
         msg='Error In Input/Output Parameter '//trim(addmsg)//' (info='//rdwr//')'
      case (wrn_inp) ! error in inputs data
         msg='Error In Input Parameter '//trim(addmsg)//rdwr//')'
      case (wrn_out) ! error in output
         msg='Error In Output Parameter '//trim(addmsg)//rdwr//')'
      case (err_IO) ! file/dir not found but stops execution
         msg='File or Directory '//trim(addmsg)//' (unit '//rdwr//') does not exist.'
      case (err_read) ! error reading a number in input
         msg = '    ...error reading file '//trim(addmsg)//' (iostat='//rdwr//')'
      case (err_write) ! error writing a number in input
         msg = '    ...error writing file '//trim(addmsg)//' (iostat='//rdwr//')'
      case (err_inp) ! error reading a number in input
         msg = 'Error In Input Parameter '//trim(addmsg)//' ( '//rdwr//' )'
      case (err_out) ! error writing a number in input
         msg = 'Error In Output Parameter '//trim(addmsg)
      case (err_alloc) ! error allocating an array
         msg = '    ...alloc fail var '//trim(addmsg)//' (stat='//rdwr//')'
      case (err_dealloc) ! error deallocating an array
         msg = '    ...dealloc fail var '//trim(addmsg)//' (stat='//rdwr//')'
      case (err_vtk) ! error writing vtk files
         msg = '    ...VTK library: '//trim(addmsg)//rdwr
      case (err_val) ! error in parameter values
         msg = '    ...wrong parameter value: '//trim(addmsg)//rdwr
      case default
         msg = '  no known error number'
      end select
    end function errmsg
  end function IOerr

  !>-------------------------------------------------------------
  !> Transform string strIn to upper case
  !> (private procedure for type IOfdescr)
  !>
  !> usage:
  !>     str = to_upper(strIn)
  !> 
  !> where:
  !> \param[in] strIn -> input string
  !> \return strOut -> output string
  !>
  !>
  !> Adapted from http://rosettacode.org/wiki/String_case#Fortran
  !> Original author: Clive Page
  !<-------------------------------------------------------------
  function to_upper(strIn) result(strOut)
    implicit none
    ! vars
    character(len=*), intent(in) :: strIn
    character(len=len(strIn)) :: strOut
    ! local vars
    integer :: i

    do i = 1, len(strIn)
       select case(strIn(i:i))
       case("a":"z")
          strOut(i:i) = achar(iachar(strIn(i:i))-32)
       end select
    end do

  end function to_upper

  !>-------------------------------------------------------------
  !> Transform string strIn to lowercase case
  !> (private procedure for type IOfdescr)
  !>
  !> usage:
  !>   str = to_lower(strIn)
  !>
  !> where:
  !> \param[in] strIn -> input string
  !> \return strOut - > output string
  !>
  !> (we'll see if it is needed somewhere else.
  !> In this case it will become public, but possibly must
  !> be moved into the Globals module (MP)
  !>
  !> Adapted from http://rosettacode.org/wiki/String_case#Fortran
  !> Original author: Clive Page
  !<-------------------------------------------------------------
  function to_lower(strIn) result(strOut)
    implicit none
    ! vars
    character(len=*), intent(in) :: strIn
    character(len=len(strIn)) :: strOut
    ! local vars
    integer :: i

    do i = 1, len(strIn)
       select case(strIn(i:i))
       case("A":"Z")
          strOut(i:i) = achar(iachar(strIn(i:i))+32)
       case default
          strOut(i:i) = strIn(i:i)
       end select
    end do
  end function to_lower

  !>-------------------------------------------------------------
  !> return string strIn with no preceding and trailing spaces
  !>     (just a short name for trim(adjustl......))
  !> (private procedure fortype IOfdescr)
  !> 
  !> usage:
  !>   str = etb(strIn)
  !>
  !> where:
  !> \param[in] strIn -> input string
  !> \return strOut -> output string
  !>
  !> (we'll see if it is needed somewhere else.
  !> In this case it will become public, but possibly must
  !> be moved into the Globals module (MP)
  !> 
  !<-------------------------------------------------------------
  function etb(strIn) result(strOut)
    implicit none
    ! vars
    character(len=*), intent(in) :: strIn
    character(len=len_trim(adjustl(strIn))) :: strOut

    strOut=trim(adjustl(strIn))
  end function etb

  !>-------------------------------------------------------------
  !> erase comments from a string
  !> (private procedure for type IOfdescr)
  !>
  !> usage:
  !>   str = erase_comment(strIn)
  !>
  !> where:
  !> \param[in] strIn -> input string
  !> \return strOut -> output string
  !>
  !> a comment is a trailing string beginning
  !> with character ! (F90-style) or 
  !> with character % (TeX-style) 
  !> with character # (script-bash-style) 
  !<-------------------------------------------------------------
  function erase_comment(strIn) result(strOut)
    implicit none
    ! vars
    character(len=*), intent(in) :: strIn
    character(len=len_trim(adjustl(strIn))) :: strOut
    ! local vars
    character(len=1) :: strloc
    integer :: i,j

    do i=1,len_trim(adjustl(strIn))

       strloc=strIn(i:i)
       if (strloc .eq. '!' .or. strloc.eq.'%' .or. strloc.eq.'#') then
          do j=i,len_trim(adjustl(strIn))
             strOut(j:j)=' '
          end do
          exit
       else
          strOut(i:i)=strloc
       end if
    end do
  end function erase_comment

  !>-------------------------------------------------------------
  !> Function that calculates vectors averages
  !> (public global procedure)
  !> Cases:
  !>  on edge -> between two vectors related to the nodes of the edge
  !>  on cell -> between three vectors related to the vertices of the triangle
  !> 
  !> usage:
  !>     res_avg = avg_vec3(avg_type,vec1,vec2 [,vec3])
  !>
  !> where:
  !> \param[in] avg_type -> integer. select type of average
  !>                        case(0) = arithmetic average
  !> \param[in] vec1, vec2 -> real(double). dimension(3)
  !>                          vectors to be averaged
  !> \param[in] (optional) vec3 -> real(double). dimension(3)
  !>                               vector to be averaged
  !>
  !> \return res_avg -> real(double). dimension(3)
  !>                    averaged vector
  !>
  !<-------------------------------------------------------------
  function  avg_vec3(avg_type, vecA, vecB, vecC) result (res_avg)
    implicit none
    ! vars
    integer, intent(in) :: avg_type
    real(kind=double), intent(in) :: vecA(3), vecB(3)
    real(kind=double), optional, intent(in) :: vecC(3)
    real(kind=double) :: res_avg(3)
    ! local vars
    integer :: i

    if (present(vecC)) then
       ! cell average
       select case(avg_type)
       case(0) 
          do i=1,3
             res_avg(i) = avg_scal(avg_type,vecA(i),vecB(i),vecC(i))
          end do
       end select
    else
       ! edge average
       select case(avg_type)
       case(0)
          do i=1,3
             res_avg(i) = avg_scal(avg_type,vecA(i),vecB(i))
          end do
       end select
    end if

  end function avg_vec3

  !>-------------------------------------------------------------
  !> Function that calculates scalars averages
  !> (public global procedure)
  !> Cases: 
  !>  on edge -> between two scalars related to the nodes of the edge
  !>  on cell -> between three scalars related to the vertices of the triangle
  !> 
  !> usage:
  !>     res_avg = avg_scal(avg_type, A, B, [,C])
  !>
  !> where:
  !> \param[in] avg_type -> integer. select type of average
  !>                        case(0) = arithmetic average
  !> \param[in] A, B -> real(double). scalars to be averaged
  !> \param[in] (optional) C -> real(double). scalar to be averaged
  !>
  !> \return res_avg -> real(double). average
  !>
  !<-------------------------------------------------------------
  function  avg_scal(avg_type, A, B, C) result (res_avg)
    implicit none
    ! vars
    integer, intent(in) :: avg_type
    real(kind=double), intent(in) :: A, B
    real(kind=double), optional, intent(in) :: C
    real(kind=double) :: res_avg
    ! local vars

    if (present(C)) then
       ! cell average
       select case(avg_type)
       case(0)
          res_avg = onethird*(A+B+C)
       end select
    else
       ! edge average
       select case(avg_type)
       case(0)
          res_avg = onehalf*(A+B)
       end select
    end if

  end function avg_scal

  !>-------------------------------------------------------------
  !> Function that calculates cross-products
  !> (public global procedure)
  !> 
  !> usage:
  !>     res_cross = cross(vec1,vec2)
  !>
  !> where:
  !> \param[in] vec1, vec2 -> real(double). dimension(3)
  !>                          vectors 
  !>
  !> \return res_cross -> real(double). dimension(3)
  !>                     vector
  !>
  !<-------------------------------------------------------------
  function  cross(vecA, vecB) result (res_cross)   
    implicit none
    ! vars
    real(kind=double), intent(in) :: vecA(3), vecB(3)
    real(kind=double) :: res_cross(3)
    ! local vars

    res_cross(1) = vecA(2)*vecB(3)-vecA(3)*vecB(2)
    res_cross(2) = vecA(3)*vecB(1)-vecA(1)*vecB(3)
    res_cross(3) = vecA(1)*vecB(2)-vecA(2)*vecB(1)

  end function cross



  !>--------------------------------------------------------------
  !> Simple sort algorithm to sort in increasing order integer 
  !> array. Use only for small narray.
  !> Use global_heapsort for big arrays
  !>-------------------------------------------------------------
  subroutine isort(narray,array)
    implicit none
    integer, intent(in   ) :: narray
    integer, intent(inout) :: array(narray)
    ! 
    integer :: itemp
    integer :: i,j ,indx,isgn

    if (narray.lt.2) return
    ! Initialize.
    i = 0
    indx = 0
    isgn = 0
    j = 0
    do 
       call global_heapsort(narray, indx, i,j,isgn)
       if (indx .gt. 0 ) then
          ! SWAP ELEMENT 
          itemp    =  array(i)
          array(i) = array(j)
          array(j) = itemp
       else if ( indx .lt. 0) then
          ! COMPARE (array(i) and array(j) )
          isgn = 0
          if ( array(i) .lt. array(j)  ) isgn = -1
          if ( array(i) .gt. array(j)  ) isgn = 1
       else if ( indx .eq. 0 ) then
          exit
       end if
    end do
  end subroutine isort

  !>-----------------------------------------
  !> Given a sorted array, it is returned with the
  !> list of uniques elements in the first positions
  !<----------------------------------
  subroutine unique_of_sorted(n_elements,elements,nunique)
    implicit none
    integer, intent(in   ) :: n_elements
    integer, intent(inout) :: elements(n_elements)
    integer, intent(inout) :: nunique
    ! local
    integer :: unique_index, unique_element, current_element, i
    
    ! nothing to do
    if (n_elements <= 1) return

    unique_index = 1
    unique_element = elements(1)
    nunique = 1
    do i = 2, n_elements
       current_element = elements(i)
       if ( current_element .ne. unique_element) then
          ! new node found. Add an moved it in the
          ! corresponding position
          nunique = nunique + 1
          elements(nunique) = current_element

          unique_element = current_element
       end if
    end do

  end subroutine unique_of_sorted

  ! says if the first array a is in lexicographic order
  ! with respect to the second array b
  function lexicographic_order(n,a,b) result(a_before_b)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: a(n),b(n)
    logical :: a_before_b
    !local 
    integer             :: i
    a_before_b = .true.
    do i=1,n
       if  ( a(i) .gt. b(i) ) then
          a_before_b = .false.    
          exit
       else if ( a(i) .lt. b(i) ) then
          a_before_b = .true.    
          exit
       end if
    end do
  end function lexicographic_order

  

  !>---------------------------------------------------------------
  !> Subroutine for ordering items (integer real etc) with the 
  !> heapsort algorithm via reverse comunication style
  !> The code is taken from 
  !>
  !> https://people.sc.fsu.edu/~jburkardt/f_src/rcm/rcm.html
  !>
  !>--------------------------------------------------------------
  subroutine global_heapsort( n, indx, i, j, isgn )
    !
    ! SORT_HEAP_EXTERNAL externally sorts a list 
    !  of items into ascending order.
    !
    !  Discussion:
    !
    !    The actual list of data is not passed to the routine.  
    !    Hence this routine may be used to sort integers, reals,
    !    numbers, names, dates, shoe sizes, and so on.  
    !    After each call, the routine asks
    !    the user to compare or interchange two items, until a special
    !    return value signals that the sorting is completed.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    05 February 2004
    !
    !  Author:
    !
    !    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
    !    FORTRAN90 version by John Burkardt.
    !
    !  Reference:
    !
    !    Albert Nijenhuis and Herbert Wilf,
    !    Combinatorial Algorithms,
    !    Academic Press, 1978, second edition,
    !    ISBN 0-12-519260-6.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N,
    !    the number of items to be sorted.
    !
    !    Input/output, integer ( kind = 4 ) INDX, 
    !    the main communication signal.
    !
    !    The user must set INDX to 0 before the first call.
    !    Thereafter, the user should not change the value of INDX until
    !    the sorting is done.
    !
    !    On return, if INDX is
    !
    !      greater than 0,
    !      * interchange items I and J;
    !      * call again.
    !
    !      less than 0,
    !      * compare items I and J;
    !      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
    !      * call again.
    !
    !      equal to 0, the sorting is done.
    !
    !   Example of use : sort array of thing of legnth nnz
    !   Code
    !   
    !   if (nnz.lt.2) return
    !   Initialize.
    !   i = 0
    !   indx = 0
    !   isgn = 0
    !   j = 0
    !   do 
    !      call global_heapsort(nnz, indx, i,j,isgn)
    !      if (indx .gt. 0 ) then
    !          ! SWAP ELEMENT 
    !          array(i) and array(j)
    !       else if ( indx .lt. 0) then
    !          ! COMPARE (array(i) and array(j) )
    !          isgn = 0
    !          if ( array(i) .lt. array(j)  ) isgn = -1
    !          if ( array(i) .gt. array(j)  ) isgn = 1
    !       else if ( indx .eq. 0 ) then
    !          exit
    !       end if
    !    end do
    !
    !  
    !
    !    Output, integer ( kind = 4 ) I, J, the indices of two items.
    !    On return with INDX positive, elements I and J should 
    !    be interchanged.
    !    On return with INDX negative, elements I and J should 
    !    be compared, and
    !    the result reported in ISGN on the next call.
    !
    !    Input, integer ( kind = 4 ) ISGN, results of comparison of elements 
    !    I and J.  (Used only when the previous call returned INDX less than 0).
    !    ISGN <= 0 means I is less than or equal to J;
    !    0 <= ISGN means I is greater than or equal to J.
    !
    implicit none

    integer ( kind = 4 ) i
    integer ( kind = 4 ), save :: i_save = 0
    integer ( kind = 4 ) indx
    integer ( kind = 4 ) isgn
    integer ( kind = 4 ) j
    integer ( kind = 4 ), save :: j_save = 0
    integer ( kind = 4 ), save :: k = 0
    integer ( kind = 4 ), save :: k1 = 0
    integer ( kind = 4 ) n
    integer ( kind = 4 ), save :: n1 = 0
    !
    !  INDX = 0: This is the first call.
    !
    if ( indx == 0 ) then

       i_save = 0
       j_save = 0
       k = n / 2
       k1 = k
       n1 = n
       !
       !  INDX < 0: The user is returning the results of a comparison.
       !
    else if ( indx < 0 ) then

       if ( indx == -2 ) then

          if ( isgn < 0 ) then
             i_save = i_save + 1
          end if

          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return

       end if

       if ( 0 < isgn ) then
          indx = 2
          i = i_save
          j = j_save
          return
       end if

       if ( k <= 1 ) then

          if ( n1 == 1 ) then
             i_save = 0
             j_save = 0
             indx = 0
          else
             i_save = n1
             n1 = n1 - 1
             j_save = 1
             indx = 1
          end if

          i = i_save
          j = j_save
          return

       end if

       k = k - 1
       k1 = k
       !
       !  0 < INDX, the user was asked to make an interchange.
       !
    else if ( indx == 1 ) then

       k1 = k

    end if

    do

       i_save = 2 * k1

       if ( i_save == n1 ) then
          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return
       else if ( i_save <= n1 ) then
          j_save = i_save + 1
          indx = -2
          i = i_save
          j = j_save
          return
       end if

       if ( k <= 1 ) then
          exit
       end if

       k = k - 1
       k1 = k

    end do

    if ( n1 == 1 ) then
       i_save = 0
       j_save = 0
       indx = 0
       i = i_save
       j = j_save
    else
       i_save = n1
       n1 = n1 - 1
       j_save = 1
       indx = 1
       i = i_save
       j = j_save
    end if

    return
  end subroutine global_heapsort

  function ifind(narray,array,tobefound) result (i)
    implicit none
    integer, intent(in) :: narray
    integer, intent(in) :: tobefound
    integer, intent(in) :: array(narray)
    integer i
    !local
    logical :: found

    i = 1
    found = ( array(i) .eq. tobefound ) 
    do while ( (.not. found) .and. (i .lt. narray) ) 
       i=i+1
       found = (array(i) .eq. tobefound )  
    end do
    if ( .not. found ) i=0
  end function ifind

  subroutine double_col_permute ( m, n, p, a )
    !****************************************************************************
    !
    !  ! R8COL_PERMUTE permutes an R8COL in place.
    !
    !  Discussion:
    !
    !    An R8COL is an M by N array of double precision values, regarded
    !    as an array of N columns of length M.
    !
    !    The same logic can be used to permute an array of objects of any
    !    arithmetic type, or an array of objects of any complexity.  The only
    !    temporary storage required is enough to store a single object.  The number
    !    of data movements made is N + the number of cycles of order 2 or more,
    !    which is never more than N + N/2.
    !
    !  Example:
    !
    !    Input:
    !
    !      M = 2
    !      N = 5
    !      P = (   2,    4,    5,    1,    3 )
    !      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
    !          (11.0, 22.0, 33.0, 44.0, 55.0 )
    !
    !    Output:
    !
    !      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
    !             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 December 2017 (Enrico Facca)
    !    simple modification and adaptation to my code
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the dimension of objects.
    !
    !    Input, integer ( kind = 4 ) N, the number of objects.
    !
    !    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
    !    that the I-th element of the output array should be the J-th
    !    element of the input array.  P must be a legal permutation
    !    of the integers from 1 to N, otherwise the algorithm will
    !    fail catastrophically.
    !
    !    Input/output, real ( kind = 8 ) A(M,N), the array to be permuted.
    !
    implicit none

    integer ( kind = 4 ) m
    integer ( kind = 4 ) n

    real ( kind = double ) a(m,n)
    real ( kind = double ), allocatable ::  a_temp(:)

    integer ( kind = 4 ) res
    integer ( kind = 4 ) iget
    integer ( kind = 4 ) iput
    integer ( kind = 4 ) istart
    integer ( kind = 4 ) p(n)
    !
    !  Search for the next element of the permutation that has not been used.
    !
    allocate(a_temp(m),stat=res)
    if (res .ne. 0) write(*,*) ' Error allocation double double_col_permute'

    do istart = 1, n

       if ( p(istart) < 0 ) then

          cycle

       else if ( p(istart) == istart ) then

          p(istart) = -p(istart)
          cycle

       else

          a_temp(1:m) = a(1:m,istart)
          iget = istart
          !
          !  Copy the new value into the vacated entry.
          !
          do

             iput = iget
             iget = p(iget)

             p(iput) = -p(iput)

             if ( iget < 1 .or. n < iget ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'R8COL_PERMUTE - Fatal error!'
                write ( *, '(a)' ) '  A permutation index is out of range.'
                write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
                stop
             end if

             if ( iget == istart ) then
                a(1:m,iput) = a_temp(1:m)
                exit
             end if

             a(1:m,iput) = a(1:m,iget)

          end do

       end if

    end do
    !
    !  Restore the signs of the entries.
    !
    p(1:n) = -p(1:n)

    return

    deallocate(a_temp,stat=res)
    if (res .ne. 0) write(*,*) ' Error deallocation double double_col_permute'
  end subroutine double_col_permute


  subroutine integer_col_permute ( m, n, p, a )

    !**************************************************************************
    !
    !  ! R8COL_PERMUTE permutes an R8COL in place.
    !
    !  Discussion:
    !
    !    An R8COL is an M by N array of double precision values, regarded
    !    as an array of N columns of length M.
    !
    !    The same logic can be used to permute an array of objects of any
    !    arithmetic type, or an array of objects of any complexity.  The only
    !    temporary storage required is enough to store a single object.  The number
    !    of data movements made is N + the number of cycles of order 2 or more,
    !    which is never more than N + N/2.
    !
    !  Example:
    !
    !    Input:
    !
    !      M = 2
    !      N = 5
    !      P = (   2,    4,    5,    1,    3 )
    !      A = ( 1.0,  2.0,  3.0,  4.0,  5.0 )
    !          (11.0, 22.0, 33.0, 44.0, 55.0 )
    !
    !    Output:
    !
    !      A    = (  2.0,  4.0,  5.0,  1.0,  3.0 )
    !             ( 22.0, 44.0, 55.0, 11.0, 33.0 ).
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    04 December 2017 (Enrico Facca)
    !    simple modification and adaptation to my code
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) M, the dimension of objects.
    !
    !    Input, integer ( kind = 4 ) N, the number of objects.
    !
    !    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
    !    that the I-th element of the output array should be the J-th
    !    element of the input array.  P must be a legal permutation
    !    of the integers from 1 to N, otherwise the algorithm will
    !    fail catastrophically.
    !
    !    Input/output, real ( kind = 8 ) A(M,N), the array to be permuted.
    !
    implicit none

    integer ( kind = 4 ) m
    integer ( kind = 4 ) n

    integer ( kind = 4 ) :: a(m,n)
    integer ( kind = 4 ) ,allocatable ::  a_temp(:)

    integer ( kind = 4 ) res
    integer ( kind = 4 ) iget
    integer ( kind = 4 ) iput
    integer ( kind = 4 ) istart
    integer ( kind = 4 ) p(n)
    !
    !  Search for the next element of the permutation that has not been used.
    !
    allocate(a_temp(m),stat=res)
    if (res .ne. 0) write(*,*) ' Error allocation double double_col_permute'

    do istart = 1, n

       if ( p(istart) < 0 ) then

          cycle

       else if ( p(istart) == istart ) then

          p(istart) = -p(istart)
          cycle

       else

          a_temp(1:m) = a(1:m,istart)
          iget = istart
          !
          !  Copy the new value into the vacated entry.
          !
          do

             iput = iget
             iget = p(iget)

             p(iput) = -p(iput)

             if ( iget < 1 .or. n < iget ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'integer COL_PERMUTE - Fatal error!'
                write ( *, '(a)' ) '  A permutation index is out of range.'
                write ( *, '(a,i8,a,i8)' ) '  P(', iput, ') = ', iget
                stop
             end if

             if ( iget == istart ) then
                a(1:m,iput) = a_temp(1:m)
                exit
             end if

             a(1:m,iput) = a(1:m,iget)

          end do

       end if

    end do
    !
    !  Restore the signs of the entries.
    !
    p(1:n) = -p(1:n)

    return

    deallocate(a_temp,stat=res)
    if (res .ne. 0) write(*,*) &
         ' Error deallocation double double_col_permute'
  end subroutine integer_col_permute

  !>------------------------------------------------------------------
  !> Evaluates the weighted p-norm
  !>------------------------------------------------------------------
  function p_norm(ndata,power,data,weight) result(total)
    implicit none
    integer,                     intent(in) :: ndata
    real(kind=double),           intent(in) :: power
    real(kind=double),           intent(in) :: data(ndata)
    real(kind=double), optional, intent(in) :: weight(ndata)
    real(kind=double) :: total
    !local 
    integer :: i
    real(kind=double) :: ddot

    if ( abs(power) < small) then
       total = maxval( abs( data ) )
    else
       if ( present(weight) ) then
          if ( abs(power-one) < small ) then
             total = ddot(ndata, abs(data) ,1, weight,1)
          else
             total = zero
             do i = 1, ndata
                total = total + abs(data(i)) ** power * weight(i)
             end do
             total = total ** ( one / power )
          end if
       else
          if ( abs(power-one) < small ) then
             total = sum(abs(data))
          else
             total = zero
             do i = 1, ndata
                total = total + abs(data(i)) ** power 
             end do
             total = total ** ( one / power )
          end if
       end if
    end if
  end function p_norm

  !>-----------------------------------------------------
  !> Procedure orthogonalizing x w.r.t. a series of vectors
  !>-----------------------------------------------------s
  subroutine ortogonalize(dim,nvectors,vectors,x)
    implicit none
    integer,           intent(in   ) :: dim
    integer,           intent(in   ) :: nvectors
    real(kind=double), intent(in   ) :: vectors(dim,nvectors)
    real(kind=double), intent(inout) :: x(dim)

    ! local
    integer :: i
    real(kind=double) :: ddot,alpha,beta

    do i=1,nvectors
       beta  = ddot(dim,vectors(1,i),1,x,1)
       alpha = -beta/ddot(dim,vectors(1,i),1,vectors(1,i),1)
       !x=x-alpha*vectors(:,i)
       call daxpy(dim,alpha,vectors(1,i),1,x,1)
    end do

  end subroutine ortogonalize

  !>-------------------------------------------------------------
  !> Procedure that computes the projection of a vector
  !> (of dimension ndim) onto the plane orthogonal to (normal)
  !> 
  !> usage:
  !>     call orthogonal_projection(ndim,vector,normal,result)
  !>
  !> where:
  !> \param[in ] ndim   -> integer. dimension of the vectors
  !> \param[in ] vector -> real(double). dim=ndim vector to be projected
  !> \param[in ] normal -> real(double). dim=ndim normal to the plane
  !> \param[out] result -> real(double). dim=ndim projected vector
  !>
  !<-------------------------------------------------------------
  subroutine orthogonal_projection(ndim, vector, normal, res_proj)
    implicit none
    ! vars
    integer,           intent(in)   :: ndim
    real(kind=double), intent(in)   :: vector(ndim), normal(ndim)
    real(kind=double), intent(out)  :: res_proj(ndim)
    ! local vars
    real(kind=double) :: alpha, beta
    real(kind=double) :: ddot
    
    beta  = ddot(ndim,vector,1,normal,1)
    alpha = -beta/ddot(ndim,normal,1,normal,1)
    !x=x-alpha*vectors(:,i)
    call dcopy(ndim,vector,1,res_proj,1)
    call daxpy(ndim,alpha,normal,1,res_proj,1)

  end subroutine orthogonal_projection



  !>-------------------------------------------------------------
  !> Info procedure.
  !> (public procedure for type file)
  !> Prints content of a variable of type file
  !> 
  !> usage:
  !>     call 'var'\%info(lun)
  !>
  !> where:
  !> \param[in] lun: output unit
  !>
  !> uses private function etb
  !<-------------------------------------------------------------
  subroutine fn_print(this, lun)
    implicit none
    class(file), intent(in) :: this
    integer,     intent(in) :: lun

    write(lun,'(a,a,a,i3)') ' filename ',etb(this%fn), &
         ' linked to lun ',this%lun

  end subroutine fn_print

  !>-------------------------------------------------------------
  !> Info procedure.
  !> (public procedure for type file)
  !> Prints content of a variable of type file
  !> 
  !> usage:
  !>     call 'var'\%info(lun)
  !>
  !> where:
  !> \param[in] lun: output unit
  !>
  !> uses private function etb
  !<-------------------------------------------------------------
  function get_dirname(str_file) result(str_dir)
    implicit none
    ! vars
    character(len=*), intent(in) :: str_file
    character(len=len_trim(adjustl(str_file))) :: str_dir
    !local 
    integer :: bar_pos,nstr,pbar

    str_dir(:)=' '
    
    nstr = len(etb(str_file))
    pbar = scan(etb(str_file),"/", BACK= .true.)
    if ( pbar .eq. 0 ) then
       str_dir(1:1)='.'
    else
       str_dir(1:pbar)=str_file(1:pbar)
    end if
  end function get_dirname

  !>-------------------------------------------------------------
  !> init procedure.
  !> (public procedure for type file)
  !> instantiates variable 
  !> 
  !> usage:
  !>     call 'var'\%init(lun_err,fn,lun,io_flag,
  !>                     [mandatory_)
  !>
  !> where:
  !> \param[in] lun_err: integer; logical unit for error
  !>            messages
  !> \param[in] fn: character; filename
  !> \param[in] lun: integer; logical unit number
  !> \param[in] io_flag: character; ="in" if filename is an input
  !>            file and its existence must be checked, "out"
  !>            if it is an output file. 
  !> \param[in] (optional) mandatory_input: logical; 
  !>            Specify if input file is mandatory 
  !>            .True.  (default) => file is mandatory 
  !>              (stop if file does not exists)
  !>            .False. => file is optional 
  !>              (proceed if file does not exists)
  !>            file and its existence must be checked, "out"
  !>            if it is an output file. 
  !> \param[in] (optional) folder: logical; 
  !>            Specify if input file is a file or a folder 
  !>            .True.            => file is folder 
  !>               (Check existence and not open unit)
  !>            .False. (Default) => file is file 
  !>               (Check existence and open if mandatory)
  !> uses private function etb
  !<-------------------------------------------------------------
  subroutine fn_init(this, lun_err, fn, lun, io_flag, mandatory_input, folder,verbose,info)
    implicit none
    class(file),      intent(inout) :: this
    integer,          intent(in)    :: lun_err
    character(len=*), intent(in)    :: fn
    integer,          intent(in)    :: lun
    character(len=*), intent(in)    :: io_flag 
    logical, optional, intent(in)   :: mandatory_input
    logical, optional, intent(in)   :: folder
    logical, optional, intent(in)   :: verbose
    integer, optional, intent(inout)    :: info

    ! local vars
    logical :: exist,file_exist,rc,mandatory,open_unit,printmsg
    integer :: res

    this%fn=etb(fn)
    this%lun=lun

    mandatory=.True.
    if ( present (mandatory_input) ) mandatory = mandatory_input
    printmsg=.True.
    if ( present (verbose) ) printmsg = verbose

    if ( present(info) ) info=0

    !write(*,*) mandatory ,present (mandatory_input),present (folder),present (verbose),present (info)
    
    !
    ! handle folders
    !
    open_unit = .True.
    if ( present (folder) ) open_unit=.not. folder 

    !
    ! for input files or folders check existence
    ! if marked as not mandatory print a warning message
    ! otherwise it stops
    !
    if(io_flag.eq.'in') then
       inquire(file=this%fn,exist=exist)
       this%exist=exist
       if (.not. exist) then
          !
          ! warning or stop
          !
          if ( mandatory ) then
             file_exist = IOerr(lun_err, err_IO, 'file%init',&
                  ' mandatory named :'//etb(this%fn)//' not found',lun)
          else
             if ( printmsg ) &
                  file_exist = IOerr(lun_err, wrn_IO, 'file%init',&
                  ' optional named : '//etb(this%fn)//' not found',lun)
          end if
			 if (present(info)) info = -1
          ! exit
          return
       end if
    end if

    !
    ! if exist and it is not a folder open unit 
    !
    if ( open_unit ) then
       open(lun,file=this%fn,iostat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_IO,&
            'file%init', &
            'err opening file '//etb(this%fn),lun)
       this%exist = .true.
    end if

  end subroutine fn_init

  !>-------------------------------------------------------------
  !> kill procedure.
  !> (public procedure for type file)
  !> kills variable 
  !> 
  !> usage:
  !>     call 'var'\%kill(lun_err)
  !>
  !> where:
  !> \param[in] lun_err: integer; logical unit for error
  !>            messages
  !>
  !<-------------------------------------------------------------
  subroutine fn_kill(this, lun_err)
    implicit none
    class(file),      intent(inout) :: this
    integer,          intent(in)    :: lun_err

    ! local vars
    logical :: rc,is_open
    integer :: res

    inquire (this%lun,opened=is_open)
    if ( is_open ) then
       close(this%lun,iostat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_IO,&
            'file%kill', &
            'err closing file '//etb(this%fn),res)
       this%exist = .false.
    end if
    this%fn=' '
    this%lun=-1

  end subroutine fn_kill

  !>-------------------------------------------------------------
  !> Reverse communication Newton-Raphson algorithm.
  !> for solving
  !> F(x)=0
  !> with x\in \REAL^{n} and
  !> F:\REAL^n\to \REAL^n
  !>
  !> The user must compute, according to the value of
  !> the integer variable flag.
  !>
  !> 
  !> usage: check test programin tests/directory
  !>
  !> where:
  !> \param[in] flag:  integer
  !>            flag=1 :: compute function F(x)
  !>            flag=2 :: compute the residual \|F(x)\|
  !>            flag=3 ::  find y with J y = - F
  !>                   If you want to use damped-
  !>                   set Newton step \alpha such that
  !>                   x^{k+1}=x^{k}+\alpha y
  !> \param[in] info     integer. Variable for error
  !>            info=0  : compute function F(x)
  !>            info=-1 : Maximum number of iterations reached
  !>            info=-2 : Newton diverged
  !> \param[in] nvar     integer. Dimension of unknown x
  !> \param[in] xvar     real(dimension=nvar)
  !>                     Unknown x in F(x)=0 and initial guess
  !> \param[in] increment real(dimension=nvar)
  !>                     Work space
  !> \param[inout] ctrl_newton: type(newton_input)
  !>                         Controls for Newton Algorithm
  !> \param[inout] info_newton: type(newton_output)
  !>                         Controls for Newton Algorithm

  !<-------------------------------------------------------------
  
  
  ! flag=2 asembly  fnewton 
  ! flag=3 solve jacobian  increment = -fnewton 
  subroutine newton_raphson_reverse_with_dt(&
       flag,info, nvar, xvar, increment,&
       ctrl,info_newton)
    implicit none
    ! comunication varibles
    integer,  intent(inout) :: flag
    integer,  intent(inout) :: info
    ! active variable
    integer,  intent(in   ) :: nvar
    real(kind=double),   intent(inout) :: xvar(nvar)
    real(kind=double),   intent(inout) :: increment(nvar)
    ! controls and info
    type(newton_input),  intent(in   ) :: ctrl
    type(newton_output), intent(inout) :: info_newton
    ! local
    real(kind=double) :: dnrm2
    real(kind=double) :: nuA, nuP

    if (info.ne.0) return

    select case (flag)
    case (1)
       !
       ! fnewton assembly
       !
       info_newton%current_newton_step = info_newton%current_newton_step + 1
       if ( info_newton%current_newton_step > ctrl%max_nonlinear_iterations ) then
          flag=-1
          info=-1
       else
          flag=2
          info=0
       end if
       if (info_newton%current_newton_step > 0 ) then
          info_newton%previuous_fnewton_norm     = ctrl%fnewton_norm
          info_newton%previuous_linear_tolerance = ctrl%tolerance_linear
       end if
       return
    case (2)
       !
       ! check if Newton convergence
       !
       if ( ctrl%fnewton_norm < ctrl%tolerance_nonlinear) then
          info = 0
          flag = 0
          return
       else
          flag=3
          info=0
       end if

       if ( info_newton%current_newton_step == 1) info_newton%initial_fnewton_norm = ctrl%fnewton_norm
       !
       ! check if Newton is diverging
       !
       if ( ctrl%fnewton_norm > ctrl%absolute_growth_factor * info_newton%initial_fnewton_norm) then
          info=-2
          flag=-2
       end if

       if ( info_newton%current_newton_step > 1) then
          if ( ctrl%fnewton_norm > ctrl%relative_growth_factor * info_newton%previuous_fnewton_norm) then
             info=-2
             flag=-1
          end if
       end if

       !
       ! suggest linear tolerance according to
       ! INEXACT NEWTON METHODS
       ! KELLER 
       !
       !write(*,*) info_newton%current_newton_step, &
       !     ctrl%fnewton_norm , info_newton%previuous_fnewton_norm, &
       !     info_newton%previuous_linear_tolerance, ctrl%gamma *info_newton%previuous_linear_tolerance**2
       if  ( info_newton%current_newton_step .eq. 1 ) then
          info_newton%inexact_linear_tolerance = ctrl%max_tolerance_linear
       else
          !
          nuA=ctrl%gamma * (ctrl%fnewton_norm / info_newton%previuous_fnewton_norm)**2
          !if ( ctrl%gamma *info_newton%previuous_linear_tolerance**2 .le. ctrl%safeguard ) then
          info_newton%inexact_linear_tolerance = min(ctrl%max_tolerance_linear,nuA)
          !else
          !   nuP=max(nuA,ctrl%gamma *info_newton%previuous_linear_tolerance**2)
          !   info_newton%inexact_linear_tolerance = &
          !        min(ctrl%max_tolerance_linear,nuP)
          !end if
       end if
       
       return
    case (3)
       !
       ! J increment = -fnewton solved
       !
       xvar=xvar+ctrl%alpha*increment
       flag=1
       info=0
       return
    end select


    
!!$    call newton_raphson_reverse(&
!!$         flag,info, &
!!$         info_newton%current_newton_step, nvar, xvar, increment,&
!!$         ctrl_newton%alpha,&
!!$         ctrl_newton%fnewton_norm, &
!!$         info_newton%initial_fnewton_norm,&
!!$         info_newton%previuous_fnewton_norm,&          
!!$         ctrl_newton%max_nonlinear_iterations,&  
!!$         ctrl_newton%tolerance_nonlinear,&
!!$         ctrl_newton%absolute_growth_factor,&
!!$         ctrl_newton%relative_growth_factor)


  end subroutine newton_raphson_reverse_with_dt

  ! flag=2 asembly  fnewton 
     ! flag=3 solve jacobian  increment = -fnewton 
     subroutine newton_raphson_reverse(&
          flag,info, &
          current_newton_step, nvar, xvar, increment,&
          alpha,&
          fnewton_norm,&
          initial_fnewton_norm, &
          previuous_fnewton_norm,&          
          max_nonlinear_iterations,  &
          tolerance_nonlinear,&
          absolute_growth_factor,relative_growth_factor)
       implicit none
       integer,  intent(inout) :: flag
       integer,  intent(inout) :: info
       integer,  intent(in   ) :: nvar
       real(kind=double),  intent(inout) :: xvar(nvar)
       real(kind=double),  intent(inout) :: increment(nvar)
       integer,  intent(in   ) :: max_nonlinear_iterations
       integer,  intent(inout) :: current_newton_step
       real(kind=double),  intent(in   ) :: alpha       
       real(kind=double),  intent(in   ) :: tolerance_nonlinear
       real(kind=double),  intent(in   ) :: fnewton_norm
       real(kind=double),  intent(inout) :: initial_fnewton_norm
       real(kind=double),  intent(inout) :: previuous_fnewton_norm
       real(kind=double),  intent(in   ) :: absolute_growth_factor
       real(kind=double),  intent(in   ) :: relative_growth_factor
       ! local
       real(kind=double) :: dnrm2

       if (info.ne.0) return
       
       select case (flag)
       case (1)
          !
          ! fnewton assembly
          !
          current_newton_step = current_newton_step + 1
          if ( current_newton_step > max_nonlinear_iterations ) then
             flag=-1
             info=-1
          else
             flag=2
             info=0
          end if
          if (current_newton_step > 0 ) then
             previuous_fnewton_norm = fnewton_norm
          end if
          return
       case (2)
          !
          ! check if Newton convergence
          !
          if ( fnewton_norm < tolerance_nonlinear) then
             info = 0
             flag = 0
             return
          else
             flag=3
             info=0
          end if
          
          if ( current_newton_step == 1) initial_fnewton_norm = fnewton_norm
          !
          ! check if Newton is diverging
          !
          if ( fnewton_norm > absolute_growth_factor * initial_fnewton_norm) then
             info=-2
             flag=-2
          end if

          if ( current_newton_step > 1) then
             if ( fnewton_norm > relative_growth_factor * previuous_fnewton_norm) then
                info=-2
                flag=-1
             end if
          end if
          return
       case (3)
          !
          ! J increment = -fnewton solved
          !
          xvar=xvar+alpha*increment
          flag=1
          info=0
          return
       end select
     end subroutine newton_raphson_reverse


end module Globals

