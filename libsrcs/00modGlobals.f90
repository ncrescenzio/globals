!-------------------------------------------------------------
! Define global vars
!
! err numbers:
! errno = [0:1][01-10] I/O errors (file existence etc...)
!       = [0:1][11-20] input errors (reads)
!       = [0:1][21-30] output errors (writes)
!       = [0:1][31-40] alloc errors
!       = [0:1][41-50] dealloc errors
!       = [0:1][51-60] vtk libray errors
!-------------------------------------------------------------
module Globals

   use KindDeclaration

   implicit none

   !> File or directory does not exist
   integer, parameter :: wrn_IO      = 1
   !> Error in input/outour parameter
   integer, parameter :: wrn_val     = 3
   !> Error reading file
   integer, parameter :: wrn_read    = 11
   !> Error writing file
   integer, parameter :: wrn_write   = 21
   !> Error in input parameter
   integer, parameter :: wrn_inp     = 13
   !> Error in outour parameter
   integer, parameter :: wrn_out     = 23
   !> File or directory does not exist
   integer, parameter :: err_IO      = 101
   !> Error in input parameter
   integer, parameter :: err_inp     = 111
   !> Error in outour parameter
   integer, parameter :: err_out     = 121
   !> Error allocation failed
   integer, parameter :: err_alloc   = 131
   !> Error deallocation failed
   integer, parameter :: err_dealloc = 141
   !> Error VTK library
   integer, parameter :: err_vtk     = 151
   !> Error reading file
   integer, parameter :: err_read    = 161
   !> Error writing file
   integer, parameter :: err_write   = 171
   !> Error in parameter value
   integer, parameter :: err_val     = 201

   type, public :: file
      !> Logical flag to check if file exists
      logical :: exist = .false.
      !> I/O unit number
      integer :: lun
      !> File or directory name
      character (len=256) :: fn
      !> Local file or directory name
      character (len=256) :: fnloc
      !  integer(hid_t)  :: hdf5_id=0 !< ID numbe for corresponding dataset in h5df
   contains
      !> Static constructor for `globals::file`
      procedure, public, pass   :: init => fn_init
      !> Static destructor for `globals::file`
      procedure, public, pass   :: kill => fn_kill
      !> Info procedure for `globals::file`
      procedure, public, pass   :: info => fn_print
      !> Get directory name
      procedure, public, nopass :: get_dirname
   end type file

contains

   !>-------------------------------------------------------------
   !> @brief Handle and write alert I/O warnings and errors
   !> @details Error number convention:
   !> * 0 < errno <= 100 -> **warning**: execution continues
   !> * errno > 100 -> **severe**: execution terminates
   !>
   !> @param[in] lun: I/O unit number for output of error messages
   !> @param[in] errno: error number
   !> @param[in] call_proc: name of the procedure where the error occured
   !> @param[in] add_msg: additional message to be printed (optional)
   !> @param[in] add_int: additional integer to be printed as
   !>  string in the message (optional)
   !> @return (logical) `.true.` if execution continues
   !>  (obviously no return otherwise as execution stops)
   !<-------------------------------------------------------------
   function IOerr(lun, errno, call_proc, add_msg, add_int) result(rc)
      implicit none
      integer,          intent(in)           :: lun
      integer,          intent(in)           :: errno
      character(len=*), intent(in)           :: call_proc
      character(len=*), intent(in), optional :: add_msg
      integer,          intent(in), optional :: add_int
      logical  :: rc
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
      if (errno.gt.100) then
         write(lun,fmt=100) '  SEVERE ERROR:'
         write(lun,fmt=101) ' in procedure: ',trim(call_proc)
         write(lun,fmt=101) trim(errmsg(errno, trim(msg), int))
         stop 'EXECUTION TERMINATED IN PROC IOerr'
      else if (errno.gt.0) then
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
      !> @brief Return message depending on input number
      !>
      !> @param[in] errno: error number
      !> @param[in] addmsg: additional message to be printed
      !> @param[in] int: to be included in msg if nonzero
      !> @return msg: error message
      !<-------------------------------------------------------------
      function errmsg(errno, addmsg, addint) result(msg)
         implicit none
         integer,          intent(in) :: errno
         character(len=*), intent(in) :: addmsg
         integer,          intent(in) :: addint
         character(len=256) :: msg
         ! local vars
         character(len=5) :: rdwr

         if (addint.eq.0) then
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
   !> @brief Transform string to upper case
   !> @details Adapted from http://rosettacode.org/wiki/String_case#Fortran.
   !> Original author: Clive Page
   !>
   !> @param[in] strin: input string
   !> @return (char) output string
   !<-------------------------------------------------------------
   function to_upper(strIn) result(strOut)
      implicit none
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
   !> @brief Transform string to lowercase case
   !> @details Adapted from http://rosettacode.org/wiki/String_case#Fortran.
   !> Original author: Clive Page
   !>
   !> @param[in] strin: input string
   !> @return
   !> (char) output string
   !<-------------------------------------------------------------
   function to_lower(strIn) result(strOut)
      implicit none
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
   !> @brief Return string strin with no preceding and trailing spaces
   !>
   !> @param[in] strin: input string
   !> @return (char) output string
   !<-------------------------------------------------------------
   function etb(strIn) result(strOut)
      implicit none
      character(len=*), intent(in) :: strIn
      character(len=len_trim(adjustl(strIn))) :: strOut

      strOut=trim(adjustl(strIn))

   end function etb

   !>-------------------------------------------------------------
   !> @brief Erase comments from a string
   !> @details A comment is a trailing string beginning
   !>  with one of the following characters:
   !>  * ! F90-style
   !>  * `%` TeX-style
   !>  * `#` script-bash-style
   !>
   !> @param[in] strin: input string
   !> @return (char) output string
   !<-------------------------------------------------------------
   function erase_comment(strIn) result(strOut)
      implicit none
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
   !> @brief Function that calculates vectors averages
   !> @details Cases:
   !>  * **on edge**, between two vectors related to the nodes of an edge
   !>  * **on cell**, between three vectors related to the vertices of a triangle
   !>
   !> @param[in] avg_type: type of average. `0`: arithmetic average.
   !> @param[in] vec1: vector to be averaged
   !> @param[in] vec2: vector to be averaged
   !> @param[in] vec3: vector to be averaged (optional)
   !> @return real(double). dimension(3), averaged vector
   !<-------------------------------------------------------------
   function  avg_vec3(avg_type, vecA, vecB, vecC) result (res_avg)
      implicit none
      integer,           intent(in)           :: avg_type
      real(kind=double), intent(in)           :: vecA(3)
      real(kind=double), intent(in)           :: vecB(3)
      real(kind=double), intent(in), optional :: vecC(3)
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
   !> @brief Function that calculates scalar averages
   !> @details Cases:
   !>  * **on edge**, between two vectors related to the nodes of an edge
   !>  * **on cell**, between three vectors related to the vertices of a triangle
   !>
   !> @param[in] avg_type: type of average. `0`: arithmetic average.
   !> @param[in] A: scalar to be averaged
   !> @param[in] B: scalar to be averaged
   !> @param[in] C: scalar to be averaged (optional)
   !> @return real(double), average
   !<-------------------------------------------------------------
   function  avg_scal(avg_type, A, B, C) result (res_avg)
      implicit none
      integer,           intent(in)           :: avg_type
      real(kind=double), intent(in)           :: A
      real(kind=double), intent(in)           :: B
      real(kind=double), intent(in), optional :: C
      real(kind=double) :: res_avg
      ! local vars

      res_avg = zero
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
   !> @brief Function that calculates cross-products
   !>
   !> @param[in] vec1: vector
   !> @param[in] vec2: vector
   !> @return real(`::double`), dimension(3), vector
   !<-------------------------------------------------------------
   function  cross(vecA, vecB) result (res_cross)
      implicit none
      real(kind=double), intent(in) :: vecA(3)
      real(kind=double), intent(in) :: vecB(3)
      real(kind=double) :: res_cross(3)

      res_cross(1) = vecA(2)*vecB(3)-vecA(3)*vecB(2)
      res_cross(2) = vecA(3)*vecB(1)-vecA(1)*vecB(3)
      res_cross(3) = vecA(1)*vecB(2)-vecA(2)*vecB(1)

   end function cross

   !>--------------------------------------------------------------
   !> @brief Simple sort algorithm to sort in increasing order an
   !> integer array. To be used only for small array.
   !> Use `globals::global_heapsort` for big arrays.
   !>
   !> @param[in   ] narray: length array to be sorted
   !> @param[inout] array: array to be sorted
   !<-------------------------------------------------------------
   subroutine isort(narray, array)
      implicit none
      integer, intent(in   ) :: narray
      integer, intent(inout) :: array(narray)
      ! local vars
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
         if (indx.gt.0) then
            ! SWAP ELEMENT
            itemp    =  array(i)
            array(i) = array(j)
            array(j) = itemp
         else if (indx.lt.0) then
            ! COMPARE (array(i) and array(j) )
            isgn = 0
            if (array(i).lt.array(j)) isgn = -1
            if (array(i).gt.array(j)) isgn = +1
         else if (indx.eq.0) then
            exit
         end if
      end do

   end subroutine isort

   !>--------------------------------------------------------------
   !> @brief Given a sorted array, it is returned with the
   !> list of uniques elements in the first positions.
   !>
   !> @param[in   ] n_elements: length of the array `elements`
   !> @param[inout] elements: sorted array
   !> @param[inout] nunique: number of unique elements
   !<--------------------------------------------------------------
   subroutine unique_of_sorted(n_elements, elements, nunique)
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

   !>--------------------------------------------------------------
   !> @brief Check if the first array is in lexicographic order
   !> with respect to the second
   !>
   !> @param[in] n: dimension of the input arrays
   !> @param[in] a: first array of dimension `n`
   !> @param[in] b: second array of dimension `n`
   !> @result (logical) `.true.` if `a` is in lexicographic order
   !>  wrt `b`, `.false.` otherwise
   !<--------------------------------------------------------------
   function lexicographic_order(n, a, b) result(a_before_b)
      implicit none
      integer, intent(in) :: n
      integer, intent(in) :: a(n)
      integer, intent(in) :: b(n)
      logical :: a_before_b
      !local
      integer :: i

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
   !> @brief global_heapsort
   !<--------------------------------------------------------------
   subroutine global_heapsort(n, indx, i, j, isgn)
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

   !>---------------------------------------------------------------
   !> @brief ifind
   !<--------------------------------------------------------------
   function ifind(narray, array, tobefound) result (i)
      implicit none
      integer, intent(in) :: narray
      integer, intent(in) :: tobefound
      integer, intent(in) :: array(narray)
      integer :: i
      !local
      logical :: found

      i = 1
      found = (array(i).eq.tobefound)
      do while ((.not.found).and.(i.lt.narray))
         i=i+1
         found = (array(i).eq.tobefound)
      end do
      if (.not.found) i=0

   end function ifind

   !>---------------------------------------------------------------
   !> @brief double_col_permute
   !<--------------------------------------------------------------
   subroutine double_col_permute(m, n, p, a)
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

   !>---------------------------------------------------------------
   !> @brief integer_col_permute
   !<--------------------------------------------------------------
   subroutine integer_col_permute(m, n, p, a)

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
   !> @brief Evaluates the weighted p-norm
   !>
   !> @param[in] ndata: length of input array
   !> @param[in] power: power for p-norm (`0` for infinity norm)
   !> @param[in] data: input array of dimension `ndata`
   !> @param[in] weight: array of weights (optional)
   !> @result real(double), (weighted) p-norm of `data`
   !<------------------------------------------------------------------
   function p_norm(ndata, power, data, weight) result(total)
      implicit none
      integer,           intent(in)           :: ndata
      real(kind=double), intent(in)           :: power
      real(kind=double), intent(in)           :: data(ndata)
      real(kind=double), intent(in), optional :: weight(ndata)
      real(kind=double) :: total
      !local
      integer :: i
      real(kind=double) :: ddot

      if (abs(power) < small) then
         total = maxval(abs(data))
      else
         if (present(weight)) then
            if (abs(power-one) < small) then
               total = ddot(ndata, abs(data) ,1, weight,1)
            else
               total = zero
               do i = 1, ndata
                  total = total + abs(data(i)) ** power * weight(i)
               end do
               total = total ** (one/power)
            end if
         else
            if (abs(power-one) < small) then
               total = sum(abs(data))
            else
               total = zero
               do i = 1, ndata
                  total = total + abs(data(i)) ** power
               end do
               total = total ** (one/power)
            end if
         end if
      end if

   end function p_norm

   !>-----------------------------------------------------
   !> @brief Procedure orthogonalizing a vector w.r.t. a
   !>  a set of vectors
   !>
   !> @param[in   ] dim: vectors dimension
   !> @param[in   ] nvectors: number of vectors
   !> @param[in   ] vectors: set of vectors wrt `x` is orthogonalized
   !> @param[inout] x: vector to be orthogonalized
   !<-----------------------------------------------------s
   subroutine ortogonalize(dim, nvectors, vectors, x)
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
   !> @brief Procedure that computes the projection of a vector
   !> onto the plane orthogonal to another vector (`normal`)
   !>
   !> @param[in ] ndim: dimension of the vectors
   !> @param[in ] vector: vector to be projected (dimension `ndim`)
   !> @param[in ] normal: normal to the plane (dimension `ndim`)
   !> @param[out] result: projected vector (dimension `ndim`)
   !<-------------------------------------------------------------
   subroutine orthogonal_projection(ndim, vector, normal, res_proj)
      implicit none
      integer,           intent(in)  :: ndim
      real(kind=double), intent(in)  :: vector(ndim)
      real(kind=double), intent(in)  :: normal(ndim)
      real(kind=double), intent(out) :: res_proj(ndim)
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
   !> @brief Info procedure for `globals::file`
   !> @details Print the name of the file and the associated
   !>  unit number
   !>
   !> @param[in] lun: unit number for output message
   !<-------------------------------------------------------------
   subroutine fn_print(this, lun)
      implicit none
      class(file), intent(in) :: this
      integer,     intent(in) :: lun

      write(lun,'(a,a,a,i3)') ' filename ',etb(this%fn), &
         ' linked to lun ',this%lun

   end subroutine fn_print

   !>-------------------------------------------------------------
   !> Get directory name
   !>
   !> @param[in] lun: unit number for output message
   !> @return (char) directory name
   !<-------------------------------------------------------------
   function get_dirname(str_file) result(str_dir)
      implicit none
      character(len=*), intent(in) :: str_file
      character(len=len_trim(adjustl(str_file))) :: str_dir
      !local
      integer :: nstr, pbar

      str_dir(:) = ' '

      nstr = len(etb(str_file))
      pbar = scan(etb(str_file),"/", BACK= .true.)
      if (pbar.eq.0) then
         str_dir(1:1) = '.'
      else
         str_dir(1:pbar) = str_file(1:pbar)
      end if

   end function get_dirname

   !>-------------------------------------------------------------
   !> @brief Static constructor for `globals::file`
   !>
   !> @param[in] lun_err: unit number for error messages
   !> @param[in] fn: name of the file
   !> @param[in] lun: unit number to associate to the file
   !> @param[in] io_flag: `"in"` if filename is an input
   !>  file and its existence must be checked, `"out"` if
   !>  it is an output file
   !> @param[in] mandatory_input: (optional)
   !>  specify if input file is mandatory:
   !>  `.true.`: file is mandatory, stop if file does
   !>   not exists (default value)
   !>  `.false.`: file is optional, proceed if file does
   !>   not exists
   !> @param[in] folder: (optional) specify if input file is
   !>  a file or a folder:
   !>  * `.true.`: input path is folder (check existence and not open unit)
   !>  * `.false.`: input path is file (default value,
   !>    check existence and open if `mandatory_input = .true.`)
   !> @param[in] verbose: (optional) logical flag for additional messages
   !> @param[inout] info: (optional) flag for existence of file:
   !>  returns `-1` if file does not exist
   !<-------------------------------------------------------------
   subroutine fn_init(this, lun_err, fn, lun, io_flag,&
         mandatory_input, folder, verbose, info)
      implicit none
      class(file),      intent(inout)           :: this
      integer,          intent(in   )           :: lun_err
      character(len=*), intent(in   )           :: fn
      integer,          intent(in   )           :: lun
      character(len=*), intent(in   )           :: io_flag
      logical,          intent(in   ), optional :: mandatory_input
      logical,          intent(in   ), optional :: folder
      logical,          intent(in   ), optional :: verbose
      integer,          intent(inout), optional :: info
      ! local vars
      logical :: exist,file_exist,rc,mandatory,open_unit,printmsg
      integer :: res

      this%fn = etb(fn)
      this%lun = lun

      mandatory = .true.
      if (present(mandatory_input)) mandatory = mandatory_input

      printmsg = .true.
      if (present(verbose)) printmsg = verbose

      if (present(info)) info = 0

      ! handle folders
      open_unit = .true.
      if (present(folder)) open_unit = .not.folder

      ! for input files or folders check existence
      ! if marked as not mandatory print a warning message
      ! otherwise it stops
      if(io_flag.eq.'in') then
         inquire(file=this%fn,exist=exist)
         this%exist=exist
         if (.not.exist) then
            ! warning or stop
            if (mandatory) then
               file_exist = IOerr(lun_err, err_IO, 'file%init',&
                  ' mandatory named :'//etb(this%fn)//' not found',lun)
            else
               if (printmsg) &
                  file_exist = IOerr(lun_err, wrn_IO, 'file%init',&
                  ' optional named : '//etb(this%fn)//' not found',lun)
            end if
            if (present(info)) info = -1
            ! exit
            return
         end if
      end if

      ! if exist and it is not a folder open unit
      if (open_unit) then
         open(lun,file=this%fn,iostat=res)
         if(res .ne. 0) rc = IOerr(lun_err, err_IO,&
            'file%init', &
            'err opening file '//etb(this%fn),lun)
         this%exist = .true.
      end if

   end subroutine fn_init

   !>-------------------------------------------------------------
   !> @brief Static destructor for `globals::file`
   !>
   !> @param[in] lun_err: unit number for output error message
   !<-------------------------------------------------------------
   subroutine fn_kill(this, lun_err)
      implicit none
      class(file), intent(inout) :: this
      integer,     intent(in)    :: lun_err
      ! local vars
      logical :: rc,is_open
      integer :: res

      inquire (this%lun,opened=is_open)
      if (is_open) then
         close(this%lun,iostat=res)
         if(res.ne.0) rc = IOerr(lun_err, err_IO,&
            'file%kill', &
            'err closing file '//etb(this%fn),res)
         this%exist = .false.
      end if
      this%fn=' '
      this%lun=-1

   end subroutine fn_kill

end module Globals
