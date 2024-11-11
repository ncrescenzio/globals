!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> This program takes a timedata and relation file (descibring the relation
!> beetween the original and the selected indexes, see example) and write 
!> a new timedata file with the selected values
!>
!> INPUT :
!> - filein   : timedata file to be selected
!> - relation : list of indeces to be selected
!> OUTPUT :
!> - fileout  : timedata file wiht selected value
!>
!> REVISION HISTORY:
!> dd mm  yyyy - 
!> 19 Aug 2019 - First version
!>
!> TODO_dd_mmm_yyyy - (TODO_name ) :TODO_describe_appropriate_changes 
!<---------------------------------------------------------------------

PROGRAM select_timedata
  use Globals
  use TimeInputs

  implicit none
  type(file)     :: filein
  type(file)     :: filerelation
  type(file)     :: fileout
  type(TimeData) :: datain

  integer :: stderr,stdout,debug, flag_reading
  integer :: i,j,k, itime
  integer :: ndata,dim,ninputs,nselected, noriginal

  character(len=256) :: arg

  character(len=256) :: fnamein
  character(len=256) :: path_relation
  character(len=256) :: fnameout
  character(len=256) :: data_type
  character(len=256) :: vtk_folder   
  character(len=256) :: fname
  character(len=256) :: name_data,vtk_name
  integer, allocatable :: selection(:)
  real(kind=double) :: time


  ! local vars
  integer :: u_number
  integer :: res,narg
  logical :: rc
  character(len=256) :: rdwr,str,first_line

  logical end_files, end_reached, steady
  integer ppos,pbar

  character(len=8) :: label

  stderr=0
  
  !
  ! read input
  !
  narg=1
  call  get_command_argument(narg, fnamein,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)
 
  narg=narg+1   
  call  get_command_argument(narg, path_relation,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)

  narg=narg+1   
  call  get_command_argument(narg, fnameout,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)


  !
  ! init input/ouptu files
  call filein%init(stderr,etb(fnamein),10,'in')
  call filerelation%init(stderr,etb(path_relation),11,'in')
  call fileout%init(stderr,etb(fnameout),12,'out')

  !
  ! init file
  !
  call datain%init(6,filein)

  !
  ! read relations
  !
  read(filerelation%lun,*,iostat=res) nselected, noriginal
  if (res .ne. 0) then
     rc = IOerr(stderr, err_read , 'select_timedata', &
       etb(filerelation%fn),res)
  end if
  if ( noriginal .ne. datain%ndata ) then
     rc = IOerr(stderr, err_inp , 'select_timedata', &
          ' norigional  in '//etb(filerelation%fn)//&
          ' do not match dimension of '//etb(filein%fn))
  end if
  allocate(selection(nselected),stat=res)
  if (res.ne.0) rc = IOerr(stderr, err_alloc , 'select_timedata', &
       ' work array selection ',res)
  do i=1,nselected
     read(filerelation%lun,*,iostat=res) selection(i)
     if (res.ne.0) then
        rc = IOerr(stderr, err_read , 'select_timedata', &
             etb(filerelation%fn)//' selection array ',res)
     end if
  end do
   
  end_files=.false.
  time=datain%TDtime(1)
  steady=.false.

  u_number    = fileout%lun
  fname       = fileout%fn
  write(u_number,*,iostat=res) datain%dimdata, nselected
  write(u_number,*,iostat=res) 'time', time

  do while ( (time .eq. datain%TDtime(1) ) .or. &
       ( (.not. steady ) .and. ( .not. end_files  )  ) ) 
     ! read inputs
     call datain%set(stderr, filein,time,end_reached)
     steady = datain%steadyTD
     write(u_number,*,iostat=res) nselected
     do j=1,nselected
        i=selection(j)
        write(u_number,*,iostat=res) j,(datain%TDactual(k,i),k=1,datain%dimdata)
        if(res .ne. 0) THEN
           write(rdwr,'(i5)') i
           str=etb(rdwr)//'/'
           rc = IOerr(6, err_out , 'data2timedata', &
                trim(etb(fname)) //&
                ' array datas  at line '//&
                trim(str),res)
        end if
     end do

     if (datain%steadyTD) write(u_number,*,iostat=res) 'time 1.0e30'

     ! next time
     time=datain%TDtime(2)
  end do

  !
  ! free memory
  !
  call datain%kill(6)

  call filein%kill(stderr)
  call filerelation%kill(stderr)
  call fileout%kill(stderr)
  
end PROGRAM select_timedata


subroutine err_handle(lun_err, narg)
  use Globals
  implicit none

  integer, intent(in) :: lun_err
  integer, intent(in) :: narg
  ! local
  logical :: rc
  write(lun_err,*) ' Error passing arguments, correct usage:'
  write(lun_err,*) ' ./select_timedata.out <filein> <selector> <file_selected>'
  write(lun_err,*) ' where '
  write(lun_err,*) ' filein        (in ) : original timedata in ASCII'
  write(lun_err,*) ' selector      (in ) : selector '
  write(lun_err,*) ' file_selected (out) : selected timedata in ASCII'
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle
