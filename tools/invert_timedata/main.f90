!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> This program converts to vtk datas in timedata format 
!> (repository globals) associated to grids
!> 
!> Moduel Geometry2d is used for all tipo of Triangularization 
!> 2d, 3d and surface. 
!>
!> REVISION HISTORY:
!> dd mm  yyyy - 
!> 14 Gen 2019 - Correction to base program on repository structures 
!>
!> TODO_dd_mmm_yyyy - (TODO_name ) :TODO_describe_appropriate_changes 
!> 14 Gen 2019 - (Enrico Facca ) :  Program should use a general 
!>               module Geometry. It should be exteded or rewritten 
!>               to HDF5 file-format 
!<---------------------------------------------------------------------

PROGRAM data2timedata
  use Globals
  use TimeInputs

  implicit none
  type(file)     :: filein, fileout
  type(TimeData) :: datain

  integer :: stderr,stdout,debug, flag_reading
  integer :: i,j,k, itria,itime
  integer :: ndata,dim,ninputs

  character(len=256) :: arg

  character(len=256) :: fnamein
  character(len=256) :: fnameout
  character(len=256) :: data_type
  character(len=256) :: vtk_folder   
  character(len=256) :: fname
  character(len=256) :: name_data,vtk_name
  real(kind=double), allocatable :: datas(:,:)
  real(kind=double) :: factor,time


  ! local vars
  integer :: u_number,narg
  integer :: res,ncoord,nnodeincell
  integer :: connection(5)
  logical :: rc
  character(len=256) :: rdwr,str,first_line
  character(len=1000) :: command_line,input

  logical end_files, end_reached, steady
  integer ppos,pbar,len

  character(len=8) :: label

  stderr=0
  
  !
  ! read input
  !
  narg=0
  call get_command (input, len, status=res)
  command_line=input(1:len)
  if (res .ne. 0) call err_handle(stderr,narg,command_line)
    

  narg=1
  call  get_command_argument(narg, fnamein,status=res)
  if (res .ne. 0) call err_handle(stderr,narg,command_line)
 

  narg=narg+1   
  call  get_command_argument(narg,fnameout,status=res)
  if (res .ne. 0) call err_handle(stderr,narg,command_line)

  
  ! set files
  call filein%init(stderr,etb(fnamein),10,'in')
  call fileout%init(stderr,etb(fnameout),11,'out')
  

  
  
  ! build
  call datain%init(stderr,filein)
  end_files=.false.
  time=datain%TDtime(1)
  steady=.false.

  u_number    = fileout%lun
  fname       = fileout%fn
  write(u_number,*,iostat=res) datain%dimdata, datain%ndata, ' ! dimdata ndata ' 
 
 do while ( (time .eq. datain%TDtime(1) ) .or. &
       ( (.not. steady ) .and. ( .not. end_files  )  ) ) 
    ! read inputs
    call datain%set(stderr, filein,time,end_reached)
    steady = datain%steadyTD
    
    write(u_number,*,iostat=res) 'time', time
    write(u_number,*,iostat=res) datain%ndata, '  ! nnz'

     do j=1,datain%ndata
        write(u_number,*,iostat=res) j,(one/datain%TDactual(k,j),k=1,datain%dimdata)
        if(res .ne. 0) THEN
           write(rdwr,'(i5)') j
           str=etb(rdwr)//'/'
           rc = IOerr(6, err_out , 'data2timedata', &
                trim(etb(fname)) //&
                ' array datas  at line '//&
                trim(str),res)
        end if
     end do

     if (datain%steadyTD) write(u_number,*,iostat=res) 'time 1.0e30'
     if (end_reached) exit

     ! next time
     time=datain%TDtime(2)
  end do
  close(u_number)
  
  call datain%kill(6)



  
end PROGRAM data2timedata


subroutine err_handle(lun_err, narg,command_line)
  use Globals
  implicit none

  integer, intent(in) :: lun_err
  integer, intent(in) :: narg
  character(len=*), intent(in) :: command_line
  ! local
  logical :: rc
  write(lun_err,*) ' Error passing arguments'
  write(lun_err,*) ' Command passed: (',etb(command_line), ' )'
  write(lun_err,*) ' Correct usage:'
  write(lun_err,*) '  scale_timedata.out <filein> <inverse>'
  write(lun_err,*) ' where : '
  write(lun_err,*) ' filein  (in ) : timedata in ASCII'
  write(lun_err,*) ' product (out) : timedata in ASCII'
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle
