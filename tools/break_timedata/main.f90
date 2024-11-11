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
  character(len=256) :: folderout
  character(len=256) :: data_type
  character(len=256) :: vtk_folder   
  character(len=256) :: fname
  character(len=256) :: name_data,nameout,basename,dataname,clean
  real(kind=double), allocatable :: datas(:,:)
  real(kind=double) :: time2read,time


  ! local vars
  integer :: u_number,narg
  integer :: res,ncoord,nnodeincell
  integer :: connection(5)
  logical :: rc,endfile
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
  
  !
  ! get data name
  !
  clean=etb(fnamein)
  ppos = scan(etb(clean),".", BACK= .true.)
  pbar = scan(etb(clean),"/", BACK= .true.)
  name_data=clean(pbar+1:ppos-1)
  basename=etb(name_data)


  narg=narg+1   
  call  get_command_argument(narg,folderout,status=res)
  if (res .ne. 0) call err_handle(stderr,narg,command_line)

  
  ! set files
  call filein%init(stderr,etb(fnamein),10,'in')
 
  
  ! build
  call datain%init(stderr,filein)
  end_files=.false.
  time=datain%TDtime(1)
  steady=.false.

  itime = 0
  do while ( &
       ((time .eq. datain%TDtime(1)) .or. &
       ( .not. datain%steadyTD) ))
     !
     ! read data at each time
     !
     call datain%set(stderr, filein, time, endfile)
     
     !
     !nameout will be in format "name0000*.vtk" only
     ! for a sequence of data, otherwise it will be
     ! "name.vtk"
     !
     if ( (itime.eq. 0) .and. ( datain%steadyTD .or. endfile) ) then
        nameout=etb(etb(folderout)//'/'//etb(basename)//'.dat')
        write(*,*) 'Steady-state data written in file= ', etb(nameout)
     else              
        write(label,'(i0.8)') itime
        nameout=etb(etb(folderout)//'/'//etb(basename)//label//'.dat')
     end if
     
     call fileout%init(stderr,etb(nameout),11,'out')
     call write2file(stderr, 'whole',&
          datain%dimdata,  datain%ndata,  datain%TDActual,time, fileout)
     call fileout%kill(stderr)


     if ( endfile) then
        exit
     end if      
     itime= itime + 1
     time = datain%TDtime(2)
   end do
  
  call datain%kill(6)
  call filein%kill(6)


  
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
  write(lun_err,*) '  scale_timedata.out <filein> <time> <fileout>'
  write(lun_err,*) ' where : '
  write(lun_err,*) ' filein  (in ) : timedata in ASCII'
  write(lun_err,*) ' factor  (in ) : time to be read'
  write(lun_err,*) ' fileout (out) : timedata in ASCII at given time '
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle
