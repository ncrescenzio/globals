!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca}
!>
!> DESCRIPTION: 
!> This program to split timedata intcompute the lp norm of timedata 
!> (repository globals) associated to grids
!> 
!> Module Geometry2d is used for all tipo of Triangularization 
!> 2d, 3d and surface. 
!>
!> REVISION HISTORY:
!> yyyymmdd - 
!> 20190413 - Creation of program
!<---------------------------------------------------------------------

PROGRAM split_timeddata
  use Globals
  use TimeInputs
  use TimeOutputs

  implicit none
  character(len=256) :: arg,input
  character(len=256) :: path_data2
  character(len=256) :: folder
  character(len=256) :: option


  type(file)     :: file_data1, file_data2, file_z_out
  type(TimeData) :: data_input1, data_input2
  type(TDOut)  :: z_out

  integer :: stderr,stdout,res,res2,res3,pbar,ppos
  integer :: itime
  integer :: ndata,dim,narg,nargs
  real(kind=double) :: time,alpha,constant
  character(len=256) :: clean,basename,label,name_data,path_z_out
  


  logical :: rc,endreading,data1isfile,print_msg
  logical endfile1,endfile2

  stderr=0
  stdout=6

  !
  ! get path
  !
  print_msg=.False.
  nargs = command_argument_count()
  if ( nargs .ne. 2) then
     call print_help_message(stderr,narg)
  end if


  narg=1
  call get_command_argument(1,input,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'split', &
          '  argument grid wrong res= ',res)
  else
     read(input,'(a)',iostat=res2) path_data2
     if ( res2 .ne. 0) then
        call print_help_message(stderr,narg)
        rc = IOerr(stderr, err_inp , 'split', &
             ' Error read alpha, res= ',res)
     end if
  end if
    


  narg=narg+1
  call get_command_argument(narg,arg,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'split_timedata', &
          '  argument cell_data wrong res= ',res)
  else
     read(arg,'(a)',iostat=res3) folder
     if ( res3 .ne. 0) then
        call print_help_message(stderr,narg)
     end if
  end if

  clean=etb(path_data2)
  ppos = scan(etb(clean),".", BACK= .true.)
  pbar = scan(etb(clean),"/", BACK= .true.)
  name_data=clean(pbar+1:ppos-1)


  !
  ! read data
  !
  call file_data2%init(stderr,etb(path_data2),12,'in')
  call data_input2%init(stderr,file_data2)

  dim=data_input2%dimdata
  ndata=data_input2%Ndata

 
  !
  ! read data and write z_out
  !
  endreading=.false.
  time  = data_input2%TDtime(1)
  itime = 0
  do while ( .not. endreading )
     !
     ! read data
     !
     itime=itime+1
     call data_input2%set(stderr, file_data2, time, endfile2)

     !
     ! init z_out and associated file
     !
     
     write(label,'(i0.8)') itime

     path_z_out=etb(etb(folder)//'/'//etb(name_data)//etb(label)//'.dat')
     write(*,*) etb(path_z_out)
     
     call file_z_out%init(stderr,etb(path_z_out),13,'out')
     call z_out%init(stderr,dim ,ndata)
     write(file_z_out%lun,*) dim,z_out%ndata,' ! dim data' 
     !
     ! set z_out
     !
     z_out%TDactual(:,:)=&
          data_input2%TDactual(:,:)
     z_out%time=time
     z_out%steadyTD=data_input2%steadyTD

     !
     ! write to file
     ! 
     call z_out%write2dat(file_z_out%lun)
     write(file_z_out%lun,*) 'time 1.0e30' 
     call file_z_out%kill(stderr)



     !
     ! exit if file reached the there is no time exit
     !
     if (  endfile2 .or. z_out%steadyTD) then
        endreading=.True.
        exit
     end if

     time = data_input2%TDtime(2)
  end do

  call file_data2%kill(stderr)


     
end PROGRAM split_timeddata
subroutine print_help_message(lun,narg)
  implicit none
  integer, intent(in) :: lun
  integer, intent(in) :: narg
  !local
  character(len=256) :: msg
  write(lun,*) ' Break one timedata file in several steady state timedata file'
  write(lun,*) ' Usage: ./split.out <data> <folder>'
  write(lun,*) '  where: '
  write(lun,*) ' data   (in ) : timedata file path '
  write(lun,*) ' folder (in ) : folder where strore data'     
end subroutine print_help_message
