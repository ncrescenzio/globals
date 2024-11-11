!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> This program compute the lp norm of timedata 
!> (repository globals) associated to grids
!> 
!> Module Geometry2d is used for all tipo of Triangularization 
!> 2d, 3d and surface. 
!>
!> REVISION HISTORY:
!> yyyymmdd - 
!> 20190413 - Creation of program
!<---------------------------------------------------------------------

PROGRAM axpy
  use Globals
  use TimeInputs
  use TimeOutputs

  implicit none
  character(len=256) :: arg,input
  character(len=1256) :: path_data1
  character(len=256) :: path_data2
  character(len=256) :: path_z_out
  character(len=256) :: option


  type(file)     :: file_data1, file_data2, file_z_out
  type(TimeData) :: data_input1, data_input2
  type(TDOut)  :: z_out

  integer :: stderr,stdout,res,res2,res3
  integer :: itime
  integer :: ndata,dim,narg,nargs
  real(kind=double) :: time,alpha,constant
  character(len=256) :: clean
  


  logical :: rc,endreading,data1isfile,print_msg
  logical endfile1,endfile2

  stderr=0
  stdout=6

  !
  ! get path
  !
  print_msg=.False.
  nargs = command_argument_count()
  if ( nargs .eq. 5) then
     call get_command_argument(5,option,status=res)
     if ( res .ne. 0) then
        call print_help_message(stderr,narg)
        rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
             '  argument rhs wrong res= ',res)
     else
        clean=etb(option)
        select case (clean)
        case ('-v')
           print_msg=.True.
        case default
           call print_help_message(stderr,narg)
        end select        
     end if
  end if


  narg=1
  call get_command_argument(1,input,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'axpy', &
          '  argument grid wrong res= ',res)
  else
     read(input,*,iostat=res2) alpha
     if ( res2 .ne. 0) then
        call print_help_message(stderr,narg)
        rc = IOerr(stderr, err_inp , 'axpy', &
             ' Error read alpha, res= ',res)
     end if
     if (print_msg) write(stdout,*) 'Using alpha = ',alpha
  end if

  narg=narg+1
  call get_command_argument(narg,arg,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
          '  argument cell_data wrong res= ',res)
  else
     if ( .not. ( (arg(1:1) .eq. '/') .or. (arg(:1) .eq. ',') ) ) then 
        read(arg,*,iostat=res2) constant
        if (res2.eq.0) then
           data1isfile=.False.
           if (print_msg)  write(stdout,*) 'Using x = constant = ',constant
        else
           read(arg,'(a)',iostat=res3) path_data1
           data1isfile=.True.         
           if (res3 .ne. 0) call print_help_message(stderr,narg)
           if (print_msg)  write(stdout,*) 'Using x from file  = ',etb(path_data1)
        end if
     else
        data1isfile = .True.
        read(arg,'(a)',iostat=res3) path_data1
        if (print_msg)  write(stdout,*) 'Using x from file  = ',etb(path_data1)
     end if
  end if

  narg=narg+1
  call get_command_argument(3,path_data2,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
          '  argument rhs wrong res= ',res)
  else
     if (print_msg) write(stdout,*) 'Using y from file  = ',etb(path_data2) 
  end if

  narg=narg+1
  call get_command_argument(4,path_z_out,status=res)
  if ( res .ne. 0) then
     call print_help_message(stderr,narg)
     rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
          '  argument rhs wrong res= ',res)
  else
     if (print_msg)  write(stdout,*) 'Using z from file  = ',etb(path_z_out) 
  end if

  

  

  if ( data1isfile) then
     !
     ! read data
     !
     call file_data1%init(stderr,etb(path_data1),11,'in')
     call file_data2%init(stderr,etb(path_data2),12,'in')

     call data_input1%init(stderr,file_data1)
     call data_input2%init(stderr,file_data2)

     !
     ! check consistency
     !
     if ( .not.( data_input1%dimdata .eq. data_input2%dimdata) ) then
        rc = IOerr(stderr, err_inp , 'main', &
             'files '//etb(file_data1%fn)//' and  '//&
             etb(file_data2%fn)//' have different dimensions')
     end if
     if ( .not.( data_input1%ndata .eq. data_input2%ndata) ) then
        rc = IOerr(stderr, err_inp , 'main', &
             'files '//etb(file_data1%fn)//' and  '//&
             etb(file_data2%fn)//' have different ndata')     
     end if
     dim=data_input1%dimdata
     ndata=data_input1%Ndata

     !
     ! init z_out and associated file
     !
     call file_z_out%init(stderr,etb(path_z_out),13,'out')
     call z_out%init(stderr,dim ,ndata)
     write(file_z_out%lun,*) dim,z_out%ndata,' ! dim data' 

     !
     ! read data and write z_out
     !
     endreading=.false.
     time  = max(data_input1%TDtime(1),data_input2%TDtime(1))
     itime = 0
     do while ( .not. endreading )
        !
        ! read data
        !
        call data_input1%set(stderr, file_data1, time, endfile1)
        call data_input2%set(stderr, file_data2, time, endfile2)
        !
        ! set z_out
        !
        z_out%TDactual(:,:)=&
             alpha*data_input1%TDactual(:,:)+data_input2%TDactual(:,:)
        z_out%time=time

        !
        ! if both file are in steady state 
        !
        if ( data_input1%steadyTD .and. data_input2%steadyTD) then
           endreading=.True.
           z_out%steadyTD=.True.
        end if

        !
        ! write to file
        ! 
        call z_out%write2dat(file_z_out%lun)


        !
        ! exit if one file reached the there is no time exit
        !
        if ( endfile1 .or. endfile2) then
           exit
        end if

        
        !write(*,*) data_input1%TDtime(2),data_input2%TDtime(2)
        time = min(data_input1%TDtime(2),data_input2%TDtime(2))
     end do

     call data_input1%kill(stderr)
     call file_data1%kill(stderr)



     call data_input2%kill(stderr)
     call z_out%kill(stderr)



     call file_data2%kill(stderr)
     call file_z_out%kill(stderr)

  else
     !
     ! read data
     !
     call file_data2%init(stderr,etb(path_data2),12,'in')
     call data_input2%init(stderr,file_data2)

     dim=data_input2%dimdata
     ndata=data_input2%Ndata

     !
     ! init z_out and associated file
     !
     call file_z_out%init(stderr,etb(path_z_out),13,'out')
     call z_out%init(stderr,dim ,ndata)
     write(file_z_out%lun,*) dim,z_out%ndata,' ! dim data' 

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
        call data_input2%set(stderr, file_data2, time, endfile2)
        !
        ! set z_out
        !
        z_out%TDactual(:,:)=&
             alpha*constant+data_input2%TDactual(:,:)
        z_out%time=time
        z_out%steadyTD=data_input2%steadyTD

        !
        ! write to file
        ! 
        call z_out%write2dat(file_z_out%lun)


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
     call file_z_out%kill(stderr) 
  end if


     
end PROGRAM axpy
subroutine print_help_message(lun,narg)
  implicit none
  integer, intent(in) :: lun
  integer, intent(in) :: narg
  !local
  character(len=256) :: msg
  write(lun,*) ' Usage: ./axpy.out <alpha> <x_data> <ydata> <z_data> [<-option>]'
  write(lun,*) '  where: '
  write(lun,*) ' alpha  (in ) : real scalar '
  write(lun,*) ' x_data (in ) : timedata file path or real constant for x'
  write(lun,*) ' y_data (in ) : timedata file path for y'     
  write(lun,*) ' z_data (out) : timedata file path for z=ax+y '   
  write(lun,*) ' option (in, opt) : option for verbose visualization '   
end subroutine print_help_message
