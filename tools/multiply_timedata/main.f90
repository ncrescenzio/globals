!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> This program multiply two time data timedata into the product
!>
!> INPUT :
!> - factor1 : first  timedata 
!> - factor2 : second timedata
!> OUTPUT :
!> - product  : timedata with factor1*factor2
!>
!> REVISION HISTORY:
!> dd mm  yyyy - 
!> 20 Aug 2019 - First version
!<---------------------------------------------------------------------

PROGRAM multiply_timedata
  use Globals
  use TimeInputs
  use TimeOutputs

  implicit none
  character(len=256) :: arg,input
  character(len=1256):: path_data1
  character(len=256) :: path_data2
  character(len=256) :: path_z_out

  type(file)     :: file_data1, file_data2, file_z_out
  type(TimeData) :: data_input1, data_input2
  type(TDOut)  :: z_out

  integer :: stderr,res,narg
  integer :: itime,i,j
  integer :: ndata,dim
  real(kind=double) :: time,alpha,constant


  logical :: rc,endreading,data1isfile
  logical endfile1,endfile2
  logical :: option1, option2, option3
  
  stderr=0
  
  !
  ! read input
  !
  narg=1
  call  get_command_argument(narg, path_data1,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)
 
  narg=narg+1   
  call  get_command_argument(narg, path_data2,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)

  narg=narg+1   
  call  get_command_argument(narg, path_z_out,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)

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
  if ( .not.( data_input1%ndata .eq. data_input2%ndata) ) then
     rc = IOerr(stderr, err_inp , 'main', &
          'files '//etb(file_data1%fn)//' and  '//&
          etb(file_data2%fn)//' have different ndata')     
  end if
  ndata = data_input1%ndata
  

  option1 = .False. ! pointwise product
  option2 = .False. 
  option3 = .False.
  if ( ( data_input1%dimdata .eq. data_input2%dimdata) ) then
     dim = data_input1%dimdata
     option1 = .True.
  else  if ( data_input1%dimdata .eq. 1 ) then        
     dim = data_input2%dimdata
     option2 = .True.
  else if ( data_input2%dimdata .eq. 1 ) then        
     dim = data_input1%dimdata
     option3 = .True.
  else
      rc = IOerr(stderr, err_inp , 'main', &
          'files '//etb(file_data1%fn)//' and  '//&
          etb(file_data2%fn)//' are not compatible')
  end if
     
  
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
  time  = min(data_input1%TDtime(1),data_input2%TDtime(1))
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
     z_out%time = time
     if ( option1 ) then
        do i=1,ndata
           do j = 1,z_out%dimdata
              z_out%TDactual(j,i) = &
                   data_input1%TDactual(j,i) * &
                   data_input2%TDactual(j,i)
           end do
        end do
     end if

     if ( option2 ) then
        do i=1,ndata
           do j = 1,z_out%dimdata
              z_out%TDactual(j,i) = &
                   data_input1%TDactual(1,i) * &
                   data_input2%TDactual(j,i)
           end do
        end do
     end if

     if ( option3 ) then
        do i=1,ndata
           do j = 1,z_out%dimdata
              z_out%TDactual(j,i) = &
                   data_input1%TDactual(j,i) * &
                   data_input2%TDactual(1,i)
           end do
        end do
     end if

     
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
     time = min(data_input1%TDtime(2),data_input2%TDtime(2))
  end do
  
  call data_input1%kill(stderr)
  call file_data1%kill(stderr)
  
  
  
  call data_input2%kill(stderr)
  call file_data2%kill(stderr)
  

  call file_z_out%kill(stderr)
  call z_out%kill(stderr)
  
  
     
end PROGRAM multiply_timedata

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
  write(lun_err,*) ' factor1 (in ) : timedata in ASCII'
  write(lun_err,*) ' factor2 (in ) : timedata in ASCII'
  write(lun_err,*) ' product (out) : timedata in ASCII'
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle
