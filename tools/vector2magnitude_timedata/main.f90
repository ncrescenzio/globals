!>---------------------------------------------------------------------
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> Program to compute the point-wise magnitude of 
!> vectorial timedata and write it into scalar timedata.
!>
!> USAGE:
!>
!> ./vector2magnitude_timedata.out data_vector data_magnitude 
!>
!> - data_vector    : data with dimdata >1 
!> - data_magnitude : data with pointwise Euclidian norm
!> 
!>
!> REVISION HISTORY:
!> 22 Lug 2016 - Initial Version
!>
!> TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!<---------------------------------------------------------------------

PROGRAM interpolate_timedata
  use Globals
  use TimeInputs
  use TimeOutputs
  
  implicit none
  

  integer  :: dim,ndata,ndataref
  logical  :: rc
  integer  :: res
  character(len=256) :: inputs
  character(len=256) :: help_passed
  character(len=256) :: data_fname
  character(len=256) :: parent_fname
  character(len=256) :: dataref_fname
  
  type(file) :: fdata
  type(file) :: fsubgrid
  type(file) :: fparent
  type(file) :: fdataref

  type(TimeData) :: tdata



  integer :: stderr,stdout,debug,count !,res
  integer :: i, j,icell
  real(kind=double) :: time,imbalance,tzero,dnrm2

  type(TDout) :: dataref

  logical :: steady,endfile
  logical :: end_reached
  logical :: end_files
  logical :: end_reached_source
  logical :: end_reached_sink  
  logical :: end_reached_dirac_source 
  logical :: end_reached_dirac_sink   


  stderr=0
  stdout=6


  !>----------------------------------------------------------------------------
  count = command_argument_count()
  if ( count .eq. 1) then
     call get_command_argument(1,inputs, status=res)
     help_passed=etb(inputs)
     if ( help_passed .eq. '--help') then
        call print_help_message(stdout)
        write(stdout,*) 'DESCRIPTION:' 
        write(stdout,*) ' Program to compute the point-wise magnitude of  '
        write(stdout,*) ' vectorial timedata and write it into scalar timedata'
        stop
     else
        call print_help_message(stderr)
     end if
  end if

           
  call get_command_argument(1,data_fname, status=res)
  if ( res.ne.0) then
     call print_help_message(stderr)
     rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
          '  argument data wrong res= ',res)
  end if
  
  call get_command_argument(2,dataref_fname, status=res)
  if ( res.ne.0) then
     call print_help_message(stderr)
     rc = IOerr(stderr, err_inp , 'interpolate_timedata', &
          '  argument data interpolated wrong res= ',res)
  end if
  
  call fdata%init(stderr,etb(data_fname),10,'in')
  call fdataref%init(stderr,etb(dataref_fname),13,'out')
  
  call tdata%init(stderr, fdata)



  call dataref%init(stderr, 1,tdata%ndata)
  write(fdataref%lun,*)  1,  dataref%ndata



  ! build
  endfile=.false.
  steady=.false.

  
  time = tdata%TDtime(1)
  do while ( (.not. steady ) .and. ( .not. endfile  ) )
     call tdata%set(stderr,fdata,time,endfile)
     dataref%time = time
     
     do i=1,tdata%ndata
        dataref%TDactual(1,i) = dnrm2(tdata%dimdata,tdata%TDactual(:,i),1)
     end do
     call dataref%write2dat(fdataref%lun)
     time =  tdata%TDtime(2)
     if ( time .eq. huge) then
        steady = .True.
        write(fdataref%lun,*) 'time ', huge
     end if
  end do

  call fdata%kill(stderr)
  call fdataref%kill(stderr)

  
  contains
    subroutine print_help_message(lun)
      implicit none
      integer, intent(in) :: lun
      !local
      character(len=256) :: msg
      write(lun,*) 'USAGE: " ./vector2magnitude_timedata.out data_vector data_magnitude " '
      write(lun,*) ' - data_vector    (in)  : vector timedata'
      write(lun,*) ' - data_mangitude (out) : pointwise magnitude'
      write(lun,*) ' or    " ./vector2magnitude_timedata.out --help " for help and description '

    end subroutine print_help_message

  

end PROGRAM interpolate_timedata
