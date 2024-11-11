!>---------------------------------------------------------------------
!>
!> \author{Enrico Facca and Mario Putti}
!>
!> DESCRIPTION: 
!> This program takes a timedata and relation file (descibring the relation
!> beetween the original and the selected indexes, see example) and write 
!> a new timedata file with the extended values
!>
!> INPUT :
!> - filein   : timedata file to be extended
!> - relation : list of indeces in the extended file
!> - extensor : cnstant or timedata file to be use for extention 
!> OUTPUT :
!> - fileout  : timedata file with extended value
!>
!> REVISION HISTORY:
!> dd mm  yyyy - 
!> 17 Sep 2019 - First version
!>
!> TODO_dd_mmm_yyyy - (TODO_name ) :TODO_describe_appropriate_changes 
!<---------------------------------------------------------------------

PROGRAM extend_timedata
  use Globals
  use TimeInputs

  implicit none
  type(file)     :: filein
  type(file)     :: filerelation
  type(file)     :: fileextensor
  type(file)     :: fileout
  type(TimeData) :: datain
  type(TimeData) :: dataextensor

  integer :: stderr,stdout,debug, flag_reading
  integer :: i,j,k, itime
  integer :: ndata,dim,ninputs,nselected, noriginal

  character(len=256) :: arg

  character(len=256) :: fnamein
  character(len=256) :: path_relation
  character(len=256) :: path_extensor
  character(len=256) :: fnameout
  character(len=256) :: fname
  character(len=256) :: input
  integer, allocatable :: selection(:)
  real(kind=double) :: time,constant
  real(kind=double), allocatable :: extended_data(:,:) 


  ! local vars
  integer :: u_number
  integer :: res,res2,res3,narg
  logical :: rc
  character(len=256) :: rdwr,str,first_line

  logical end_datain, end_extensor, endfile,steady,extensor_is_file
  integer ppos,pbar

  character(len=8) :: label

  stderr=0
  
  !
  ! read input file path
  !
  narg=1
  call  get_command_argument(narg, fnamein,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)

  !
  ! read relation file path
  !
  narg=narg+1   
  call  get_command_argument(narg, path_relation,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)

  !
  ! read string, try to read a constant
  ! if it fails, try to read from file
  !
  narg=narg+1 
  call  get_command_argument(narg, input,status=res)
  if (res .eq. 0) then
     read(input,*,iostat=res2) constant
     if ( res2 .eq. 0) then
        extensor_is_file = .False.
        write(*,*) 'Extensor by constant: ', constant
     else
        read(input,'(a)',iostat=res3) path_extensor
        if ( res3 .ne.0 )  call err_handle(stderr,narg)
        extensor_is_file = .True.
        write(*,*) 'Extensor taken form file: ',etb(path_extensor)
     end if
  else
     call err_handle(stderr,narg)
  end  if
  
     
  !
  ! read output file path
  !
  narg=narg+1   
  call  get_command_argument(narg, fnameout,status=res)
  if (res .ne. 0) call err_handle(stderr,narg)


  !
  ! init input/ouptut files
  !  
  call filein%init(stderr,etb(fnamein),10,'in')
  call filerelation%init(stderr,etb(path_relation),11,'in')
  if ( extensor_is_file) call fileextensor%init(stderr,etb(path_extensor),12,'in')
  call fileout%init(stderr,etb(fnameout),13,'out')

  !
  ! init timedata 
  !
  call datain%init(stderr,filein)
  if ( extensor_is_file) call dataextensor%init(stderr,fileextensor)
  

  !
  ! read relations
  !
  read(filerelation%lun,*,iostat=res) nselected, noriginal
  if (res .ne. 0) then
     rc = IOerr(stderr, err_read , 'select_timedata', &
       etb(filerelation%fn),res)
  end if
  !
  ! check cosnistency
  !
  if ( nselected .ne. datain%ndata ) then
     rc = IOerr(stderr, err_inp , 'extend_timedata', &
          ' nselected  in '//etb(filerelation%fn)//&
          ' do not match dimension of '//etb(filein%fn))
  end if
  
  if ( extensor_is_file .and. ( noriginal .ne. dataextensor%ndata)  ) then
     rc = IOerr(stderr, err_inp , 'extend_timedata', &
          ' norigional  in '//etb(filerelation%fn)//&
          ' do not match dimension of '//etb(fileextensor%fn))
  end if
  !
  allocate(selection(nselected),stat=res)
  if (res.ne.0) rc = IOerr(stderr, err_alloc , 'extend_timedata', &
       ' work array selection ',res)
  do i=1,nselected
     read(filerelation%lun,*,iostat=res) selection(i)
     if (res.ne.0) then
        rc = IOerr(stderr, err_read , 'extend_timedata', &
             etb(filerelation%fn)//' selection array ',res)
     end if
  end do

  allocate(extended_data(datain%dimdata,noriginal),stat=res)
  if (res.ne.0) rc = IOerr(stderr, err_alloc , 'extend_timedata', &
       ' work array extended_data',res)


  !
  ! write to file extensor
  !
  endfile=.false.
  time=datain%TDtime(1)
  steady=.false.

  u_number    = fileout%lun
  fname       = fileout%fn
  write(u_number,*,iostat=res) datain%dimdata, noriginal

  if ( extensor_is_file ) then
     do while ( (time .eq. datain%TDtime(1) ) .or. &
          ( (.not. steady ) .and. ( .not. endfile  )  ) ) 
        ! read input
        call datain%set(stderr, filein,time,end_datain)        
        call dataextensor%set(stderr, fileextensor,time,end_extensor)  
        steady  = datain%steadyTD .and. dataextensor%steadyTD
        endfile = end_datain .or. end_extensor

        !
        ! copy data from extensor and overwrite other data
        !
        extended_data = dataextensor%TDactual  
        do j=1,nselected
           i=selection(j)
           extended_data(:,i) = datain%TDactual(:,i)
        end do

        !
        ! write to file
        !
        write(u_number,*,iostat=res) 'time', time
        write(u_number,*,iostat=res) noriginal
        do j=1,noriginal
           write(u_number,*,iostat=res) j,(extended_data(k,j),k=1,datain%dimdata)
           if(res .ne. 0) THEN
              write(rdwr,'(i5)') i
              str=etb(rdwr)//'/'
              rc = IOerr(stderr, err_out , 'extend_timedata', &
                   trim(etb(fname)) //&
                   ' array datas  at line '//&
                   trim(str),res)
           end if
        end do

        if (steady) write(u_number,*,iostat=res) 'time 1.0e30'
        if (endfile) exit
        ! next time
        time=min(datain%TDtime(2),dataextensor%TDtime(2))
     end do
  else
     do while ( (time .eq. datain%TDtime(1) ) .or. &
          ( (.not. steady ) .and. ( .not. end_datain  )  ) ) 
        ! read inputs
        call datain%set(stderr, filein,time,endfile)        
        steady = datain%steadyTD 
        
        !
        ! copy data from extensor and overwrite other data
        !
         do j=1,noriginal
           do k=1,datain%dimdata
              extended_data(k,i) = constant
           end do
        end do
        extended_data(:,:) = constant
        do j=1,nselected
           i=selection(j)
           do k=1,datain%dimdata
              extended_data(k,i) = datain%TDactual(k,j)
           end do
        end do

        !
        ! write to file
        !
        write(u_number,*,iostat=res) 'time', time
        write(u_number,*,iostat=res) noriginal
        do j=1,noriginal
           write(u_number,*,iostat=res) j,(extended_data(k,j),k=1,datain%dimdata)
           if(res .ne. 0) THEN
              write(rdwr,'(i5)') i
              str=etb(rdwr)//'/'
              rc = IOerr(stderr, err_out , 'extend_timedata', &
                   trim(etb(fname)) //&
                   ' array datas  at line '//&
                   trim(str),res)
           end if
        end do

        if (steady) write(u_number,*,iostat=res) 'time 1.0e30'
        if (endfile) exit

        ! next time
        time=datain%TDtime(2)
     end do
  end if
        
  

  !
  ! free memory
  !
  deallocate(extended_data,stat=res)
  if (res.ne.0) rc = IOerr(stderr, err_dealloc , 'extend_timedata', &
       ' work array extended_data',res)

  call datain%kill(6)
  call filein%kill(stderr)
  if (extensor_is_file) then 
     call dataextensor%kill(6)  
     call fileextensor%kill(6)
  end if
  call filerelation%kill(stderr)
  call fileout%kill(stderr)
  
end PROGRAM extend_timedata


subroutine err_handle(lun_err, narg)
  use Globals
  implicit none

  integer, intent(in) :: lun_err
  integer, intent(in) :: narg
  ! local
  logical :: rc
  write(lun_err,*) ' Error passing arguments, correct usage:'
  write(lun_err,*) ' extend_timedata.out <filein> <relation> <extensor> <file_extended>'
  write(lun_err,*) ' where '
  write(lun_err,*) ' filein        (in ) : selected timedata in ASCII'
  write(lun_err,*) ' relation      (in ) : original-selected relation  '
  write(lun_err,*) ' extendor      (in ) : cosntant or timedata in ASCII'
  write(lun_err,*) ' file_extended (out) : extended timedata in ASCII'
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle
