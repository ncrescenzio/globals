!>---------------------------------------------------------------------
!> \author{Enrico Facca}
!>
!> DESCRIPTION: 
!> This program compute the lp norm of timedata 
!> (repository globals) associated to grids
!> 
!> usage ./normp.out "path_timedata_file" [power] [weight] 
!>
!> path_timedata [in     ] :: path of timedata of [1,:]
!> p_exponent    [in, opt] :: $p_exponent \eq  0$ =>$L^{\inty}$-norm=maxval
!>                            $p_exponent \geq 1$ =>$L^{p}$-norm 
!>                            Default = 2.0
!> weight        [in,opt ] :: timedata file weight for norm (i.e., cell areas)
!>                            Default = 2.0
!>
!> REVISION HISTORY:
!> yyyymmdd - 
!> 20190413 - Creation of program
!<---------------------------------------------------------------------

PROGRAM lpnorm
  use Globals
  use TimeInputs
  implicit none
  type(file)     :: file_data,file_weight
  type(TimeData) :: data_input,data_weight

  integer :: stderr=0,stdout=6
  integer :: i,j,k, itria,itime
  integer :: ndata,dim
  real(kind=double) :: time

  character(len=256) :: arg
  character(len=256) :: path_grid
  character(len=256) :: path_data
  character(len=256) :: path_weight
  character(len=256) :: clean
  character(len=256) :: second

  ! local vars
  integer :: res,res2,ncoord,nnodeincell,nnode,ncell,icell,inode
  integer :: connection(5)
  logical :: rc
  character(len=256) :: rdwr,str,first_line,fname
  integer :: u_number


  logical endfile,endreading,endfile_weight,Euclidean
  integer info,narg,nargs

  real(kind=double) :: vec12(3),vec13(3),vec14(3),cross_prod(3)
  real(kind=double), allocatable :: weight(:)
  real(kind=double) :: p_exponent, norm_out,dnrm2,ddot
  
  info=0
  stderr=0
  
  nargs = command_argument_count()
  
 
  select case (nargs) 
  case (0)
     call err_handle(stderr,0)
  case (1)
     ! handle first argument
     call  get_command_argument(1, path_data,status=res)
     if ( res .ne. 0 )  call err_handle(stderr,1)
     clean = etb(path_data)
     if ( clean == '--help') then
        call err_handle(stderr,narg)
        return
     end if
     ! set other as default
     p_exponent= 2.0
     Euclidean = .True.
  case (2)
     call  get_command_argument(1, path_data,status=res)
     if ( res .ne. 0 )  call err_handle(stderr,1)
     clean = etb(path_data)
     if ( clean == '--help') then
        call err_handle(stderr,narg)
        return
     end if

     call  get_command_argument(2, second,status=res)
     clean = etb(second)
     read( clean, *,iostat=res) p_exponent
     if ( res .eq. 0 ) Euclidean = .True.
     
        
  case (3) 
     call  get_command_argument(1, path_data,status=res)
     if ( res .ne. 0 )  call err_handle(stderr,1)
     clean = etb(path_data)
     if ( clean == '--help') then
        call err_handle(stderr,narg)
        return
     end if
     
     call  get_command_argument(2, second,status=res)
     clean = etb(second)
     read( clean, *,iostat=res) p_exponent
     if ( res .ne. 0 ) call err_handle(stderr,2)
     
     Euclidean = .False.
     call  get_command_argument(3, second,status=res)
     clean = etb(second)
     read( clean, *,iostat=res) path_weight
     if ( res .ne. 0 ) call err_handle(stderr,2)
        
     
  end select
  
  !
  ! init data
  !
  call file_data%init(stderr,etb(path_data),11,'in')
  call data_input%init(stderr,file_data)
  !
  ! check consistency
  !
  if ( data_input%dimdata .ne. 1) then    
     rc = IOerr(stderr, err_inp , 'main', &
          ' files '//etb(file_data%fn) //'has not dimension 1')
  end if

  
  


  if ( Euclidean ) then     
     dim=data_input%dimdata
     ndata=data_input%Ndata
     endfile=.false.
     endreading=.false.
     time  = data_input%TDtime(1)
     itime = 0
     do while ( .not. endreading )
        !
        ! get data
        !
        call data_input%set(stderr, file_data, time, endfile)
        
        !
        ! compute and print norm
        !
        norm_out=p_norm(ndata, p_exponent, data_input%TDActual(1,:))
        write(stdout,'(2(1pe16.8))') time, norm_out

        !
        ! if both file are in steady state 
        !
        if ( data_input%steadyTD ) then
           endreading=.True.
        end if

        !
        ! if one file reached the there is no time exit
        !
        if ( endfile ) then
           endreading=.True.
        end if


        !
        ! next time
        ! 
        time = data_input%TDtime(2)
     end do


     

  else
     call file_weight%init(stderr,etb(path_weight),12,'in')
     call data_weight%init(stderr,file_weight)
     allocate(weight(data_weight%ndata),stat=res)
     if (res .ne. 0) rc = IOerr(stderr, err_alloc , 'main', &
             ' work array weight')

     !
     ! check consistency
     !
     if ( data_input%ndata .ne. data_weight%ndata ) then    
        rc = IOerr(stderr, err_inp , 'main', &
             ' Dimension mistmatch between'//&
             ' files '//etb(file_data%fn)//&
             ' files '//etb(file_weight%fn))
     end if
     
     dim=data_input%dimdata
     ndata=data_input%Ndata
     endfile=.false.
     endreading=.false.
     time  = max(data_input%TDtime(1),data_weight%TDtime(1))
     itime = 0
     do while ( .not. endreading )
        !
        ! get data
        !
        call data_input%set(stderr, file_data, time, endfile)
        call data_weight%set(stderr, file_data, time, endfile_weight)
        

        
        !
        ! compute and print norm
        !
        do i=1,data_weight%ndata
           weight(i) = data_weight%TDActual(1,i)
        end do
        norm_out=p_norm(ndata, p_exponent, data_input%TDActual(1,:),data_weight%TDActual(1,:))
        write(stdout,'(2e16.8)') time, norm_out

        !
        ! if both file are in steady state 
        !
        if ( ( data_input%steadyTD ) .and. ( data_weight%steadyTD ) )then
           endreading=.True.
        end if

        !
        ! if one file reached the there is no time exit
        !
        if ( ( endfile ) .or. (endfile_weight) ) then
           endreading=.True.
        end if


        !
        ! next time
        ! 
        time = min(data_input%TDtime(2),data_weight%TDtime(2))
     end do

     close(file_weight%lun)
     deallocate(weight,stat=res)
     if (res .ne. 0) rc = IOerr(stderr, err_dealloc , 'main', &
             ' work array weight')
  end if

  close(file_data%lun)
  

end PROGRAM lpnorm

subroutine err_handle(lun_err, narg)
  use Globals
  implicit none

  integer, intent(in) :: lun_err
  integer, intent(in) :: narg
  ! local
  logical :: rc
  write(lun_err,*) ' Error passing arguments, correct usage:'
  write(lun_err,*) ' usage ./normp.out "path_timedata_file" [power] [weight] '
  write(lun_err,*) ' path_timedata [in     ] :: path of timedata of [1,:] '
  write(lun_err,*) ' p_exponent    [in, opt] :: $p_exponent \eq  0$ =>$L^{\inty}$-norm=maxval '
  write(lun_err,*) '                            $p_exponent \geq 1$ =>$L^{p}$-norm  '
  write(lun_err,*) '                            Default = 2.0  '
  write(lun_err,*) ' weight        [in,opt ] :: timedata file weight for norm (i.e., cell areas)'
  write(lun_err,*) '                            Default = 1.0 '
  rc = IOerr(lun_err, err_inp, 'main', &
       ' Errors read argument number =',narg)
end subroutine err_handle

