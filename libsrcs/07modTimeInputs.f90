module TimeInputs
  use Globals
  implicit none
  private
  public write_steady,read_steady,write2file,writearray2file
  !> structure variable containing vars for variable in time input data
  !> (e.g., forcing functions, exponents, k(x,t)...)
  !> up to no, only forcing function input
  type, public :: TimeData
     !> true/false flag if is initialized
     logical :: built
     !> Dimension of real array
     integer :: dimdata
     !> Dimension of real array
     integer :: ndata
     !> Dimension (dimdata,ndata,2)
     !> Input values
     real(kind=double), allocatable :: TDval(:,:,:)
     !> Dimension (2)
     !> Input values
     real(kind=double), allocatable :: TDtime(:)
     !> Number of current non-zeros
     integer :: nnonzeros=0
     !> Dimension (dimdata,Ndata)
     !> Actual time values     
     real(kind=double), allocatable :: TDactual(:,:)
     !> Dimension (dimdata)
     !> Scratch array for reading values     
     real(kind=double), allocatable :: val(:)
     !> Time of evaluated TDactual
     real(kind=double) :: time
     !> SteadyTD=.true.  : data do not change during time
     !> SteadyTD=.false. : data do    change during time
     logical :: steadyTD
     !> steadyTD_written=.true.  : closing time     written
     !> SteadyTD_written=.false. : closing time not written
     logical :: steadyTD_written=.false.
   contains
     !> static constructor
     !> (procedure public for type TimeData)
     procedure, public, pass :: init => init_TD
     !> static destructor
     !> (procedure public for type TimeData)
     procedure, public, pass :: kill => kill_TD
     !> Info procedure.
     !> (public procedure for type TimeData)
     procedure, public, pass :: info => info_TD
     !> Set (read if necessary) time data
     procedure, public, pass :: set => set_TD
     !> Set number of non zero element
     procedure, public, pass :: eval_ninput
     !> Set number of non zero element
     !procedure, public, nopass :: write2file
  end type TimeData

contains
  !>-------------------------------------------------------------
  !> Static constructor.
  !> (procedure public for type TimeData)
  !> Instantiate (allocate)
  !> and initilize (by also reading from input file)
  !> variable of type TimeData
  !>
  !> usage:
  !>     call 'var'%init( InputFdescr, Ndata)
  !>
  !> where:
  !> \param[in] InputFdescr -> type(file) I/O file information for
  !>                           input variable 
  !> \param[in] Ndata -> number of data to be read in
  !>
  !<-------------------------------------------------------------------
  subroutine init_TD(this, stderr, InputFdescr, dimdata, Ndata, default_data)
    use Globals
    implicit none
    !vars
    class(TimeData),   intent(out) :: this
    integer,           intent(in ) :: stderr
    type(file),        intent(in ) :: InputFdescr
    integer, optional, intent(in ) :: dimdata
    integer, optional, intent(in ) :: Ndata
    real(kind=double) , optional,  intent(in) :: default_data(:)
    ! local vars
    integer :: u_number
    integer :: NInput,ival
    integer :: res,i,j,k
    logical :: rc
    character(len=15) :: scratch
    character(len=256) ::  fname,msg

    this%built=.true.
    
    if (  InputFdescr%exist ) then 
       
       u_number = InputFdescr%lun
       fname    = InputFdescr%fn

       ! read file
       ! read dimdata, Ndata
       read(u_number,*,iostat=res) this%dimdata, this%Ndata
       if(res .ne. 0) &
            rc = IOerr(stderr, err_inp , 'read_TD', &
            trim(fname) // ' type TimeData member dimdata, Ndata ',res)

       ! check dimension
       if ( present (dimdata) ) then
          if ( dimdata .ne. this%dimdata) then
             rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(fname) // ' mismatch between read and given dimdata')
          end if
       end if

       if ( present (Ndata) ) then
          if ( Ndata .ne. this%Ndata) then
             rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(fname) // ' mismatch between read and given Ndata')
          end if
       end if

       ! allocate
       allocate(this%TDactual(this%dimdata,this%Ndata),stat=res)
       if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
            '  type TimeData member TDactual (array)',res)
       this%TDactual=zero

       allocate(this%TDval(this%dimdata,this%Ndata,2),stat=res)
       if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
            '  type TimeData member TDval (array)',res)
       this%TDval=zero

       allocate(this%TDtime(2),stat=res)
       if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
            '  type TimeData member TDtime (array)',res)
       this%TDtime=zero

       allocate(this%val(this%dimdata),stat=res)
       if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
            '  type TimeData member val (array)',res)

       this%TDtime=zero
       this%steadyTD=.false.


       ! read the first time for TD
       read(u_number,*,iostat=res) scratch, this%TDtime(1)
       if(res .ne. 0) &
            rc = IOerr(stderr, err_inp , 'read_TD', &
            trim(fname) // ' type TimeData member TDtime(1) ',res)

       ! read Ninputs
       read(u_number,*,iostat=res) NInput
       if(res .ne. 0) &
            rc = IOerr(stderr, err_inp , 'read_TD', &
            trim(fname) // ' type TimeData member NInput ',res)
       this%nnonzeros=NInput

       ! read Data
       do i = 1,NInput
          read(u_number,*,iostat=res) ival, (this%val(k), k=1,this%dimdata)
          if(res .ne. 0) then
             write(msg,*) i
             write(*,*) res
             rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(fname) // ' type TimeDara member ival '//etb(msg),res)
          end if
          j=ival
          this%TDval(:,ival,1)=this%val(:)
          !write(*,*) i, this%val
       end do

       ! read the second time for TD
       read(u_number,*,iostat=res) scratch, this%TDtime(2)
       if(res .ne. 0) then
          ! end file => file assume to be in stready state
          if ( res .eq. -1 )  then
             this%TDtime(2) = huge
             this%TDactual(:,:)=this%TDval(:,:,1)
             this%TDval(:,:,2)=this%TDval(:,:,1)
             this%steadyTD=.true.
          else
             rc = IOerr(stderr, err_inp , 'read_TD', &
                  etb(trim(InputFdescr%fn)//&
                  ' type TimeData member timein(2) '),res)
          end if
       end if

       
       if (this%TDtime(2).ge.huge)then
          ! if Time data is in steady state copy TD(:,:,1) into TD(:,:,2)

          this%TDactual(:,:)=this%TDval(:,:,1)
          this%TDval(:,:,2)=this%TDval(:,:,1)
          this%steadyTD=.true.
       else
          ! if not in stready state read TD(2,:,:)

          ! read Ninputs
          read(u_number,*,iostat=res) NInput
          if(res .ne. 0) &
               rc = IOerr(stderr, err_inp , 'read_TD', &
               trim(InputFdescr%fn) // ' type TimeData member NInput ',res)
          this%nnonzeros=NInput
          
          ! read Data
          do i = 1,NInput
             read(u_number,*,iostat=res) ival, (this%val(k), k=1,this%dimdata) 
             if(res .ne. 0) &
                  write(*,*) res
                  rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(InputFdescr%fn) // ' type TimeDara member ival,val ',res)
             this%TDval(:,ival,2)=this%val(:)
          end do
       end if
    else 
       if ( present(dimdata) .and. present(ndata) .and. present(default_data) ) then
          ! 
          ! if a defualt data data is passed time data is set in steady state 
          !
          !write(6,'(a,1pe10.2,a,1pe8.2)') 'Data default with min = ', minval(default_data), ' max= ', maxval(default_data)
          
          this%dimdata=dimdata
          this%Ndata=Ndata
          this%steadyTD=.True.

          ! allocate
          allocate(this%TDactual(this%dimdata,this%Ndata),stat=res)
          if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
               '  type TimeData member TDactual (array)',res)
          
          allocate(this%TDval(this%dimdata,this%Ndata,2),stat=res)
          if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
               '  type TimeData member TDval (array)',res)
          
          allocate(this%val(this%dimdata),stat=res)
          if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
               '  type TimeData member val (array)',res)

          !
          ! copy defualt data
          !
          k=0
          do j=1,ndata
             do i=1,dimdata
                k=k+1
                this%TDactual(i,j) = default_data(k)
                this%TDVal(i,j,1)  = default_data(k)
                this%TDVal(i,j,2)  = default_data(k)
             end do
          end do

          allocate(this%TDtime(2),stat=res)
          if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
               '  type TimeData member TDtime (array)',res)
          
          this%TDtime(1)=-huge
          this%TDtime(2)= huge
       else
          if(res .ne. 0) rc = IOerr(stderr, err_inp, 'read_TD', &
               '  file '//etb(InputFdescr%fn)//' does not esxits'//&
               ' and default not assigned ')
       end if
    end if
       

  end subroutine init_TD
  
  !>-------------------------------------------------------------
  !> Static destructor.
  !> (procedure public for type TimeData)
  !> deallocate all arrays for a var of type TimeData
  !>
  !> usage:
  !>     call 'var'%kill(lun)
  !>
  !> where:
  !> \param[in] lun -> integer. I/O unit for error message output.
  !<-----------------------------------------------------------
  subroutine kill_TD(this, lun)
    use Globals
    implicit none
    ! vars
    class(TimeData), intent(inout):: this
    integer, intent(in) :: lun
    ! local vars
    integer :: res
    logical :: rc
    
    this%built=.false.
    deallocate(this%TDval,this%TDtime,this%TDActual,this%val,stat=res)
    if (res.ne.0) rc=IOerr(lun, err_dealloc, 'kill_TD', &
         'dealloc fail for TimeData var TDvar,TDtime,TDactual, val')
  end subroutine kill_TD
  
  !>-------------------------------------------------------------
  !> Info procedure.
  !> (public procedure for type TimeData)
  !> Prints content of a variable of type TimeData.
  !> Report if the time-range covered [t1,t2], actual time
  !> If (nsample .gt. 0) it prints the first nsample non-zero terms
  !> of TDval(1,:.:), TDactual(:,:) and TDval(2,:.:)   
  !> 
  !> usage:
  !>     call 'var'%info(lun,nsample)
  !>
  !> where:
  !> \param[in] lun -> integer. I/O unit for error message output.
  !> \param[in] lun -> integer. Number of first no zero samples to be printed
  !<-------------------------------------------------------------
  subroutine info_TD(this, lun, nsample)
    use Globals
    implicit none
    ! vars
    class(TimeData), intent(in) :: this
    integer,         intent(in) :: lun
    integer,         intent(in) :: nsample

    ! local vars
    integer :: i,j,k
    real(kind=double) :: dnrm2
        
    write(lun,*) ' '
    write(lun,*) ' Info: TimeData structure definition:'
    
    write(lun,*) 'ndata', this%ndata
    if( this%steadyTD       ) write(lun,*) ' Steady state'
    write(lun,*) ' t1          = ', this%TDtime(1)
    write(lun,*) ' actual time = ', this%time
    write(lun,*) ' t2          = ', this%TDtime(2)
    if ( nsample .gt. 0 ) then
       write(lun,*) ' First Data'
       i=0
       j=0
       do while ( (i .lt. this%ndata) .and. (j .lt. nsample) )
          i=i+1
          if( dnrm2(this%dimdata,this%TDval(:,i,1),1) .ne.zero ) then
             j=j+1
             write(lun,'(5(i5,e11.3))') i,(this%TDval(k,i,1),k=1,this%dimdata)
          end if
       end do
       write(lun,*)
       write(lun,*) ' Actual Data'
       i=0
       j=0
       do while ( (i .lt. this%ndata) .and. (j .lt. nsample) )
          i=i+1
          if( dnrm2(this%dimdata,this%TDactual(:,i),1).ne.zero ) then
             j=j+1
             write(lun,'(5(i5,e11.3))') i,(this%TDactual(k,i),k=1,this%dimdata)
          end if
       end do
       write(lun,*)
       write(lun,*) ' Second time  Data'
       i=0
       j=0
       do while ( (i .lt. this%ndata) .and. (j .lt. nsample) )
          i=i+1
          if( dnrm2(this%dimdata,this%TDval(:,i,2),1) .ne.zero ) then
             j=j+1
             write(lun,'(5(i5,e11.3))') i,(this%TDval(k,i,2),k=1,this%dimdata)
          end if
       end do
    end if
  end subroutine info_TD

  !>-------------------------------------------------------------
  !> Set (read if necessary) Time dependent variables
  !> (public procedure for type TimeData) at given 
  !> time 
  !> 
  !> usage:
  !>     call 'var'%set(stderr, InputFdescr, time)
  !>
  !> where:
  !> \param[in] stderr      -> integer. I/O err msg unit. 
  !> \param[in] InputFdescr -> type(file) I/O file information for
  !>                           input variable 
  !> \param[in] time        -> real(double). Require time
  !<-------------------------------------------------------------
  subroutine set_TD(this, stderr, InputFdescr, time,endfile,info)
    use Globals
    implicit none
    ! vars
    class(TimeData),   intent(inout) :: this
    integer,           intent(in   ) :: stderr
    type(file),        intent(in   ) :: InputFdescr
    real(kind=double), intent(in   ) :: time
    logical,           intent(inout) :: endfile
    integer,  optional,intent(inout) :: info
    ! local vars
    integer :: res, u_number
    integer :: NInput
    integer :: i,k,ival
    real(kind=double) ::  TDt1,TDt2,tperc,next_time
    logical :: rc, read_next
    character(len=15)  :: scratch
    character(len=256) :: fname

    endfile=.false.

    this%time = time
    u_number = InputFdescr%lun
    fname    = InputFdescr%fn
    
    if (.not. this%steadyTD) then
       if ( time .lt. this%TDtime(1) ) then
          rc = IOerr(stderr, wrn_inp , 'set_TD', &
               ' time required is smaller TDtime(1)')
          if (present (info) ) info=1
          return
       end if
       if ( time .ge. this%TDtime(2) ) then
          read_next = .true.
          do while ( read_next )
             ! Read time2 
             read(u_number,*,iostat=res) scratch, next_time
             if(res .ne. 0) then
                if ( res .eq. -1 ) then
                   this%TDval(:,:,1)  = this%TDval(:,:,2)
                   this%TDactual(:,:) = this%TDval(:,:,2)
                   endfile=.true.
                   return
                else
                   rc = IOerr(stderr, err_inp , 'set_TD', &
                        trim(fname)&
                        //' type TimeData member TDtime(2)',res)
                end if
             else
                this%TDtime(1)=this%TDtime(2)
                this%TDtime(2)=next_time
             end if
             ! copy TD2 into TD1 before overwritten
             this%TDval(:,:,1)=this%TDval(:,:,2)

             ! If steady state (time2 .ge. huge) then frezee
             if ( this%TDtime(2) .ge. huge ) then
                this%steadyTD = .true.
                this%TDval(:,:,2)  = this%TDval(:,:,1)
                this%TDactual(:,:) = this%TDval(:,:,1)
                read_next = .false.
             else
                ! Otherwise read new data TD2                
                ! read ninputs
                read(u_number,*,iostat=res) NInput              
                if(res .ne. 0) then
                   if ( res .eq. -1 ) then
                      this%TDval(:,:,1)  = this%TDval(:,:,2)
                      this%TDactual(:,:) = this%TDval(:,:,2)
                      endfile=.true.
                      return
                   else
                      rc = IOerr(stderr, wrn_inp , 'set_TD', &
                           trim(fname)//' type TimeData member NInput ',res)
                      
                      endfile=.True.
                      return
                   end if
                end if
                this%nnonzeros=NInput
                
                ! read data
                this%TDval(:,:,2)=zero
                do i = 1,NInput
                   read(u_number,*,iostat=res) ival, (this%val(k),k=1,this%dimdata)
                   if(res .ne. 0) &
                        rc = IOerr(stderr, err_inp , 'set_TD', &
                        trim(fname)&
                        //' type TimeData aux varibles i,val',res) 
                   this%TDval(:,ival,2) = this%val(:)
                end do
                !> Test if continue reading file 
                read_next = ( time .ge. this%TDtime(2) ) 
             end if
          end do
       end if
       
       TDt1=this%TDtime(1)
       TDt2=this%TDtime(2)
       tperc=(time-TDt1)/(TDt2-TDt1)


       
       do i=1,this%ndata
          this%TDactual(:,i)=(one-tperc)*this%TDval(:,i,1)+&
               tperc*this%TDval(:,i,2)
       end do
    end if


  end subroutine set_TD

  function eval_ninput(this) result(ninput)
    use Globals
    implicit none
    class(TimeData), intent(in) :: this
    integer :: ninput
    !local
    integer :: i
    real(kind=double) :: dnrm2
    ninput= 0
    do i = 1, this%NData
       if ( dnrm2(this%dimdata,this%TDactual(:,i),1) > zero ) then 
          ninput=ninput+1
       end if
    end do
  end function eval_ninput

  !>-------------------------------------------------------------
  !> Write array into file in the form of steady state data
  !> (public procedure for type TimeData) at given 
  !> time 
  !> 
  !> usage:
  !>     call write_steady(lun_err, lun, ndata, data)
  !>
  !> where:
  !> \param[in] lun_err    -> integer. I/O err msg unit. 
  !> \param[in] lun        -> integer. I/O err msg unit. 
  !> \param[in] time       -> real(double). Require time
  !<-------------------------------------------------------------
  subroutine write_steady(stderr, lun, ndata, data,fname)
    use Globals
    implicit none
    ! vars
    integer,            intent(in   ) :: stderr
    integer,            intent(in   ) :: lun
    integer,            intent(in   ) :: ndata
    real(kind=double),  intent(in   ) :: data(ndata)
    character(len=*), optional,   intent(in   ) :: fname
    ! local vars
    integer :: res, u_number
    integer :: NInput
    integer :: i
    logical :: rc
    character(len=15)  :: scratch
    character(len=15)  :: filename


    if ( present(fname) ) then
       filename=etb(fname)
    else
       filename='File name not passed'
    end if
    write(lun,*,iostat=res) 1, ndata
    if(res .ne. 0) &
         rc = IOerr(stderr, err_out , 'write_steady', &
         etb(filename),res) 
    write(lun,*,iostat=res) 'time 1.0e-30'
    write(lun,*,iostat=res) ndata
    do i=1,ndata
       write(lun,*) i, data(i)
    end do
    write(lun,*,iostat=res) 'time 1.0e+30'
    

  end subroutine write_steady

  !>-------------------------------------------------------------
  !> Read array from file in the form of steady state data
  !> (public procedure for type TimeData) at given 
  !> time 
  !> 
  !> usage:
  !>     call read_steady(lun_err, ndata, data,file)
  !>
  !> where:
  !> \param[in] lun_err    -> integer. I/O err msg unit.  
  !> \param[in] ndata      -> integer. Data lenght
  !> \param[in] data       -> real(double). data to be read
  !> \param[in] file       -> type(file). Opened file
  !<-------------------------------------------------------------
  subroutine read_steady(stderr, ndata, data,open_file)
    use Globals
    implicit none
    ! vars
    integer,            intent(in   ) :: stderr
    integer,            intent(in   ) :: ndata
    real(kind=double),  intent(inout) :: data(ndata)
    type(file),         intent(in   ) :: open_file
    ! local vars
    integer :: res, u_number
    integer :: NInput
    integer :: i
    logical :: rc,end_of_file
    character(len=15)  :: scratch
    character(len=15)  :: filename
    type(TimeData):: tdata
    

    call tdata%init(stderr, open_file, 1, ndata)
    call tdata%set(stderr, open_file,&
         onehalf*(tdata%TDtime(1)+tdata%TDtime(2)),&
         end_of_file)
    data(:) = tdata%TDactual(1,:)
    call tdata%kill(stderr)

  end subroutine read_steady



  
  !>-------------------------------------------------------------
  !> Write array into file in the form of steady state data
  !> (public procedure for type TimeData) at given 
  !> time 
  !> 
  !> usage:
  !>     call write_steady(lun_err, lun, ndata, data)
  !>
  !> where:
  !> \param[in] lun_err              -> integer. I/O err msg unit. 
  !> \param[in] head_body_tail_whole -> character. What has to be written
  !>                                   head  : write dimensions
  !>                                   body  : write time and data
  !>                                   tail  : close time
  !>                                   whole : write head+body+tail
  !> \param[in] time                 -> real(double). Time of data
  !> \param[in] dimdata              -> integer. first dimension of data
  !> \param[in] ndata                -> integer. second dimension of data
  !> \param[in] data                 -> real. Actual (dim x ndata) data
  !> \param[in] file                 -> type(file). File where data where to write
  !<-------------------------------------------------------------
  subroutine write2file(lun_err, head_body_tail_whole,&
       dimdata, ndata, data,time, fileout)
    use Globals
    implicit none
    ! vars
    integer,            intent(in   ) :: lun_err
    character(len=*),   intent(in   ) :: head_body_tail_whole
    real(kind=double),  intent(in   ) :: time
    integer,            intent(in   ) :: dimdata
    integer,            intent(in   ) :: ndata
    real(kind=double),  intent(in   ) :: data(dimdata,ndata)
    type(file),         intent(in   ) :: fileout
    ! local vars
    integer :: res, u_number
    integer :: NInput
    integer :: i,j,k,lun,nnz
    integer, allocatable :: indeces_nonzeros(:)
    logical :: rc
    character(len=15)  :: scratch
    character(len=256)  :: str,rdwr
    
    scratch=etb(head_body_tail_whole)
    lun=fileout%lun

    select case ( head_body_tail_whole)
    case ('whole')
       ! write head time nonzeros
       write(lun,*, iostat=res) dimdata, ndata, ' !  dimensions'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if  
       write(lun,*, iostat=res) 'time  ', time
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if     
       write(lun,*, iostat=res) ndata, ' !  non zeros'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if 
       do i = 1, ndata
          write(lun,*, iostat=res) i,( data(j,i), j=1,dimdata) 
          if(res .ne. 0) THEN
             write(rdwr,'(i5)') i
             str=trim(adjustl(rdwr))//'/'
             rc = IOerr(lun_err, err_out , 'write2dat3', &
                  trim(fileout%fn) // &
                  ' at line '//trim(str),res)
          end if
       end do

       write(lun,*, iostat=res) 'time  ', time+huge
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if

    case ('head')
       ! write head time
       write(lun,*, iostat=res) dimdata, ndata, ' !  dimensions'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if
    case ('body')
        ! write data current data
       write(lun,*, iostat=res) 'time  ', time
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if

       !
       ! find non zeros entries
       !
       allocate(indeces_nonzeros(ndata),stat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_alloc, &
            ' read_TD ', &
            '  work array indeces_nonzeros',res)
       call find_nonzeros(dimdata,ndata,data,nnz,indeces_nonzeros)
       write(lun,*, iostat=res) nnz, ' !  non zeros'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if
       !
       ! write non zeroes entry
       !
       do j = 1, nnz
          i = indeces_nonzeros(j)
          write(lun,*,iostat=res) i, (data(k,i), k=1,dimdata)
          if(res .ne. 0) THEN
             write(rdwr,'(i5)') i
             str=trim(adjustl(rdwr))//'/'
             rc = IOerr(lun_err, err_out , 'write2dat3', &
                  trim(fileout%fn) // &
                  ' at line '//trim(str),res)
          end if
       end do

       !
       ! free memory
       !
       deallocate(indeces_nonzeros,stat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_dealloc, &
            ' read_TD ', &
            '  work array indeces_nonzeros',res)
       
    case ('tail')      
       write(lun,*, iostat=res) 'time  ', time+huge
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fileout%fn)// &
               ' time',res)
       end if
    end select
 
  end subroutine write2file

  !>-------------------------------------------------------------
  !> Write array into file in the form of steady state data
  !> (public procedure for type TimeData) at given 
  !> time 
  !> 
  !> usage:
  !>     call write_steady(lun_err, lun, ndata, data)
  !>
  !> where:
  !> \param[in] lun_err              -> integer. I/O err msg unit. 
  !> \param[in] head_body_tail_whole -> character. What has to be written
  !>                                   head  : write dimensions
  !>                                   body  : write time and data
  !>                                   tail  : close time
  !>                                   whole : write head+body+tail
  !> \param[in] time                 -> real(double). Time of data
  !> \param[in] dimdata              -> integer. first dimension of data
  !> \param[in] ndata                -> integer. second dimension of data
  !> \param[in] data                 -> real. Actual (dim x ndata) data
  !> \param[in] file                 -> type(file). File where data where to write
  !<-------------------------------------------------------------
  subroutine writearray2file(lun_err, head_body_tail_whole,&
       time, ndata, data,&
       lun,fn)
    use Globals
    implicit none
    ! vars
    integer,            intent(in   ) :: lun_err
    character(len=*),   intent(in   ) :: head_body_tail_whole
    real(kind=double),  intent(in   ) :: time
    integer,            intent(in   ) :: ndata
    real(kind=double),  intent(in   ) :: data(ndata)
    integer,            intent(in   ) :: lun
    character(len=*), intent(in   ) :: fn
    ! local vars
    integer :: res, u_number
    integer :: NInput
    integer :: i,j,k,nnz
    integer, allocatable :: indeces_nonzeros(:)
    logical :: rc
    character(len=15)  :: scratch
    character(len=256)  :: str,rdwr
    
    scratch=etb(head_body_tail_whole)
    select case ( head_body_tail_whole)
    case ('whole')
       ! write head time
       write(lun,*, iostat=res) 1, ndata, ' !  dimensions'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if       
      
       ! write data current data
       write(lun,*, iostat=res) 'time  ', time
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if 
       
       ! 
       write(lun,*, iostat=res) ndata, ' !  nonzeros'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if 

       do i = 1, ndata
          write(lun,*, iostat=res) i, data(i)
          if(res .ne. 0) THEN
             write(rdwr,'(i5)') i
             str=trim(adjustl(rdwr))//'/'
             rc = IOerr(lun_err, err_out , 'write2dat3', &
                  trim(fn) // &
                  ' at line '//trim(str),res)
          end if
       end do

       write(lun,*, iostat=res) 'time  ', time+huge
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if

    case ('head')
       ! write head time
       write(lun,*, iostat=res) 1, ndata, ' !  dimensions'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if
    case ('body')
        ! write data current data
       write(lun,*, iostat=res) 'time  ', time
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if     

              !
       ! find non zeros entries
       !
       allocate(indeces_nonzeros(ndata),stat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_alloc, &
            ' read_TD ', &
            '  work array indeces_nonzeros',res)
       call find_nonzeros(1,ndata,data,nnz,indeces_nonzeros)
       write(lun,*, iostat=res) nnz, ' !  non zeros'
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if
       !
       ! write non zeroes entry
       !
       do j = 1, nnz
          i = indeces_nonzeros(j)
          write(lun,*,iostat=res) i, data(i)
          if(res .ne. 0) THEN
             write(rdwr,'(i5)') i
             str=trim(adjustl(rdwr))//'/'
             rc = IOerr(lun_err, err_out , 'write2dat3', &
                  trim(fn) // &
                  ' at line '//trim(str),res)
          end if
       end do

       !
       ! free memory
       !
       deallocate(indeces_nonzeros,stat=res)
       if(res .ne. 0) rc = IOerr(lun_err, err_dealloc, &
            ' read_TD ', &
            '  work array indeces_nonzeros',res)

    case ('tail')      
       write(lun,*, iostat=res) 'time  ', time+small
       if(res .ne. 0) then
          rc = IOerr(lun_err, err_out , 'write2dat3', &
               trim(fn)// &
               ' time',res)
       end if
    end select
 
  end subroutine writearray2file

  function eval_ninput_data(dimdata,ndata,data) result(ninput)
    use Globals
    implicit none
    integer, intent(in) :: dimdata
    integer, intent(in) :: ndata
    real(kind=double) :: data(dimdata,ndata)
    integer :: ninput
    !local
    integer :: i
    real(kind=double) :: dnrm2
    ninput= 0
    do i = 1, ndata
       if ( dnrm2(dimdata,data(1:dimdata,i),1) > zero ) then 
          ninput=ninput+1
       end if
    end do
  end function eval_ninput_data

  subroutine find_nonzeros(dimdata,ndata,data,nnz,indeces_nonzeros)
    use Globals
    implicit none
    integer, intent(in) :: dimdata
    integer, intent(in) :: ndata
    real(kind=double), intent(in) :: data(dimdata,ndata)
    integer,intent(inout) ::nnz
    integer,intent(inout) :: indeces_nonzeros(ndata)
    !local
    integer :: i
    real(kind=double) :: dnrm2
    nnz= 0
    do i = 1, ndata
       if ( dnrm2(dimdata,data(1:dimdata,i),1) > zero ) then 
         nnz=nnz+1
         indeces_nonzeros(nnz) = i
       end if
    end do
  end subroutine find_nonzeros
  



end module TimeInputs
       
  
