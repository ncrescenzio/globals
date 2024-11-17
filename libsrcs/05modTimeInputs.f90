module TimeInputs

   use Globals

   implicit none

   private

   public write_steady, read_steady, write2file, writearray2file

   !> @brief Data structure containing values of a variable
   !> possibly depending on time (e.g., forcing function)
   type, public :: TimeData
      !> Logical flag to check if object is initialized
      logical :: built
      !> Dimension of real data array
      integer :: dimdata
      !> Number of data arrays
      integer :: ndata
      !> Dimension (`::dimdata`,`::ndata`,2).
      !> Input values. If data is steady state, then
      !> `::tdval(:,:,1) == ::tdval(:,:,2)`
      real(kind=double), allocatable :: TDval(:,:,:)
      !> Dimension (2)
      !> Time values. If data is steady state, then
      !> `::tdtime(2) = ::huge`
      real(kind=double), allocatable :: TDtime(:)
      !> Number of current non-zeros values
      integer :: nnonzeros = 0
      !> Dimension (`::dimdata`,`::ndata`).
      !> Actual values. If data is steady state, then
      !> `::tdactual = ::tdval(:,:,1)`
      real(kind=double), allocatable :: TDactual(:,:)
      !> Dimension (`::dimdata`)
      !> Scratch array for reading values
      real(kind=double), allocatable :: val(:)
      !> Time of evaluated `::tdactual`
      real(kind=double) :: time
      !> `.true.` if data do not depend on time,
      !> `.false.` if data depend on time
      logical :: steadyTD
      !> `.true.` if closing time has been written,
      !> `.false.` if closing time has not been written
      logical :: steadyTD_written = .false.
   contains
      !> Static constructor for `timeinputs::timedata`
      procedure, public, pass :: init => init_TD
      !> Static destructor for `timeinputs::timedata`
      procedure, public, pass :: kill => kill_TD
      !> Info procedure for `timeinputs::timedata`
      procedure, public, pass :: info => info_TD
      !> Set (read if necessary) non-steady time data
      procedure, public, pass :: set => set_TD
      !> Compute number of non zero elements of `::tdactual`
      procedure, public, pass :: eval_ninput
   end type TimeData

contains

   !>-------------------------------------------------------------
   !> @brief Static constructor for `timeinputs::timedata`
   !> @details Read data from input file or from input vector.
   !>
   !> Input file for **steady state data** must be written in the form
   !>
   !> ```
   !> dimdata ndata
   !> TIME: time1
   !> nnonzeros
   !> 1 data1, ..., data_dimdata
   !> 2 data1, ..., data_dimdata
   !> ...
   !> nonzeros data1, ..., data_dimdata
   !> ```
   !>
   !> If **input data depend on time**, the input file must be in the form:
   !>
   !> ```
   !> dimdata ndata
   !> TIME: time1
   !> nnonzeros
   !> 1 data1, ..., data_dimdata
   !> 2 data1, ..., data_dimdata
   !> ...
   !> nonzeros data1, ..., data_dimdata
   !> TIME: time2
   !> nnonzeros
   !> 1 data1, ..., data_dimdata
   !> 2 data1, ..., data_dimdata
   !> ...
   !> nonzeros data1, ..., data_dimdata
   !> ```
   !>
   !> Steady state input data can also be given using the input vector
   !> `default_data`. In this case:
   !>
   !> ```
   !> k = (j-1)*this%dimdata + i
   !> this%tdactual(i,j) = default_data(k)
   !> ```
   !>
   !> @param[in] stderr: unit number for error message
   !> @param[in] InputFdescr: file for input data. If file does not
   !>  exist, `default_data` must be given.
   !> @param[in] dimdata: valaue to be assigned to `timedata::dimdata`.
   !>  Optional if file is provided, otherwise must be given.
   !> @param[in] ndata: value to be assigned to `timedata::ndata`.
   !>  Optional if file is provided, otherwise must be given.
   !> @param[in] default_data: steady state input data. This variable
   !>  is required only if input file does not exist.
   !<-------------------------------------------------------------------
   subroutine init_TD(this, stderr, InputFdescr, dimdata, ndata, default_data)
      implicit none
      class(TimeData),   intent(out  )           :: this
      integer,           intent(in   )           :: stderr
      type(file),        intent(in   )           :: InputFdescr
      integer,           intent(in   ), optional :: dimdata
      integer,           intent(in   ), optional :: Ndata
      real(kind=double), intent(in   ), optional :: default_data(:)
      ! local vars
      integer :: u_number
      integer :: NInput,ival
      integer :: res,i,j,k
      logical :: rc
      character(len=15) :: scratch
      character(len=256) ::  fname,msg

      this%built = .true.

      if (InputFdescr%exist) then

         u_number = InputFdescr%lun
         fname    = InputFdescr%fn

         ! read file
         ! read dimdata, Ndata
         read(u_number,*,iostat=res) this%dimdata, this%Ndata
         if(res .ne. 0) &
            rc = IOerr(stderr, err_inp , 'read_TD', &
            trim(fname) // ' type TimeData member dimdata, Ndata ',res)

         ! check dimension
         if (present(dimdata)) then
            if (dimdata.ne.this%dimdata) then
               rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(fname) // ' mismatch between read and given dimdata')
            end if
         end if

         if (present(Ndata)) then
            if (Ndata.ne.this%Ndata) then
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

         this%TDtime = zero
         this%steadyTD = .false.

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
         this%nnonzeros = NInput

         ! read Data
         do i = 1,NInput
            read(u_number,*,iostat=res) ival, (this%val(k), k=1,this%dimdata)
            if(res .ne. 0) then
               write(msg,*) i
               write(*,*) res
               rc = IOerr(stderr, err_inp , 'read_TD', &
                  trim(fname) // ' type TimeDara member ival '//etb(msg),res)
            end if
            this%TDval(:,ival,1) = this%val(:)
         end do

         ! read the second time for TD
         read(u_number,*,iostat=res) scratch, this%TDtime(2)
         if(res.ne.0) then
            ! end file => file assume to be in stready state
            if (res.eq.-1)  then
               this%TDtime(2) = huge
               this%TDactual(:,:) = this%TDval(:,:,1)
               this%TDval(:,:,2) = this%TDval(:,:,1)
               this%steadyTD = .true.
            else
               rc = IOerr(stderr, err_inp , 'read_TD', &
                  etb(trim(InputFdescr%fn)//&
                  ' type TimeData member timein(2) '),res)
            end if
         end if

         if (this%TDtime(2).ge.huge)then
            ! if Time data is in steady state copy TD(:,:,1) into TD(:,:,2)
            this%TDactual(:,:) = this%TDval(:,:,1)
            this%TDval(:,:,2) = this%TDval(:,:,1)
            this%steadyTD = .true.
         else
            ! if not in stready state read TD(:,:,2)
            ! read Ninputs
            read(u_number,*,iostat=res) NInput
            if(res.ne.0) &
               rc = IOerr(stderr, err_inp , 'read_TD', &
               trim(InputFdescr%fn) // ' type TimeData member NInput ',res)
            this%nnonzeros = NInput

            ! read Data
            do i = 1,NInput
               read(u_number,*,iostat=res) ival, (this%val(k), k=1,this%dimdata)
               if(res.ne.0) &
                  rc = IOerr(stderr, err_inp , 'read_TD', &
                     trim(InputFdescr%fn) // ' type TimeDara member ival,val ',res)
               this%TDval(:,ival,2) = this%val(:)
            end do
         end if
      else
         if (present(dimdata).and.present(ndata).and.present(default_data)) then
            ! if a defualt data data is passed
            ! then time data is set in steady state

            this%dimdata = dimdata
            this%Ndata = Ndata
            this%steadyTD = .true.

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

            ! copy defualt data
            k = 0
            do j = 1,ndata
               do i = 1,dimdata
                  k = k+1
                  this%TDactual(i,j) = default_data(k)
                  this%TDVal(i,j,1)  = default_data(k)
                  this%TDVal(i,j,2)  = default_data(k)
               end do
            end do

            allocate(this%TDtime(2),stat=res)
            if(res .ne. 0) rc = IOerr(stderr, err_alloc, 'read_TD', &
               '  type TimeData member TDtime (array)',res)
            this%TDtime(1) = -huge
            this%TDtime(2) =  huge
         else
            if(res .ne. 0) rc = IOerr(stderr, err_inp, 'read_TD', &
               '  file '//etb(InputFdescr%fn)//' does not esxits'//&
               ' and default not assigned ')
         end if
      end if

   end subroutine init_TD

   !>-------------------------------------------------------------
   !> @brief Static destructor for `timeinputs::timedata`
   !>
   !> @param[in] lun: unit number for error message output
   !<-----------------------------------------------------------
   subroutine kill_TD(this, lun)
      implicit none
      class(TimeData), intent(inout) :: this
      integer,         intent(in   ) :: lun
      ! local vars
      integer :: res
      logical :: rc

      this%built = .false.
      deallocate(&
         this%TDval,&
         this%TDtime,&
         this%TDActual,&
         this%val,&
         stat=res)
      if (res.ne.0) rc=IOerr(lun, err_dealloc, 'kill_TD', &
         'dealloc fail for TimeData var TDvar,TDtime,TDactual, val')

   end subroutine kill_TD

   !>-------------------------------------------------------------
   !> @brief Info procedure for `timeinputs::timedata`.
   !> @details Print the first `nsample` non-zero terms
   !> of `timedata::tdval(:,:.1)`, `timedata::tdactual(:,:)`
   !> and `timedata::tdval(:,:,2)`.
   !>
   !> @param[in] lun: unit number for error message output.
   !> @param[in] nsample: number of first non-zero samples to be printed
   !<-------------------------------------------------------------
   subroutine info_TD(this, lun, nsample)
      implicit none
      class(TimeData), intent(in) :: this
      integer,         intent(in) :: lun
      integer,         intent(in) :: nsample
      ! local vars
      integer :: i,j,k
      real(kind=double) :: dnrm2

      write(lun,*) ' '
      write(lun,*) ' Info: TimeData structure definition:'

      write(lun,*) 'ndata', this%ndata
      if(this%steadyTD) write(lun,*) ' Steady state'

      write(lun,*) ' t1          = ', this%TDtime(1)
      write(lun,*) ' actual time = ', this%time
      write(lun,*) ' t2          = ', this%TDtime(2)

      if (nsample.gt.0) then
         write(lun,*) ' First Data'
         i=0
         j=0
         do while ((i.lt.this%ndata).and.(j.lt.nsample))
            i=i+1
            if (dnrm2(this%dimdata,this%TDval(:,i,1),1).ne.zero) then
               j=j+1
               write(lun,'(5(i5,e11.3))') i,(this%TDval(k,i,1),k=1,this%dimdata)
            end if
         end do
         write(lun,*)
         write(lun,*) ' Actual Data'
         i=0
         j=0
         do while ((i.lt.this%ndata).and.(j.lt.nsample))
            i=i+1
            if (dnrm2(this%dimdata,this%TDactual(:,i),1).ne.zero) then
               j=j+1
               write(lun,'(5(i5,e11.3))') i,(this%TDactual(k,i),k=1,this%dimdata)
            end if
         end do
         write(lun,*)
         write(lun,*) ' Second time  Data'
         i=0
         j=0
         do while ((i.lt.this%ndata).and.(j.lt.nsample))
            i=i+1
            if (dnrm2(this%dimdata,this%TDval(:,i,2),1).ne.zero) then
               j=j+1
               write(lun,'(5(i5,e11.3))') i,(this%TDval(k,i,2),k=1,this%dimdata)
            end if
         end do
      end if

   end subroutine info_TD

   !>-------------------------------------------------------------
   !> @brief Set (read if necessary) non-steady time data.
   !> @details If `timedata::steadytd = .true.`, do nothing.
   !> Set the value of `timedata::tdactual` using a linear
   !> interpolation in time of the values in `timedata::tdval`.
   !>
   !> @param[in   ] stderr: unit number for error message
   !> @param[in   ] InputFdescr: input file for reading data
   !> @param[in   ] time: time for the computation of `timedata::tdactual`
   !> @param[inout] endfile: `.true.` if end of file is reached
   !> @param[inout] info: error code (optional)
   !<-------------------------------------------------------------
   subroutine set_TD(this, stderr, InputFdescr, time, endfile, info)
      implicit none
      class(TimeData),   intent(inout)           :: this
      integer,           intent(in   )           :: stderr
      type(file),        intent(in   )           :: InputFdescr
      real(kind=double), intent(in   )           :: time
      logical,           intent(inout)           :: endfile
      integer,           intent(inout), optional :: info
      ! local vars
      integer :: res, u_number
      integer :: NInput
      integer :: i,k,ival
      real(kind=double) ::  TDt1,TDt2,tperc,next_time
      logical :: rc, read_next
      character(len=15)  :: scratch
      character(len=256) :: fname

      endfile = .false.

      this%time = time
      u_number = InputFdescr%lun
      fname    = InputFdescr%fn

      ! If steady, do nothing
      if (.not.this%steadyTD) then
         if (time.lt.this%TDtime(1)) then
            rc = IOerr(stderr, wrn_inp , 'set_TD', &
               ' time required is smaller TDtime(1)')
            if (present(info)) info = 1
            return
         end if
         if (time.ge.this%TDtime(2)) then
            read_next = .true.
            do while (read_next)
               ! Read time2
               read(u_number,*,iostat=res) scratch, next_time
               if (res.ne.0) then
                  if (res.eq.-1) then
                     this%TDval(:,:,1)  = this%TDval(:,:,2)
                     this%TDactual(:,:) = this%TDval(:,:,2)
                     endfile = .true.
                     return
                  else
                     rc = IOerr(stderr, err_inp , 'set_TD', &
                        trim(fname)&
                        //' type TimeData member TDtime(2)',res)
                  end if
               else
                  this%TDtime(1) = this%TDtime(2)
                  this%TDtime(2) = next_time
               end if
               ! copy TD2 into TD1 before overwritten
               this%TDval(:,:,1) = this%TDval(:,:,2)

               ! If steady state (time2 .ge. huge) then frezee
               if (this%TDtime(2).ge.huge) then
                  this%steadyTD = .true.
                  this%TDval(:,:,2)  = this%TDval(:,:,1)
                  this%TDactual(:,:) = this%TDval(:,:,1)
                  read_next = .false.
               else
                  ! Otherwise read new data TD2
                  ! read ninputs
                  read(u_number,*,iostat=res) NInput
                  if (res.ne.0) then
                     if (res.eq.-1) then
                        this%TDval(:,:,1)  = this%TDval(:,:,2)
                        this%TDactual(:,:) = this%TDval(:,:,2)
                        endfile = .true.
                        return
                     else
                        rc = IOerr(stderr, wrn_inp , 'set_TD', &
                           trim(fname)//' type TimeData member NInput ',res)

                        endfile = .True.
                        return
                     end if
                  end if
                  this%nnonzeros = NInput

                  ! read data
                  this%TDval(:,:,2) = zero
                  do i = 1,NInput
                     read(u_number,*,iostat=res) ival, (this%val(k),k=1,this%dimdata)
                     if (res.ne.0) &
                        rc = IOerr(stderr, err_inp , 'set_TD', &
                        trim(fname)&
                        //' type TimeData aux varibles i,val',res)
                     this%TDval(:,ival,2) = this%val(:)
                  end do
                  !> Test if continue reading file
                  read_next = (time.ge.this%TDtime(2))
               end if
            end do
         end if

         TDt1 = this%TDtime(1)
         TDt2 = this%TDtime(2)
         tperc = (time-TDt1)/(TDt2-TDt1)

         do i = 1,this%ndata
            this%TDactual(:,i) = (one-tperc)*this%TDval(:,i,1) + &
               tperc*this%TDval(:,i,2)
         end do
      end if

   end subroutine set_TD

   !>-------------------------------------------------------------
   !> @brief Compute number of non-zero elements of `timedata::tdactual`
   !>
   !> @param[out] result: number of non-zero elements of `timedata::tdactual`
   !<-------------------------------------------------------------
   function eval_ninput(this) result(ninput)
      implicit none
      class(TimeData), intent(in) :: this
      integer                     :: ninput
      !local
      integer :: i
      real(kind=double) :: dnrm2

      ninput = 0

      do i = 1,this%NData
         if (dnrm2(this%dimdata,this%TDactual(:,i),1) > zero) then
            ninput=ninput+1
         end if
      end do

   end function eval_ninput

   !>-------------------------------------------------------------
   !> @brief Write data array into file in the form of steady state data
   !> @details Output file has the form:
   !>
   !> ```
   !> 1, ndata
   !> time 1.0e-30
   !> ndata
   !> 1     data(1)
   !> 2     data(2)
   !> ...
   !> ndata data(ndata)
   !> time 1.0e+30
   !> ```
   !>
   !> @param[in] stderr: unit number for error message
   !> @param[in] lun: unit number for file in which data is written
   !> @param[in] ndata: length of `data` array
   !> @param[in] data: array of data to be written
   !> @param[in] fname: name of file where data is written (optional)
   !<-------------------------------------------------------------
   subroutine write_steady(stderr, lun, ndata, data, fname)
      implicit none
      integer,           intent(in   )           :: stderr
      integer,           intent(in   )           :: lun
      integer,           intent(in   )           :: ndata
      real(kind=double), intent(in   )           :: data(ndata)
      character(len=*),  intent(in   ), optional :: fname
      ! local vars
      integer :: res, u_number
      integer :: NInput
      integer :: i
      logical :: rc
      character(len=15)  :: scratch
      character(len=15)  :: filename

      if (present(fname)) then
         filename = etb(fname)
      else
         filename = 'File name not passed'
      end if

      write(lun,*,iostat=res) 1, ndata
      if(res .ne. 0) &
         rc = IOerr(stderr, err_out , 'write_steady', &
         etb(filename),res)

      write(lun,*,iostat=res) 'time 1.0e-30'
      write(lun,*,iostat=res) ndata
      do i = 1,ndata
         write(lun,*) i, data(i)
      end do
      write(lun,*,iostat=res) 'time 1.0e+30'

   end subroutine write_steady

   !>-------------------------------------------------------------
   !> @brief Read data array from file in the form of steady state data
   !>
   !> @param[in   ] stderr: unit number for error message
   !> @param[in   ] ndata: data length
   !> @param[inout] data: data to read (length `ndata`)
   !> @param[in   ] open_file: file containing input data
   !<-------------------------------------------------------------
   subroutine read_steady(stderr, ndata, data, open_file)
      implicit none
      integer,           intent(in   ) :: stderr
      integer,           intent(in   ) :: ndata
      real(kind=double), intent(inout) :: data(ndata)
      type(file),        intent(in   ) :: open_file
      ! local vars
      logical :: end_of_file
      type(TimeData):: tdata

      call tdata%init(stderr, open_file, 1, ndata)
      call tdata%set(stderr, open_file,&
         onehalf*(tdata%TDtime(1)+tdata%TDtime(2)),&
         end_of_file)
      data(:) = tdata%TDactual(1,:)
      call tdata%kill(stderr)

   end subroutine read_steady

   !>-------------------------------------------------------------
   !> @brief Write data matrix into file in the form of steady state data
   !>
   !> @param[in] lun_err: unit number for error message
   !> @param[in] head_body_tail_whole: what has to be written
   !>            * `head`: write dimensions
   !>            * `body`: write time and data
   !>            * `tail`: close time
   !>            * `whole`: write head, body and tail
   !> @param[in] time: time of data
   !> @param[in] dimdata: first dimension of `data`
   !> @param[in] ndata: second dimension of `data`
   !> @param[in] data: data to write
   !> @param[in] file: file where to write data
   !<-------------------------------------------------------------
   subroutine write2file(lun_err, head_body_tail_whole,&
         dimdata, ndata, data,time, fileout)
      implicit none
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
            if(res .ne. 0) then
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
   !> @brief Write data array into file in the form of steady state data
   !>
   !> @param[in] lun_err: unit number for error message
   !> @param[in] head_body_tail_whole: what has to be written
   !>            * `head`: write dimensions
   !>            * `body`: write time and data
   !>            * `tail`: close time
   !>            * `whole`: write head, body and tail
   !> @param[in] time: time of data
   !> @param[in] ndata: second dimension of `data`
   !> @param[in] data: data to write
   !> @param[in] lun: unit number for output file
   !> @param[in] fn: output file name
   !<-------------------------------------------------------------
   subroutine writearray2file(lun_err, head_body_tail_whole,&
         time, ndata, data, lun, fn)
      implicit none
      integer,           intent(in   ) :: lun_err
      character(len=*),  intent(in   ) :: head_body_tail_whole
      real(kind=double), intent(in   ) :: time
      integer,           intent(in   ) :: ndata
      real(kind=double), intent(in   ) :: data(ndata)
      integer,           intent(in   ) :: lun
      character(len=*),  intent(in   ) :: fn
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

   !>-------------------------------------------------------------
   !> @brief Compute number of non-zero input data.
   !> @details Compute the number of columns of input
   !> variable `data` having non-zero norm.
   !>
   !> @param[in] dimdata: number of columns of `data`
   !> @param[in] ndata: number of rows of `data`
   !> @param[in] data: input data
   !<-------------------------------------------------------------
   function eval_ninput_data(dimdata,ndata,data) result(ninput)
      implicit none
      integer,           intent(in) :: dimdata
      integer,           intent(in) :: ndata
      real(kind=double), intent(in) :: data(dimdata,ndata)
      integer                       :: ninput
      !local
      integer :: i
      real(kind=double) :: dnrm2

      ninput = 0
      do i = 1,ndata
         if (dnrm2(dimdata,data(1:dimdata,i),1) > zero) then
            ninput=ninput+1
         end if
      end do

   end function eval_ninput_data

   !>-------------------------------------------------------------
   !> @brief Compute number and indices of non-zero input data.
   !> @details Compute the number of columns of input
   !> variable `data` having zero norm and their indices.
   !>
   !> @param[in   ] dimdata: number of columns of `data`
   !> @param[in   ] ndata: number of rows of `data`
   !> @param[in   ] data: input data
   !> @param[out  ] nnz: number of columns of `data` having non-zero norm
   !> @param[inout] indeces_nonzeros: indices of columns of `data`
   !>  having non-zero norm
   !<-------------------------------------------------------------
   subroutine find_nonzeros(dimdata,ndata,data,nnz,indeces_nonzeros)
      implicit none
      integer,           intent(in   ) :: dimdata
      integer,           intent(in   ) :: ndata
      real(kind=double), intent(in   ) :: data(dimdata,ndata)
      integer,           intent(  out) :: nnz
      integer,           intent(inout) :: indeces_nonzeros(ndata)
      !local
      integer :: i
      real(kind=double) :: dnrm2

      nnz = 0
      do i = 1,ndata
         if (dnrm2(dimdata,data(1:dimdata,i),1) > zero) then
            nnz = nnz+1
            indeces_nonzeros(nnz) = i
         end if
      end do

   end subroutine find_nonzeros

end module TimeInputs
