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

  implicit none
  type(file)     :: file_data, file_timedata

  integer :: stderr,stdout,debug, flag_reading
  integer :: i,j,k, itria,itime
  integer :: ndata,dim,ninputs
  integer,allocatable :: data_dims(:),nnz(:)

  character(len=256) :: arg

  character(len=256) :: path_data
  character(len=256) :: path_timedata
  character(len=256) :: data_type
  character(len=256) :: vtk_folder   
  character(len=256) :: fname
  character(len=256) :: name_data,vtk_name
  real(kind=double), allocatable :: datas(:,:)

  ! local vars
  integer :: u_number
  integer :: res,ncoord,nnodeincell
  integer :: connection(5)
  logical :: rc
  character(len=256) :: rdwr,str,first_line

  logical endfile
  integer ppos,pbar

  character(len=8) :: label

  stderr = 6

  CALL getarg(1, arg)
  read(arg,'(a256)') path_data

  CALL getarg(2, arg)
  read(arg,'(a256)') path_timedata
  
  ! read datas and count non zeros
  file_data%fn=etb(path_data)
  file_data%lun=10
  open(file_data%lun,file=file_data%fn)
  
  u_number    = file_data%lun
  fname       = file_data%fn

  read(u_number,*,iostat=res) dim, ndata
  allocate(datas(dim,ndata),nnz(ndata),stat=res)
  if(res .ne. 0) rc = IOerr(6, err_alloc, 'read_mesh', &
       '  datas (array)',res)
  ninputs=0
  do j=1,ndata
     read(u_number,*,iostat=res) (datas(k,j),k=1,dim)
     if(res .ne. 0) THEN
        write(rdwr,'(i5)') j
        str=etb(rdwr)//'/'
        rc = IOerr(6, err_inp , 'read_mesh', &
             trim(etb(fname)) //&
             ' type mesh member array coord at line '//&
             trim(str),res)
     end if
     if (maxval(abs(datas(:,j))) > 0.0) then
        ninputs=ninputs+1
        nnz(ninputs) = j
     end if
  end do
  close(file_data%lun)

  ! read datas and count non zeros
  file_timedata%fn=etb(path_timedata)
  file_timedata%lun=10
  open(file_data%lun,file=file_timedata%fn)
  
  u_number    = file_timedata%lun
  fname       = file_timedata%fn

  write(u_number,*,iostat=res) dim, ndata
  write(u_number,*,iostat=res) 'time 0.0'
  write(u_number,*,iostat=res) ninputs
  do j=1,ninputs
     write(u_number,*,iostat=res) nnz(j),(datas(k,nnz(j)),k=1,dim)
     if(res .ne. 0) THEN
        write(rdwr,'(i5)') j
        str=etb(rdwr)//'/'
        rc = IOerr(6, err_out , 'data2timedata', &
             trim(etb(fname)) //&
             ' array datas  at line '//&
             trim(str),res)
     end if
  end do
  write(u_number,*,iostat=res) 'time 1.0e30'
  close(file_timedata%lun)

  deallocate(datas,nnz,stat=res)


  
end PROGRAM data2timedata
