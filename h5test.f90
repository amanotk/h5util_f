!
! HDF5 utility test program
!
program h5test
  use hdf5
  use h5util
  use mpi
  implicit none

  integer :: error
  integer(hid_t) :: file
  integer(hsize_t) :: gdims(3), goffset(3)
  integer(hsize_t) :: ldims(3), loffset(3)
  integer(hsize_t) :: dimm(3), current_dims(3)

  integer :: ndim, nrank, nsize, current_ndim
  real(8) :: x(4,8,2)
  real(8) :: y(4,4)

  ! attribute
  character(len=*), parameter :: c_attr = 'this is comment attribute'
  integer :: i_attr = 10000, i_arr_attr(3) = (/1, 2, 3/)
  real(4) :: r_attr = 0.1_8, r_arr_attr(2) = (/0.1_4, 0.2_4/)
  real(8) :: d_attr = 1.0_8, d_arr_attr(1) = (/1.0_8/)

  ! initialize
  call mpi_init(error)
  call h5util_init()

  call mpi_comm_size(MPI_COMM_WORLD, nsize, error)
  call mpi_comm_rank(MPI_COMM_WORLD, nrank, error)

  ! create and open file
  call h5util_create_file("h5test.h5")
  call h5util_open_file("h5test.h5", file)

  ! put attribute
  call h5util_put_attribute(file, "int_attr", i_attr)
  call h5util_put_attribute(file, "real_attr", r_attr)
  call h5util_put_attribute(file, "double_attr", d_attr)
  call h5util_put_attribute(file, "string_attr", c_attr)
  call h5util_put_attribute(file, "int_arr_attr", i_arr_attr)
  call h5util_put_attribute(file, "real_arr_attr", r_arr_attr)
  call h5util_put_attribute(file, "double_arr_attr", d_arr_attr)

  ! array
  ndim          = 3
  ldims         = shape(x)
  loffset       = 0
  gdims         = ldims
  gdims(ndim)   = ldims(ndim) * nsize
  goffset       = 0
  goffset(ndim) = ldims(ndim) * nrank

  x = nrank
  call h5util_create_dataset(file, "x", H5T_NATIVE_DOUBLE, ndim, gdims)
  call h5util_write_dataset(file, "x", ndim, ldims, ldims, loffset, goffset, &
       & reshape(x, (/size(x)/)))

  ! extensible array
  ndim          = 2
  ldims(1:ndim) = shape(y)
  ldims(ndim+1) = 1
  loffset       = 0
  gdims         = ldims
  gdims(ndim)   = ldims(ndim) * nsize
  goffset       = 0
  goffset(ndim) = ldims(ndim) * nrank
  dimm          = gdims
  gdims(ndim+1) = 1
  dimm(ndim+1)  = H5S_UNLIMITED_F

  ! initial
  y = nrank
  call h5util_create_dataset(file, "y", H5T_NATIVE_DOUBLE, ndim+1, gdims, dimm)
  call h5util_write_dataset(file, "y", ndim+1, ldims, ldims, loffset, goffset, &
       & reshape(y, (/size(y)/)))

  ! next
  call h5util_extend_dimension(file, "y")
  call h5util_get_dimension(file, "y", current_ndim, current_dims)
  goffset(ndim+1) = current_dims(ndim+1) - 1
  y               = y + nsize
  call h5util_write_dataset(file, "y", ndim+1, ldims, ldims, loffset, goffset, &
       & reshape(y, (/size(y)/)))

  ! and next
  call h5util_extend_dimension(file, "y")
  call h5util_get_dimension(file, "y", current_ndim, current_dims)
  goffset(ndim+1) = current_dims(ndim+1) - 1
  y               = y + nsize
  call h5util_write_dataset(file, "y", ndim+1, ldims, ldims, loffset, goffset, &
       & reshape(y, (/size(y)/)))

  ! close file
  call h5util_close_file(file)

  ! finalize
  call h5util_finalize()
  call mpi_finalize(error)

  stop
end program h5test
