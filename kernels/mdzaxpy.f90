!> @brief ZAXPY3 3D point-wise operator performing Z = aX + Y (scalar a)
!>
!> ZAXPY3 performs Z = XY where inputs @em X, @em Y and output @em Z 
!> are each three-dimensional arrays.
!> The kernel operates on the rectangular interval specified by
!> @em bufferInterval.
!> The shape of the input and output arrays are
!> specified by @em bufferSize, which is an @em numDim - dimensional
!> array that specifies the size in each of @em numDim dimesions.
!>
!> @param bufferInterval - const 64-bit integer array of size 2 x @em numDim indicating the rectangular interval in which the kernel should operate; e.g. [ @em iStart @em iEnd @em jStart @em jEnd ]
!> @param a - const double precision input scalar
!> @param X - const double precision input array
!> @param Y - const double precision input array
!> @param Z - double precision output array
subroutine zaxpy3(m,n,l,roi,a,x,y,z)
  
  integer*8 :: roi(6),m,n,l
  real*8    :: a,x(m,n,l),y(m,n,l)
  real*8    :: z(m,n,l)
  
  integer*8 :: i, j, k
  
  do k = roi(5),roi(6)
     do j = roi(3), roi(4)
        do i = roi(1), roi(2)
           z(i,j,k) = y(i,j,k) + a*x(i,j,k)
        end do
     end do
  end do
  
end subroutine zaxpy3
