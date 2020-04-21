!> @brief ZAXPY3 3D point-wise operator performing Z = a*X + Y (scalar a)
!>
!> ZAXPY3 performs Z = a*X + Y where a is a double precision scalar and 
!> inputs @em x, @em y and output @em z are each three-dimensional arrays.
!> The kernel operates on the rectangular interval specified by
!> @em {ijk}min, @em {ijk}max
!> The size of the input and output arrays are
!> specified by @em (m,n,l)
!>
!> @param m - const long integer indicating size in I direction
!> @param n - const long integer indicating size in J direction
!> @param l - const long integer indicating size in K direction
!> @param imin - const long integer indicating Region of Interest (ROI) in I direction
!> @param imax - const long integer indicating Region of Interest (ROI) in I direction
!> @param jmin - const long integer indicating Region of Interest (ROI) in J direction
!> @param jmax - const long integer indicating Region of Interest (ROI) in J direction
!> @param kmin - const long integer indicating Region of Interest (ROI) in K direction
!> @param kmax - const long integer indicating Region of Interest (ROI) in K direction
!> @param a - const double precision input scalar
!> @param x - const double precision input array
!> @param y - const double precision input array
!> @param z - double precision output array
subroutine zaxpy3(m,n,l,imin,imax,jmin,jmax,kmin,kmax,a,x,y,z)
  
  implicit none

  ! === Input & Parameters ===
  integer*8 :: m,n,l
  integer*8 :: imin,imax,jmin,jmax,kmin,kmax
  real*8    :: a,x(m,n,l),y(m,n,l)
  ! === Output ===
  real*8    :: z(m,n,l)
  
  ! === Local ===
  integer*8 :: i, j, k
  
  do k = kmin,kmax
     do j = jmin, jmax
        do i = imin, imax
           z(i,j,k) = y(i,j,k) + a*x(i,j,k)
        end do
     end do
  end do
  
end subroutine zaxpy3
