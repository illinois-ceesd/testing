MODULE SIMPLE

  IMPLICIT NONE

CONTAINS

  !> @brief YAXPY point-wise operator performing Y = aX + Y (scalar a)
  SUBROUTINE YAXPY(numDim,numPoints,bufferSize,bufferInterval,a,X,Y)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a,X(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = Y(I) + a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = Y(bufferIndex) + a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = Y(bufferIndex) + a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE YAXPY

  !> @brief ZAXPY point-wise operator performing Z = aX + Y (scalar a)
  SUBROUTINE ZAXPY(numDim,numPoints,bufferSize,bufferInterval,a,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a,X(numPoints),Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = Y(I) + a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = Y(bufferIndex) + a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = Y(bufferIndex) + a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZAXPY
  
  !> @brief ZAXPBY point-wise operator performing Z = aX + bY (scalar a,b)
  SUBROUTINE ZAXPBY(numDim,numPoints,bufferSize,bufferInterval,a,b,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a,b,X(numPoints),Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = b*Y(I) + a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = b*Y(bufferIndex) + a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = b*Y(bufferIndex) + a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZAXPBY

  !> @brief XAXM1 point-wise operator performing X = a/X (scalar a)
  SUBROUTINE XAXM1(numDim,numPoints,bufferSize,bufferInterval,a,X)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(INOUT)   :: X(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          X(I) = a/X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             X(bufferIndex) = a/X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                X(bufferIndex) = a/X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE XAXM1
  

  !> @brief XAM1X point-wise operator performing X = X/a (scalar a)
  SUBROUTINE XAM1X(numDim,numPoints,bufferSize,bufferInterval,a,X)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(INOUT)   :: X(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          X(I) = X(I)/a
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             X(bufferIndex) = X(bufferIndex)/a
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                X(bufferIndex) = X(bufferIndex)/a
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE XAM1X
  


  !> @brief YAXPBY point-wise operator performing Y = aX + bY (scalar a,b)
  SUBROUTINE YAXPBY(numDim,numPoints,bufferSize,bufferInterval,a,b,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a,b,X(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = b*Y(I) + a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = b*Y(bufferIndex) + a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = b*Y(bufferIndex) + a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE YAXPBY

  !> @brief YAX point-wise operator performing Y = aX  (scalar a)
  SUBROUTINE YAX(numDim,numPoints,bufferSize,bufferInterval,a,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a,X(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE YAX

  

  !> @brief ZXYM1 point-wise operator performing Z = X/Y (all vectors)
  SUBROUTINE ZXYM1(numDim,numPoints,bufferSize,bufferInterval,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints),Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = X(I)/Y(I)
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = X(bufferIndex)/Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = X(bufferIndex)/Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE ZXYM1

  !> @brief ZXY point-wise operator performing Z = XY (all vectors)
  !>
  !> ZXY performs Z = XY where @em X, @em Y are each contiguous arrays.
  !> Operand arrays are contiguous of size @em numPoints.
  !> The kernel operates on the rectangular interval specified by
  !> @em bufferInterval.
  !> The shape of the input and output arrays are
  !> specified by @em bufferSize, which is an @em numDim - dimensional
  !> array that specifies the size in each of @em numDim dimesions.
  !>
  !> @param numDim - const integer input specifies the number of dimensions of the input and output arrays
  !> @param numPoints - const 64-bit integer input specifies the total size of the input and output arrays
  !> @param bufferSize - const 64-bit integer array of size @em numDim specifies the size of the input and output arrays in each of @em numDim dimensions 
  !> @param bufferInterval - const 64-bit integer array of size 2 x @em numDim indicating the rectangular interval in which the kernel should operate; e.g. [ @em iStart @em iEnd @em jStart @em jEnd ]
  !> @param X - const double precision input array
  !> @param Y - const double precision input array
  !> @param Z - double precision output array
  SUBROUTINE ZXY(numDim,numPoints,bufferSize,bufferInterval,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = X(I)*Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = X(bufferIndex)*Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = X(bufferIndex)*Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZXY

  !> @brief ZAXY point-wise operator performing Z = aXY (scalar a, vectors X,Y)
  SUBROUTINE ZAXY(numDim,numPoints,bufferSize,bufferInterval,a,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = a*X(I)*Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = a*X(bufferIndex)*Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = a*X(bufferIndex)*Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZAXY

  !> @brief YXY point-wise operator performing Y = XY (all vectors)
  SUBROUTINE YXY(numDim,numPoints,bufferSize,bufferInterval,X,Y)

    INTEGER(KIND=4), INTENT(IN)    :: numDim
    INTEGER(KIND=8), INTENT(IN)    :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)    :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)    :: X(numPoints)
    REAL(KIND=8),    INTENT(INOUT) :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = X(I)*Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = X(bufferIndex)*Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = X(bufferIndex)*Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE YXY

  !> @brief ZWXPY point-wise operator performing Z = WX + Y, where all are vectors
  SUBROUTINE ZWXPY(numDim,numPoints,bufferSize,bufferInterval,W,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: W(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = X(I)*W(I) + Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = X(bufferIndex)*W(bufferIndex) + Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = X(bufferIndex)*W(bufferIndex) + Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZWXPY

  !> @brief YWXPY point-wise operator performing Y = WX + Y, where all are vectors
  SUBROUTINE YWXPY(numDim,numPoints,bufferSize,bufferInterval,W,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: W(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = X(I)*W(I) + Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = X(bufferIndex)*W(bufferIndex) + Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = X(bufferIndex)*W(bufferIndex) + Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE YWXPY

  !> @brief ZAWPXY point-wise operator performing Z = aW + XY 
  !>
  !> ZAWPXY kernel performs Z = aW + XY with scalar @em a, contiguous input vectors
  !> @em W, @em X, and @em Y, and output vector @em Z.   
  !> Operand arrays are contiguous of size @em numPoints.
  !> The kernel operates on the rectangular interval specified by
  !> @em bufferInterval.
  !> The shape of the input and output arrays are
  !> specified by @em bufferSize, which is an @em numDim - dimensional
  !> array that specifies the size in each of @em numDim dimesions.
  !>
  !> @param numDim - const integer input specifies the number of dimensions of the input and output arrays
  !> @param numPoints - const 64-bit integer input specifies the total size of the input and output arrays
  !> @param bufferSize - const 64-bit integer array of size @em numDim specifies the size of the input and output arrays in each of @em numDim dimensions 
  !> @param bufferInterval - const 64-bit integer array of size 2 x @em numDim indicating the rectangular interval in which the kernel should operate; e.g. [ @em iStart @em iEnd @em jStart @em jEnd ]
  !> @param a - const input double precision scalar
  !> @param W - const double precision contiguous array of size @em numPoints
  !> @param X - const double precision contiguous array of size @em numPoints
  !> @param Y - const double precision contiguous array of size @em numPoints
  !> @param Z - output double precision contiguous array of size @em numpoints
  SUBROUTINE ZAWPXY(numDim,numPoints,bufferSize,bufferInterval,a,W,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: W(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = a*W(I) + X(I)*Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = a*W(bufferIndex) + X(bufferIndex)*Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = a*W(bufferIndex) + X(bufferIndex)*Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZAWPXY

  !> @brief ZVWPXY point-wise operator performing Z = VW + XY 
  !>
  !> ZVWPXY kernel performs Z = VW + XY with contiguous double precision
  !> input arrays  @em V, @em W, @em X, and @em Y, and output array @em Z.
  !> Operand arrays are contiguous of size @em numPoints.
  !> The kernel operates on the rectangular interval specified by
  !> @em bufferInterval.
  !> The shape of the input and output arrays are
  !> specified by @em bufferSize, which is an @em numDim - dimensional
  !> array that specifies the size in each of @em numDim dimesions.
  !>
  !> @param numDim - const integer input specifies the number of dimensions of the input and output arrays
  !> @param numPoints - const 64-bit integer input specifies the total size of the input and output arrays
  !> @param bufferSize - const 64-bit integer array of size @em numDim specifies the size of the input and output arrays in each of @em numDim dimensions 
  !> @param bufferInterval - const 64-bit integer array of size 2 x @em numDim indicating the rectangular interval in which the kernel should operate; e.g. [ @em iStart @em iEnd @em jStart @em jEnd ]
  !> @param V - input const double precision contiguous array of size @em numPoints
  !> @param W - input const double precision contiguous array of size @em numPoints
  !> @param X - input const double precision contiguous array of size @em numPoints
  !> @param Y - input const double precision contiguous array of size @em numPoints
  !> @param Z - output double precision contiguous array of size @em numpoints
  SUBROUTINE ZVWPXY(numDim,numPoints,bufferSize,bufferInterval,V,W,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: V(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: W(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = V(I)*W(I) + X(I)*Y(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = V(bufferIndex)*W(bufferIndex) + &
                  X(bufferIndex)*Y(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = V(bufferIndex)*W(bufferIndex) + &
                     X(bufferIndex)*Y(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZVWPXY
  
  !> @brief ZWMXPY point-wise operator performing Z = W(X+Y) where all are vectors
  !
  !> ZVWPXY kernel performs Z = W(X+Y) with contiguous double precision
  !> input arrays  @em W, @em X, and @em Y, and output array @em Z.
  !> Operand arrays are contiguous of size @em numPoints.
  !> The kernel operates on the rectangular interval specified by
  !> @em bufferInterval.
  !> The shape of the input and output arrays are
  !> specified by @em bufferSize, which is an @em numDim - dimensional
  !> array that specifies the size in each of @em numDim dimesions.
  !>
  !> @param numDim - const integer input specifies the number of dimensions of the input and output arrays
  !> @param numPoints - const 64-bit integer input specifies the total size of the input and output arrays
  !> @param bufferSize - const 64-bit integer array of size @em numDim specifies the size of the input and output arrays in each of @em numDim dimensions 
  !> @param bufferInterval - const 64-bit integer array of size 2 x @em numDim indicating the rectangular interval in which the kernel should operate; e.g. [ @em iStart @em iEnd @em jStart @em jEnd ]
  !> @param W - input const double precision contiguous array of size @em numPoints
  !> @param X - input const double precision contiguous array of size @em numPoints
  !> @param Y - input const double precision contiguous array of size @em numPoints
  !> @param Z - output double precision contiguous array of size @em numpoints
  SUBROUTINE ZWMXPY(numDim,numPoints,bufferSize,bufferInterval,W,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: W(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Z(I) = W(I)*(X(I)+Y(I))
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Z(bufferIndex) = W(bufferIndex)*(X(bufferIndex)+Y(bufferIndex))
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Z(bufferIndex) = W(bufferIndex)*(X(bufferIndex) + Y(bufferIndex))
             END DO
          END DO
       END DO
    ENDIF

  END SUBROUTINE ZWMXPY

  !> @brief ZXDOTY numComponents-vector inner product Z =  X * Y 
  SUBROUTINE ZXDOTY(numDim,numPoints,bufferSize,bufferInterval,numComponents,X,Y,Z)

    INTEGER(KIND=4), INTENT(IN)      :: numDim, numComponents
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints*numComponents)
    REAL(KIND=8),    INTENT(IN)      :: Y(numPoints*numComponents)
    REAL(KIND=8),    INTENT(OUT)     :: Z(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=4) :: iComp
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd, compOffset

    REAL(KIND=8)    :: zero = 0.0_8

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)
    
    CALL ASSIGNMENTXA(numDim,numPoints,bufferSize,bufferInterval, &
         zero,Z)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          DO iComp = 0, numComponents-1
             compOffset = iComp*numPoints
             Z(I) = Z(I) + X(I+compOffset)*Y(I+compOffset)
          END DO
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             DO iComp = 0,numComponents-1
                compOffset = iComp*numPoints
                
                bufferIndex = yIndex + I
                Z(bufferIndex) = Z(bufferIndex) + &
                     X(bufferIndex+compOffset)*Y(bufferIndex+compOffset)
             END DO
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                DO iComp = 0,numComponents-1
                   compOffset = iComp*numPoints
                   
                   bufferIndex = yzIndex + I
                   Z(bufferIndex) = Z(bufferIndex) + &
                        X(bufferIndex+compOffset)*Y(bufferIndex+compOffset)
                END DO
             END DO
          END DO
       END DO
    ENDIF
  END SUBROUTINE ZXDOTY

  !> @brief XAX point-wise operator performing X = aX (scalar a)
  SUBROUTINE XAX(numDim,numPoints,bufferSize,bufferInterval,a,X)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(INOUT)   :: X(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          X(I) = a*X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             X(bufferIndex) = a*X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                X(bufferIndex) = a*X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE XAX

  !> @brief ASSIGNMENTYX point-wise operator performing Y = X
  SUBROUTINE ASSIGNMENTYX(numDim,numPoints,bufferSize,bufferInterval,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints
    INTEGER(KIND=8), INTENT(IN)      :: bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE ASSIGNMENTYX

  !> @brief ASSIGNMENTXA point-wise operator performing X = scalar a
  SUBROUTINE ASSIGNMENTXA(numDim,numPoints,bufferSize,bufferInterval,a,X)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints
    INTEGER(KIND=8), INTENT(IN)      :: bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(INOUT)   :: X(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd


    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          X(I) = a
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             X(bufferIndex) = a
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                X(bufferIndex) = a
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE ASSIGNMENTXA

  !> @brief ASSIGNMENTYABSX point-wise operator performing X = scalar a
  SUBROUTINE ASSIGNMENTYABSX(numDim,numPoints,bufferSize,bufferInterval,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints
    INTEGER(KIND=8), INTENT(IN)      :: bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: X(numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd


    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = ABS(X(I))
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = ABS(X(bufferIndex))
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = ABS(X(bufferIndex))
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE ASSIGNMENTYABSX

  !> @brief VECLEN point-wise operator returning the length of a numComp-dimensional vector
  SUBROUTINE VECLEN(numDim,numPoints,bufferSize,bufferInterval,numComp,V,lenV)

    INTEGER(KIND=4), INTENT(IN)      :: numDim, numComp
    INTEGER(KIND=8), INTENT(IN)      :: numPoints
    INTEGER(KIND=8), INTENT(IN)      :: bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: V(numComp*numPoints)
    REAL(KIND=8),    INTENT(OUT)     :: lenV(numPoints)

    INTEGER(KIND=8) :: I, J, K, L
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd


    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          lenV(I) = 0.0_8
          DO L = 1, numComp
             lenV(I) = lenV(I) + V((L-1)*numPoints+I)*V((L-1)*numPoints+I)
          END DO
          lenV(I) = SQRT(lenV(I))
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             lenV(bufferIndex) = 0.0_8
             DO L = 1,numComp
                lenV(bufferIndex) = lenV(bufferIndex) +  &
                     V((L-1)*numPoints+bufferIndex)*V((L-1)*numPoints+bufferIndex)
             END DO
             lenV(bufferIndex) = SQRT(lenV(bufferIndex))
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                lenV(bufferIndex) = 0.0_8
                DO L = 1,numComp
                   lenV(bufferIndex) = lenV(bufferIndex) +  &
                        V((L-1)*numPoints+bufferIndex)*V((L-1)*numPoints+bufferIndex)
                END DO
                lenV(bufferIndex) = SQRT(lenV(bufferIndex))
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE VECLEN
  

  !> @brief YAXM1 point-wise operator performing Y = a/X (scalar a)
  SUBROUTINE YAXM1(numDim,numPoints,bufferSize,bufferInterval,a,X,Y)

    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(INOUT)   :: X(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = a/X(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             Y(bufferIndex) = a/X(bufferIndex)
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                Y(bufferIndex) = a/X(bufferIndex)
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE YAXM1


  !> @brief VASUPV point-wise operator performing V = V + aW where
  !>        a is a constant scalar, S is a scalar field, and V and W 
  !>        are vectors with numDim components.
  SUBROUTINE VASUPV(numDim,numPoints,bufferSize,bufferInterval,a,S,U,V)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: S(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: U(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: V(numDim*numPoints)

    INTEGER(KIND=4) :: iDim
    INTEGER(KIND=8) :: I, J, K,vectorIndex
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd
    REAL(KIND=8)    :: scalFac

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          V(I) = V(I) + a*S(I)*U(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             scalFac = a*S(bufferIndex)
             DO iDim = 1, numDim
                vectorIndex = (iDim-1)*numPoints+bufferIndex
                V(vectorIndex) = V(vectorIndex) + scalFac*U(vectorIndex)
             END DO
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                scalFac = a*S(bufferIndex)
                DO iDim = 1, numDim
                   vectorIndex = (iDim-1)*numPoints+bufferIndex
                   V(vectorIndex) = V(vectorIndex) + scalFac*U(vectorIndex)
                END DO
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE VASUPV
  
  !> @brief YASSMWDOTUPY point-wise operator performing Y = Y + a*(S1+S2)*(W dot U) where
  !>        W, and U are vector fields with numDim components, and a is
  !>        a constant scalar, and S1, S2, and Y are scalar fields
  SUBROUTINE YASSMWDOTUPY(numDim,numPoints,bufferSize,bufferInterval,a,S1,S2,W,U,Y)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: S1(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: S2(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: W(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)      :: U(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=4) :: iDim
    INTEGER(KIND=8) :: I, J, K,vectorIndex
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd
    REAL(KIND=8)    :: scalFac

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = Y(I) + a*(S1(I)+S2(I))*W(I)*U(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I  
             scalFac = a*(S1(bufferIndex)+S2(bufferIndex))
             DO iDim = 1, numDim
                vectorIndex = (iDim-1)*numPoints+bufferIndex
                Y(bufferIndex) = Y(bufferIndex) + scalFac*W(vectorIndex)*U(vectorIndex)
             END DO
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                scalFac = a*(S1(bufferIndex)+S2(bufferIndex))
                DO iDim = 1, numDim
                   vectorIndex = (iDim-1)*numPoints+bufferIndex
                   Y(bufferIndex) = Y(bufferIndex) + scalFac*W(vectorIndex)*U(vectorIndex)
                END DO
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE YASSMWDOTUPY
  

  !> @brief YASMWDOTUPY point-wise operator performing Y = Y + a*S*(W dot U) where
  !>        W, and U are vector fields with numDim components, and a is
  !>        a constant scalar, and S,Y are scalar fields
  SUBROUTINE YASMWDOTUPY(numDim,numPoints,bufferSize,bufferInterval,a,S,W,U,Y)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: S(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: W(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)      :: U(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=4) :: iDim
    INTEGER(KIND=8) :: I, J, K,vectorIndex
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd
    REAL(KIND=8)    :: scalFac

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = Y(I) + a*S(I)*W(I)*U(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I  
             scalFac = a*S(bufferIndex)
             DO iDim = 1, numDim
                vectorIndex = (iDim-1)*numPoints+bufferIndex
                Y(bufferIndex) = Y(bufferIndex) + scalFac*W(vectorIndex)*U(vectorIndex)
             END DO
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                scalFac = a*S(bufferIndex)
                DO iDim = 1, numDim
                   vectorIndex = (iDim-1)*numPoints+bufferIndex
                   Y(bufferIndex) = Y(bufferIndex) + scalFac*W(vectorIndex)*U(vectorIndex)
                END DO
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE YASMWDOTUPY

  !> @brief YAVDOTWPY point-wise operator performing Y = Y + a(V dot W) where
  !>        Y is a scalar field, a is a constant scalar, V and W are vector fields
  !>        with numDim components
  SUBROUTINE YAVDOTWPY(numDim,numPoints,bufferSize,bufferInterval,a,V,W,Y)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: a
    REAL(KIND=8),    INTENT(IN)      :: V(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)      :: W(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: Y(numPoints)

    INTEGER(KIND=4) :: iDim
    INTEGER(KIND=8) :: I, J, K,vectorIndex
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          Y(I) = Y(I) + a*V(I)*W(I)  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             DO iDim = 1, numDim
                vectorIndex = (iDim-1)*numPoints+bufferIndex
                Y(bufferIndex) = Y(bufferIndex) + a*V(vectorIndex)*W(vectorIndex)
             END DO
          END DO
       END DO
    ELSE IF(numDim == 3) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       nPlane = xSize * bufferSize(2)
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = zIndex + (J-1)*xSize
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                DO iDim = 1, numDim
                   vectorIndex = (iDim-1)*numPoints+bufferIndex
                   Y(bufferIndex) = Y(bufferIndex) + a*V(vectorIndex)*W(vectorIndex)
                END DO
             END DO
          END DO
       END DO
    ENDIF
    
  END SUBROUTINE YAVDOTWPY
  
END MODULE SIMPLE
