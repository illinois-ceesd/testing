MODULE SPECIAL

  IMPLICIT NONE

  CONTAINS

  !> @brief Computes determinant of 3x3 matrix
  SUBROUTINE DETERMINANT3X3(numPoints,bufferSize,bufferInterval,inMatrix,matrixDeterminant)
    
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(3)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(6)
    REAL(KIND=8),    INTENT(IN)      :: inMatrix(9*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: matrixDeterminant(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)
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

             matrixDeterminant(bufferIndex) =                &
                  inMatrix(bufferIndex)*                     &
                  (inMatrix(bufferIndex+8*numPoints) * &
                  inMatrix(bufferIndex+4*numPoints)-   &
                  inMatrix(bufferIndex+7*numPoints)*   &
                  inMatrix(bufferIndex+5*numPoints))-  &

                  inMatrix(bufferIndex+3*numPoints)*   &
                  (inMatrix(bufferIndex+8*numPoints)*  &
                  inMatrix(bufferIndex+numPoints) -    &
                  inMatrix(bufferIndex+7*numPoints) *  &
                  inMatrix(bufferIndex+2*numPoints)) + &

                  inMatrix(bufferIndex+6*numPoints)*   &
                  (inMatrix(bufferIndex+5*numPoints)*  &
                  inMatrix(bufferIndex+numPoints)-     &
                  inMatrix(bufferIndex+4*numPoints)*   &
                  inMatrix(bufferIndex+2*numPoints))

          END DO
       END DO
    END DO
  END SUBROUTINE DETERMINANT3X3

  !> @brief Computes determinant of 2x2 matrix
  SUBROUTINE DETERMINANT2X2(numPoints,bufferSize,bufferInterval,inMatrix,matrixDeterminant)
    
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(2)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(4)
    REAL(KIND=8),    INTENT(IN)      :: inMatrix(4*numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: matrixDeterminant(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)
    jStart = bufferInterval(3)
    jEnd   = bufferInterval(4)
    nPlane = xSize * bufferSize(2)

    DO J = jStart, jEnd
       yIndex = (J-1)*xSize
       DO I = iStart, iEnd
          bufferIndex = yIndex + I
          
          matrixDeterminant(bufferIndex) =                &
               (inMatrix(bufferIndex)*                    &
               inMatrix(bufferIndex+3*numPoints)) -       &
               (inMatrix(bufferIndex+numPoints)*          &
               inMatrix(bufferIndex+2*numPoints))
       END DO
    END DO

  END SUBROUTINE DETERMINANT2X2

  !> @brief Computes buf1*buf4 - buf2*buf3 + buf7*(buf5 - buf6)
  SUBROUTINE METRICSUM4(numDim,numPoints,bufferSize,bufferInterval,&
       buf1,buf2,buf3,buf4,buf5,buf6,buf7,metricSum)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: buf1(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf2(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf3(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf4(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf5(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf6(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: buf7(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: metricSum(numPoints)

    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)
    
    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          metricSum(I) = buf1(I)*buf4(I) - buf2(I)*buf3(I) + buf7(I)*(buf5(I)-buf6(I))
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             metricSum(bufferIndex) = buf1(bufferIndex)*buf4(bufferIndex) - &
                  buf2(bufferIndex)*buf3(bufferIndex) + &
                  buf7(bufferIndex)*(buf5(bufferIndex)-buf6(bufferIndex))
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
                metricSum(bufferIndex) = buf1(bufferIndex)*buf4(bufferIndex) - &
                     buf2(bufferIndex)*buf3(bufferIndex) + &
                     buf7(bufferIndex)*(buf5(bufferIndex)-buf6(bufferIndex))
             END DO
          END DO
       END DO
    ENDIF
  END SUBROUTINE METRICSUM4

  SUBROUTINE VECTORCROSSPRODUCT(v1,v2,y)

    IMPLICIT NONE

    REAL(KIND=8), INTENT(IN)  :: v1(3), v2(3)
    REAL(KIND=8), INTENT(OUT) :: y(3)

    y(1) = v1(2)*v2(3) - v1(3)*v2(2)
    y(2) = v1(3)*v2(1) - v1(1)*v2(3)
    y(3) = v1(1)*v2(2) - v1(2)*v2(1)
    
  END SUBROUTINE VECTORCROSSPRODUCT

END MODULE SPECIAL
