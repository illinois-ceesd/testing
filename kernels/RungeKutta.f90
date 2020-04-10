MODULE RungeKutta
  
  IMPLICIT NONE
  
CONTAINS

  ! @ICE block=rkSumHydra
  
  SUBROUTINE RK4SUM(numDim,numPoints,bufferSize,bufferInterval,h,K1,K2,K3,K4,stateData)
    
    INTEGER(KIND=4), INTENT(IN)      :: numDim
    INTEGER(KIND=8), INTENT(IN)      :: numPoints,bufferSize(numDim)
    INTEGER(KIND=8), INTENT(IN)      :: bufferInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)      :: h
    REAL(KIND=8),    INTENT(IN)      :: K1(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: k2(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: K3(numPoints)
    REAL(KIND=8),    INTENT(IN)      :: k4(numPoints)
    REAL(KIND=8),    INTENT(INOUT)   :: stateData(numPoints)
    
    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd
    REAL(KIND=8)    :: fac1,fac2
  
    fac1 = h/6.0_8
    fac2 = h/3.0_8
    
    iStart = bufferInterval(1)
    iEnd = bufferInterval(2)
    
    xSize = bufferSize(1)
    
    IF(numDim == 3) THEN
       
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       kStart = bufferInterval(5)
       kEnd   = bufferInterval(6)
       
       nPlane = xSize*bufferSize(2)
       
       DO K = kStart, kEnd
          zIndex = (K-1)*nPlane
          DO J = jStart, jEnd
             yzIndex = (J-1)*xSize + zIndex
             DO I = iStart, iEnd
                bufferIndex = yzIndex + I
                stateData(bufferIndex) = fac1*(K1(bufferIndex)+K4(bufferIndex)) + &
                     fac2*(K2(bufferIndex) + K3(bufferIndex)) + stateData(bufferIndex)
             END DO
          END DO
       END DO
       
    ELSE IF (numDim == 2) THEN
       
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             stateData(bufferIndex) = fac1*(K1(bufferIndex)+K4(bufferIndex)) + &
                  fac2*(K2(bufferIndex) + K3(bufferIndex)) + stateData(bufferIndex)
          END DO
       END DO
       
    ELSE IF (numDim == 1) THEN
       
       DO bufferIndex = iStart, iEnd
          stateData(bufferIndex) = fac1*(K1(bufferIndex)+K4(bufferIndex)) + &
               fac2*(K2(bufferIndex) + K3(bufferIndex)) + stateData(bufferIndex)
       END DO
       
    ENDIF
    
    
  END SUBROUTINE RK4SUM

  ! @ICE endblock
  
END MODULE RungeKutta
