MODULE NSUTIL

  IMPLICIT NONE

CONTAINS

  SUBROUTINE GetPointState(numDim,numScalar,numPointsBuffer, &
       rhoBuffer,rhoVBuffer,rhoEBuffer,scalarBuffer,pointID, pointState)
    
    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT(IN) :: numDim, numScalar
    INTEGER(KIND=8), INTENT(IN) :: pointID, numPointsBuffer
    REAL(KIND=8),    INTENT(IN) :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN) :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN) :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8),    INTENT(IN) :: scalarBuffer(numScalar*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT):: pointState(numDim+2+numScalar)
    
    !-----local variables-------
    INTEGER(KIND=4) :: iDim
    
    pointState(1) = rhoBuffer(pointID)
    DO iDim = 1, numDim
       pointState(iDim+1) = rhoVBuffer((iDim-1)*numPointsBuffer+pointID) 
    ENDDO
    pointState(numDim+2) = rhoEBuffer(pointID)
    DO iDim = 1, numScalar
       pointState(numDim+2+iDim) = scalarBuffer((iDim-1)*numPointsBuffer+pointID) 
    ENDDO
    
    RETURN
    
  END SUBROUTINE GetPointState

END MODULE NSUTIL
