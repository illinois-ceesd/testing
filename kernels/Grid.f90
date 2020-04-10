MODULE Grid

  IMPLICIT NONE

  INTEGER(KIND=4), PARAMETER :: CARTESIAN   = 0
  INTEGER(KIND=4), PARAMETER :: UNIRECT     = 1
  INTEGER(KIND=4), PARAMETER :: RECTILINEAR = 2
  INTEGER(KIND=4), PARAMETER :: CURVILINEAR = 3

CONTAINS

  SUBROUTINE ComputeGridMetrics( &
       numDim,gridSizes,numPoints,          &
       opInterval, numStencils,             &
       numStencilValues, stencilSizes,      &   
       stencilStarts, stencilOffsets,       &
       stencilWeights,stencilID,            &
       gridCoordinates, gridMetricTensor,   &
       gridJacobianDeterminants)

    USE OPERATORS 

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)            :: numDim
    INTEGER(KIND=8), INTENT(IN)            :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)            :: opInterval(2*numDim) 
    INTEGER(KIND=4), INTENT(IN)            :: numStencils,numStencilValues
    INTEGER(KIND=4), INTENT(IN)            :: stencilSizes(numStencils)
    INTEGER(KIND=4), INTENT(IN)            :: stencilStarts(numStencils)
    INTEGER(KIND=4), INTENT(IN)            :: stencilOffsets(numStencilValues)
    INTEGER(KIND=4), INTENT(IN),    TARGET :: stencilID(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)            :: stencilWeights(numStencilValues)
    REAL(KIND=8),    INTENT(IN),    TARGET :: gridCoordinates(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: gridMetricTensor(numDim*numDim*numPoints)       
    REAL(KIND=8),    INTENT(INOUT), TARGET :: gridJacobianDeterminants(2*numPoints)      
    
    REAL(KIND=8),    DIMENSION(:), ALLOCATABLE :: dCoords
    REAL(KIND=8),    DIMENSION(:), POINTER     :: jacPtr, jacM1Ptr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: gridMetricTensorPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: coordPtr
    INTEGER(KIND=4), DIMENSION(:), POINTER     :: stencilConnPtr

    INTEGER(KIND=4) :: numComponents, iDim

  END SUBROUTINE ComputeGridMetrics

  ! NOT THREADED
  SUBROUTINE ComputeCurvilinearGridMetrics( &
       numDim,gridSizes,numPoints,          &
       opInterval, numStencils,             &
       numStencilValues, stencilSizes,      &   
       stencilStarts, stencilOffsets,       &
       stencilWeights,stencilID,            &
       gridCoordinates, gridMetricTensor,   &
       gridJacobianDeterminants)
    
    USE OPERATORS 
    USE SIMPLE
    USE SPECIAL

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)            :: numDim
    INTEGER(KIND=8), INTENT(IN)            :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)            :: opInterval(2*numDim) 
    INTEGER(KIND=4), INTENT(IN)            :: numStencils,numStencilValues
    INTEGER(KIND=4), INTENT(IN)            :: stencilSizes(numStencils)
    INTEGER(KIND=4), INTENT(IN)            :: stencilStarts(numStencils)
    INTEGER(KIND=4), INTENT(IN)            :: stencilOffsets(numStencilValues)
    INTEGER(KIND=4), INTENT(IN),    TARGET :: stencilID(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)            :: stencilWeights(numStencilValues)
    REAL(KIND=8),    INTENT(IN),    TARGET :: gridCoordinates(numDim*numPoints)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: gridMetricTensor(numDim*numDim*numPoints)       
    REAL(KIND=8),    INTENT(INOUT), TARGET :: gridJacobianDeterminants(2*numPoints)      
    
    REAL(KIND=8),    DIMENSION(:), ALLOCATABLE :: dCoords
    REAL(KIND=8),    DIMENSION(:), POINTER     :: jacPtr, jacM1Ptr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: m1Ptr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: coordPtr
    INTEGER(KIND=4), DIMENSION(:), POINTER     :: stencilConnPtr

    INTEGER(KIND=4) :: numComponents, iDim
    INTEGER(KIND=8) :: offSet

    ALLOCATE(dCoords(numDim*numPoints))

    numComponents = 1
    iDim = 1
    jacM1Ptr => gridJacobianDeterminants(numPoints+1:2*numPoints)
    jacPtr => gridJacobianDeterminants(1:numPoints)


    IF(numDim == 1) THEN
       CALL APPLYOPERATOR(numDim, gridSizes, numComponents, numPoints, iDim, opInterval, &
            numStencils, stencilSizes, stencilStarts, numStencilValues,                  &
            stencilWeights, stencilOffsets, stencilID, gridCoordinates, jacPtr)
       
       ! Y = a/X
       CALL YAXM1(numDim,numPoints,gridSizes,opInterval,1.0_8,jacM1Ptr,jacPtr)
       
       ! X = a
       CALL ASSIGNMENTXA(numDim,numPoints,gridSizes,opInterval,1.0_8,gridMetricTensor)

   ELSE IF(numDim == 2) THEN


      iDim = 2 ! d(xi)/dx = dy/d(eta)
      coordPtr       => gridCoordinates(numPoints+1:numPoints+numPoints)
      m1Ptr          => gridMetricTensor(1:numPoints)
      jacPtr         => gridJacobianDeterminants(1:numPoints)
      jacM1Ptr       => gridJacobianDeterminants(1:numPoints)
      stencilConnPtr => stencilID(numPoints+1:numPoints+numPoints)

      ! calculate 
      CALL APPLYOPERATOR(numDim,gridSizes,numComponents,numPoints,iDim,opInterval, &
           numStencils,stencilSizes,stencilStarts,numStencilValues,                &
           stencilWeights, stencilOffsets,stencilConnPtr,coordPtr,jacM1Ptr)

      ! 1/J = 1/dy
      CALL ASSIGNMENTYX(numDim,numPoints,gridSizes,opInterval,jacM1Ptr,m1Ptr)
      
      iDim = 1 ! d(eta)/dy = dx/d(xi)
      offSet         = 3*numPoints
      coordPtr       => gridCoordinates(1:numPoints)
      m1Ptr          => gridMetricTensor(offSet+1:offSet+numPoints)
      jacPtr         => gridJacobianDeterminants(1:numPoints)
      jacM1Ptr       => gridJacobianDeterminants(1:numPoints)
      stencilConnPtr => stencilID(1:numPoints)

      ! calculate 1/dx
      CALL APPLYOPERATOR(numDim,gridSizes,numComponents,numPoints,iDim,opInterval, &
           numStencils,stencilSizes,stencilStarts,numStencilValues,                &
           stencilWeights, stencilOffsets,stencilConnPtr,coordPtr,m1Ptr)

      ! 1/J = 1/dy * 1/dx
!      CALL YXY(numDim,numPoints,gridSizes,opInterval,jacM1Ptr,m1Ptr)
      
      iDim = 2 ! d(xi)/dy = -dx/d(eta)
      offSet        =  numPoints
      coordPtr      => gridCoordinates(numPoints+1:numPoints+numPoints)
      m1Ptr         => gridMetricTensor(offSet+1:offSet+numPoints)
      jacM1Ptr      => gridJacobianDeterminants(1:numPoints)
      
      ! calculate 1/dx
      CALL APPLYOPERATOR(numDim,gridSizes,numComponents,numPoints,iDim,opInterval, &
           numStencils,stencilSizes,stencilStarts,numStencilValues,                &
           stencilWeights, stencilOffsets,stencilConnPtr,coordPtr,m1Ptr)
      
      iDim = 1 ! d(eta)/dx = -dy/d(xi)

      offSet        =  numPoints
      coordPtr      => gridCoordinates(numPoints+1:numPoints+numPoints)
      m1Ptr         => gridMetricTensor(offSet+1:offSet+numPoints)
      jacM1Ptr      => gridJacobianDeterminants(1:numPoints)

      ! calculate 1/dx
      CALL APPLYOPERATOR(numDim,gridSizes,numComponents,numPoints,iDim,opInterval, &
           numStencils,stencilSizes,stencilStarts,numStencilValues,                &
           stencilWeights, stencilOffsets,stencilConnPtr,coordPtr,m1Ptr)

   ELSE IF(numDim == 3) THEN
   END IF
   
   DEALLOCATE(dCoords)

  END SUBROUTINE ComputeCurvilinearGridMetrics

  SUBROUTINE ApplyUniformGridMetric( &
       numDim, gridSizes, numPoints, &
       opInterval, gridMetric, vBuffer,vHat)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim
    INTEGER(KIND=8), INTENT(IN)  :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)  :: opInterval(2*numDim) 
    REAL(KIND=8),    INTENT(IN)  :: gridMetric(numDim)
    REAL(KIND=8),    INTENT(IN)  :: vBuffer(numDim*numPoints)
    REAL(KIND=8),    INTENT(OUT) :: vHat(numDim*numPoints)
    
    
    INTEGER         :: iDim
    INTEGER(KIND=8) :: iPoint,iX,iY,iZ,iStart,iEnd,jStart,jEnd,kStart,kEnd
    INTEGER(KIND=8) :: xIndex,zIndex,yIndex,yzIndex,xSize,ySize,zSize
    INTEGER(KIND=8) :: iPoint2, iPoint3, nPlane, numPoints2, numPoints3
    REAL(KIND=8)    :: xMetric,yMetric,zMetric
    
    xSize   = gridSizes(1)
    iStart  = opInterval(1)
    iEnd    = opInterval(2)
    xMetric = gridMetric(1)


    IF( numDim == 1 ) THEN

       DO iX = iStart, iEnd
          vHat(iX) = xMetric*vBuffer(iX)
       END DO

    ELSE IF( numDim == 2 ) THEN

       yMetric = gridMetric(2)
       ySize = gridSizes(2)
       jStart = opInterval(3)
       jEnd   = opInterval(4)

       DO iY = jStart,jEnd
          yIndex = (iY-1)*xSize
          DO iX = iStart,iEnd
             iPoint = yIndex + iX
             iPoint2 = iPoint+numPoints
             vHat(iPoint)  = xMetric  * vBuffer(iPoint)
             vHat(iPoint2) = yMetric  * vBuffer(iPoint2)
          END DO
       END DO

    ELSE IF( numDim == 3 ) THEN

       yMetric = gridMetric(2) 
       ySize   = gridSizes(2)
       jStart  = opInterval(3)
       jEnd    = opInterval(4)

       zMetric = gridMetric(3)
       zSize   = gridSizes(3)
       kStart  = opInterval(5)
       kEnd    = opInterval(6)

       nPlane     = xSize  * ySize
       numPoints2 = 2*numPoints

       DO iZ = kStart, kEnd
          zIndex = (iZ-1)*nPlane
          DO iY = jStart,jEnd
             yzIndex = zIndex + (iY-1)*xSize
             DO iX = iStart, iEnd
                iPoint  = yzIndex + iX
                iPoint2 = iPoint + numPoints
                iPoint3 = iPoint + numPoints2
                vHat(iPoint)  = xMetric * vBuffer(iPoint)
                vHat(iPoint2) = yMetric * vBuffer(iPoint2)
                vHat(iPoint3) = zMetric * vBuffer(iPoint3)
             END DO
          END DO
       END DO
    END IF

  END SUBROUTINE ApplyUniformGridMetric


  SUBROUTINE ApplyCartesianGridMetric( &
       numDim, gridSizes, numPoints,   &
       opInterval, gridMetric, vBuffer, vHat)
    
    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT(IN)  :: numDim
    INTEGER(KIND=8), INTENT(IN)  :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)  :: opInterval(2*numDim) 
    REAL(KIND=8),    INTENT(IN)  :: gridMetric(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)  :: vBuffer(numDim*numPoints)
    REAL(KIND=8),    INTENT(OUT) :: vHat(numDim*numPoints)
    
    
    INTEGER         :: iDim
    INTEGER(KIND=8) :: iPoint,iX,iY,iZ,iStart,iEnd,jStart,jEnd,kStart,kEnd
    INTEGER(KIND=8) :: xIndex,zIndex,yIndex,yzIndex,xSize,ySize,zSize
    INTEGER(KIND=8) :: iPoint2, iPoint3, nPlane, numPoints2, numPoints3
    REAL(KIND=8)    :: xMetric,yMetric,zMetric

    xSize  = gridSizes(1)
    iStart = opInterval(1)
    iEnd   = opInterval(2)

    IF( numDim == 1 ) THEN

       DO iX = iStart, iEnd
          vHat(iX) = gridMetric(iX)*vBuffer(iPoint)
       END DO

    ELSE IF( numDim == 2 ) THEN
       
       ySize = gridSizes(2)
       jStart = opInterval(3)
       jEnd   = opInterval(4)
       
       DO iY = jStart,jEnd
          yIndex = (iY-1)*xSize
          DO iX = iStart,iEnd
             iPoint = yIndex + iX
             iPoint2 = iPoint+numPoints
             vHat(iPoint)  = gridMetric(iPoint)  * vBuffer(iPoint)
             vHat(iPoint2) = gridMetric(iPoint2) * vBuffer(iPoint2)
          END DO
       END DO

    ELSE IF( numDim == 3 ) THEN

       ySize  = gridSizes(2)
       jStart = opInterval(3)
       jEnd   = opInterval(4)

       zSize  = gridSizes(3)
       kStart = opInterval(5)
       kEnd   = opInterval(6)

       nPlane     = xSize  * ySize
       numPoints2 = 2*numPoints

       DO iZ = kStart, kEnd
          zIndex = (iZ-1)*nPlane
          DO iY = jStart,jEnd
             yzIndex = zIndex + (iY-1)*xSize
             DO iX = iStart, iEnd
                iPoint = yzIndex + iX
                iPoint2 = iPoint + numPoints
                iPoint3 = iPoint + numPoints2
                vHat(iPoint)  = gridMetric(iPoint)  * vBuffer(iPoint)
                vHat(iPoint2) = gridMetric(iPoint2) * vBuffer(iPoint2)
                vHat(iPoint3) = gridMetric(iPoint3) * vBuffer(iPoint3)
             END DO
          END DO
       END DO
    END IF

  END SUBROUTINE ApplyCartesianGridMetric

END MODULE Grid
