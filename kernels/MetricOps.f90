MODULE MetricOps

  USE SIMPLE
  USE SPECIAL
  USE GRID

  IMPLICIT NONE

CONTAINS


  SUBROUTINE ALPHAWEIGHT(                          &
       numDim,numPointsBuffer,bufferSizes,opInterval, &
       gridType,gridMetric,alphaDir,alphaW)

    IMPLICIT NONE 

    INTEGER(KIND=4), INTENT(IN)         :: numDim, gridType, alphaDir
    INTEGER(KIND=8), INTENT(IN)         :: bufferSizes(numDim),numPointsBuffer
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: alphaW(numPointsBuffer)

    REAL(KIND=8)    :: gridScale
    INTEGER(KIND=8) :: metricOffset

    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr

    IF(gridType < RECTILINEAR) THEN

       gridScale = ABS(gridMetric(alphaDir))

       CALL ASSIGNMENTXA(numDim,numPointsBuffer,bufferSizes,opInterval, &
            gridScale,alphaW)

    ELSE IF(gridType == RECTILINEAR) THEN


       metricOffset =  (alphaDir-1)*numPointsBuffer
       metricPtr    => gridMetric(metricOffset+1:metricOffset+numPointsBuffer)

       CALL ASSIGNMENTYABSX(numDim,numPointsBuffer,bufferSizes,opInterval, &
            metricPtr,alphaW);

    ELSE ! Curvilinear

       metricOffset =  (alphaDir-1)*numDim*numPointsBuffer
       metricPtr    => gridMetric(metricOffset+1:metricOffset+(numDim*numPointsBuffer))

       CALL VECLEN(numDim,numPointsBuffer,bufferSizes,opInterval, &
            numDim,metricPtr,alphaW)

    END IF

  END SUBROUTINE ALPHAWEIGHT

  SUBROUTINE ALPHAWEIGHT2(                          &
       numDim,numPointsBuffer,bufferSizes,opInterval, &
       gridType,gridMetric,gridJacobian,alphaDir,alphaW)

    IMPLICIT NONE 

    INTEGER(KIND=4), INTENT(IN)         :: numDim, gridType, alphaDir
    INTEGER(KIND=8), INTENT(IN)         :: bufferSizes(numDim),numPointsBuffer
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridJacobian(numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: alphaW(numPointsBuffer)

    REAL(KIND=8)    :: gridScale
    INTEGER(KIND=8) :: metricOffset

    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: jacobianM1Ptr

    IF(gridType < RECTILINEAR) THEN

       gridScale = gridJacobian(1)*ABS(gridMetric(alphaDir))

       CALL ASSIGNMENTXA(numDim,numPointsBuffer,bufferSizes,opInterval, &
            gridScale,alphaW)

    ELSE IF(gridType == RECTILINEAR) THEN


       metricOffset  =  (alphaDir-1)*numPointsBuffer
       metricPtr     => gridMetric(metricOffset+1:metricOffset+numPointsBuffer)

       CALL ASSIGNMENTYABSX(numDim,numPointsBuffer,bufferSizes,opInterval, &
            metricPtr,alphaW);
       CALL YXY(numDim,numPointsBuffer,bufferSizes,opInterval, &
            gridJacobian,alphaW);

    ELSE ! Curvilinear

       metricOffset  =  (alphaDir-1)*numDim*numPointsBuffer
       metricPtr     => gridMetric(metricOffset+1:metricOffset+(numDim*numPointsBuffer))


       CALL VECLEN(numDim,numPointsBuffer,bufferSizes,opInterval, &
            numDim,metricPtr,alphaW)       
       CALL YXY(numDim,numPointsBuffer,bufferSizes,opInterval, &
            gridJacobian,alphaW);

    END IF

  END SUBROUTINE ALPHAWEIGHT2

  SUBROUTINE VHATCOMPONENT(                          &
       numDim,numPointsBuffer,bufferSizes,opInterval, &
       gridType,gridMetric,velDir,velocity,velHatComponent)

    IMPLICIT NONE 

    INTEGER(KIND=4), INTENT(IN)         :: numDim, gridType,velDir
    INTEGER(KIND=8), INTENT(IN)         :: bufferSizes(numDim),numPointsBuffer
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(IN), TARGET :: velocity(numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT)        :: velHatComponent(numPointsBuffer)


    INTEGER         :: iDim, jDim
    INTEGER(KIND=8) :: metricOffset, velOffset
    REAL(KIND=8)    :: gridScale

    REAL(KIND=8),    DIMENSION(:), POINTER :: velPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr

    ! Get vHat component for given grid type and direction
    !
    ! If grid < rectilinear,   vhat   = constant*V(idim)
    !                          metric = (constant_x, constant_y, constant_z)
    ! If grid == rectilinear,  vhat   = metric(idim)*V(idim)
    !                          metric = <M_x,M_y,M_z> [i.e. diagonal per point]
    ! If grid == curvilinear,  vhat   = Metric(idim)*V(idim)
    !                          metric = <Mx_i Mx_j Mx_k My_i ... Mz_k> [tensor per point]
    !
    !    DO iDim = 1,numDim 
    !       velPtr => velocity((iDim-1)*numPointsBuffer+1:(iDim-1)*numPointsBuffer+numPointsBuffer)
    !       WRITE(*,*) 'Velocity(',iDim,') = ',velPtr
    !    END DO

    IF(gridType < RECTILINEAR) THEN

       gridScale = gridMetric(velDir)
       velOffset = (velDir-1)*numPointsBuffer
       velPtr    => velocity(velOffset+1:velOffset+numPointsBuffer)

       CALL YAX(numDim,numPointsBuffer,bufferSizes,opInterval, &
            gridScale,velPtr,velHatComponent)

    ELSE IF(gridType == RECTILINEAR) THEN


       metricOffset =  (velDir-1)*numPointsBuffer
       metricPtr    => gridMetric(metricOffset+1:metricOffset+numPointsBuffer)
       velPtr       => velocity(metricOffset+1:metricOffset+numPointsBuffer)

       CALL ZXY(numDim,numPointsBuffer,bufferSizes,opInterval, &
            metricPtr,velPtr,velHatComponent);

    ELSE ! Curvilinear

       metricOffset =  (velDir-1)*numDim*numPointsBuffer
       metricPtr    => gridMetric(metricOffset+1:metricOffset+(numDim*numPointsBuffer))

       CALL ZXDOTY(numDim,numPointsBuffer,bufferSizes,opInterval, &
            numDim,metricPtr,velocity,velHatComponent);

    END IF

    !    WRITE(*,*) 'VelHat(',velDir,') = ',velHatComponent

  END SUBROUTINE VHATCOMPONENT


  SUBROUTINE IJKGRADTOXYZDIV(                         &
       numDim, numPoints, gridSizes, opInterval,      & 
       gridType, gridJacobian,gridMetric,gradVBuffer, & 
       divBuffer)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, gridType
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)         :: gridJacobian(numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: gradVBuffer(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(OUT)        :: divBuffer(numPoints)

    INTEGER         :: iDim, jDim
    INTEGER(KIND=8) :: metricOffset, gradOffset
    REAL(KIND=8)    :: gridScale

    REAL(KIND=8),    DIMENSION(:), POINTER :: gradPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr

    IF(gridType > RECTILINEAR) THEN ! full curvilinear
       DO iDim = 1,numDim
          DO jDim = 1,numDim
             gradOffset   =  ((iDim-1)*numDim+(jDim-1))*numPoints
             metricOffset =  ((iDim-1)+(jDim-1)*numDim)*numPoints
             metricPtr    => gridMetric(metricOffset+1:metricOffset+numPoints)
             gradPtr      => gradVBuffer(gradOffset+1:gradOffset+numPoints)
             CALL YWXPY(numDim,numPoints,gridSizes,opInterval, &
                  metricPtr,gradPtr,divBuffer)
          END DO
       END DO
       CALL YXY(numDim,numPoints,gridSizes,opInterval,gridJacobian,divBuffer)
    ELSE IF(gridType < RECTILINEAR) THEN ! uniform rectangular
       DO iDim = 1,numDim
          gridScale  = gridMetric(iDim)
          gradOffset = ((iDim-1)*(numDim+1))*numPoints
          gradPtr    => gradVBuffer(gradOffset+1:gradOffset+numPoints)
          CALL YAXPY(numDim,numPoints,gridSizes,opInterval, &
               gridScale,gradPtr, divBuffer)
       END DO
       CALL XAX(numDim,numPoints,gridSizes,opInterval, &
            gridJacobian(1),divBuffer)
    ELSE ! rectilinear (weirdo)
       DO iDim = 1,numDim
          metricOffset =  (iDim-1)*numPoints
          metricPtr    => gridMetric(metricOffset+1:metricOffset+numPoints)
          gradOffset   = ((iDim-1)*(numDim+1))*numPoints
          gradPtr      => gradVBuffer(gradOffset+1:gradOffset+numPoints)
          CALL YWXPY(numDim,numPoints,gridSizes,opInterval, &
               metricPtr,gradPtr,divBuffer)          
       END DO
       CALL YXY(numDim,numPoints,gridSizes,opInterval,gridJacobian,divBuffer)
    END IF

  END SUBROUTINE IJKGRADTOXYZDIV

  !> @brief Converts Cartesian (computational) gradient to physical coordinates
  SUBROUTINE GRADIJKTOGRADXYZ(                             &
       numDim, numPoints, gridSizes, opInterval, gridType, &
       gridJacobian,gridMetric,gradIJK, gradXYZ)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, gridType
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN)         :: gridJacobian(numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: gradIJK(numDim*numPoints)
    REAL(KIND=8),    INTENT(OUT),TARGET :: gradXYZ(numDim*numPoints)

    INTEGER         :: iDim, jDim
    INTEGER(KIND=8) :: metricOffset, gradIJKOffset, gradXYZOffset
    REAL(KIND=8)    :: gridScale
    REAL(KIND=8)    :: zero

    REAL(KIND=8),    DIMENSION(:), POINTER :: gradIJKPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: gradXYZPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr

    zero = 0.0_8

    IF(gridType > RECTILINEAR) THEN ! full curvilinear

       ! Note: only need to zero for curvilinear, other metric types
       ! utilize assignment-type kernels
       DO iDim = 1,numDim
          gradXYZOffset =  (iDim-1)*numPoints
          gradXYZPtr    => gradXYZ(gradXYZOffset+1:gradXYZOffset+numPoints)
          CALL ASSIGNMENTXA(numDim,numPoints,gridSizes,opInterval,zero,gradXYZPtr)
       END DO

       DO iDim = 1,numDim
          gradXYZOffset   =  (iDim-1)*numPoints
          gradXYZPtr      => gradXYZ(gradXYZOffset+1:gradXYZOffset+numPoints)
          DO jDim = 1,numDim
             gradIJKOffset   =  (jDim-1)*numPoints
             gradIJKPtr      => gradIJK(gradIJKOffset+1:gradIJKOffset+numPoints)
             metricOffset    =  ((jDim-1)*numDim+(iDim-1))*numPoints
             metricPtr       => gridMetric(metricOffset+1:metricOffset+numPoints)
             CALL YWXPY(numDim,numPoints,gridSizes,opInterval, &
                  metricPtr,gradIJKPtr,gradXYZPtr)
          END DO
          CALL YXY(numDim,numPoints,gridSizes,opInterval,gridJacobian,gradXYZPtr)
       END DO

    ELSE IF(gridType < RECTILINEAR) THEN ! uniform rectangular
       DO iDim = 1,numDim
          gridScale     = gridMetric(iDim)*gridJacobian(1)
          gradIJKOffset = (iDim-1)*numPoints
          gradIJKPtr    => gradIJK(gradIJKOffset+1:gradIJKOffset+numPoints)
          gradXYZPtr    => gradXYZ(gradIJKOffset+1:gradIJKOffset+numPoints)
          CALL YAX(numDim,numPoints,gridSizes,opInterval, &
               gridScale,gradIJKPtr, gradXYZPtr)
       END DO
    ELSE ! rectilinear (weirdo)
       DO iDim = 1,numDim
          metricOffset =  (iDim-1)*numPoints
          metricPtr    => gridMetric(metricOffset+1:metricOffset+numPoints)
          gradIJKPtr   => gradIJK(metricOffset+1:metricOffset+numPoints)
          gradXYZPtr   => gradXYZ(metricOffset+1:metricOffset+numPoints)
          CALL ZXY(numDim,numPoints,gridSizes,opInterval, &
               metricPtr,gradIJKPtr,gradXYZPtr)          
          CALL YXY(numDim,numPoints,gridSizes,opInterval,gridJacobian,gradXYZPtr)
       END DO
    END IF

  END SUBROUTINE GRADIJKTOGRADXYZ

  

  ! Grab a full (numDim*numDim) metric tensor for a point
  SUBROUTINE GetPointMetric(numDim,numPointsBuffer,pointID,gridType,gridMetric,pointMetric)
    
    USE Grid
    
    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT(IN)  :: numDim,gridType
    INTEGER(KIND=8), INTENT(IN)  :: pointID,numPointsBuffer
    REAL(KIND=8),    INTENT(IN)  :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT) :: pointMetric(numDim*numDim)
    
    
    INTEGER(KIND=4) :: iDim, dimID, iDir
    INTEGER(KIND=8) :: bufferOffset,bufferIndex
    
    pointMetric(:) = 0.0_8
    
    IF(gridType == CURVILINEAR) THEN
       DO iDim = 1,numDim
          bufferOffset = (iDim-1)*numDim*numPointsBuffer + pointID        
          DO iDir = 1,numDim
             bufferIndex = bufferOffset + (iDir-1)*numPointsBuffer
             pointMetric((iDim-1)*numDim + iDir) = gridMetric(bufferIndex)
          END DO
       END DO
    ELSE IF(gridType == RECTILINEAR) THEN
       DO iDim = 1, numDim
          bufferOffset = (iDim-1)*numPointsBuffer + pointID
          pointMetric((iDim-1)*numDim + iDim) = gridMetric(bufferOffset)
       END DO
    ELSE ! Cartesian/Uniform rectangular
       DO iDim = 1, numDim
          pointMetric((iDim-1)*numDim + iDim) = gridMetric(iDim)
       END DO
    ENDIF
    
  END SUBROUTINE GetPointMetric
  
  ! Returns a version of the metric with permuted rows such that the first row is that corresponding to the 
  ! boundary-normal direction, and the other rows are assigned according to cyclic permutation
  SUBROUTINE BoundaryPointMetric(numDim,normDir,numPointsBuffer,pointID,gridType,gridMetric,pointMetric)

    USE Grid

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim,normDir,gridType
    INTEGER(KIND=8), INTENT(IN)  :: pointID,numPointsBuffer
    REAL(KIND=8),    INTENT(IN)  :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT) :: pointMetric(numDim*numDim)


    INTEGER(KIND=4) :: iDim, dimID, iDir
    INTEGER(KIND=8) :: bufferOffset,bufferIndex

    pointMetric(:) = 0.0_8

    IF(gridType == CURVILINEAR) THEN
       bufferOffset = (normDir-1)*numDim*numPointsBuffer + pointID
       DO iDim = 1,numDim
          bufferIndex = bufferOffset + (iDim-1)*numPointsBuffer
          pointMetric(iDim) = gridMetric(bufferIndex)
       END DO
       DO iDim = 1,numDim-1
          dimID = normDir + iDim
          IF(dimID > numDim) dimID = dimID - numDim
          bufferOffset = (dimID-1)*numDim*numPointsBuffer + pointID        
          DO iDir = 1,numDim
             bufferIndex = bufferOffset + (iDir-1)*numPointsBuffer
             pointMetric(iDim*numDim + iDir) = gridMetric(bufferIndex)
          END DO
       END DO
    ELSE IF(gridType == RECTILINEAR) THEN
       bufferOffset = (normDir-1)*numPointsBuffer + pointID
       pointMetric(normDir) = gridMetric(bufferOffset)
       DO iDim = 1, numDim-1
          dimID = normDir + iDim
          IF(dimID > numDim) dimID = dimID - numDim
          bufferOffset = (dimID-1)*numPointsBuffer + pointID
          pointMetric(iDim*numDim + dimID) = gridMetric(bufferOffset)
       END DO
    ELSE ! Cartesian/Uniform rectangular
       pointMetric(normDir) = gridMetric(normDir)
       DO iDim = 1, numDim-1
          dimID = normDir + iDim
          IF(dimID > numDim) dimID = dimID - numDim
          pointMetric(iDim*numDim + dimID) = gridMetric(dimID)
       END DO
    ENDIF
    
  END SUBROUTINE BoundaryPointMetric


  SUBROUTINE PointEigenVectors(numDim,pointMetric,eigenVectors,metricMags)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim 
    REAL(KIND=8),    INTENT(IN)  :: pointMetric(numDim*numDim)
    REAL(KIND=8),    INTENT(OUT) :: eigenVectors(numDim*numDim)
    REAL(KIND=8),    INTENT(OUT) :: metricMags(numDim)

    REAL(KIND=8) :: ja
    INTEGER(KIND=4) :: iDim,dimDir,bb

    DO iDim = 1,numDim
       bb  = (iDim-1)*numDim 
       metricMags(iDim) = 0.0_8
       DO dimDir = 1,numDim
          metricMags(iDim) = metricMags(iDim) + pointMetric(bb+dimDir)*pointMetric(bb+dimDir)
       END DO
       metricMags(iDim) = SQRT(metricMags(iDim))
       ja  = 1.0_8/metricMags(iDim)
       DO dimDir = 1,numDim
          eigenVectors(bb+dimDir) = pointMetric(bb+dimDir)*ja
       END DO
    END DO

  END SUBROUTINE PointEigenVectors

  REAL(KIND=8) FUNCTION VDot(numDim,u,v)

    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT(IN) :: numDim
    REAL(KIND=8), INTENT(IN)    :: u(numDim),v(numDim)

    INTEGER(KIND=4) :: iDir
    REAL(KIND=8)    :: dotProduct
    
    dotProduct = 0.0_8

    DO iDir = 1,numDim
       dotProduct = dotProduct + u(iDir)*v(iDir)
    END DO

    VDot = dotProduct

  END FUNCTION VDot

  SUBROUTINE PointEigenVectorsGS(numDim,pointMetric,eigenVectors,eigenComponents)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim 
    REAL(KIND=8),    INTENT(IN)  :: pointMetric(numDim*numDim)
    REAL(KIND=8),    INTENT(OUT) :: eigenVectors(numDim*numDim)
    REAL(KIND=8),    INTENT(OUT) :: eigenComponents(numDim*numDim)

    REAL(KIND=8)    :: vecMag,zeroTol
    INTEGER(KIND=4) :: iDim,dimDir,bb,dirStart,rowStart,iVec

    zeroTol            = 1.0E-24_8
    eigenComponents(:) = 0.0_8
    vecMag             = 0.0_8

    DO iDim = 1,numDim
       rowStart  = (iDim-1)*numDim 
       DO iVec = 1, numDim
          eigenVectors(rowStart+iVec) = pointMetric(rowStart+iVec)
       END DO
       DO dimDir = 1,iDim-1
          dirStart = (dimDir-1)*numDim
          eigenComponents(rowStart+dimDir) = VDot(numDim,eigenVectors(dirStart+1),eigenVectors(rowStart+1))
          DO iVec = 1, numDim
             eigenVectors(rowStart+iVec) = eigenVectors(rowStart+iVec) - &
                  eigenComponents(rowStart+dimDir)*eigenVectors(dirStart+iVec)
          END DO
       END DO
       vecMag = 0.0_8
       DO iVec = 1, numDim
          vecMag = vecMag + eigenVectors(rowStart+iVec)*eigenVectors(rowStart+iVec) 
       END DO
       IF(vecMag < zeroTol) RETURN
       eigenComponents(rowStart+iDim) = SQRT(vecMag)
       vecMag = 1.0_8/eigenComponents(rowStart+iDim)
       DO iVec = 1, numDim
          eigenVectors(rowStart+iVec) = eigenVectors(rowStart+iVec)*vecMag
       END DO
    END DO

  END SUBROUTINE PointEigenVectorsGS

END MODULE MetricOps
