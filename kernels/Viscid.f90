MODULE Viscid

  USE SIMPLE
  USE SPECIAL
  USE GRID

  IMPLICIT NONE

CONTAINS


  !>
  !! @brief Compute the curvilinear cartesian viscous fluxes in 1 dimension
  !!
  !! retains logic to simplify calculations on uniform or stretched cartesian grids
  !!
  !! See /ref conserve for theory
  !!
  SUBROUTINE StrongFlux1D                                            &
       (numDim, fluxDir, gridSizes, numPoints, opInterval, gridType, & 
       gridMetric, tauBuffer, energyBuffer, fluxBuffer)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, fluxDir, gridType
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: tauBuffer(numPoints*numDim*(numDim+1)/2)
    REAL(KIND=8),    INTENT(IN), TARGET :: energyBuffer(numPoints*numDim)
    REAL(KIND=8),    INTENT(OUT),TARGET :: fluxBuffer(numPoints*(numDim+2))

    INTEGER         :: iDim, tensorIndex, iVel
    INTEGER(KIND=8) :: fluxOffset, tensorOffset, dirOffset, metricOffset
    INTEGER(KIND=8) :: velIndex, fluxIndex, velOffset, heatOffset, dimOffset
    REAL(KIND=8)    :: gridScale
    REAL(KIND=8)    :: minusOne

    REAL(KIND=8),    DIMENSION(:), POINTER :: bufPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: fluxPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: tauPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: energyPtr

    INTEGER, DIMENSION(2,2) :: index2D
    INTEGER, DIMENSION(3,3) :: index3D

    ! Map 0-based index for (2,3)-dimensional symmetric tensor
    index2D = RESHAPE((/ 0, 1, 1, 2 /), SHAPE(index2D))
    index3D = RESHAPE((/ 0, 1, 2, 1, 3, 4, 2, 4, 5 /), SHAPE(index3D))

    dirOffset = (fluxDir-1)*numPoints
    fluxOffset = 0

    IF(gridType < RECTILINEAR) THEN

       gridScale  =  gridMetric(fluxDir)
       
       ! Momentum terms 
       DO iDim = 1, numDim
          
          IF(numDim == 2) THEN
             tensorIndex = index2D(fluxDir,iDim)
          ELSE
             tensorIndex = index3D(fluxDir,iDim)
          ENDIF
          
          tensorOffset =  tensorIndex*numPoints
          fluxOffset   =  fluxOffset + numPoints
          fluxPtr      => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          tauPtr       => tauBuffer(tensorOffset+1:tensorOffset+numPoints)

          ! Grid metric is scalar and constant, this gets flux = metric * tau_(iDim)(
          CALL YAX(numDim,numPoints,gridSizes,opInterval,gridScale,tauPtr,fluxPtr)       

       END DO
       
       ! Energy
       fluxOffset  =  fluxOffset + numPoints
       fluxPtr     => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)  
       energyPtr   => energyBuffer(dirOffset+1:dirOffset+numPoints)

       CALL YAX(numDim,numPoints,gridSizes,opInterval,gridScale,energyPtr,fluxPtr)

    ELSE IF (gridType == RECTILINEAR) THEN
       
       metricPtr    => gridMetric(dirOffset+1:dirOffset+numPoints)

       ! Momentum terms 
       DO iDim = 1, numDim
          
          IF(numDim == 2) THEN
             tensorIndex = index2D(fluxDir,iDim)
          ELSE
             tensorIndex = index3D(fluxDir,iDim)
          ENDIF
          
          tensorOffset =  tensorIndex*numPoints
          fluxOffset   =  fluxOffset + numPoints
          fluxPtr      => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          tauPtr       => tauBuffer(tensorOffset+1:tensorOffset+numPoints)

          ! Grid metric is scalar, this gets flux = metric * tau_(iDim)(
          CALL ZXY(numDim,numPoints,gridSizes,opInterval,metricPtr,tauPtr,fluxPtr)       

       END DO
       
       ! Energy
       fluxOffset  =  fluxOffset + numPoints
       fluxPtr     => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)  
       energyPtr   => energyBuffer(dirOffset+1:dirOffset+numPoints)

       CALL ZXY(numDim,numPoints,gridSizes,opInterval,metricPtr,energyPtr,fluxPtr)

    ELSE ! must be curvilinear
       
       ! need row 'fluxDir' of the metric tensor
       metricOffset = (fluxDir-1)*numDim*numPoints

       ! For curvilinear
       ! Momentum terms flux_i  = (metric_fluxDir_j * tau_j_i) i=[1:numDim]
       DO iDim = 1, numDim
          
          fluxOffset   =  fluxOffset + numPoints
          fluxPtr      => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          
          CALL ASSIGNMENTXA(numDim,numPoints,gridSizes,opInterval,0.0_8,fluxPtr)
          
          DO iVel = 1, numDim
             
             velOffset = (iVel-1)*numPoints+metricOffset
             metricPtr => gridMetric(velOffset+1:velOffset+numPoints)

             IF(numDim == 2) THEN
                tensorIndex = index2D(iVel,iDim)
             ELSE
                tensorIndex = index3D(iVel,iDim)
             ENDIF
          
             tensorOffset =  tensorIndex*numPoints
             tauPtr       => tauBuffer(tensorOffset+1:tensorOffset+numPoints)

             ! Get fluxPtr += (metric_fluxDir_iVel * tau_iVel_iDim) 
             CALL YWXPY(numDim,numPoints,gridSizes,opInterval,metricPtr,tauPtr,fluxPtr)
             
          END DO

       END DO
       
       ! Energy flux is just (metric_fluxDir_i * Q_i), since 
       ! both have proper vector storage the DOT operator 
       ! can be used
       fluxOffset  =  fluxOffset + numPoints
       fluxPtr     => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)  
       metricPtr   => gridMetric(metricOffset+1:metricOffset+numDim*numPoints)

       CALL ZXDOTY(numDim,numPoints,gridSizes,opInterval,numDim,metricPtr,energyBuffer,fluxPtr)

    ENDIF

  END SUBROUTINE StrongFlux1D

  !>
  !! @brief Compute the curvilinear cartesian viscous fluxes in 1 dimension
  !!
  !! retains logic to simplify calculations on uniform or stretched cartesian grids
  !!
  !! See /ref conserve for theory
  !!
  SUBROUTINE ScalarFlux1D                                            &
       (numDim, fluxDir, gridSizes, numPoints, opInterval, gridType, & 
       gridMetric, gradScalar, fluxBuffer)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, fluxDir, gridType
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: gradScalar(numPoints*numDim)
    REAL(KIND=8),    INTENT(OUT)        :: fluxBuffer(numPoints)

    INTEGER(KIND=8) :: dirOffset, metricOffset
    INTEGER(KIND=8) :: velIndex, fluxIndex, velOffset, heatOffset, dimOffset
    REAL(KIND=8)    :: gridScale
    REAL(KIND=8)    :: minusOne

    REAL(KIND=8),    DIMENSION(:), POINTER :: gradPtr
    REAL(KIND=8),    DIMENSION(:), POINTER :: metricPtr

    dirOffset = (fluxDir-1)*numPoints

    IF(gridType < RECTILINEAR) THEN

       gridScale    =  gridMetric(fluxDir)
       gradPtr      => gradScalar(dirOffset+1:dirOffset+numPoints)

       ! Grid metric is scalar and constant, this gets flux = metric * grad(Y)
       CALL YAX(numDim,numPoints,gridSizes,opInterval,gridScale,gradPtr,fluxBuffer)       

    ELSE IF (gridType == RECTILINEAR) THEN
       
       metricPtr    => gridMetric(dirOffset+1:dirOffset+numPoints)
       gradPtr      => gradScalar(dirOffset+1:dirOffset+numPoints)

       ! Grid metric is scalar, this gets flux = metric * tau_(iDim)(
       CALL ZXY(numDim,numPoints,gridSizes,opInterval,metricPtr,gradPtr,fluxBuffer)       

    ELSE ! must be curvilinear
       
       metricOffset = dirOffset*numDim
       metricPtr => gridMetric(metricOffset+1:metricOffset+numDim*numPoints)
       
       CALL ZXDOTY(numDim,numPoints,gridSizes,opInterval,numDim,metricPtr,gradScalar,fluxBuffer)
       
    ENDIF

  END SUBROUTINE ScalarFlux1D

END MODULE Viscid
