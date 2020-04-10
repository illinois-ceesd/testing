MODULE Euler

  USE SIMPLE
  USE SPECIAL
  USE GRID

  IMPLICIT NONE

CONTAINS


  !> @brief Computes the inviscid fluxes in one dimension
  !> 
  !> This routine computes the inviscid fluxes of the single gas NS in one dimension.
  !> For example in the \f$\hat{\xi}\f$ direction, the inviscid flux is: 
  !> \f[
  !> \hat{\vec{F}}^I_1 = 
  !> \begin{bmatrix} \rho \hat{U} \\ \rho u \hat{U} + p\hat{\xi}_x \\ \rho v \hat{U} + p \hat{\xi}_y \\ \rho w \hat{U} + p\hat{\xi}_z \\ (\rho E + p)\hat{U} - \hat{\xi}_t p \end{bmatrix},
  !> \f]
  !> @param numDim - const integer indicating number of dimensions
  !> @param numPoints - const 64-bit integer indicating total size of input arrays
  !> @param gridSizes - const 64-bit integer 1d array of size @em numDim, which indicates the number of points in each dimension for the grid 
  !> @param opInterval - const 64-bit integer 1d array of size 2 x @em numDim which indicates the rectangular interval over which this routine should operate.  e.g. [@em iStart @em iEnd @em jStart @em jEnd ... ]
  !> @param fluxDir - const integer indicating the direction in which the flux is to be calculated
  !> @param gridType - const integer indicating whether the @em gridMetric parameter is a Cartesian, Rectilinear, or Curvilinear metric
  !> @param gridMetric - const double precision 1d array containing the grid metric
  !> @param rhoBuffer - (\f$\rho\f$) const double precision contiguous 1d array of size @em numPoints representing the @em density at each point on the grid
  !> @param rhoVBuffer - (\f$\rho\vec{V}\f$) const double precision contiguous 1d array of size @em numDim x @em numPoints representing the @em momentum density at each point on the grid.
  !> @param rhoEBuffer - (\f$\rho E\f$) const double precision contiguous 1d array of size @em numPoints representing the energy density at each point on the grid
  !> @param velHat - (\f$\hat{U}\f$) - const double precision contiguous 1d array of size @em numPoints representing the component of the contravariant velocity corresponding to the direction (@em fluxDir) in which the fluxes are to be calculated
  !> @param pressureBuffer - (p) - const double precision contiguous 1d array of size @em numPoints representing the @m pressure at each point on the grid
  !> @param fluxBuffer - double precision contiguous 1d array of size (@em numDim + 2) x @em numPoints for output of the fluxes for each equation. The @em fluxBuffer is returned with (@em numDim + 2) blocks of size @em numPoints, one block for each equation. 
  SUBROUTINE Flux1D(                                &
       numDim, numPoints, gridSizes, opInterval,    &
       fluxDir, gridType, gridMetric,               & 
       rhoBuffer,rhoVBuffer,rhoEBuffer,velHat,   &
       pressureBuffer, fluxBuffer)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, fluxDir, gridType
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: gridMetric(numDim*numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)         :: rhoBuffer(numPoints)
    REAL(KIND=8),    INTENT(IN), TARGET :: rhoVBuffer(numDim*numPoints)
    REAL(KIND=8),    INTENT(IN)         :: rhoEBuffer(numPoints)
    REAL(KIND=8),    INTENT(IN)         :: velHat(numPoints)
    REAL(KIND=8),    INTENT(IN)         :: pressureBuffer(numPoints)
    REAL(KIND=8),    INTENT(OUT),TARGET :: fluxBuffer(numPoints*(numDim+2))

    INTEGER         :: iDim, numComponents, iVelDim
    INTEGER(KIND=8) :: iPoint,iX,iY,iZ,iStart,iEnd,jStart,jEnd,kStart,kEnd
    INTEGER(KIND=8) :: xIndex,zIndex,yIndex,yzIndex,xSize,ySize,zSize,fluxOffset
    INTEGER(KIND=8) :: iPoint2, iPoint3, pointOffset, pointIndex,vectorPointIndex
    INTEGER(KIND=8) :: velIndex, fluxIndex, velOffset, dimOffset, metricOffset
    REAL(KIND=8)    :: gridScale

    REAL(KIND=8),    DIMENSION(:), POINTER     :: rhoVPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: rhoVRHSPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: fluxPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: metricPtr
    INTEGER(KIND=4), DIMENSION(:), POINTER     :: stencilConnectivity


    fluxOffset =  0
    fluxPtr    => fluxBuffer(1:numPoints)

    ! Continuity 
    ! Calculates fluxBuffer = rho * vHat(iDim)
    CALL ZXY(numDim,numPoints,gridSizes,opInterval,rhoBuffer,velHat,fluxPtr)

    ! numDim components of Momentum
    IF(gridType < RECTILINEAR) THEN

       gridScale = gridMetric(fluxDir)

       DO iVelDim = 1, numDim
          
          fluxOffset =  fluxOffset + numPoints
          velOffset  =  (iVelDim-1)*numPoints
          fluxPtr    => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          rhoVPtr    => rhoVBuffer((velOffset+1):(velOffset+numPoints))    
          
          IF(fluxDir == iVelDim) THEN
             ! gridMetric(iDim) is single constant over all points (0 for other dimensions)
             ! Diagonal term: flux = rhoV(iDim)*vHat(iDim) + gridMetric(iDim)*pressure
             CALL ZAWPXY(numDim,numPoints,gridSizes,opInterval,gridScale,pressureBuffer,&
                  rhoVPtr,velHat,fluxPtr)
          ELSE
             ! Cross terms: flux = rhoV(iVelDim)*vHat(iDim)
             CALL ZXY(numDim,numPoints,gridSizes,opInterval,rhoVPtr,velHat,fluxPtr)
          ENDIF
       END DO

    ELSE IF(gridType < CURVILINEAR) THEN

       metricOffset = (fluxDir-1)*numPoints
       metricPtr => gridMetric(metricOffset+1:metricOffset+numPoints)

       DO iVelDim = 1, numDim
          
          fluxOffset =  fluxOffset + numPoints
          velOffset  =  (iVelDim-1)*numPoints
          fluxPtr    => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          rhoVPtr    => rhoVBuffer((velOffset+1):(velOffset+numPoints))    
          
          IF(fluxDir == iVelDim) THEN
             ! gridMetric(iDim) is nodal scalar (0 for other dimensions)
             ! Diagonal term: flux = rhoV(iDim)*vHat(iDim) + gridMetric(iDim)*pressure
             CALL ZVWPXY(numDim,numPoints,gridSizes,opInterval,metricPtr,pressureBuffer,rhoVPtr,velHat,fluxPtr)
          ELSE
             ! Cross terms: flux = rhoV(iVelDim)*vHat(iDim)
             CALL ZXY(numDim,numPoints,gridSizes,opInterval,rhoVPtr,velHat,fluxPtr)
          ENDIF

       END DO

    ELSE ! gridMetric(iDim) is a nodal numDim-vector

       metricOffset = (fluxDir-1)*numDim*numPoints
       
       DO iVelDim = 1, numDim
          
          fluxOffset =  fluxOffset + numPoints
          velOffset  =  (iVelDim-1)*numPoints
          fluxPtr    => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)
          rhoVPtr    => rhoVBuffer((velOffset+1):(velOffset+numPoints))
          metricPtr  => gridMetric(metricOffset+velOffset+1:metricOffset+velOffset+numPoints)

          ! flux = rhoV(iDim)*vHat(iDim) + p*gridMetric(iDim)  
          CALL ZVWPXY(numDim,numPoints,gridSizes,opInterval,metricPtr,pressureBuffer,rhoVPtr,velHat,fluxPtr)

       END DO

    ENDIF
    
    ! Energy
    fluxOffset = fluxOffset + numPoints
    fluxPtr => fluxBuffer(fluxOffset+1:fluxOffset+numPoints)       

    ! Calculate flux = vHat(iDim)*(rhoE + pressure)
    CALL ZWMXPY(numDim,numPoints,gridSizes,opInterval,velHat,rhoEBuffer,pressureBuffer,fluxPtr)
    
  END SUBROUTINE Flux1D


  !> @brief Flux for scalar transport
  SUBROUTINE ScalarFlux1D(                          &
       numDim, numPoints, gridSizes, opInterval,    &
       numScalars,scalarBuffer,velHat, fluxBuffer)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)         :: numDim, numScalars
    INTEGER(KIND=8), INTENT(IN)         :: gridSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)         :: opInterval(2*numDim)
    REAL(KIND=8),    INTENT(IN), TARGET :: scalarBuffer(numScalars*numPoints)
    REAL(KIND=8),    INTENT(IN)         :: velHat(numPoints)
    REAL(KIND=8),    INTENT(OUT),TARGET :: fluxBuffer(numScalars*numPoints)

    INTEGER         :: iScalar
    INTEGER(KIND=8) :: scalarOffset

    REAL(KIND=8),    DIMENSION(:), POINTER     :: fluxPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: scalarPtr


    scalarOffset =  0
    fluxPtr    => fluxBuffer(1:numPoints)

    ! Scalar transport
    ! Calculates fluxBuffer = scalar * vHat
    DO iScalar = 1, numScalars
       scalarPtr => scalarBuffer(scalarOffset+1:scalarOffset+numPoints)
       fluxPtr   => fluxBuffer(scalarOffset+1:scalarOffset+numPoints)
       CALL ZXY(numDim,numPoints,gridSizes,opInterval,scalarPtr,velHat,fluxPtr)
       scalarOffset = scalarOffset + numPoints
    END DO
    
  END SUBROUTINE ScalarFlux1D

END MODULE Euler
