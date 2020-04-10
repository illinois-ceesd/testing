MODULE OPERATORS

  IMPLICIT NONE

CONTAINS

  !> @file Operators.f90
  !> @brief Implementation of routines that apply differential (or other) operators to all points of the state data in the indicated interval

! 
  !> @brief applyoperator applies an operator specified as a stencil set to the provided state data
  !>
  !> applyoperator is a brute-force method of applying a set of stencils to a given data buffer. The
  !> The stencilset and operand are given as flat, primitive arrays, with supporting data to indicate
  !> the size of the data structures. An additional <em>stencil connectivity</em> is given which 
  !> indicates which stencil (i.e. which stencil from the stencil set) to apply to each point.
  !> This brute-force method simply loops through all points, and applies the stencil indicated by
  !> the resolved stencil connectivity to each point.
  !> @image html images/ApplyOperatorBrute.png "applyoperator brute-force method cartoon" width=256
  !> @param[in] numDim indicates the number of dimensions for the input data (dimSizes,opInterval)
  !> @param[in] dimSizes indicates the number of points in each dimension [Nx Ny Nz]
  !> @param[in] numComponents indicates the number of components in the input/output data (U,dU)
  !> @param[in] numPoints indicates the total number of points [Nx*Ny*Nz] (needed for C/Fortran interface)
  !> @param[in] opDir indicates in which direction the operator will be applied [X=1 | Y=2 | Z=3]
  !> @param[in] opInterval array of size (2*numDim) which indicates the index interval on which to operate [xStart,xEnd,yStart,yEnd...]
  !> @param[in] numStencils indicates the number of stencils in the input stencilset
  !> @param[in] stencilSizes array of size (numStencils) which indicates the number of weight values for each stencil
  !> @param[in] stencilStarts array of size (numStencils) which indicates the starting index into (stencilWeights and stencilOffsets) for each stencil
  !> @param[in] numValues total number of stencil weight values (numStencils[1]*numStencils[2]*...numStecils[numStencils]) (req'd for C/Fort interface)
  !> @param[in] stencilWeights array of size (numValues) which contains the weights for all the stencils in the stencilset
  !> @param[in] stencilOffsets array of size (numValues) which indicates the offsets from the @e considered point to the point where each weight is applied
  !> @param[in] stencilID array of size (numPoints) which indicates the stencil ID for each point 
  !> @param[in] U the data on which to operate
  !> @param[out] dU where to stuff the result
  SUBROUTINE applyoperator(numDim,dimSizes,numComponents,numPoints,opDir,opInterval,numStencils, &
       stencilSizes,stencilStarts,numValues,stencilWeights,stencilOffsets,stencilID,U,dU)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim, opDir,numStencils, numValues, numComponents
    INTEGER(KIND=8), INTENT(IN)  :: dimSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)  :: opInterval(2*numDim) 
    INTEGER(KIND=4), INTENT(IN)  :: stencilSizes(numStencils),stencilStarts(numStencils)
    INTEGER(KIND=4), INTENT(IN)  :: stencilOffsets(numValues)
    REAL(KIND=8),    INTENT(IN)  :: stencilWeights(numValues)
    INTEGER(KIND=4), INTENT(IN)  :: stencilID(numPoints)

    REAL(KIND=8),    INTENT(IN),  TARGET :: U(numPoints*numComponents)
    REAL(KIND=8),    INTENT(OUT), TARGET :: dU(numPoints*numComponents)

    REAL(KIND=8)    :: fac
    INTEGER(KIND=4) :: iStencil, iWeight, iComp
    INTEGER(KIND=8) :: plane, pointOffset, compOffset
    INTEGER(KIND=8) :: I, J, K, jIndex, jkIndex, kIndex, iPoint

    REAL(KIND=8),    DIMENSION(:), POINTER     :: uCompPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: duCompPtr

    !    DO iComp = 1, numComponents

    !       compOffset = (iComp-1)*numPoints
    !       uCompPtr   => U(compOffset+1:compOffset+numPoints)
    !       duCompPtr  => dU(compOffset+1:compOffset+numPoints)

    IF(numDim == 1) THEN
       DO I = opInterval(1), opInterval(2)
          iStencil = stencilID(I)
          dU(I) = 0.0_8
          DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
             dU(I) = dU(I) + stencilWeights(iWeight)*U(I+stencilOffsets(iWeight))
          END DO
       END DO
    ELSE IF(numDim == 2) THEN
       IF(opDir == 1) THEN
          DO J = opInterval(3), opInterval(4)
             jIndex = (J-1)*dimSizes(1)
             DO I = opInterval(1), opInterval(2)
                iPoint = jIndex + I
                iStencil = stencilID(iPoint)
                dU(iPoint) = 0.0_8
                DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                   dU(iPoint) = dU(iPoint) + &
                        stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight))
                END DO
             END DO
          END DO
       ELSE IF(opDir == 2) THEN
          plane = dimSizes(1)
          DO J = opInterval(3), opInterval(4)
             jIndex = (J-1)*plane
             DO I = opInterval(1), opInterval(2)
                iPoint = jIndex + I
                iStencil = stencilID(iPoint)
                dU(iPoint) = 0.0_8
                DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                   dU(iPoint) = dU(iPoint) + &
                        stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight)*plane)
                END DO
             END DO
          END DO
       ENDIF
    ELSE IF(numDim == 3) THEN
       plane = dimSizes(1) * dimSizes(2)
       IF(opDir == 1) THEN
          DO K = opInterval(5), opInterval(6)
             kIndex = (K-1)*plane
             DO J = opInterval(3), opInterval(4)
                jkIndex = kIndex + (J-1)*dimSizes(1)
                DO I = opInterval(1), opInterval(2)
                   iPoint   = jkIndex + I
                   iStencil = stencilID(iPoint) 
                   dU(iPoint) = 0.0_8
                   DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                      dU(iPoint) = dU(iPoint) + &
                           stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight))
                   END DO
                END DO
             END DO
          END DO
       ELSE 
          pointOffset = 1
          IF(opDir == 2) THEN
             pointOffset = dimSizes(1)
          ELSE
             pointOffset = plane
          ENDIF
          DO K = opInterval(5), opInterval(6)
             kIndex = (K-1)*plane
             DO J = opInterval(3), opInterval(4)
                jkIndex = kIndex + (J-1)*dimSizes(1)
                DO I = opInterval(1), opInterval(2)
                   iPoint   = jkIndex + I
                   iStencil = stencilID(iPoint)
                   dU(iPoint) = 0.0_8
                   DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                      dU(iPoint) = dU(iPoint) + &
                           stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight)*pointOffset)
                   END DO ! weight
                END DO ! I
             END DO ! J 
          END DO ! K
       ENDIF ! (opDir)
    ENDIF ! (numDim)
  END SUBROUTINE applyoperator
  
  !> @file ApplyOperator.f90
  !> @brief Implementation of routines that apply differential (or other) operators to all points of the state data in the indicated interval

  !> @brief applyoperator applies an operator specified as a stencil set to the provided state data
  !>
  !> applyoperator is a brute-force method of applying a set of stencils to a given data buffer. The
  !> The stencilset and operand are given as flat, primitive arrays, with supporting data to indicate
  !> the size of the data structures. An additional <em>stencil connectivity</em> is given which 
  !> indicates which stencil (i.e. which stencil from the stencil set) to apply to each point.
  !> This brute-force method simply loops through all points, and applies the stencil indicated by
  !> the resolved stencil connectivity to each point.
  !> @image html images/ApplyOperatorBrute.png "applyoperator brute-force method cartoon" width=256
  !> @param[in] numDim indicates the number of dimensions for the input data (dimSizes,opInterval)
  !> @param[in] dimSizes indicates the number of points in each dimension [Nx Ny Nz]
  !> @param[in] numComponents indicates the number of components in the input/output data (U,dU)
  !> @param[in] numPoints indicates the total number of points [Nx*Ny*Nz] (needed for C/Fortran interface)
  !> @param[in] opDir indicates in which direction the operator will be applied [X=1 | Y=2 | Z=3]
  !> @param[in] opInterval array of size (2*numDim) which indicates the index interval on which to operate [xStart,xEnd,yStart,yEnd...]
  !> @param[in] numStencils indicates the number of stencils in the input stencilset
  !> @param[in] stencilSizes array of size (numStencils) which indicates the number of weight values for each stencil
  !> @param[in] stencilStarts array of size (numStencils) which indicates the starting index into (stencilWeights and stencilOffsets) for each stencil
  !> @param[in] numValues total number of stencil weight values (numStencils[1]*numStencils[2]*...numStecils[numStencils]) (req'd for C/Fort interface)
  !> @param[in] stencilWeights array of size (numValues) which contains the weights for all the stencils in the stencilset
  !> @param[in] stencilOffsets array of size (numValues) which indicates the offsets from the @e considered point to the point where each weight is applied
  !> @param[in] stencilID array of size (numPoints) which indicates the stencil ID for each point 
  !> @param[in] U the data on which to operate
  !> @param[out] dU where to stuff the result
  SUBROUTINE applyoperatorv(numDim,dimSizes,numComponents,numPoints,opDir,opInterval,numStencils, &
       stencilSizes,stencilStarts,numValues,stencilWeights,stencilOffsets,stencilID,U,dU)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim, opDir,numStencils, numValues, numComponents
    INTEGER(KIND=8), INTENT(IN)  :: dimSizes(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)  :: opInterval(2*numDim) 
    INTEGER(KIND=4), INTENT(IN)  :: stencilSizes(numStencils),stencilStarts(numStencils)
    INTEGER(KIND=4), INTENT(IN)  :: stencilOffsets(numValues)
    REAL(KIND=8),    INTENT(IN)  :: stencilWeights(numValues)
    INTEGER(KIND=4), INTENT(IN)  :: stencilID(numPoints)

    REAL(KIND=8),    INTENT(IN),  TARGET :: U(numPoints*numComponents)
    REAL(KIND=8),    INTENT(OUT), TARGET :: dU(numPoints*numComponents)

    REAL(KIND=8)    :: fac
    INTEGER(KIND=4) :: iStencil, iWeight, iComp
    INTEGER(KIND=8) :: plane, pointOffset, compOffset
    INTEGER(KIND=8) :: I, J, K, jIndex, jkIndex, kIndex, iPoint

    REAL(KIND=8),    DIMENSION(:), POINTER     :: uCompPtr
    REAL(KIND=8),    DIMENSION(:), POINTER     :: duCompPtr

    DO iComp = 1, numComponents

       compOffset = (iComp-1)*numPoints
       uCompPtr   => U(compOffset+1:compOffset+numPoints)
       duCompPtr  => dU(compOffset+1:compOffset+numPoints)

       IF(numDim == 1) THEN
          DO I = opInterval(1), opInterval(2)
             iStencil = stencilID(I)
             duCompPtr(I) = 0.0_8
             DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                duCompPtr(I) = duCompPtr(I) + stencilWeights(iWeight)*uCompPtr(I+stencilOffsets(iWeight))
             END DO
          END DO
       ELSE IF(numDim == 2) THEN
          IF(opDir == 1) THEN
             DO J = opInterval(3), opInterval(4)
                jIndex = (J-1)*dimSizes(1)
                DO I = opInterval(1), opInterval(2)
                   iPoint = jIndex + I
                   iStencil = stencilID(iPoint)
                   duCompPtr(iPoint) = 0.0_8
                   DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                      duCompPtr(iPoint) = duCompPtr(iPoint) + &
                           stencilWeights(iWeight)*uCompPtr(iPoint+stencilOffsets(iWeight))
                   END DO
                END DO
             END DO
          ELSE IF(opDir == 2) THEN
             plane = dimSizes(1)
             DO J = opInterval(3), opInterval(4)
                jIndex = (J-1)*plane
                DO I = opInterval(1), opInterval(2)
                   iPoint = jIndex + I
                   iStencil = stencilID(iPoint)
                   duCompPtr(iPoint) = 0.0_8
                   DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                      duCompPtr(iPoint) = duCompPtr(iPoint) + &
                           stencilWeights(iWeight)*uCompPtr(iPoint+stencilOffsets(iWeight)*plane)
                   END DO
                END DO
             END DO
          ENDIF
       ELSE IF(numDim == 3) THEN
          plane = dimSizes(1) * dimSizes(2)
          IF(opDir == 1) THEN
             DO K = opInterval(5), opInterval(6)
                kIndex = (K-1)*plane
                DO J = opInterval(3), opInterval(4)
                   jkIndex = kIndex + (J-1)*dimSizes(1)
                   DO I = opInterval(1), opInterval(2)
                      iPoint   = jkIndex + I
                      iStencil = stencilID(iPoint) 
                      duCompPtr(iPoint) = 0.0_8
                      DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                         duCompPtr(iPoint) = duCompPtr(iPoint) + &
                              stencilWeights(iWeight)*uCompPtr(iPoint+stencilOffsets(iWeight))
                      END DO
                   END DO
                END DO
             END DO
          ELSE 
             pointOffset = 1
             IF(opDir == 2) THEN
                pointOffset = dimSizes(1)
             ELSE
                pointOffset = plane
             ENDIF
             DO K = opInterval(5), opInterval(6)
                kIndex = (K-1)*plane
                DO J = opInterval(3), opInterval(4)
                   jkIndex = kIndex + (J-1)*dimSizes(1)
                   DO I = opInterval(1), opInterval(2)
                      iPoint   = jkIndex + I
                      iStencil = stencilID(iPoint)
                      duCompPtr(iPoint) = 0.0_8
                      DO  iWeight = stencilStarts(iStencil),stencilStarts(iStencil)+stencilSizes(iStencil) - 1
                         duCompPtr(iPoint) = duCompPtr(iPoint) + &
                              stencilWeights(iWeight)*uCompPtr(iPoint+stencilOffsets(iWeight)*pointOffset)
                      END DO ! weight
                   END DO ! I
                END DO ! J 
             END DO ! K
          ENDIF ! (opDir)
       ENDIF ! (numDim)
    END DO ! (loop over components)
  END SUBROUTINE applyoperatorv

  !> @brief applyoperatorblobs applies an operator by applying each stencil in turn to all the points to which it applies
  !>
  !> applyoperatorblobs is a @e blobbed method of applying a set of stencils to a given data buffer. 
  !> The stencilset and operand are given as flat, primitive arrays, with supporting data to indicate
  !> the size of the data structures. An additional <em>dual stencil connectivity</em> is given which 
  !> indicates which points to apply to a given stencil.
  !> This blobbed method loops through all the stencils, and applies each stencil to the set of pionts indicated by
  !> the resolved dual stencil connectivity for each stencil.
  !> @image html images/ApplyOperatorBlobs.png "applyoperatorBLOBS - blobbed method cartoon" width=256
  !> @param[in] numDim indicates the number of dimensions for the input data (dimSizes,opInterval)
  !> @param[in] dimSizes indicates the number of points in each dimension [Nx Ny Nz]
  !> @param[in] numComponents indicates the number of components in the input/output data (U,dU)
  !> @param[in] numPointsBuffer indicates the total number of points [Nx*Ny*Nz] (needed for C/Fortran interface)
  !> @param[in] opDir indicates in which direction the operator will be applied [X=1 | Y=2 | Z=3]
  !> @param[in] numStencils indicates the number of stencils in the input stencilset
  !> @param[in] stencilSizes array of size (numStencils) which indicates the number of weight values for each stencil
  !> @param[in] stencilStarts array of size (numStencils) which indicates the starting index into (stencilWeights and stencilOffsets) for each stencil
  !> @param[in] numStencilValues total number of stencil weight values (numStencils[1]*numStencils[2]*...numStecils[numStencils]) (req'd for C/Fort interface)
  !> @param[in] stencilWeights array of size (numValues) which contains the weights for all the stencils in the stencilset
  !> @param[in] stencilOffsets array of size (numValues) which indicates the offsets from the @e considered point to the point where each weight is applied
  !> @param[in] numPointsStencil array of size (numStencils) which indicates how many points to apply each stencil
  !> @param[in] numPointsApply total number of points in the stencilPoints array (needed for C/Fortran interface)
  !> @param[in] stencilPoints array of size (numPointsStencil(1)*numPointsStencil(2)*...numPointsStencil(numStencils)) indicating the points to which each stencil applies
  !> @param[in] U the data on which to operate
  !> @param[out] dU where to stuff the result
  SUBROUTINE applyoperatorblobs(numDim,dimSizes,numComponents,numPointsBuffer,opDir,numStencils, &
       stencilSizes,stencilStarts,numStencilValues,stencilWeights,stencilOffsets,numPointsStencil,&
       numPointsApply,stencilPoints,U,dU)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim, opDir,numStencils, numStencilValues, numComponents
    INTEGER(KIND=8), INTENT(IN)  :: numPointsApply, numPointsBuffer
    INTEGER(KIND=8), INTENT(IN)  :: dimSizes(numDim),numPointsStencil(numStencils)
    INTEGER(KIND=8), INTENT(IN)  :: stencilPoints(numPointsApply)
    INTEGER(KIND=4), INTENT(IN)  :: stencilSizes(numStencils),stencilStarts(numStencils)
    INTEGER(KIND=4), INTENT(IN)  :: stencilOffsets(numStencilValues)
    REAL(KIND=8),    INTENT(IN)  :: stencilWeights(numStencilValues)
    REAL(KIND=8),    INTENT(IN)  :: U(numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT) :: dU(numPointsBuffer)

    INTEGER(KIND=8) :: stencilPointsOffset, iPoint, pointsStart,pointsEnd,numPointsThisStencil
    INTEGER(KIND=4) :: iStencil, iWeight, stencilSize, stencilStart, stencilEnd

    !  dU(:) = 0.0_8
    stencilPointsOffset = 1
    !  WRITE(*,*) 'NumDim: ',numDim 
    !  WRITE(*,*) 'dimSizes:',dimSizes
    !  WRITE(*,*) 'numComponents:',numComponents
    !  WRITE(*,*) 'numPointsBuffer:',numPointsBuffer
    !  WRITE(*,*) 'opDir:',opDir
    !  WRITE(*,*) 'numStencils:',numStencils
    !  WRITE(*,*) 'stencilSizes:',stencilSizes 
    !  WRITE(*,*) 'stencilStarts:',stencilStarts
    !  WRITE(*,*) 'numStencilValues:',numStencilValues
    !  WRITE(*,*) 'stencilWeights:',stencilWeights
    !  WRITE(*,*) 'stencilOffsets:',stencilOffsets
    !  WRITE(*,*) 'numPointsStencil:',numPointsStencil
    !    WRITE(*,*) 'numPointsApply:',numPointsApply
    DO iStencil = 1, numStencils
       numPointsThisStencil = numPointsStencil(iStencil)
       stencilSize          = stencilSizes(iStencil)
       stencilStart         = stencilStarts(iStencil)
       stencilEnd           = stencilStart+stencilSize-1
       pointsStart          = stencilPointsOffset
       pointsEnd            = pointsStart + numPointsThisStencil-1
       CALL applysinglestencil(numDim,dimSizes,numComponents,numPointsBuffer,opDir,numPointsThisStencil, &
            stencilPoints(pointsStart:pointsEnd),stencilSize,stencilWeights(stencilStart:stencilEnd),&
            stencilOffsets(stencilStart:stencilEnd),U,dU)
       stencilPointsOffset = stencilPointsOffset+numPointsThisStencil
    END DO

  END SUBROUTINE applyoperatorblobs



  !> @brief applysinglestencil applies an operator by applying a given stencil to the specified points
  !>
  !> applysinglestencil is a single-stencil method which operates on the given points
  !> The stencil and operand are given as flat, primitive arrays, with supporting data to indicate
  !> the size of the data structures. An additional array of points is given which 
  !> indicates the points on which to operate. This single-stencil method loops through all 
  !> the specified points and applies the stencil to each.
  !> @image html images/ApplyOperatorBlobs.png "applyoperatorBLOBS - blobbed method cartoon" width=256
  !> @param[in] numDim indicates the number of dimensions for the input data (dimSizes,opInterval)
  !> @param[in] dimSizes indicates the number of points in each dimension [Nx Ny Nz]
  !> @param[in] numComponents indicates the number of components in the input/output data (U,dU)
  !> @param[in] numPointsBuffer indicates the total number of points [Nx*Ny*Nz] (needed for C/Fortran interface)
  !> @param[in] opDir indicates in which direction the operator will be applied [X=1 | Y=2 | Z=3]
  !> @param[in] numPointsApply total number of points in the stencilPoints array (needed for C/Fortran interface)
  !> @param[in] applyPoints array of size (numPointsApply) indicating the points to which to apply the stencil
  !> @param[in] stencilSize number of stencil weights
  !> @param[in] stencilWeights array of size (stencilSize) which contains the weights for all the stencils in the stencilset
  !> @param[in] stencilOffsets array of size (stencilSize) which indicates the offsets from the @e considered point to the point where each weight is applied
  !> @param[in] U the data on which to operate
  !> @param[out] dU where to stuff the result
  SUBROUTINE applysinglestencil(numDim,dimSizes,numComponents,numPointsBuffer,opDir,numPointsApply, &
       applyPoints,stencilSize,stencilWeights,stencilOffsets,U,dU)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim, opDir, stencilSize, numComponents
    INTEGER(KIND=8), INTENT(IN)  :: dimSizes(numDim),  numPointsBuffer, numPointsApply
    INTEGER(KIND=8), INTENT(IN)  :: applyPoints(numPointsApply) 
    INTEGER(KIND=4), INTENT(IN)  :: stencilOffsets(stencilSize)
    REAL(KIND=8),    INTENT(IN)  :: stencilWeights(stencilSize)
    REAL(KIND=8),    INTENT(IN)  :: U(numPointsBuffer)
    REAL(KIND=8),    INTENT(OUT) :: dU(numPointsBuffer)

    INTEGER(KIND=8) :: I, iPoint, plane
    INTEGER(KIND=4) :: iStencil, iWeight

    IF(opDir == 1) THEN
       DO I = 1,numPointsApply
          iPoint = applyPoints(I)
          dU(iPoint) = 0.0_8
          DO  iWeight = 1, stencilSize
             dU(iPoint) = dU(iPoint) + stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight))
          END DO
       END DO
    ELSE
       IF(opDir == 2) THEN
          plane = dimSizes(1)
       ELSE IF(opDir == 3) THEN
          plane = dimSizes(1)*dimSizes(2)
       ENDIF
       DO I = 1, numPointsApply
          iPoint = applyPoints(I) 
          dU(iPoint) = 0.0_8
          DO  iWeight = 1,stencilSize
             dU(iPoint) = dU(iPoint) + stencilWeights(iWeight)*U(iPoint+stencilOffsets(iWeight)*plane)
          END DO
       END DO
    END IF

  END SUBROUTINE applysinglestencil

END MODULE OPERATORS
