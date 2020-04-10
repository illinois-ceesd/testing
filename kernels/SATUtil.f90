MODULE SATUtil

  USE Grid
  USE MetricOps
  USE Roe
  USE EOS

  IMPLICIT NONE

CONTAINS
  
  SUBROUTINE FARFIELD(eosInfo,eosNonDimen,eosGasParams,                  &
       numDim,bufferSizes,numPointsBuffer,patchNormalDir,patchSizes,     &
       numPointsPatch,numPatchPointsOp,patchPointsOp,gridType,gridMetric,&
       jacobianDeterminant,bcParams,gasParams,rhoBuffer,rhoVBuffer,      &
       rhoEBuffer,viscousFluxBuffer,numscalar,scalarBuffer,              &
       rhoRHS,rhoVRHS,rhoERHS,scalarRHS,                                 &
       rhoTarget,rhoVTarget,rhoETarget,scalarTarget)
    
    IMPLICIT NONE

    INTEGER(KIND=8) :: eosInfo(NUMGASINFOS)
    INTEGER(KIND=4) :: numDim,numscalar,patchNormalDir,gridType
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim)
    INTEGER(KIND=8) :: patchSizes(numDim),numPointsPatch
    INTEGER(KIND=8) :: numPatchPointsOp
    INTEGER(KIND=8) :: patchPointsOp(numPatchPointsOp)
    REAL(KIND=8)    :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8)    :: eosGasParams(NUMGASPARAMS*(numScalar+1))
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: bcParams(3),gasParams(5)
    REAL(KIND=8)    :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8)    :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8)    :: viscousFluxBuffer((numDim+2+numScalar)*numPointsBuffer)
    REAL(KIND=8)    :: scalarBuffer(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: rhoRHS(numPointsBuffer)
    REAL(KIND=8)    :: rhoVRHS(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoERHS(numPointsBuffer)
    REAL(KIND=8)    :: scalarRHS(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: rhoTarget(numPointsBuffer)
    REAL(KIND=8)    :: rhoVTarget(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoETarget(numPointsBuffer)
    REAL(KIND=8)    :: scalarTarget(numScalar*numPointsBuffer)

    ! gasParams(:) = Cref Gamma Cp Re
    ! bcParams(:)  = sigma1 sigma2

    ! ... Local variables
    INTEGER(4) :: ND, jj, iDim, iDir, jDim, iScalar
    INTEGER(8) :: Nc, l0, iPoint, pointIndex, metricOffset
    INTEGER(4) :: normDir, sgn ! , gas_dv_model
    REAL(8) :: dsgn, sndspdref2, invtempref, tempref, gamref, spcGasConst
    REAL(8) :: XI_X, XI_Y, XI_Z, XI_T, bndry_h,sbpBoundaryWeight
    REAL(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, gridJacobian
    REAL(8) :: rhoFac, penaltyFac, scalarPenaltyFac, scalarDiff
    
    REAL(8) :: pointMetric(numDim*numDim)
    REAL(8) :: metricUnitVectors(numDim*numDim)
    REAL(8) :: metricMags(numDim)
    REAL(8) :: dvBoundary(numDim+2), dvTarget(numDim+2)

    REAL(8) :: uRoe(numDim+2+numScalar),dvRoe(numDim+2),dpRoe(2+numScalar),cRoe
    REAL(8) :: uTarget(numDim+2+numScalar), gI2(numDim+2+numScalar)
    REAL(8) :: uBoundary(numDim+2+numScalar)
    REAL(8) :: penalty(numDim+2+numScalar), correction(numDim+2+numScalar)
    REAL(8) :: lambda(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: aPrime(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tMat(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tInv(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: matX(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: gamma, ucon, ucon2,ubAux,xh
    REAL(8) :: sigmaI1, sigmaI2, sigmaAux, Sfactor
    REAL(8) :: spd_snd, RE, REInv, SC, SCInv, SAT_sigmaI2_FF, SAT_sigmaI1_FF
    REAL(8) :: theta, theta2
    INTEGER(4) :: error
    INTEGER(4) :: numSpecies,numEquations
    INTEGER(8) :: eosContext

    LOGICAL :: TRIGGERED
    TRIGGERED = .TRUE.

    eosContext = eosInfo(1)
    error = 0

    numSpecies = numScalar
!    numSpecies = 0

    ! ... Problem size
    ND = numDim
    Nc = numPointsBuffer
    numEquations = numDim+2+numSpecies

    ! ... Useful quantities about this boundary
    normDir = abs(patchNormalDir)
    sgn = normDir / patchNormalDir
    dsgn = dble(sgn)


    ! ... Reference quantities
    ! gasParams = (sndspdref,gamma,Cp,1/Re)
    ! MJA changed for new nonDimensionalization
    !sndspdref2 = gasParams(1)*gasParams(1) ! input%sndspdref * input%sndspdref
    !invtempref = gasParams(3)/sndspdref2  ! input%Cpref / sndspdref2
    !tempref    = 1.0_8 / invtempref 
    !gamref     = gasParams(2)
    !REinv      = gasParams(4)
    RE         = gasParams(1)
    SC         = gasParams(2)
    spcGasConst= gasParams(3)
    sndspdref2 = gasParams(5)*gasParams(5)
    tempref    = gasParams(4)
    invtempref = tempref
    IF(RE .gt. 0) REInv = 1.0_8/RE
    IF(SC .gt. 0) SCInv = 1.0_8/SC


    ! ... BC Constants
    ! bcParams = (sigma1,sigma2,sbpBoundaryStencilWeight)
    SAT_sigmaI1_FF              = bcParams(1) ! input%SAT_sigmaI1_FF
    SAT_sigmaI2_FF              = bcParams(2) ! input%SAT_sigmaI2_FF
    sbpBoundaryWeight           = bcParams(3)

    DO iDim = 1, numScalar
      dpRoe(iDim+2) = 0.0_8
    ENDDO

    ! ... loop over all of the points
    DO iPoint = 1, numPatchPointsOp
       l0 = patchPointsOp(iPoint) + 1 ! assume the points coming in are from C (0-based)
       XI_T = 0.0_8
       IF(gridType >= RECTILINEAR) THEN
          gridJacobian = jacobianDeterminant(l0)
       ELSE
          gridJacobian = jacobianDeterminant(1)
       ENDIF

       CALL BoundaryPointMetric(numDim,normDir,numPointsBuffer,l0,gridType,&
            gridMetric,pointMetric)
       CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)

       xh = metricMags(1)*gridJacobian
       bndry_h = 1.0_8 / xh

       ! ... inviscid penalty parameter
       sigmaI1 = -SAT_sigmaI1_FF * dsgn
       
       ! ... pull off the boundary data 
       uBoundary(1) = rhoBuffer(l0)
       DO iDim = 1, numDim
          uBoundary(1+iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO
       uBoundary(numDim+2) = rhoEBuffer(l0)
       DO iDim = 1, numScalar
          uBoundary(numDim+2+iDim) = scalarBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO

       ! access EOS for DV(p,T,v)
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uBoundary(1),dvBoundary(1))

       ! ... target data
       uTarget(1) = rhoTarget(l0)
       DO iDim = 1, numDim
          uTarget(1+iDim) = rhoVTarget(l0+(iDim-1)*numPointsBuffer)
       END DO
       uTarget(numDim+2) = rhoETarget(l0)
       DO iDim = 1, numScalar
          uTarget(numDim+2+iDim) = scalarTarget(l0+(iDim-1)*numPointsBuffer)
       END DO

       ! access EOS for DV
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uTarget(1),dvTarget(1))

       ! Get Roe State
       CALL PointRoeState2(numDim,numSpecies,uBoundary,dvBoundary,uTarget,dvTarget,uRoe)

       CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uRoe(1),dvRoe(1),dpRoe(1),cRoe)

       ! Get NS EigenVectors(tMat,tInv) and EigenValues(lambda)
       CALL EigenMatrices(numDim,numSpecies,uRoe,dvRoe,dpRoe,cRoe,xh, &
            metricUnitVectors,tMat,tInv,lambda)


       error = 0
       
       ! ... save ucon, speed_sound
       ucon    = Lambda(1,1)
       spd_snd = Lambda(ND+1,ND+1) - ucon
       
       
       ! ... only pick those Lambda that are incoming
       If ( sgn == 1 ) Then
          Do jj = 1, numEquations
             Lambda(jj,jj) = max(Lambda(jj,jj),0.0_8)
          End Do
       Else 
          Do jj = 1, numEquations
             Lambda(jj,jj) = min(Lambda(jj,jj),0.0_8)
          End Do
       End If
       
       ! ... modify Lambda if subsonic outflow
       ! ... left boundary
       if (sgn == 1 .and. &
            ucon < 0.0_8 .and. ucon + spd_snd > 0.0_8) then
          Lambda(ND+2,ND+1) = Lambda(ND+2,ND+1) - ucon - spd_snd
          ! right boundary
       else if (sgn == -1 .and. &
            ucon > 0.0_8 .and. ucon - spd_snd < 0.0_8) then
          Lambda(ND+1,ND+2) = Lambda(ND+1,ND+2) - ucon + spd_snd
       end if

       ! ... multiply Lambda' into X
       matX = MATMUL(Lambda,Tinv)
       
       ! ... compute the characteristic matrix
       Aprime = MATMUL(Tmat,matX)

       ! ... subtract off the target
       uBoundary(:) = uBoundary(:) - uTarget(:)

       ! ... compute the characteristic penalty vector
       gI2(:) = MATMUL(Aprime,uBoundary)

       penaltyFac = sbpBoundaryWeight*sigmaI1
       correction(:) = penaltyFac*gI2(:)

       !if(l0 == 1066) then
         !write(*,*) "SAT FF Before RHS update, l0=", l0
         !write(*,*) "rhoRHS", rhoRHS(l0)
         !DO iDim = 1,numDim
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "rhoVRHS", iDim, rhoVRHS(pointIndex)
         !END DO
         !write(*,*) "rhoERHS", rhoERHS(l0)
         !DO iDim = 1,numDim
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "scalarRHS", iDim, scalarRHS(pointIndex)
         !END DO
         !write(*,*) "*********"
         !write(*,*) "Tmat"
         !write(*,*) Tmat 
         !write(*,*) "Tinv"
         !write(*,*) Tinv 
         !write(*,*) "lambda"
         !write(*,*) lambda 
         !write(*,*) "Aprime"
         !write(*,*) Aprime 
         !write(*,*) "uBoundary"
         !write(*,*) uBoundary
         !write(*,*) "uTarget"
         !write(*,*) uTarget
         !write(*,*) "correction"
         !write(*,*) correction
       !endif

       ! ... compute penalty (Bndry_h is included in Lambda already)
       !       Do jj = 1, ND+2
       !          rhs(l0,jj) = rhs(l0,jj) + penaltyFac * gI2(jj)
       !       End Do
       rhoRHS(l0) = rhoRHS(l0) + correction(1)
       DO iDim = 1,numDim
          pointIndex = l0 + (iDim-1)*numPointsBuffer
          rhoVRHS(pointIndex) = rhoVRHS(pointIndex) + correction(1+iDim)
       END DO
       rhoERHS(l0) = rhoERHS(l0) + correction(numDim+2)
       
       ! ... (species treatment)
       DO jj = 1, numscalar
          pointIndex = (jj-1)*numPointsBuffer + l0
          scalarRHS(pointIndex) = scalarRHS(pointIndex) + correction(numDim+2+jj)
       ENDDO

       ! VISCOUS PART 
       IF(RE > 0.0) THEN
         ! ... factor
!         sigmaI2 = dsgn * SAT_sigmaI2_FF * SBP_invPdiag_block1(1) / bndry_h !* JAC(l0)
         !sigmaI2 = dsgn * SAT_sigmaI2_FF*sbpBoundaryWeight/bndry_h !* JAC(l0)
         sigmaI2 = -SAT_sigmaI2_FF
          
         ! ... target state
         do jj = 1, ND+2
           gI2(jj) = 0.0_8
         end do

         ! ... boundary data (already includes 1/Re factor)
         uBoundary(:) = ViscousFluxBuffer(l0) * metricUnitVectors(1)
         if (ND >= 2) &
              uBoundary(:) = uBoundary(:) + ViscousFluxBuffer(l0) * metricUnitVectors(2)
         if (ND == 3) &
              uBoundary(:) = uBoundary(:) + ViscousFluxBuffer(l0) * metricUnitVectors(3)
         !MJA this is broken, need to fix with correct viscous flux terms
         uBoundary(:) = 0
         ! ... penalty term
         penaltyFac = dsgn*SAT_sigmaI2_FF*sbpBoundaryWeight/bndry_h
         penalty(1:numDim+2) = penaltyFac * (uBoundary(1:numDim+2) - gI2(1:numDim+2))

         ! ... add to the rhs
         rhoRHS(l0) = rhoRHS(l0) + penalty(1)
         DO iDim = 1,numDim
           pointIndex = l0 + (iDim-1)*numPointsBuffer
           rhoVRHS(pointIndex) = rhoVRHS(pointIndex) + penalty(1+iDim)
         END DO
         rhoERHS(l0) = rhoERHS(l0) + penalty(numDim+2)

         DO iScalar = 1, numScalar
           pointIndex = (iScalar-1)*numPointsBuffer + l0
           uBoundary(2+ND+iScalar) = scalarBuffer(pointIndex)
           gI2(2+ND+iScalar) = scalarTarget(pointIndex)
           !if(l0 == 1446) then
             !write(*,*) "iScalar", iScalar,"iDim",iDim,"ScalarGrad", scalarGrad(pointIndex)
             !write(*,*) "iScalar", iScalar,"iDim",iDim,"metricUnitVectors", metricUnitVectors(iDim)
           !endif
         END DO

         penaltyFac = dsgn*SAT_sigmaI2_FF*sbpBoundaryWeight/bndry_h

         !MJA comment this part out for now...I'm not sure it's right
         !    I think what needs to be done is to attempt to cancel the 
         !    dissipation terms from scalar diffusion, but that's not currently
         !    stored in the boundary data structure
         !DO iScalar = 1, numScalar
           !penalty(numDim+2+iScalar) = penaltyFac * (uBoundary(numDim+2+iScalar) - gI2(numDim+2+iScalar))
         !END DO
          
         !> @note Scalar diffusion not yet done [mtc]
         ! species transport
         !          sigmaAux = ibfac_local * sigmaI2
         !          do jj = 1, nAuxVars
         !             ubAux = patch_ViscousFluxAux(jj,lp,normDir) 
         !             rhs_auxVars(l0,jj) = rhs_auxVars(l0,jj) + sigmaAux* ubAux
         !          end do
         
       END IF
    END DO

    RETURN

  END SUBROUTINE FARFIELD

  SUBROUTINE SLIP_ADIABATIC(eosInfo,eosNonDimen,eosGasParams, numDim,  &
       bufferSizes,numPointsBuffer,patchNormalDir,sconn,patchSizes,    &
       numPointsPatch,numPatchPointsOp,patchPointsOp,gridType,         &
       gridMetric,jacobianDeterminant,bcParams,gasParams,              &
       rhoBuffer,rhoVBuffer,rhoEBuffer,numscalar,scalarBuffer,         &
       scalarGrad,rhoRHS,rhoVRHS,rhoERHS, scalarRHS,rhoTarget,         &
       rhoVTarget,rhoETarget,scalarTarget)
    
    IMPLICIT NONE

    INTEGER(KIND=8) :: eosInfo(NUMGASINFOS)
    INTEGER(KIND=4) :: numDim,numscalar,patchNormalDir,gridType
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim)
    INTEGER(KIND=4) :: sconn(numPointsBuffer)
    INTEGER(KIND=8) :: patchSizes(numDim),numPointsPatch
    INTEGER(KIND=8) :: numPatchPointsOp
    INTEGER(KIND=8) :: patchPointsOp(numPatchPointsOp)
    REAL(KIND=8)    :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8)    :: eosGasParams(NUMGASPARAMS*(numScalar+1))
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: bcParams(3),gasParams(5)
    REAL(KIND=8)    :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8)    :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8)    :: scalarBuffer(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: scalarGrad(numscalar*numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoRHS(numPointsBuffer)
    REAL(KIND=8)    :: rhoVRHS(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoERHS(numPointsBuffer)
    REAL(KIND=8)    :: scalarRHS(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: rhoTarget(numPointsBuffer)
    REAL(KIND=8)    :: rhoVTarget(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoETarget(numPointsBuffer)
    REAL(KIND=8)    :: scalarTarget(numScalar*numPointsBuffer)


    ! ... Local variables
    INTEGER(4) :: ND, jj, iDim, iDir, iScalar
    INTEGER(8) :: Nc, l0, iPoint, pointIndex,metricOffset
    INTEGER(4) :: normDir, sgn ! , gas_dv_model
    REAL(8) :: dsgn, sndspdref2, invtempref, tempref, gamref, spcGasConst
    REAL(8) :: XI_X, XI_Y, XI_Z, XI_T, bndry_h,sbpBoundaryWeight
    REAL(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, gridJacobian
    REAL(8) :: rhoFac, penaltyFac, scalarPenaltyFac, scalarDiff

    REAL(8) :: bVelocity(numDim), vWall(numDim), vWallTarget(numDim), vTarget(numDim)
    REAL(8) :: pointMetric(numDim*numDim)
    REAL(8) :: metricUnitVectors(numDim*numDim)
    REAL(8) :: metricMags(numDim)
    REAL(8) :: dvBoundary(numDim+2), dvTarget(numDim+2)

    REAL(8) :: uRoe(numDim+2+numScalar),dvRoe(numDim+2),dpRoe(2+numScalar),cRoe
    REAL(8) :: uTarget(numDim+2+numScalar), gI2(numDim+2+numScalar)
    REAL(8) :: uBoundary(numDim+2+numScalar)
    REAL(8) :: penalty(numDim+2+numScalar), correction(numDim+2+numScalar)
    REAL(8) :: lambda(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: aPrime(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tMat(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tInv(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: matX(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: dv2Roe(3)
    REAL(8) :: gamma, uCon, ubAux, vDotXi, xh
    REAL(8) :: sigmaI1, sigmaI2, sigmaAux, Sfactor
    REAL(8) :: spd_snd, RE, REInv, SC, SCInv, SAT_sigmaI1, SAT_sigmaI2, pressureTarget
    INTEGER(4) :: numEquations, numSpecies
    INTEGER(4) :: corner
    INTEGER(8) :: eosContext

    LOGICAL :: TRIGGERED
    TRIGGERED = .FALSE.

    eosContext = eosInfo(1)

    ! ... Problem size
    ND = numDim
    Nc = numPointsBuffer
!    numSpecies = 0
    numSpecies = numScalar

    numEquations = numDim + 2 + numSpecies

    ! ... Useful quantities about this boundary
    normDir = abs(patchNormalDir)
    sgn = normDir / patchNormalDir
    dsgn = dble(sgn)


    ! ... Reference quantities
    ! gasParams = (sndspdref,gamma,Cp,1/Re)
    ! bcParams(:)  = sigma1 sigma2
    ! MJA changed for new nonDimensionalization
    !sndspdref2 = gasParams(1)*gasParams(1) ! input%sndspdref * input%sndspdref
    !invtempref = gasParams(3)/sndspdref2  ! input%Cpref / sndspdref2
    !tempref    = 1.0_8 / invtempref 
    !gamref     = gasParams(2)
    !REinv      = gasParams(4)
    RE         = gasParams(1)
    SC          = gasParams(2)
    spcGasConst= gasParams(3)
    sndspdref2 = gasParams(5)*gasParams(5)
    tempref    = gasParams(4)
    invtempref = tempref
    IF(RE .gt. 0) REInv = 1.0_8/RE
    IF(SC .gt. 0) SCInv = 1.0_8/SC

    ! ... BC Constants
    ! bcParams = (sigma1,sbpBoundaryStencilWeight)
    SAT_sigmaI1                 = bcParams(1) ! input%SAT_sigmaI1_FF
    SAT_sigmaI2                 = bcParams(2) ! input%SAT_sigmaI2_FF
    sbpBoundaryWeight           = bcParams(3)

    DO iDim = 1, numScalar
      dpRoe(iDim+2) = 0.0_8
    ENDDO

    ! ... loop over all of the points
    DO iPoint = 1, numPatchPointsOp

       l0 = patchPointsOp(iPoint) + 1 ! assume the points coming in are from C (0-based)
       if(sconn(l0) == 1) then
         sbpBoundaryWeight = 1.0_8
         corner = 1
       else
         sbpBoundaryWeight = bcParams(3)
         corner = 0
       endif

       XI_T = 0.0_8
       IF(gridType >= RECTILINEAR) THEN
          gridJacobian = jacobianDeterminant(l0)
       ELSE
          gridJacobian = jacobianDeterminant(1)
       ENDIF
       CALL BoundaryPointMetric(numDim,normDir,numPointsBuffer,l0,gridType,&
            gridMetric,pointMetric)
       CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)

       xh = metricMags(1)*gridJacobian
       bndry_h = 1.0_8 / xh

       ! ... penalty parameter
       sigmaI1 = -SAT_sigmaI1 * dsgn
       
       ! ... pull off the boundary data 
       uBoundary(1) = rhoBuffer(l0)
       vDotXi = 0
       DO iDim = 1, numDim
          uBoundary(1+iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)
          bVelocity(iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)/rhoBuffer(l0)
          vDotXi = vDotXi + bVelocity(iDim)*metricUnitVectors(iDim)
       END DO
       uBoundary(numDim+2) = rhoEBuffer(l0)
       DO iDim = 1, numScalar
          uBoundary(numDim+2+iDim) = scalarBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO

       ! Access EOS for DV(p,T,v)
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uBoundary(1),dvBoundary(1))

       vWall(1:numDim) = vDotXi*metricUnitVectors(1:numDim)
       ! NON MOVING GRIDS!!
       vWallTarget(1:numDim) = 0.0_8
       !       = dot_product(XYZ_TAU(l0,1:ND),norm_vec(1:ND)) * norm_vec(1:ND) 


       DO iDim = 1,numDim
          vTarget(iDim) = bVelocity(iDim) - ( vWall(iDim) - vWallTarget(iDim) )
       END DO

       ! ... target data
       uTarget(1) = uBoundary(1)
       uTarget(numDim+2) = uBoundary(numDim+2)
       DO iDim = 1, numDim
          uTarget(1+iDim)   = uTarget(1) * vTarget(iDim)
          uTarget(numDim+2) = uTarget(numDim+2) + 0.5_8*uTarget(1)*(vTarget(iDim)**2 - bVelocity(iDim)**2)
       END DO
       DO iDim = 1, numScalar
          uTarget(numDim+2+iDim) = scalarBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO
       
       ! Access EOS for DV(p,T,v)
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uTarget(1),dvTarget(1))
       ! Get Roe State
       CALL PointRoeState2(numDim,numSpecies,uBoundary,dvBoundary,uTarget,dvTarget,uRoe)
       CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uRoe(1),dvRoe(1),dpRoe(1),cRoe)

       CALL EigenMatrices(numDim,numSpecies,uRoe,dvRoe,dpRoe,cRoe,xh, &
            metricUnitVectors,tMat,tInv,lambda)
 
       ! ... only pick those Lambda that are incoming
       If ( sgn == 1 ) Then
          Do jj = 1, numEquations
             Lambda(jj,jj) = max(Lambda(jj,jj),0.0_8)
          End Do
       Else 
          Do jj = 1, numEquations
             Lambda(jj,jj) = min(Lambda(jj,jj),0.0_8)
          End Do
       End If

       matX = MATMUL(Lambda,Tinv)
       
       ! ... compute the characteristic matrix
       Aprime = MATMUL(Tmat,matX)

       ! ... subtract off the target
       uBoundary(:) = uBoundary(:) - uTarget(:)

       ! ... compute the characteristic penalty vector
       gI2(:) = MATMUL(Aprime,uBoundary)

       penaltyFac = sbpBoundaryWeight*sigmaI1
       correction(:) = penaltyFac*gI2(:)

       !write(*,*) l0

       !if(l0 == 1326) then
         !write(*,*) "NSCBC Before RHS update, l0=", l0
         !write(*,*) "rhoRHS", rhoRHS(l0)
         !DO iDim = 1,numDim
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "rhoVRHS", iDim, rhoVRHS(pointIndex)
         !END DO
         !write(*,*) "rhoERHS", rhoERHS(l0)
         !DO iDim = 1,numScalar
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "rhoScalarRHS", iDim, scalarRHS(pointIndex)
         !END DO
         !write(*,*) "*********"
         !write(*,*) "Tmat"
         !write(*,*) Tmat 
         !write(*,*) "Tinv"
         !write(*,*) Tinv 
         !write(*,*) "lambda"
         !write(*,*) lambda 
         !write(*,*) "Aprime"
         !write(*,*) Aprime 
         !write(*,*) "correction"
         !write(*,*) correction
         !write(*,*) "dpRoe"
         !write(*,*) dpRoe
         !write(*,*) "ibfac_local"
         !write(*,*) ibfac_local
       !endif


       rhoRHS(l0) = rhoRHS(l0) + correction(1)
       DO iDim = 1,numDim
          pointIndex = l0 + (iDim-1)*numPointsBuffer
          rhoVRHS(pointIndex) = rhoVRHS(pointIndex) + correction(1+iDim)
       END DO
       rhoERHS(l0) = rhoERHS(l0) + correction(numDim+2)
       
       ! ... SAT_SLIP_ADIABATIC (species treatment)
       DO jj = 1, numscalar
          pointIndex = (jj-1)*numPointsBuffer + l0
          scalarRHS(pointIndex) = scalarRHS(pointIndex) + correction(numDim+2+jj)
       ENDDO

       !> @todo add viscous contribution for SAT_SLIP_ADIABATIC
       ! VISCOUS PART OMITTED (for now)


       IF(SC > 0) then
          penalty(:) = 0.0_8
          gI2(:) = 0.0_8
          
          !uBoundary(:) = 0
          ! ... scalar gradient boundary data 
          DO iScalar = 1, numScalar
             vDotXi = 0
             DO iDim = 1, numDim
                pointIndex = (numDim*(iScalar-1)+(iDim-1))*numPointsBuffer + l0
                vDotXi = vDotXi + scalarGrad(pointIndex)*metricUnitVectors(iDim)
             END DO
             !write(*,*) "pointIndex ", pointIndex
             !write(*,*) "norm_vec(1) ", norm_vec(1)
             !write(*,*) "scalarGradBuffer ", pointIndex
             uBoundary(2+ND+iScalar) = vDotXi*bndry_h
             !if(l0 .eq. 304) then
             !write(*,*) "normDir",normDir,"iScalar ",iScalar, "l0", l0
             !write(*,*) "scalarGrad_normDir",scalarGrad(pointIndex)
             !write(*,*) "scalar",scalarBuffer((iScalar-1)*numPointsBuffer+l0), &
             !scalarBuffer((iScalar-1)*numPointsBuffer+l0+1), &
             !scalarBuffer((iScalar-1)*numPointsBuffer+l0+2)
             !end if
          END DO

          ! MJA, I may need a factor or mu here
          !      have to modify the subroutine to take that buffer as input
          penaltyFac = (sbpBoundaryWeight/bndry_h)**2*sigmaI2/4.0_8 * &
               SCInv/rhoBuffer(l0)
               !SCInv*muBuffer(l0)/gI2(1)

          DO iScalar = 1, numScalar
            penalty(numDim+2+iScalar) = penaltyFac * (uBoundary(numDim+2+iScalar) - gI2(numDim+2+iScalar))
            ! don't enforce the gradient at corner points
            if(corner == 1) then
              penalty(numDim+2+iScalar) = 0.0_8
            endif
          END DO

          ! ... add to the rhs
          DO iScalar = 1,numScalar
            pointIndex = (iScalar-1)*numPointsBuffer + l0
            scalarRHS(pointIndex) = scalarRHS(pointIndex) - dsgn*penalty(2+ND+iScalar)
          ENDDO
       END IF
    END DO

    RETURN

  END SUBROUTINE SLIP_ADIABATIC

  SUBROUTINE NOSLIP_ISOTHERMAL(eosInfo,eosNonDimen,eosGasParams,numDim,&
       bufferSizes,numPointsBuffer,patchNormalDir,sconn, patchSizes,   &
       numPointsPatch,numPatchPointsOp,patchPointsOp,gridType,         &
       gridMetric,jacobianDeterminant,bcParams,gasParams,              &
       rhoBuffer,rhoVBuffer,rhoEBuffer,numscalar,scalarBuffer,         &
       scalarGrad,rhoRHS,rhoVRHS,rhoERHS, scalarRHS,rhoTarget,         &
       rhoVTarget,rhoETarget,scalarTarget,muBuffer,lambdaBuffer)
    
    IMPLICIT NONE

    INTEGER(KIND=8) :: eosInfo(NUMGASINFOS)
    INTEGER(KIND=4) :: numDim,numscalar,patchNormalDir,gridType
    INTEGER(KIND=8) :: numPointsBuffer,bufferSizes(numDim)
    INTEGER(KIND=4) :: sconn(numPointsBuffer)
    INTEGER(KIND=8) :: patchSizes(numDim),numPointsPatch
    INTEGER(KIND=8) :: numPatchPointsOp
    INTEGER(KIND=8) :: patchPointsOp(numPatchPointsOp)
    REAL(KIND=8)    :: eosNonDimen(NUMNONDIMENS)
    REAL(KIND=8)    :: eosGasParams(NUMGASPARAMS*(numScalar+1))
    REAL(KIND=8)    :: gridMetric(numDim*numDim*numPointsBuffer)
    REAL(KIND=8)    :: jacobianDeterminant(numPointsBuffer)
    REAL(KIND=8)    :: bcParams(4),gasParams(6)
    REAL(KIND=8)    :: rhoBuffer(numPointsBuffer)
    REAL(KIND=8)    :: rhoVBuffer(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoEBuffer(numPointsBuffer)
    REAL(KIND=8)    :: scalarBuffer(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: scalarGrad(numscalar*numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoRHS(numPointsBuffer)
    REAL(KIND=8)    :: rhoVRHS(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoERHS(numPointsBuffer)
    REAL(KIND=8)    :: scalarRHS(numscalar*numPointsBuffer)
    REAL(KIND=8)    :: rhoTarget(numPointsBuffer)
    REAL(KIND=8)    :: rhoVTarget(numDim*numPointsBuffer)
    REAL(KIND=8)    :: rhoETarget(numPointsBuffer)
    REAL(KIND=8)    :: scalarTarget(numScalar*numPointsBuffer)
    REAL(KIND=8)    :: muBuffer(numPointsBuffer)
    REAL(KIND=8)    :: lambdaBuffer(numPointsBuffer)


    ! ... Local variables
    INTEGER(4) :: ND, jj, iDim, iDir, iScalar
    INTEGER(8) :: Nc, l0, iPoint, pointIndex, metricOffset
    INTEGER(4) :: normDir, sgn ! , gas_dv_model
    INTEGER(4) :: numDebugPoints, iDebugPoint,debugPoints(6)
    REAL(8) :: dsgn, sndspdref2, invtempref, tempref, gamref, spcGasConst
    REAL(8) :: XI_X, XI_Y, XI_Z, XI_T, bndry_h,sbpBoundaryWeight, bcWallTemp
    REAL(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, gridJacobian
    REAL(8) :: rhoFac, penaltyFac, scalarPenaltyFac, scalarDiff

    REAL(8) :: bVelocity(numDim), vWall(numDim), vWallTarget(numDim), vTarget(numDim)
    REAL(8) :: pointMetric(numDim*numDim)
    REAL(8) :: metricUnitVectors(numDim*numDim)
    REAL(8) :: metricMags(numDim)
    REAL(8) :: dvBoundary(numDim+2), dvTarget(numDim+2)

    REAL(8) :: uRoe(numDim+2+numScalar),dvRoe(numDim+2),dpRoe(2+numScalar),cRoe
    REAL(8) :: uTarget(numDim+2+numScalar), gI2(numDim+2+numScalar), uBoundary(numDim+2+numScalar)
    REAL(8) :: penalty(numDim+2+numScalar), correction(numDim+2+numScalar)
    REAL(8) :: lambda(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: aPrime(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tMat(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: tInv(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: matX(numDim+2+numScalar,numDim+2+numScalar)
    REAL(8) :: dv2Roe(3)
    REAL(8) :: gamma, uCon, ubAux, vDotXi, xh
    REAL(8) :: sigmaI1, sigmaI2, sigmaAux, Sfactor, T_wall
    REAL(8) :: spd_snd, SC, SCInv, RE, REInv, SAT_sigmaI1, SAT_sigmaI2, pressureTarget
    REAL(8) :: viscousPenaltyScale
    INTEGER(4) :: numSpecies, numEquations
    INTEGER(4) :: corner
    INTEGER(8) :: eosContext

    LOGICAL :: TRIGGERED
    TRIGGERED = .FALSE.

    eosContext = eosInfo(1)

    !    numSpecies = 0
    numSpecies = numScalar
    ! ... Problem size
    ND = numDim
    Nc = numPointsBuffer
    numEquations = numDim+2+numSpecies

    ! ... Useful quantities about this boundary
    normDir = abs(patchNormalDir)
    sgn = normDir / patchNormalDir
    dsgn = dble(sgn)
    numDebugPoints = 0

    ! ... Reference quantities
    ! gasParams = (sndspdref,gamma,Cp,1/Re)
    ! bcParams(:)  = sigma1 sigma2
    ! MJA changed for new nonDimensionalization
    !sndspdref2 = gasParams(1)*gasParams(1) ! input%sndspdref * input%sndspdref
    !invtempref = gasParams(3)/sndspdref2  ! input%Cpref / sndspdref2
    !tempref    = 1.0_8 / invtempref 
    !gamref     = gasParams(2)
    !REinv      = gasParams(4)
    RE         = gasParams(1)
    SC         = gasParams(2)
    spcGasConst= gasParams(3)
    sndspdref2 = gasParams(5)*gasParams(5)
    tempref    = gasParams(4)
    invtempref = tempref
    IF(RE .gt. 0) REInv = 1.0_8/RE
    IF(SC .gt. 0) SCInv = 1.0_8/SC
    viscousPenaltyScale = gasParams(6)

    ! ... BC Constants
    ! bcParams = (sigma1,sbpBoundaryStencilWeight)
    SAT_sigmaI1                 = bcParams(1) ! input%SAT_sigmaI1_FF
    SAT_sigmaI2                 = bcParams(2) ! /input%SAT_sigmaI2
    bcWallTemp                  = bcParams(3) ! input%bcic_wallTemp
    sbpBoundaryWeight           = bcParams(4) ! 

    DO iDim = 1, numScalar
      dpRoe(iDim+2) = 0.0_8
    ENDDO

    ! ... loop over all of the points
    DO iPoint = 1, numPatchPointsOp

       l0 = patchPointsOp(iPoint) + 1 ! assume the points coming in are from C (0-based)

       ! MJA temporary for testing
       !if(l0 == 1446 .or. l0 == 626) then
         !write(*,*) "sconn(l0) ", sconn(l0)
       !endif

       if(sconn(l0) == 1) then
         sbpBoundaryWeight = 1.0_8
         corner = 1
       else
         sbpBoundaryWeight = bcParams(4)
         corner = 0
       endif
       
       XI_T = 0.0_8
       IF(gridType >= RECTILINEAR) THEN
          gridJacobian = jacobianDeterminant(l0)
       ELSE
          gridJacobian = jacobianDeterminant(1)
       ENDIF
       CALL BoundaryPointMetric(numDim,normDir,numPointsBuffer,l0,gridType,&
            gridMetric,pointMetric)
       CALL PointEigenVectors(numDim,pointMetric,metricUnitVectors,metricMags)
       xh = metricMags(1)*gridJacobian
       bndry_h = 1.0_8 / xh

       ! ... penalty parameter
       sigmaI1 = -SAT_sigmaI1 * dsgn
       
       ! ... pull off the boundary data 
       uBoundary(1) = rhoBuffer(l0)
       vDotXi = 0
       DO iDim = 1, numDim
          uBoundary(1+iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)
          bVelocity(iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)/rhoBuffer(l0)
          vDotXi = vDotXi + bVelocity(iDim)*metricUnitVectors(iDim)
       END DO
       uBoundary(numDim+2) = rhoEBuffer(l0)
       DO iDim = 1, numScalar
          uBoundary(numDim+2+iDim) = scalarBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO

       ! access EOS for DV(p,T,v)
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uBoundary(1),dvBoundary(1))

       vWall(1:numDim) = vDotXi*metricUnitVectors(1:numDim)
       ! NON MOVING GRIDS!!
       vWallTarget(1:numDim) = 0.0_8
       !       = dot_product(XYZ_TAU(l0,1:ND),norm_vec(1:ND)) * norm_vec(1:ND) 


       DO iDim = 1,numDim
          vTarget(iDim) = bVelocity(iDim) - ( vWall(iDim) - vWallTarget(iDim) )
       END DO

       ! ... target data
       !       uTarget(:) = cvTarget(l0,:)
       uTarget(1) = uBoundary(1)
       uTarget(numDim+2) = uBoundary(numDim+2)
       DO iDim = 1, numDim
          uTarget(1+iDim)   = uTarget(1) * vTarget(iDim)
          uTarget(numDim+2) = uTarget(numDim+2) + 0.5_8*uTarget(1)*(vTarget(iDim)**2 - bVelocity(iDim)**2)
       END DO
       DO iDim = 1, numScalar
          uTarget(numDim+2+iDim) = scalarBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO
       
       ! Access EOS for DV(p,T,v)
       CALL GASDV(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uTarget(1),dvTarget(1))

       ! Get Roe State
       CALL PointRoeState2(numDim,numSpecies,uBoundary,dvBoundary,uTarget,dvTarget,uRoe)
       CALL GASEOSROE(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),uRoe(1),dvRoe(1),dpRoe(1),cRoe)

       ! Get NS EigenMatrices(tMat,tInv) and EigenValues(lambda)
       CALL EigenMatrices(numDim,numSpecies,uRoe,dvRoe,dpRoe,cRoe,xh, &
            metricUnitVectors,tMat,tInv,lambda)

       !uCon    = vDotXi*xh

       ! ... only pick those Lambda that are incoming
       If ( sgn == 1 ) Then
          Do jj = 1, numEquations
             Lambda(jj,jj) = max(Lambda(jj,jj),0.0_8)
          End Do
       Else 
          Do jj = 1, numEquations
             Lambda(jj,jj) = min(Lambda(jj,jj),0.0_8)
          End Do
       End If

       matX = MATMUL(Lambda,Tinv)
       
       ! ... compute the characteristic matrix
       Aprime = MATMUL(Tmat,matX)

       ! ... subtract off the target
       uBoundary(:) = uBoundary(:) - uTarget(:)

       ! ... compute the characteristic penalty vector
       gI2(:) = MATMUL(Aprime,uBoundary)

       penaltyFac = sbpBoundaryWeight*sigmaI1
       correction(:) = penaltyFac*gI2(:)


       !if(l0 == 440) then
         !write(*,*) "*********"
         !write(*,*) "NSCBC Before RHS update, l0=", l0
         !write(*,*) "rho", rhoBuffer(l0)
         !DO iDim = 1,numDim
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "rhoV", iDim, rhoVBuffer(pointIndex)
         !END DO
         !write(*,*) "rhoE", rhoEBuffer(l0)
         !DO iDim = 1,numScalar
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "scalar", iDim, scalarBuffer(pointIndex)
         !END DO
!
         !write(*,*) "rhoRHS", rhoRHS(l0)
         !DO iDim = 1,numDim
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "rhoVRHS", iDim, rhoVRHS(pointIndex)
         !END DO
         !write(*,*) "rhoERHS", rhoERHS(l0)
         !DO iDim = 1,numScalar
           !pointIndex = l0+(iDim-1)*numPointsBuffer
           !write(*,*) "scalarRHS", iDim, scalarRHS(pointIndex)
         !END DO
         !!write(*,*) "Tmat"
         !!write(*,*) Tmat 
         !!write(*,*) "Tinv"
         !!write(*,*) Tinv 
         !!write(*,*) "lambda"
         !!write(*,*) lambda 
         !!write(*,*) "Aprime"
         !!write(*,*) Aprime 
         !!write(*,*) "correction"
         !!write(*,*) correction
       !endif




       ! Inviscid correction
       rhoRHS(l0) = rhoRHS(l0) + correction(1)
       DO iDim = 1,numDim
          pointIndex = l0 + (iDim-1)*numPointsBuffer
          rhoVRHS(pointIndex) = rhoVRHS(pointIndex) + correction(1+iDim)
       END DO
       rhoERHS(l0) = rhoERHS(l0) + correction(numDim+2)

       DO jj = 1, numscalar
          pointIndex = (jj-1)*numPointsBuffer + l0
          scalarRHS(pointIndex) = scalarRHS(pointIndex) + correction(numDim+2+jj)
       ENDDO
       
       ! VISCOUS PART
       ! uBoundary(:) = cv(l0,:)
       uBoundary(1) = rhoBuffer(l0)
       DO iDim = 1, numDim
          uBoundary(1+iDim) = rhoVBuffer(l0+(iDim-1)*numPointsBuffer)
       END DO
       uBoundary(numDim+2) = rhoEBuffer(l0)
       
       ! ... wall temperature
       ! T_wall = bcic_WallTemp / ((gamref - 1.0_8)*tempref)
       ! new nondimensionalization...can be moved outside this loop altogether
       !T_wall = bcWallTemp / tempref
       T_wall = bcWallTemp

       ! ... target state
       gI2(:) = 0.0_8
       gI2(1) = rhoBuffer(l0)
               
       ! ... wall velocity
       ! we populate gI2 for the active scalars to get the energy at the boundary temperature
       ! the boundary condition on the scalars is computed later
       DO iScalar = 1, numscalar
          pointIndex = (iScalar-1)*numPointsBuffer + l0
          uBoundary(2+numDim+iScalar) = scalarBuffer(pointIndex)
          gI2(2+numDim+iScalar) = scalarBuffer(pointIndex)
       END DO

       IF(T_wall < 0.0_8) THEN
          gI2(numDim+2) = rhoETarget(l0)
       ELSE
          CALL GASISOTHERMAL(numDim,eosInfo(1),eosGasParams(1),eosNonDimen(1),T_wall,gI2(1))

       ENDIF

       sigmaI2 = -SAT_sigmaI2

       penaltyFac = (sbpBoundaryWeight/bndry_h)**2*sigmaI2/4.0_8 * &
            REInv*viscousPenaltyScale*max(muBuffer(l0),lambdaBuffer(l0))/gI2(1)
       penalty(:) = 0.0_8
       penalty(1:numDim+2) = penaltyFac * (uBoundary(1:numDim+2) - gI2(1:numDim+2))
       
       ! ... compute penalty

       ! now populate the target and boundary states for scalars, dot(grad(Y),n) = 0
       IF (SC > 0) THEN
          DO iScalar = 1, numScalar
             vDotXi = 0.0_8
             DO iDim = 1, numDim
                pointIndex = (numDim*(iScalar-1)+(iDim-1))*numPointsBuffer + l0
                vDotXi = vDotXi + scalarGrad(pointIndex)*metricUnitVectors(iDim)
                !if(l0 == 1446) then
                  !write(*,*) "iScalar", iScalar,"iDim",iDim,"ScalarGrad", scalarGrad(pointIndex)
                  !write(*,*) "iScalar", iScalar,"iDim",iDim,"metricUnitVectors", metricUnitVectors(iDim)
                !endif
             END DO
             !uBoundary(2+ND+iScalar) = vDotXi*xh
             !uBoundary(2+ND+iScalar) = vDotXi
             uBoundary(2+ND+iScalar) = vDotXi*bndry_h
          END DO
          !write(*,*) "sbpBoundryWeight", sbpBoundaryWeight
          !write(*,*) "bndry_h", bndry_h
          !write(*,*) "xh", xh
          !write(*,*) "gridJacobian", gridJacobian
          penaltyFac = (sbpBoundaryWeight/bndry_h)**2*sigmaI2/4.0_8 * &
               SCInv*muBuffer(l0)/gI2(1)
               !REInv*viscousPenaltyScale*max(muBuffer(l0),lambdaBuffer(l0))/gI2(1)
               
          !penaltyFac = 0.5_8
          DO iScalar = 1, numScalar
            gI2(numDim+2+iScalar) = 0.0_8
            penalty(numDim+2+iScalar) = penaltyFac * (uBoundary(numDim+2+iScalar) - gI2(numDim+2+iScalar))
            ! don't enforce the gradient at corner points
            if(corner == 1) then
              penalty(numDim+2+iScalar) = 0.0_8
            endif
          END DO
       ENDIF

       ! calculate the penalty

       !if(l0 == 1246) then
       !write(*,*) "Before Viscid"
       !write(*,*) "T_wall"
       !write(*,*) 'uB:',uB
       !write(*,*) 'gI2:',gI2
       !write(*,*) 'rhs:',rhoVRHS(l0),rhoVRHS(l0+numPointsBuffer),rhoVRHS(l0+2*numPointsBuffer),rhoERHS(l0)
       !write(*,*) 'penalty: ',penalty
       !write(*,*) "uB(:)"
       !write(*,*) uB
       !write(*,*) "gI2"
       !write(*,*) gI2 
       !write(*,*) "penalty"
       !write(*,*) penalty
       !endif
       
       ! ... add to the rhs
       ! do jj = 1, ND+2
       !    rhs(l0,jj) = rhs(l0,jj) + ibfac_local * penalty(jj)
       ! end do
       rhoRHS(l0) = rhoRHS(l0) + penalty(1)
       DO iDim = 1,numDim
          pointIndex = l0 + (iDim-1)*numPointsBuffer
          rhoVRHS(pointIndex) = rhoVRHS(pointIndex) + penalty(1+iDim)
       END DO
       rhoERHS(l0) = rhoERHS(l0) + penalty(numDim+2)

       DO iScalar = 1,numScalar
         pointIndex = (iScalar-1)*numPointsBuffer + l0
         scalarRHS(pointIndex) = scalarRHS(pointIndex) - dsgn*penalty(2+ND+iScalar)
       ENDDO

    END DO

    RETURN

  END SUBROUTINE NOSLIP_ISOTHERMAL


  SUBROUTINE DISSIPATIONWEIGHT(numDim,bufferSize,numPoints,bufferInterval,   &
       sigmaDissipation,sigmaDilatation,dilatationRamp,dilatationCutoff,     &
       divV,sigmaDiss)
    
    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)  :: numDim
    INTEGER(KIND=8), INTENT(IN)  :: bufferSize(numDim),  numPoints
    INTEGER(KIND=8), INTENT(IN)  :: bufferInterval(2*numDim) 
    REAL(KIND=8),    INTENT(IN)  :: sigmaDissipation,sigmaDilatation
    REAL(KIND=8),    INTENT(IN)  :: dilatationRamp,dilatationCutoff
    REAL(KIND=8),    INTENT(IN)  :: divV(numPoints)
    REAL(KIND=8),    INTENT(OUT) :: sigmaDiss(numPoints)
    
    INTEGER(KIND=8) :: I, J, K
    INTEGER(KIND=8) :: nPlane, zIndex, yIndex, yzIndex, bufferIndex, xSize
    INTEGER(KIND=8) :: iStart,iEnd,jStart,jEnd,kStart,kEnd

    iStart = bufferInterval(1)
    iEnd   = bufferInterval(2)
    xSize  = bufferSize(1)

    IF(numDim == 1) THEN
       DO I = iStart, iEnd
          sigmaDiss(I) = (sigmaDissipation + &
               sigmaDilatation*0.5_8*(1.0_8 + TANH(dilatationRamp*(dilatationCutoff-divV(I)))))  
       END DO
    ELSE IF(numDim == 2) THEN
       jStart = bufferInterval(3)
       jEnd   = bufferInterval(4)
       DO J = jStart, jEnd
          yIndex = (J-1)*xSize
          DO I = iStart, iEnd
             bufferIndex = yIndex + I
             sigmaDiss(bufferIndex) = (sigmaDissipation + &
                  sigmaDilatation*0.5_8*(1.0_8 + TANH(dilatationRamp*(dilatationCutoff-divV(bufferIndex)))))  
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
                sigmaDiss(bufferIndex) = (sigmaDissipation + &
                     sigmaDilatation*0.5_8*(1.0_8 + TANH(dilatationRamp*(dilatationCutoff-divV(bufferIndex)))))
             END DO
          END DO
       END DO
    ENDIF
       
       
  END SUBROUTINE DISSIPATIONWEIGHT

END MODULE SATUtil
