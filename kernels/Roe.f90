MODULE Roe

  USE GRID
  USE SIMPLE
  USE SPECIAL
  USE LIMITS

  IMPLICIT NONE

CONTAINS

  ! Calculate the Roe averaged state 
  ! R(f) = (sqrt(rhoR)*fR + sqrt(rhoL)*fL)/(sqrt(rhoR)+sqrt(rhoL))
  ! 
  ! Calculates the Roe average for rho, v, H, and Y
  ! Where H is the total enthalpy (h+0.5*V^2)
  !
  ! u is (rho,rhoV,rhoE) dv is (p,T,v)
  SUBROUTINE PointRoeState2(numDim,numScalar,uLeft,dvLeft,uRight,dvRight,uRoe)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)            :: numDim, numScalar
    REAL(KIND=8),    INTENT(IN),    TARGET :: uLeft(numDim+2+numScalar)
    REAL(KIND=8),    INTENT(IN),    TARGET :: dvLeft(numDim+2)
    REAL(KIND=8),    INTENT(IN),    TARGET :: uRight(numDim+2+numScalar)
    REAL(KIND=8),    INTENT(IN),    TARGET :: dvRight(numDim+2)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: uRoe(numDim+2+numScalar)

    INTEGER(KIND=4) :: numEquations, iDim, iScalar
    INTEGER(KIND=8) :: bufferLeftdex,bufferOffset
    REAL(KIND=8)    :: keLeft,hLeft,keRight,hRight,keRoe,hRoe,vRoe2
    REAL(KIND=8)    :: rhoLeft,rhoELeft,pLeft,pRight,pRoe,tRoe,rhoERight
    REAL(KIND=8)    :: rhoLeftN1,rhoRightN1,rhoRoeN1,rhoRoe,rhoRight
    REAL(KIND=8)    :: cc, bb

    rhoLeft   =  uLeft(1)
    rhoELeft  =  uLeft(numDim+2)
    rhoLeftN1 =  1.0_8/rhoLeft
    pLeft     =  dvLeft(1)

    keLeft = 0.0_8
    DO iDim = 1,numDim
       keLeft = keLeft + dvLeft(iDim+2)*dvLeft(iDim+2)
    END DO
    keLeft = .5_8 * keLeft
    hLeft  = (rhoELeft + pLeft)*rhoLeftN1; ! enthalpy H (includes KE)

    
    rhoRight   =  uRight(1)
    rhoERight  =  uRight(numDim+2)
    rhoRightN1 =  1.0_8/rhoRight
    pRight     =  dvRight(1)

    keRight = 0.0_8
    DO iDim = 1,numDim
       keRight = keRight + dvRight(iDim+2)*dvRight(iDim+2)
    END DO
    keRight = .5_8 * keRight
    hRight  = (rhoERight + pRight)*rhoRightN1; ! enthalpy H (includes KE)


    cc = SQRT(rhoRight*rhoLeftN1)
    bb = 1.0_8/(1.0_8 + cc)
    uRoe(1) = uLeft(1)*cc  ! Roe rho
    rhoRoeN1 = 1.0_8/uRoe(1)
    vRoe2 = 0.0_8
    DO iDim = 1, numDim
       uRoe(iDim+1)  = uRoe(1)*(dvLeft(iDim+2)+dvRight(iDim+2)*cc)*bb ! Roe rhoV
    END DO

    uRoe(numDim+2) = (hLeft + hRight*cc)*bb      ! Roe enthalpy (H)
    IF(numScalar .gt. 0) THEN
       DO iScalar = 1, numScalar
          bufferLeftdex = numDim+2+iScalar
          uRoe(bufferLeftdex) = uRoe(1)*(uLeft(bufferLeftdex)*rhoLeftN1+ &
               uRight(bufferLeftdex)*rhoRightN1*cc)*bb  ! Roe rhoY
       END DO
    END IF
    
  END SUBROUTINE PointRoeState2


! MJA
! deprecated in favor of PointRoeState2, which does not calculate the average energy
! only the average enthalpy
SUBROUTINE PointRoeState(numDim,numScalar,uIn,dvIn,uOut,dvOut,uRoe,dvRoe,dv2Roe)

    IMPLICIT NONE

    INTEGER(KIND=4), INTENT(IN)            :: numDim, numScalar
    REAL(KIND=8),    INTENT(IN),    TARGET :: uIn(numDim+2+numScalar)
    REAL(KIND=8),    INTENT(IN),    TARGET :: dvIn(numDim+2)
    REAL(KIND=8),    INTENT(IN),    TARGET :: uOut(numDim+2+numScalar)
    REAL(KIND=8),    INTENT(IN),    TARGET :: dvOut(numDim+2)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: uRoe(numDim+2+numScalar)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: dvRoe(numDim+2)
    REAL(KIND=8),    INTENT(INOUT), TARGET :: dv2Roe(3)

    INTEGER(KIND=4) :: numEquations, iDim, iScalar
    INTEGER(KIND=8) :: bufferIndex,bufferOffset
    REAL(KIND=8)    :: keIn,hIn,keOut,hOut,keRoe,hRoe,vRoe2
    REAL(KIND=8)    :: rhoIn,rhoEIn,pIn,pOut,pRoe,tRoe,rhoEOut
    REAL(KIND=8)    :: rhoInN1,rhoOutN1,rhoRoeN1,rhoRoe,rhoOut
    REAL(KIND=8)    :: eIn, eOut, eRoe
    REAL(KIND=8)    :: cc, bb

    rhoIn   =  uIn(1)
    rhoEIn  =  uIn(numDim+2)
    rhoInN1 =  1.0_8/rhoIn
    pIn     =  dvIn(1)

    keIn = 0.0_8
    DO iDim = 1,numDim
       keIn = keIn + dvIn(iDim+2)*dvIn(iDim+2)
    END DO
    keIn = .5_8 * keIn
    eIn  = rhoEIn*rhoInN1 - keIn;
    hIn  = (rhoEIn + pIn)*rhoInN1;

    
    rhoOut   =  uOut(1)
    rhoEOut  =  uOut(numDim+2)
    rhoOutN1 =  1.0_8/rhoOut
    pOut     =  dvOut(1)

    keOut = 0.0_8
    DO iDim = 1,numDim
       keOut = keOut + dvOut(iDim+2)*dvOut(iDim+2)
    END DO
    keOut = .5_8 * keOut
    eOut  = rhoEOut*rhoOutN1 - keOut;
    hOut  = (rhoEOut + pOut)*rhoOutN1;


    cc = SQRT(rhoOut*rhoInN1)
    bb = 1.0_8/(1.0_8 + cc)
    uRoe(1) = uIn(1)*cc  ! Roe rho
    rhoRoeN1 = 1.0_8/uRoe(1)
    vRoe2 = 0.0_8
    DO iDim = 1, numDim
       dvRoe(iDim+2) = (dvIn(iDim+2)+dvOut(iDim+2)*cc)*bb  ! Roe V
       uRoe(iDim+1)  = uRoe(1)*dvRoe(iDim+2)               ! Roe rhoV
       vRoe2 = vRoe2 + dvRoe(iDim+2)*dvRoe(iDim+2)
    END DO
    keRoe = .5_8*vRoe2                       ! Roe ke
    eRoe  = (eIn + eOut*cc)*bb               ! Roe e
    uRoe(numDim+2) = uRoe(1)*(eRoe+keRoe)    ! Roe rhoE
    hRoe  = (hIn + hOut*cc)*bb               ! Roe enthalpy
    dvRoe(2) = hRoe - keRoe                  ! Roe temperature
    dvRoe(1) = uRoe(1)*hRoe - uRoe(numDim+2) ! Roe pressure
    IF(numScalar .gt. 0) THEN
       DO iScalar = 1, numScalar
          bufferIndex = numDim+2+iScalar
          uRoe(bufferIndex) = uRoe(1)*(uIn(bufferIndex)*rhoInN1+ &
               uOut(bufferIndex)*rhoOutN1*cc)*bb  ! Roe rhoY
       END DO
    END IF
    
    dv2Roe(1) = eRoe
    dv2Roe(2) = keRoe
    dv2Roe(3) = hRoe
    
  END SUBROUTINE PointRoeState

  subroutine cross_product1(v1,v2,y)

    Implicit None

    Real(KIND=8) :: v1(3), v2(3), y(3)

    y(1) = v1(2)*v2(3) - v1(3)*v2(2)
    y(2) = v1(3)*v2(1) - v1(1)*v2(3)
    y(3) = v1(1)*v2(2) - v1(2)*v2(1)

    return

  end subroutine cross_product1

  ! Point metric coming in is already g*J
  ! The Roe EigenMatrices are:
  ! Forward transformation eigenvector matrix: Pfor
  ! Inverse transformation eigenvector matrix: Pinv
  ! Eigenvalue diagonal matrix: lambda
  SUBROUTINE EigenMatrices(numDim,numScalars,qRoe,dvRoe,dp,soundSpeed, &
       gridScale,metricEigenVectors,Pfor,Pinv,lambda)
    
    USE MetricOps

    IMPLICIT NONE
    
    INTEGER(KIND=4), INTENT(IN)  :: numDim,numScalars
    REAL(KIND=8),    INTENT(IN)  :: qRoe(numDim+2+numScalars), dvRoe(numDim+2)
    REAL(KIND=8),    INTENT(IN)  :: dp(2+numScalars)
    REAL(KIND=8),    INTENT(IN)  :: soundSpeed,gridScale
    REAL(KIND=8),    INTENT(IN)  :: metricEigenVectors(numDim*numDim)
    REAL(KIND=8),    INTENT(OUT) :: Pinv(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8),    INTENT(OUT) :: Pfor(numDim+2+numScalars,numDim+2+numScalars)
    REAL(KIND=8),    INTENT(OUT) :: lambda(numDim+2+numScalars,numDim+2+numScalars)
    
    ! Local vars
    REAL(KIND=8)    :: RHO,SPD_SND_INV,metricMag
    REAL(KIND=8)    :: ENTHALPY,KE,SPD_SND,SPVOL,MACH2
    REAL(KIND=8)    :: alpha,beta,theta,b1,b2,b3,tempVec(3),tempVec2(3)
    REAL(KIND=8)    :: k_x,k_y,k_z,l_x,l_y,l_z,m_x,m_y,m_z
    REAL(KIND=8)    :: metricBasis(9),basisComponents(9)
    REAL(KIND=8)    :: K(3),L(3),M(3)
    REAL(KIND=8)    :: uCon,uhat,vhat,what,ja
    REAL(KIND=8)    :: zSpecies(NUM_SCALARS_MAX+1)
    INTEGER(KIND=4) :: iDim,iScalar,iEquation,numEquations,offset,jj

    numEquations = numDim + 2 + numScalars
    
    lambda = 0.0_8
    Pinv = 0.0_8
    Pfor = 0.0_8

    RHO = qRoe(1)
    SPVOL = 1.0_8/RHO
    KE = 0.0_8
    theta = 0.0_8
    DO iDim = 1,numDim
       KE = KE + dvRoe(2+iDim)*dvRoe(2+iDim)
       theta = theta + dvRoe(2+iDim)*metricEigenVectors(iDim)
    END DO
    uCon = gridScale*theta
    KE = .5_8 * KE
    ENTHALPY = (qRoe(numDim+2) + dvRoe(1))*SPVOL
!    ENTHALPY = dvRoe(2) + KE ! Packed 
    SPD_SND = soundSpeed
    SPD_SND_INV = 1.0_8/soundSpeed
    MACH2 = 2.0_8*KE*SPD_SND_INV*SPD_SND_INV

    b1 = dp(2)*SPVOL / SPD_SND**2 ! = 1/T for single-component gas
    b2 = 1.0_8 + b1 * 2.0_8 * KE - b1 * ENTHALPY
    b3 = 0.0_8
    DO iScalar = 1,numScalars
       zSpecies(iScalar) = -RHO * dp(2+iScalar)/dp(2)
       b3 = b3 + zSpecies(iScalar)*qRoe(numDim+2+iScalar)
    ENDDO
    b3 = b3*SPVOL*b1
    b2 = b2 + b3

    lambda(:,:) = 0.0_8

    DO iEquation = 1, numEquations
       lambda(iEquation,iEquation) = uCon
    END DO
    lambda(numDim+1,numDim+1) = lambda(numDim+1,numDim+1) + SPD_SND*gridScale
    lambda(numDim+2,numDim+2) = lambda(numDim+2,numDim+2) - SPD_SND*gridScale

    alpha = RHO / (2.0_8 * SPD_SND)

    IF(numDim == 2) THEN

    ! ... the forward matrix P
       Pfor(1,1) = 1.0_8
       Pfor(1,2) = 0.0_8
       Pfor(1,3) = alpha
       Pfor(1,4) = alpha

       Pfor(2,1) = dvRoe(3)
       Pfor(2,2) = RHO * metricEigenVectors(2)
       Pfor(2,3) = alpha * (dvRoe(3) + soundSpeed * metricEigenVectors(1))
       Pfor(2,4) = alpha * (dvRoe(3) - soundSpeed * metricEigenVectors(1))

       Pfor(3,1) = dvRoe(4)
       Pfor(3,2) = -RHO * metricEigenVectors(1)
       Pfor(3,3) = alpha * (dvRoe(4) + soundSpeed * metricEigenVectors(2))
       Pfor(3,4) = alpha * (dvRoe(4) - soundSpeed * metricEigenVectors(2))
       
       Pfor(4,1) = ENTHALPY - 1.0_8/b1 ! This is KE for single-component 
       Pfor(4,2) = RHO * (dvRoe(3) * metricEigenVectors(2) - dvRoe(4) * metricEigenVectors(1))
       Pfor(4,3) = alpha * (ENTHALPY + soundSpeed * theta)
       Pfor(4,4) = alpha * (ENTHALPY - soundSpeed * theta)

       DO iScalar = 1, numScalars
          jj = 4 + iScalar
          Pfor(4,jj)  = zSpecies(iScalar)
          Pfor(jj,1)  = qRoe(jj)*SPVOL
          Pfor(jj,3)  = alpha * qRoe(jj)*SPVOL
          Pfor(jj,4)  = alpha * qRoe(jj)*SPVOL
          Pfor(jj,jj) = 1.0_8
       END DO

       ! ... the inverse matrix Pinv
       Pinv(1,1) = 1.0_8 - b2
       Pinv(1,2) =  b1 * dvRoe(3)
       Pinv(1,3) =  b1 * dvRoe(4)
       Pinv(1,4) = -b1

       Pinv(2,1) =  SPVOL * (dvRoe(4) * metricEigenVectors(1) - dvRoe(3) * metricEigenVectors(2))
       Pinv(2,2) =  SPVOL * metricEigenVectors(2)
       Pinv(2,3) = -SPVOL * metricEigenVectors(1)
       Pinv(2,4) = 0.0_8
       
       Pinv(3,1) = soundSpeed * SPVOL * (b2 - theta * SPD_SND_INV)
       Pinv(3,2) = SPVOL * (metricEigenVectors(1) - b1 * dvRoe(3) * SPD_SND)
       Pinv(3,3) = SPVOL * (metricEigenVectors(2) - b1 * dvRoe(4) * SPD_SND)
       Pinv(3,4) = b1 * SPVOL * SPD_SND

       Pinv(4,1) = soundSpeed * SPVOL * (b2 + theta*SPD_SND_INV)
       Pinv(4,2) =-SPVOL * (metricEigenVectors(1) + b1 * dvRoe(3) * SPD_SND)
       Pinv(4,3) =-SPVOL * (metricEigenVectors(2) + b1 * dvRoe(4) * SPD_SND)
       Pinv(4,4) = b1 * SPVOL * SPD_SND


       DO iScalar = 1,numScalars
          jj = 4 + iScalar
          Pinv(1,jj)  = b1 * zSpecies(iScalar)
          Pinv(3,jj)  = -SPVOL * b1 * SPD_SND * zSpecies(iScalar)
          Pinv(4,jj)  = -SPVOL * b1 * SPD_SND * zSpecies(iScalar)
          Pinv(jj,1)  = -qRoe(jj)*SPVOL
          Pinv(jj,jj) = 1.0_8
       END DO

    ELSE IF (numDim == 3) THEN

       beta = 1.0_8 / (2.0_8 * alpha)
       
       CALL POINTEIGENVECTORSGS(numDim,metricEigenVectors,metricBasis,basisComponents)

       k_x = metricEigenVectors(1)
       k_y = metricEigenVectors(2)
       k_z = metricEigenVectors(3)
       l_x = metricBasis(4)
       l_y = metricBasis(5)
       l_z = metricBasis(6)
       m_x = metricBasis(7)
       m_y = metricBasis(8)
       m_z = metricBasis(9)

!       CALL VECTORCROSSPRODUCT(metricEigenVectors(1:3),metricEigenVectors(4:6),tempVec)
!       metricMag = SQRT(tempVec(1)**2 + tempVec(2)**2 + tempVec(3)**2)
!       metricMag = 1.0_8/metricMag
!       tempVec2(1) = tempVec(1)*metricMag
!       tempVec2(2) = tempVec(2)*metricMag
!       tempVec2(3) = tempVec(3)*metricMag
!!       l_x = tempVec2(1)
!       l_y = tempVec2(2)
!       l_z = tempVec2(3)
       
!       CALL VECTORCROSSPRODUCT(metricEigenVectors(1:3),tempVec2,tempVec)
!       metricMag = SQRT(tempVec(1)**2 + tempVec(2)**2 + tempVec(3)**2)
!       metricMag = 1.0_8/metricMag
!       m_x = tempVec(1)*metricMag
!       m_y = tempVec(2)*metricMag
!       m_z = tempVec(3)*metricMag

       uhat = theta ! k_x * UX + k_y * dvRoe(4) + k_z * dvRoe(5)
       vhat = l_x * dvRoe(3) + l_y * dvRoe(4) + l_z * dvRoe(5)
       what = m_x * dvRoe(3) + m_y * dvRoe(4) + m_z * dvRoe(5)
       
       Pfor(1,1) = 1.0_8
       Pfor(1,2) = 0.0_8
       Pfor(1,3) = 0.0_8
       Pfor(1,4) = alpha
       Pfor(1,5) = alpha

       Pfor(2,1) = dvRoe(3)
       Pfor(2,2) = RHO * l_x
       Pfor(2,3) = RHO * m_x
       Pfor(2,4) = alpha * (dvRoe(3) + SPD_SND * k_x)
       Pfor(2,5) = alpha * (dvRoe(3) - SPD_SND * k_x)

       Pfor(3,1) = dvRoe(4)
       Pfor(3,2) = RHO * l_y
       Pfor(3,3) = RHO * m_y
       Pfor(3,4) = alpha * (dvRoe(4) + SPD_SND * k_y)
       Pfor(3,5) = alpha * (dvRoe(4) - SPD_SND * k_y)

       Pfor(4,1) = dvRoe(5)
       Pfor(4,2) = RHO * l_z
       Pfor(4,3) = RHO * m_z
       Pfor(4,4) = alpha * (dvRoe(5) + SPD_SND * k_z)
       Pfor(4,5) = alpha * (dvRoe(5) - SPD_SND * k_z)

       Pfor(5,1) = ENTHALPY - 1.0_8/b1 ! = KE for single-component gas
       Pfor(5,2) = RHO * vhat
       Pfor(5,3) = RHO * what
       Pfor(5,4) = alpha * (ENTHALPY + SPD_SND * uhat)
       Pfor(5,5) = alpha * (ENTHALPY - SPD_SND * uhat)
       
       DO iScalar = 1,numScalars
          jj = 5 + iScalar
          Pfor(5,jj)  = zSpecies(iScalar)
          Pfor(jj,1)  = qRoe(jj)*SPVOL
          Pfor(jj,4)  = alpha*qRoe(jj)*SPVOL
          Pfor(jj,5)  = alpha*qRoe(jj)*SPVOL
          Pfor(jj,jj) = 1.0_8
       END DO
          
       Pinv(1,1) = 1.0_8 - b2
       Pinv(1,2) = b1 * dvRoe(3)
       Pinv(1,3) = b1 * dvRoe(4)
       Pinv(1,4) = b1 * dvRoe(5)
       Pinv(1,5) = -b1

       Pinv(2,1) = -vhat * SPVOL
       Pinv(2,2) = l_x * SPVOL
       Pinv(2,3) = l_y * SPVOL
       Pinv(2,4) = l_z * SPVOL
       Pinv(2,5) = 0.0_8

       Pinv(3,1) = -what * SPVOL
       Pinv(3,2) = m_x * SPVOL
       Pinv(3,3) = m_y * SPVOL
       Pinv(3,4) = m_z * SPVOL
       Pinv(3,5) = 0.0_8

       Pinv(4,1) =  beta * (b2 - uhat / SPD_SND)
       Pinv(4,2) = -beta * (-k_x / SPD_SND + b1 * dvRoe(3))
       Pinv(4,3) = -beta * (-k_y / SPD_SND + b1 * dvRoe(4))
       Pinv(4,4) = -beta * (-k_z / SPD_SND + b1 * dvRoe(5))
       Pinv(4,5) =  beta * b1

       Pinv(5,1) =  beta * (b2 + uhat / SPD_SND)
       Pinv(5,2) = -beta * (k_x / SPD_SND + b1 * dvRoe(3))
       Pinv(5,3) = -beta * (k_y / SPD_SND + b1 * dvRoe(4))
       Pinv(5,4) = -beta * (k_z / SPD_SND + b1 * dvRoe(5))
       Pinv(5,5) =  beta * b1

       DO iScalar = 1,numScalars
          jj = 5 + iScalar
          Pinv(1,jj)  = b1*zSpecies(iScalar)
          Pinv(4,jj)  = -b1*zSpecies(iScalar)*SPD_SND*SPVOL
          Pinv(5,jj)  = -b1* zSpecies(iScalar)*SPD_SND*SPVOL
          Pinv(jj,1)  = -qRoe(jj)*SPVOL
          Pinv(jj,jj) = 1.0_8
       END DO

    END IF
    
  END SUBROUTINE EigenMatrices


  ! dvRoe = (p,T,v)
  subroutine Form_Roe_State(ND, numScalars, u_in, u_out, gamma, norm, uRoe, dvRoe,&
       ucon,cRoe,theta)

    IMPLICIT NONE

    INTEGER(4), INTENT(IN) :: ND,numScalars
    Real(8), INTENT(IN)    :: u_in(ND+2), u_out(ND+2), norm(ND), gamma
    REAL(8), INTENT(OUT)   :: uRoe(ND+2),dvRoe(ND+3),ucon,cRoe,theta

    Real(8) :: rho_in, ux_in, uy_in, uz_in, ke_in, p_in, en_in, h_in
    Real(8) :: gm1, rho_out, ux_out, uy_out, uz_out, ke_out, p_out, en_out, h_out
    Real(8) :: cc, bb, ux_Roe, uy_Roe, uz_Roe, h_Roe, ke_Roe, spd_snd_Roe, rho_Roe
    Real(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, rho, phi2
    Real(8) :: XI_X, XI_Y, XI_Z, denom, e_Roe

    INTEGER(KIND=4) :: iDim
    Integer :: jj

    ! ... size of this problem
    !ND = size(u_in) - 2

    ! ... constants
    gm1 = gamma - 1.0_8

    ! ... form primative variables from "in"
    rho_in = u_in(1)
    ux_in  = u_in(2) / rho_in; uy_in = 0.0_8; uz_in = 0.0_8
    if (ND >= 2) uy_in  = u_in(3) / rho_in
    if (ND == 3) uz_in  = u_in(4) / rho_in
    ke_in  = 0.5_8 * (ux_in**2 + uy_in**2 + uz_in**2)
    en_in  = u_in(ND+2)/rho_in - ke_in
    p_in   = gm1 * rho_in * en_in
    h_in   = (gamma / gm1) * p_in / rho_in + ke_in 

    ! ... form primative variables from "out"
    rho_out = u_out(1)
    ux_out  = u_out(2) / rho_out; uy_out = 0.0_8; uz_out = 0.0_8
    if (ND >= 2) uy_out  = u_out(3) / rho_out
    if (ND == 3) uz_out  = u_out(4) / rho_out
    ke_out  = 0.5_8 * (ux_out**2 + uy_out**2 + uz_out**2)
    en_out  = u_out(ND+2)/rho_out - ke_out
    p_out   = gm1 * rho_out * en_out
    h_out   = (gamma / gm1) * p_out / rho_out + ke_out 

    ! ... form Roe variables
    cc = sqrt(rho_out / rho_in)
    bb = 1.0_8 / (1.0_8 + cc)
    uRoe(1) = rho_in * cc
    dvRoe(3) = (Ux_in + Ux_out * cc) * bb
    dvRoe(4) = (Uy_in + Uy_out * cc) * bb
    ux_Roe = dvRoe(3)
    uy_Roe = dvRoe(4)
    uz_Roe = 0.0_8
    IF(ND == 3) THEN
       dvRoe(5) = (Uz_in + Uz_out * cc) * bb
       uz_Roe = dvRoe(5)
    ENDIF
    h_Roe  = ( h_in +  h_out * cc) * bb
    e_Roe  = (en_in + en_out*cc)*bb
    ke_Roe = 0.0_8
    DO iDim = 1,ND
       uRoe(1+iDim) = uRoe(1)*dvRoe(2+iDim)
       ke_Roe = ke_Roe + dvRoe(2+iDim)*dvRoe(2+iDim)
    END DO
    ke_Roe = 0.5_8 * ke_Roe
    uRoe(ND+2) = uRoe(1)*(e_Roe + ke_Roe)
    dvRoe(2) = h_Roe - ke_Roe
    cRoe = sqrt(gm1*dvRoe(2))
    dvRoe(1) = uRoe(1)*h_Roe - uRoe(ND+2)
   
    ! ... variable renaming
    XI_X = norm(1); XI_Y = 0.0_8; XI_Z = 0.0_8
    if (ND >= 2) XI_Y = norm(2)
    if (ND == 3) XI_Z = norm(3)
    denom = 1.0_8 / sqrt(XI_X**2 + XI_Y**2 + XI_Z**2)
    XI_X_TILDE = XI_X * denom
    XI_Y_TILDE = XI_Y * denom
    XI_Z_TILDE = XI_Z * denom
    RHO = rho_Roe

    ucon  = ux_Roe * XI_X + uy_Roe * XI_Y + uz_Roe * XI_Z
    theta = ux_Roe * XI_X_TILDE + uy_Roe * XI_Y_TILDE + uz_Roe * XI_Z_TILDE
 
  end subroutine Form_Roe_State

  subroutine Form_Roe_Matrices(ND, u_in, u_out, gamma, norm, tmat, tinv, lambda)

!    USE ModGlobal

    IMPLICIT NONE

    INTEGER(KIND=4) :: ND
    Real(8) :: u_in(ND+2), u_out(ND+2), tmat(ND+2,ND+2), tinv(ND+2,ND+2), norm(ND), lambda(ND+2,ND+2)
    Real(8) :: gamma, rho_in, ux_in, uy_in, uz_in, ke_in, p_in, en_in, h_in
    Real(8) :: gm1, rho_out, ux_out, uy_out, uz_out, ke_out, p_out, en_out, h_out
    Real(8) :: cc, bb, ux_Roe, uy_Roe, uz_Roe, h_Roe, ke_Roe, spd_snd_Roe, rho_Roe
    Real(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, ucon, rho, alpha, theta, beta, phi2
    Real(8) :: XI_X, XI_Y, XI_Z, denom
    Integer :: jj

    ! ... size of this problem
    !ND = size(u_in) - 2

    ! ... constants
    gm1 = gamma - 1.0_8

    ! ... form primative variables from "in"
    rho_in = u_in(1)
    ux_in  = u_in(2) / rho_in; uy_in = 0.0_8; uz_in = 0.0_8
    if (ND >= 2) uy_in  = u_in(3) / rho_in
    if (ND == 3) uz_in  = u_in(4) / rho_in
    ke_in  = 0.5_8 * (ux_in**2 + uy_in**2 + uz_in**2)
    en_in  = u_in(ND+2)/rho_in - ke_in
    p_in   = gm1 * rho_in * en_in
    h_in   = (gamma / gm1) * p_in / rho_in + ke_in 

    ! ... form primative variables from "out"
    rho_out = u_out(1)
    ux_out  = u_out(2) / rho_out; uy_out = 0.0_8; uz_out = 0.0_8
    if (ND >= 2) uy_out  = u_out(3) / rho_out
    if (ND == 3) uz_out  = u_out(4) / rho_out
    ke_out  = 0.5_8 * (ux_out**2 + uy_out**2 + uz_out**2)
    en_out  = u_out(ND+2)/rho_out - ke_out
    p_out   = gm1 * rho_out * en_out
    h_out   = (gamma / gm1) * p_out / rho_out + ke_out 

    ! ... form Roe variables
    cc = sqrt(rho_out / rho_in)
    bb = 1.0_8 / (1.0_8 + cc)
    ux_Roe = (Ux_in + Ux_out * cc) * bb
    uy_Roe = (Uy_in + Uy_out * cc) * bb
    uz_Roe = (Uz_in + Uz_out * cc) * bb
    h_Roe  = ( h_in +  h_out * cc) * bb
    ke_Roe = 0.5_8 * (ux_Roe**2 + uy_Roe**2 + uz_Roe**2)
    if (h_Roe - ke_Roe <= 0.0_8) then
       !      write (*,'(6(E20.8,1X))') gamma, h_Roe, ke_Roe, ux_roe, uy_roe, uz_roe
       write (*,'(5(a,E20.8,1X))') "SATUTIL:FORM_ROE_MATRICES: ERRONEOUS CONDITIONS: gamma = ", gamma, ", ke_in = ", &
            ke_in, ", en_in = ", en_in, ", p_in = ", p_in, ", h_in = ", h_in
       write (*,*) 'u_in = ',u_in
       stop
    end if
    spd_snd_Roe = sqrt(gm1*(h_Roe - ke_Roe))
    rho_Roe = rho_in * cc

    ! ... variable renaming
    XI_X = norm(1); XI_Y = 0.0_8; XI_Z = 0.0_8
    if (ND >= 2) XI_Y = norm(2)
    if (ND == 3) XI_Z = norm(3)
    denom = 1.0_8 / sqrt(XI_X**2 + XI_Y**2 + XI_Z**2)
    XI_X_TILDE = XI_X * denom
    XI_Y_TILDE = XI_Y * denom
    XI_Z_TILDE = XI_Z * denom
    RHO = rho_Roe

    ! ... form transformation matrices
    ucon = ux_Roe * XI_X + uy_Roe * XI_Y + uz_Roe * XI_Z

    if (ND == 2) then

      ! ... directly from Pulliam & Chaussee
      alpha = RHO / (sqrt(2.0_8) * SPD_SND_ROE)
!      alpha = RHO / (2.0_8 * SPD_SND_ROE)
      beta = 1.0_8 / (sqrt(2.0_8) * RHO * SPD_SND_ROE)
!      beta = 1.0_8 / (2.0_8 * RHO * SPD_SND_ROE)
      theta = XI_X_TILDE * UX_ROE + XI_Y_TILDE * UY_ROE
      phi2 = 0.5_8 * (GAMMA - 1.0_8) * (UX_ROE**2 + UY_ROE**2)

      Tmat(1,1) = 1.0_8
      Tmat(1,2) = 0.0_8
      Tmat(1,3) = alpha
      Tmat(1,4) = alpha
      Tmat(2,1) = UX_ROE
      Tmat(2,2) = XI_Y_TILDE * RHO
      Tmat(2,3) = alpha * (UX_ROE + XI_X_TILDE * SPD_SND_ROE)
      Tmat(2,4) = alpha * (UX_ROE - XI_X_TILDE * SPD_SND_ROE)
      Tmat(3,1) = UY_ROE
      Tmat(3,2) = -XI_X_TILDE * RHO
      Tmat(3,3) = alpha * (UY_ROE + XI_Y_TILDE * SPD_SND_ROE)
      Tmat(3,4) = alpha * (UY_ROE - XI_Y_TILDE * SPD_SND_ROE)
      Tmat(4,1) = PHI2 / (GAMMA - 1.0_8)
      Tmat(4,2) = RHO * (XI_Y_TILDE * UX_ROE - XI_X_TILDE * UY_ROE)
      Tmat(4,3) = alpha * ((PHI2 + SPD_SND_ROE**2)/(GAMMA - 1.0_8) + SPD_SND_ROE * THETA)
      Tmat(4,4) = alpha * ((PHI2 + SPD_SND_ROE**2)/(GAMMA - 1.0_8) - SPD_SND_ROE * THETA)


      Tinv(1,1) = 1.0_8 - PHI2 / SPD_SND_ROE**2
      Tinv(1,2) = (GAMMA - 1.0_8) * UX_ROE / SPD_SND_ROE**2
      Tinv(1,3) = (GAMMA - 1.0_8) * UY_ROE / SPD_SND_ROE**2
      Tinv(1,4) = -(GAMMA - 1.0_8) / SPD_SND_ROE**2
      Tinv(2,1) = -(XI_Y_TILDE * UX_ROE - XI_X_TILDE * UY_ROE) / RHO
      Tinv(2,2) =  XI_Y_TILDE / RHO
      Tinv(2,3) = -XI_X_TILDE / RHO
      Tinv(2,4) = 0.0_8
      Tinv(3,1) = beta * (PHI2 - SPD_SND_ROE * theta)
      Tinv(3,2) = beta * (XI_X_TILDE * SPD_SND_ROE - (GAMMA - 1.0_8) * UX_ROE)
      Tinv(3,3) = beta * (XI_Y_TILDE * SPD_SND_ROE - (GAMMA - 1.0_8) * UY_ROE)
      Tinv(3,4) = beta * (GAMMA - 1.0_8)
      Tinv(4,1) = beta * (PHI2 + SPD_SND_ROE * theta)
      Tinv(4,2) = -beta * (XI_X_TILDE * SPD_SND_ROE + (GAMMA - 1.0_8) * UX_ROE)
      Tinv(4,3) = -beta * (XI_Y_TILDE * SPD_SND_ROE + (GAMMA - 1.0_8) * UY_ROE)
      Tinv(4,4) = beta * (GAMMA - 1.0_8)

      ! ... compute the diagonal matrix lambda
      Lambda(:,:) = 0.0_8
      do jj = 1, ND
        Lambda(jj,jj) = ucon
      end do
      Lambda(ND+1,ND+1) = ucon + SPD_SND_ROE * sqrt(XI_X**2 + XI_Y**2)
      Lambda(ND+2,ND+2) = ucon - SPD_SND_ROE * sqrt(XI_X**2 + XI_Y**2)

    else if (ND == 3) then

      ! ... directly from Pulliam & Chaussee
      alpha = RHO / (SQRT(2.0_8)*SPD_SND_ROE)
      beta = 1.0_8 / (SQRT(2.0_8)* RHO * SPD_SND_ROE)
!      alpha = RHO / (2.0_8*SPD_SND_ROE)
!      beta = 1.0_8 / (2.0_8*RHO*SPD_SND_ROE)
      theta = XI_X_TILDE * UX_ROE + XI_Y_TILDE * UY_ROE + XI_Z_TILDE * UZ_ROE
      phi2 = 0.5_8 * (GAMMA - 1.0_8) * (UX_ROE**2 + UY_ROE**2 + UZ_ROE**2)

      
!      WRITE(*,*) 'Original: (uCon,soundSpeed) = ',uCon,SPD_SND_ROE
!      WRITE(*,*) 'Original: (alpha,beta,theta,phi2) = ',alpha,beta,theta,phi2
!      WRITE(*,*) 'Original u  = ',ux_Roe,uy_Roe,uz_Roe
!      WRITE(*,*) 'Original XI = ',XI_X,XI_Y,XI_Z
      

      Tmat(1,1) = XI_X_TILDE
      Tmat(1,2) = XI_Y_TILDE
      Tmat(1,3) = XI_Z_TILDE
      Tmat(1,4) = alpha
      Tmat(1,5) = alpha
      Tmat(2,1) = XI_X_TILDE * UX_ROE
      Tmat(2,2) = XI_Y_TILDE * UX_ROE - XI_Z_TILDE * RHO
      Tmat(2,3) = XI_Z_TILDE * UX_ROE + XI_Y_TILDE * RHO
      Tmat(2,4) = alpha * (UX_ROE + XI_X_TILDE * SPD_SND_ROE)
      Tmat(2,5) = alpha * (UX_ROE - XI_X_TILDE * SPD_SND_ROE)
      Tmat(3,1) = XI_X_TILDE * UY_ROE + XI_Z_TILDE * RHO
      Tmat(3,2) = XI_Y_TILDE * UY_ROE
      Tmat(3,3) = XI_Z_TILDE * UY_ROE - XI_X_TILDE * RHO
      Tmat(3,4) = alpha * (UY_ROE + XI_Y_TILDE * SPD_SND_ROE)
      Tmat(3,5) = alpha * (UY_ROE - XI_Y_TILDE * SPD_SND_ROE)
      Tmat(4,1) = XI_X_TILDE * UZ_ROE - XI_Y_TILDE * RHO
      Tmat(4,2) = XI_Y_TILDE * UZ_ROE + XI_X_TILDE * RHO
      Tmat(4,3) = XI_Z_TILDE * UZ_ROE
      Tmat(4,4) = alpha * (UZ_ROE + XI_Z_TILDE * SPD_SND_ROE)
      Tmat(4,5) = alpha * (UZ_ROE - XI_Z_TILDE * SPD_SND_ROE)
      Tmat(5,1) = XI_X_TILDE * PHI2 / (GAMMA - 1.0_8) + RHO * (XI_Z_TILDE * UY_ROE - XI_Y_TILDE * UZ_ROE)
      Tmat(5,2) = XI_Y_TILDE * PHI2 / (GAMMA - 1.0_8) + RHO * (XI_X_TILDE * UZ_ROE - XI_Z_TILDE * UX_ROE)
      Tmat(5,3) = XI_Z_TILDE * PHI2 / (GAMMA - 1.0_8) + RHO * (XI_Y_TILDE * UX_ROE - XI_X_TILDE * UY_ROE)
      Tmat(5,4) = alpha * ((PHI2 + SPD_SND_ROE**2)/(GAMMA - 1.0_8) + SPD_SND_ROE * THETA)
      Tmat(5,5) = alpha * ((PHI2 + SPD_SND_ROE**2)/(GAMMA - 1.0_8) - SPD_SND_ROE * THETA)

      Tinv(1,1) =  XI_X_TILDE * (1.0_8 - PHI2 / SPD_SND_ROE**2) - (XI_Z_TILDE * UY_ROE - XI_Y_TILDE * UZ_ROE) / RHO
      Tinv(1,2) =  XI_X_TILDE * (GAMMA - 1.0_8) * UX_ROE / SPD_SND_ROE**2
      Tinv(1,3) =  XI_Z_TILDE / RHO + XI_X_TILDE * (GAMMA - 1.0_8) * UY_ROE / SPD_SND_ROE**2
      Tinv(1,4) = -XI_Y_TILDE / RHO + XI_X_TILDE * (GAMMA - 1.0_8) * UZ_ROE / SPD_SND_ROE**2
      Tinv(1,5) = -XI_X_TILDE * (GAMMA - 1.0_8) / SPD_SND_ROE**2
      Tinv(2,1) =  XI_Y_TILDE * (1.0_8 - PHI2 / SPD_SND_ROE**2) - (XI_X_TILDE * UZ_ROE - XI_Z_TILDE * UX_ROE) / RHO
      Tinv(2,2) = -XI_Z_TILDE / RHO + XI_Y_TILDE * (GAMMA - 1.0_8) * UX_ROE / SPD_SND_ROE**2
      Tinv(2,3) =  XI_Y_TILDE * (GAMMA - 1.0_8) * UY_ROE / SPD_SND_ROE**2
      Tinv(2,4) =  XI_X_TILDE / RHO + XI_Y_TILDE * (GAMMA - 1.0_8) * UZ_ROE / SPD_SND_ROE**2
      Tinv(2,5) = -XI_Y_TILDE * (GAMMA - 1.0_8) / SPD_SND_ROE**2
      Tinv(3,1) =  XI_Z_TILDE * (1.0_8 - PHI2 / SPD_SND_ROE**2) - (XI_Y_TILDE * UX_ROE - XI_X_TILDE * UY_ROE) / RHO
      Tinv(3,2) =  XI_Y_TILDE / RHO + XI_Z_TILDE * (GAMMA - 1.0_8) * UX_ROE / SPD_SND_ROE**2
      Tinv(3,3) = -XI_X_TILDE / RHO + XI_Z_TILDE * (GAMMA - 1.0_8) * UY_ROE / SPD_SND_ROE**2
      Tinv(3,4) =  XI_Z_TILDE * (GAMMA - 1.0_8) * UZ_ROE / SPD_SND_ROE**2
      Tinv(3,5) = -XI_Z_TILDE * (GAMMA - 1.0_8) / SPD_SND_ROE**2
      Tinv(4,1) =  beta * (PHI2 - SPD_SND_ROE * theta)
      Tinv(4,2) =  beta * (XI_X_TILDE * SPD_SND_ROE - (GAMMA - 1.0_8) * UX_ROE)
      Tinv(4,3) =  beta * (XI_Y_TILDE * SPD_SND_ROE - (GAMMA - 1.0_8) * UY_ROE)
      Tinv(4,4) =  beta * (XI_Z_TILDE * SPD_SND_ROE - (GAMMA - 1.0_8) * UZ_ROE)
      Tinv(4,5) =  beta * (GAMMA - 1.0_8)
      Tinv(5,1) =  beta * (PHI2 + SPD_SND_ROE * theta)
      Tinv(5,2) = -beta * (XI_X_TILDE * SPD_SND_ROE + (GAMMA - 1.0_8) * UX_ROE)
      Tinv(5,3) = -beta * (XI_Y_TILDE * SPD_SND_ROE + (GAMMA - 1.0_8) * UY_ROE)
      Tinv(5,4) = -beta * (XI_Z_TILDE * SPD_SND_ROE + (GAMMA - 1.0_8) * UZ_ROE)
      Tinv(5,5) =  beta * (GAMMA - 1.0_8)

      ! ... compute the diagonal matrix lambda
      Lambda(:,:) = 0.0_8
      do jj = 1, ND
        Lambda(jj,jj) = ucon
      end do
      Lambda(ND+1,ND+1) = ucon + SPD_SND_ROE * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2)
      Lambda(ND+2,ND+2) = ucon - SPD_SND_ROE * sqrt(XI_X**2 + XI_Y**2 + XI_Z**2)

    end if

    return

  end subroutine Form_Roe_Matrices


  SUBROUTINE LIMITDP(numDim,numScalars,qIn,dvIn,qOut,dvOut,qRoe,dvRoe,dpRoe)

    IMPLICIT NONE

    INTEGER(4), INTENT(IN)    :: numDim, numScalars
    REAL(8),    INTENT(IN)    :: qRoe(numDim+2+numScalars),dvRoe(numDim+2)
    REAL(8),    INTENT(IN)    :: qIn(numDim+2+numScalars),dvIn(numDim+2)
    REAL(8),    INTENT(IN)    :: qOut(numDim+2+numScalars),dvOut(numDim+2)
    REAL(8),    INTENT(INOUT) :: dpRoe(numScalars+2)

    REAL(8)    :: Residual,p_out,p_in,dp_drho_bar,rho_out,rho_in,dp_de_bar,rhoRoe
    REAL(8)    :: en_out,en_in,ke_in,ke_out,ja
    INTEGER(4) :: iDim,iScalar,dpnew1,dpnew2

    rho_out     = qOut(1)
    rho_in      = qIn(1)
    p_out       = dvOut(1)
    p_in        = dvIn(1)
    dp_drho_bar = dpRoe(1)
    dp_de_bar   = dpRoe(2)
    rhoRoe      = qRoe(1)
    en_out      = qOut(numDim+2)/rho_out
    en_in       = qIn(numDim+2)/rho_in
    ke_in       = 0.0_8
    ke_out      = 0.0_8

    DO iDim = 1,numDim
       ke_in  = ke_in  + dvIn(2+iDim)*dvIn(2+iDim)
       ke_out = ke_out + dvOut(2+iDim)*dvOut(2+iDim)
    END DO
    ke_in  = .5_8*ke_in
    ke_out = .5_8*ke_out
    en_in  = en_in - ke_in
    en_out = en_out - ke_out
    
    Residual = (p_out-p_in)-(dp_drho_bar*(rho_out-rho_in)+dp_de_bar*rhoRoe*(en_out-en_in))

    DO iScalar = 1, numScalars
      Residual = Residual - dpRoe(2+iScalar) * (qOut(numDim+2+iScalar)-qIn(numDim+2+iScalar))
    ENDDO

    ja = (dp_de_bar*(en_out-en_in))**2 + (dp_drho_bar*(rho_out-rho_in))**2
    DO iScalar = 1, numScalars
       ja = ja + (dpRoe(2+iScalar)*(qOut(numDim+2+iScalar)-qIn(numDim+2+iScalar)))**2
    ENDDO

    IF((residual .NE. 0.0_8) .OR. (ja > 1d-13)) THEN
       dpnew2 = dpRoe(2)*(1.0_8+dpRoe(2)*rhoRoe*(en_out-en_in)*Residual/ja)
       dpnew1 = dpRoe(1)*(1.0_8+dpRoe(1)*(rho_out-rho_in)*Residual/ja)
       IF(dpnew1 > 0.0_8 .AND. dpnew2 > 0.0_8) THEN
          dpRoe(1) = dpnew1
          dpRoe(2) = dpnew2
          DO iScalar =1 , numScalars
             dpRoe(2+iScalar) = dpRoe(2+iScalar)*(1.0_8+dpRoe(2+iScalar)*(qOut(numDim+2+iScalar)- &
                  qIn(numDim+2+iScalar))*Residual/ja)
          ENDDO
       ENDIF
    ENDIF
    
  END SUBROUTINE LIMITDP
  
  subroutine Original_FrontMatter(ND, numScalars,u_in, u_out, p_in, p_out, &
       uRoe,dvRoe,gamma, norm, MT1,alpha,beta,theta,phi2,b1,b2,metricEV)

!    USE ModGlobal

    Implicit None
    INTEGER(4) :: ND, numScalars
    REAL(8) :: uRoe(ND+2+numScalars),dvRoe(ND+2)
    Real(8), Pointer :: u_in(:), u_out(:), tmat(:,:), tinv(:,:), norm(:), lambda(:,:), MT1(:),metricEV(:)
    Real(8) :: gamma, rho_in, ux_in, uy_in, uz_in, ke_in, p_in, en_in, h_in
    Real(8) :: gm1, rho_out, ux_out, uy_out, uz_out, ke_out, p_out, en_out, h_out
    Real(8) :: cc, bb, ux_Roe, uy_Roe, uz_Roe, h_Roe, ke_Roe, spd_snd_Roe, rho_Roe,en_Roe
    Real(8) :: XI_X_TILDE, XI_Y_TILDE, XI_Z_TILDE, ucon, rho, alpha, theta, beta, phi2
    Real(8) :: XI_X, XI_Y, XI_Z, denom, b1, b2, U2
    Integer :: N, jj, dir, i
    Real(8) :: uhat, vhat, what, k_x, k_y, k_z, l_x, l_y, l_z, m_x, m_y, m_z, ja!,check(5,5)


!NEW
    ! ... constants
    gm1 = gamma - 1.0_8

    ! ... form primative variables from "in"
    rho_in = u_in(1)
    ux_in  = u_in(2) / rho_in
    uy_in = 0.0_8
    uz_in = 0.0_8
    if (ND >= 2) uy_in  = u_in(3) / rho_in
    if (ND == 3) uz_in  = u_in(4) / rho_in

    ke_in  = 0.5_8 * (ux_in**2 + uy_in**2 + uz_in**2)
    en_in  = u_in(ND+2)/rho_in - ke_in
    h_in   = (gamma / gm1) * p_in / rho_in + ke_in
    ! ... form primative variables from "out"
    rho_out = u_out(1)
    ux_out  = u_out(2) / rho_out
    uy_out = 0.0_8
    uz_out = 0.0_8
    if (ND >= 2) uy_out  = u_out(3) / rho_out
    if (ND == 3) uz_out  = u_out(4) / rho_out

    ke_out  = 0.5_8 * (ux_out**2 + uy_out**2 + uz_out**2)
    en_out  = u_out(ND+2)/rho_out - ke_out
    h_out   = (gamma / gm1) * p_out / rho_out + ke_out

    ! ... form Roe variables
    cc = sqrt(rho_out / rho_in)
    bb = 1.0_8 / (1.0_8 + cc)
    ux_Roe = (Ux_in + Ux_out * cc) * bb
    uy_Roe = (Uy_in + Uy_out * cc) * bb
    uz_Roe = (Uz_in + Uz_out * cc) * bb
    h_Roe  = ( h_in +  h_out * cc) * bb
    en_Roe = (en_in + en_out*cc)*bb 
    ke_Roe = 0.5_8 * (ux_Roe**2 + uy_Roe**2 + uz_Roe**2)

    if (h_Roe - ke_Roe <= 0.0_8) then
       write (*,'(5(a,E20.8,1X))') "SATUTIL:FORM_ROE_MATRICES: ERRONEOUS CONDITIONS: gamma = ", gamma, ", ke_in = ", &
            ke_in, ", en_in = ", en_in, ", p_in = ", p_in, ", h_in = ", h_in
      stop
    end if

    spd_snd_Roe = sqrt(gm1*(h_Roe - ke_Roe))
    rho_Roe = rho_in * cc

    ! ... variable renaming
    XI_X = norm(1); XI_Y = 0.0_8; XI_Z = 0.0_8
    if (ND >= 2) XI_Y = norm(2)
    if (ND == 3) XI_Z = norm(3)
    denom = 1.0_8 / sqrt(XI_X**2 + XI_Y**2 + XI_Z**2)
    XI_X_TILDE = XI_X * denom
    XI_Y_TILDE = XI_Y * denom
    XI_Z_TILDE = XI_Z * denom
    RHO = rho_Roe

    ! ... form transformation matrices
    ucon = ux_Roe * XI_X + uy_Roe * XI_Y + uz_Roe * XI_Z
    dvRoe(1) = gm1*RHO*en_Roe
    dvRoe(2) = h_Roe + ke_Roe
    dvRoe(3) = ux_Roe
    dvRoe(4) = uy_Roe
    IF(ND == 3) dvRoe(5) = uz_Roe

! PlasComCM _modified 2D and _original 2D and 3D
!      alpha = RHO / (sqrt(2.0_rfreal) * SPD_SND_ROE)
!      beta = 1.0_rfreal / (sqrt(2.0_rfreal) * RHO * SPD_SND_ROE)
!      theta = XI_X_TILDE * UX_ROE + XI_Y_TILDE * UY_ROE
!      phi2 = 0.5_rfreal * (GAMMA - 1.0_rfreal) * (UX_ROE**2 + UY_ROE**2)

    if (ND == 2) then

! PlasComCM and PC2 _multispecies (see below alpha and beta)
!        alpha = RHO / (sqrt(2.0_rfreal) * SPD_SND_ROE)
!        beta = 1.0_rfreal / (sqrt(2.0_rfreal) * RHO * SPD_SND_ROE)
!        b1 = dp_de_ROE / RHO / SPD_SND_ROE**2
!        b2 = 1.0_rfreal + b1 * (UX_ROE**2 + UY_ROE**2 + UZ_ROE**2) - b1 * H_ROE
!      b3 = 0.0_rfreal
!      do jj = 1, nAuxVars
!        b3 = b3 + Y_ROE(jj)* z_Roe(jj)
!      enddo
!      b3 = b3 * b1
!      theta = XI_X_TILDE * UX_ROE + XI_Y_TILDE * UY_ROE + XI_Z_TILDE * UZ_ROE

!       PC2 _multispecies and Pooya
!      alpha = RHO / (2.0_8 * SPD_SND_ROE)
!      beta = 1.0_8 / (RHO * SPD_SND_ROE)




! PlasCom2 _modified
      alpha = 0.5_8 * RHO / SPD_SND_ROE
      beta = 1.0_8 / (RHO * SPD_SND_ROE)
      theta = XI_X_TILDE * UX_ROE + XI_Y_TILDE * UY_ROE
      phi2 = 0.5_8 * (GAMMA - 1.0_8) * (UX_ROE**2 + UY_ROE**2)

    else if (ND == 3) then

! PlasComCM and PC2 _modified and Pooya
!      U2 = UX_ROE**2+UY_ROE**2+UZ_ROE**2
!      uhat = k_x * UX_ROE + k_y * UY_ROE + k_z * UZ_ROE
!      vhat = l_x * UX_ROE + l_y * UY_ROE + l_z * UZ_ROE
!      what = m_x * UX_ROE + m_y * UY_ROE + m_z * UZ_ROE
!      alpha = RHO / (2.0_rfreal * SPD_SND_ROE)
!      b1 = (GAMMA - 1.0_rfreal) / SPD_SND_ROE**2
!      b2 = 0.5_rfreal * (GAMMA - 1.0_rfreal) * (UX_ROE**2 + UY_ROE**2 + UZ_ROE**2) / SPD_SND_ROE**2
!      beta = 1.0_rfreal / (2.0_rfreal * alpha)

! PlasComCM _multispecies
!      alpha = RHO / (2.0_rfreal * SPD_SND_ROE)
!      beta = 1.0_rfreal / (2.0_rfreal * alpha)


! PC2 _modified and Pooya
      alpha = 0.5_8 * RHO / SPD_SND_ROE
      b1 = (GAMMA - 1.0_8) / SPD_SND_ROE**2
      b2 = 0.5_8 * (GAMMA - 1.0_8) * (UX_ROE**2 + UY_ROE**2 + UZ_ROE**2) / SPD_SND_ROE**2
      beta = 1.0_8 / (2.0_8 * alpha)

! PC2 _multispecies
!    alpha = RHO / (2.0_8 * SPD_SND_ROE)
!    beta = 1.0_8 / (2.0_8 * alpha)

      ! ............................................................
      ! ... Modified by Pooya (2018) ...............................
      ! ... Reference: Nonomura & Fujii, JCP (2017) ................
      ! ............................................................
      ! Ported to PlasCom2 [mtc]
       
!      if (dir==1) then
!       metricEV(1) = MT(1)
!       metricEV(2) = MT(2)
!       metricEV(3) = MT(3)
       k_x = MT1(1) 
       k_y = MT1(2) 
       k_z = MT1(3)
       ja = dsqrt(k_x**2+k_y**2+k_z**2)
       k_x = k_x/ja 
       k_y = k_y/ja 
       k_z = k_z/ja
       
        l_x = MT1(4) 
        l_y = MT1(5) 
        l_z = MT1(6)
        ja = dsqrt(l_x**2+l_y**2+l_z**2)
        l_x = l_x/ja 
        l_y = l_y/ja 
        l_z = l_z/ja

        m_x = MT1(7) 
        m_y = MT1(8) 
        m_z = MT1(9)
        ja =dsqrt(m_x**2+m_y**2+m_z**2)
        m_x = m_x/ja 
        m_y = m_y/ja 
        m_z = m_z/ja


      U2 = UX_ROE**2+UY_ROE**2+UZ_ROE**2

      uhat = k_x * UX_ROE + k_y * UY_ROE + k_z * UZ_ROE
      vhat = l_x * UX_ROE + l_y * UY_ROE + l_z * UZ_ROE
      what = m_x * UX_ROE + m_y * UY_ROE + m_z * UZ_ROE

      alpha = RHO / (2.0_8 * SPD_SND_ROE)
      b1 = (GAMMA - 1.0_8) / SPD_SND_ROE**2
      b2 = 0.5_8 * (GAMMA - 1.0_8) * (UX_ROE**2 + UY_ROE**2 + UZ_ROE**2) / SPD_SND_ROE**2
      beta = 1.0_8 / (2.0_8 * alpha)

    end if

  end subroutine Original_FrontMatter


END MODULE Roe
