MODULE EOS

  IMPLICIT NONE

  INTEGER(KIND=4), PARAMETER :: NUMGASINFOS  = 4
  INTEGER(KIND=4), PARAMETER :: NUMNONDIMENS = 16
  INTEGER(KIND=4), PARAMETER :: NUMGASPARAMS = 5

  INTERFACE

     SUBROUTINE GASDV(numDim,eosInfo,eosParams,eosNonDim,qIn,dvIn)
       IMPLICIT NONE
       INTEGER(4), INTENT(IN)  :: numDim
       INTEGER(8), INTENT(IN)  :: eosInfo
       REAL(8),    INTENT(IN)  :: eosParams
       REAL(8),    INTENT(IN)  :: eosNonDim
       REAL(8),    INTENT(IN)  :: qIn
       REAL(8),    INTENT(OUT) :: dvIn
     END SUBROUTINE GASDV

     SUBROUTINE GASDP(numDim,eosInfo,eosParams,eosNonDim,qIn,dvIn,dp)
       IMPLICIT NONE
       INTEGER(4), INTENT(IN)  :: numDim
       INTEGER(8), INTENT(IN)  :: eosInfo
       REAL(8),    INTENT(IN)  :: eosParams
       REAL(8),    INTENT(IN)  :: eosNonDim
       REAL(8),    INTENT(IN)  :: qIn
       REAL(8),    INTENT(IN)  :: dvIn
       REAL(8),    INTENT(OUT) :: dp 
     END SUBROUTINE GASDP

     SUBROUTINE GASISOTHERMAL(numDim,eosInfo,eosParams,eosNonDim,isoT,q)
       IMPLICIT NONE
       INTEGER(4), INTENT(IN)    :: numDim
       INTEGER(8), INTENT(IN)    :: eosInfo
       REAL(8),    INTENT(IN)    :: eosParams
       REAL(8),    INTENT(IN)    :: eosNonDim
       REAL(8),    INTENT(IN)    :: isoT
       REAL(8),    INTENT(INOUT) :: q
     END SUBROUTINE GASISOTHERMAL

     SUBROUTINE GASEOSROE(numDim,eosInfo,eosParams,eosNonDim,qIn,dv,dp,c)
       IMPLICIT NONE
       INTEGER(4), INTENT(IN)  :: numDim
       INTEGER(8), INTENT(IN)  :: eosInfo
       REAL(8),    INTENT(IN)  :: eosParams
       REAL(8),    INTENT(IN)  :: eosNonDim
       REAL(8),    INTENT(IN)  :: qIn
       REAL(8),    INTENT(OUT) :: dv
       REAL(8),    INTENT(OUT) :: dp
       REAL(8),    INTENT(OUT) :: c
     END SUBROUTINE GASEOSROE

     SUBROUTINE GASSOUNDSPEED(numDim,eosInfo,eosParams,eosNonDim,qIn,dvIn,dpIn,c)
       IMPLICIT NONE
       INTEGER(4), INTENT(IN)  :: numDim
       INTEGER(8), INTENT(IN)  :: eosInfo
       REAL(8),    INTENT(IN)  :: eosParams
       REAL(8),    INTENT(IN)  :: eosNonDim
       REAL(8),    INTENT(IN)  :: qIn
       REAL(8),    INTENT(IN)  :: dvIn
       REAL(8),    INTENT(IN)  :: dpIn
       REAL(8),    INTENT(OUT) :: c
     END SUBROUTINE GASSOUNDSPEED

  END INTERFACE

  CONTAINS

    !> Calculate average material properties for mixture
    SUBROUTINE MIXPROPERTIES(numPointsBuffer,numSpecies,pointIndex,speciesMolWt, &
         speciesHf,speciesCp,univR,gasDensity,rhoY,tMix)
      
      IMPLICIT NONE
      
      INTEGER(KIND=8) :: numPointsbuffer,pointIndex
      INTEGER(KIND=4) :: numSpecies
      REAL(KIND=8)    :: speciesMolWt(numSpecies)
      REAL(KIND=8)    :: speciesHf(numSpecies)
      REAL(KIND=8)    :: speciesCp(numSpecies)
      REAL(KIND=8)    :: gasDensity(numPointsBuffer)
      REAL(KIND=8)    :: rhoY(numSpecies*numPointsBuffer)
      REAL(KIND=8)    :: tMix(3), univR
      
      ! Local
      REAL(KIND=8)    :: bufWtM1, baseR, baseHf, baseCp, rhoM1, specFrac
      INTEGER(KIND=8) :: specIndex
      INTEGER(KIND=4) :: iSpec

      bufWtM1 = 1.0_8/speciesMolWt(numSpecies)
      
      baseR   = univR*bufWtM1
      baseHf  = speciesHf(numSpecies)
      baseCp  = speciesCp(numSpecies)
      
      tMix(1) = baseR
      tMix(2) = baseHf
      tMix(3) = baseCp
      
      rhoM1   = 1.0_8/gasDensity(pointIndex)
      
      DO iSpec = 1, numSpecies-1
         specIndex   = (iSpec-1)*numPointsBuffer + pointIndex
         specFrac    = rhoY(specIndex)*rhoM1 
         tMix(1) = tMix(1) + specFrac*(univR/speciesMolWt(iSpec) - baseR)
         tMix(2) = tMix(2) + specFrac*(speciesHf(iSpec) - baseHf)
         tMix(3) = tMix(3) + specFrac*(speciesCp(iSpec) - baseCp)
      END DO
      
    END SUBROUTINE MIXPROPERTIES
    
    
END MODULE EOS
