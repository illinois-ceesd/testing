#ifndef __WENO_KERNELS_H__
#define __WENO_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(navierstokesrhsweno,ns_rhs_weno,NAVIERSTOKESRHSWENO,NS_RHS_WENO)
    (const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *numScalars,const int *numEquations,const int *wenoDir,const int *sconn,
     double *dFluxBuffer, const double *inviscidFluxBuffer,const size_t *opInterval,
     const double *gridMetric,const double *jacobianDeterminant,
     const double *gridCoordinates,const int *gridType,
     const double *rhoBuffer,const double *rhoVBuffer,const double *rhoEBuffer,
     const double *pressureBuffer,const double *temperatureBuffer,
     const double *sosBuffer, const double *scalarBuffer, 
     const double *wenoAlpha, const int *wenoOptions, const size_t *eosInfo,
     const double *eosNonDimen,const double *eosGasParams);


  void FC_MODULE(navierstokesrhsweno,ns_rhs_muscl,NAVIERSTOKESRHSWENO,NS_RHS_MUSCL)
    (const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *dir,const int *sconn, const size_t *opInterval,const size_t *bufferInterval,
     const int *gridType,const double *gridMetric,const double *jacobianDeterminant,
     const double *bcParams,const double *gasParams,const int *numScalars,
     const double *rhoBuffer,const double *rhoVBuffer,const double *rhoEBuffer,
     const double *scalarBuffer,const double *pressureBuffer,const double *temperatureBuffer,
     const double *sosBuffer, const double *rhoRHSBuffer,const double *rhoVRHSBuffer,
     const double *rhoERHSBuffer,const double *scalarRHSBuffer,
     const double *wenoAlpha, const int *wenoOptions, const size_t *eosInfo,
     const double *eosNonDimen,const double *eosGasParams);
  
  void FC_MODULE(navierstokesrhsweno,ns_bc_weno,NAVIERSTOKESRHSWENO,NS_BC_WENO)
    (const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *dir,const size_t *opInterval,const size_t *bufferInterval,
     const int *gridType,const double *gridMetric,const double *jacobianDeterminant,
     const double *gridMetric2,const int *bcType,
     const double *bcParams,const double *gasParams,const int *numScalars,
     const double *rhoBuffer,const double *rhoVBuffer,const double *rhoEBuffer,
     const double *scalarBuffer,const double *pressureBuffer,const double *temperatureBuffer,
     const double *sosBuffer, const double *rhoRHSBuffer,const double *rhoVRHSBuffer,
     const double *rhoERHSBuffer,const double *scalarRHSBuffer,const size_t *eosInfo,
     const double *eosNonDimen,const double *eosGasParams);

  void FC_MODULE(navierstokesrhsweno,ns_bc_fixvalue,NAVIERSTOKESRHSWENO,NS_BC_FIXVALUE)
    (const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *dir,const size_t *numPatchPointsOp,const size_t *patchPointsOp,
     const int *gridType,const double *gridMetric,const double *jacobianDeterminant,
     const int *bcType,const double *bcParams,const double *gasParams,const int *numScalars,
     const double *sosBuffer, const double *pressureBuffer,const double *temperatureBuffer,
     const size_t *eosInfo, const double *eosNonDim, const double *eosGasParams,
     const double *rhoTarget,const double *rhoVTarget,const double *rhoETarget,
     const double *scalarTarget,double *rhoBuffer,double *rhoVBuffer,
     double *rhoEBuffer,double *scalarBuffer);

  void FC_MODULE(navierstokesrhsweno,ns_rhs_axisymmetric,NAVIERSTOKESRHSWENO,NS_RHS_AXISYMMETRIC)
       (const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
        const int *numScalars,const size_t *opInterval, const double *gridCoordinates,
        const double *rhoBuffer, const double *rhoVBuffer, const double *rhoEBuffer,
        const double *pressureBuffer, const double *scalarBuffer,
        double *rhoRHSBuffer, double *rhoVRHSBuffer, double *rhoERHSBuffer,double *scalarRHSBuffer);

  void FC_MODULE(navierstokesrhsweno,ns_rhs_axisymmetric_axis,NAVIERSTOKESRHSWENO,NS_RHS_AXISYMMETRIC_AXIS)	
       (const int *numDim,const size_t *bufferInterval,const size_t *numPointsBuffer,
        const int *numScalars,const int *normDir,const size_t *opInterval,const double *gridMetric,
        const double *jacobianDeterminant,const int *gridType, const double *gridCoordinates,
        const double *rhoBuffer,const double *rhoVBuffer,const double *rhoEBuffer,
        const double *pressureBuffer,const double *scalarBuffer,
        double *rhoRHSBuffer,double *rhoVRHSBuffer,double *rhoERHSBuffer,double *scalarRHSBuffer);
  
};

#endif
