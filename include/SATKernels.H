#ifndef __SAT_KERNELS_H__
#define __SAT_KERNELS_H__

#include "FC.h"

extern "C" {


  void FC_MODULE(satutil,farfield,SATUTIL,FARFIELD)
    (const size_t *eosInfo,const double *eosNonDimen,const double *eosGasParams,
     const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *patchNormalDir,const size_t *patchSizes,const size_t *numPointsPatch,
     const size_t *numPatchPointsOp,const size_t *patchPointsOp,const int *gridType,
     const double *gridMetric,const double *gridJacobian,const double *bcParams,
     const double *gasParams,const double *rhoBuffer,const double *rhoVBuffer, 
     const double *rhoEBuffer,const double *viscousFluxBuffer,const int *numScalar,
     const double *scalarBuffer,double *rhoRHS,double *rhoVRHS,
     double *rhoERHS,double *scalarRHS,const double *rhoTarget,
     const double *rhoVTarget,const double *rhoETarget,const double *scalarTarget);
  
  void FC_MODULE(satutil,noslip_isothermal,SATUTIL,NOSLIP_ISOTHERMAL)
    (const size_t *eosInfo,const double *eosNonDimen,const double *eosGasParams,
     const int *numDim,const size_t *bufferSizes,const size_t *numPointsBuffer,
     const int *patchNormalDir,const int *sconn,const size_t *patchSizes,
     const size_t *numPointsPatch,const size_t *numPatchPointsOp,
     const size_t *patchPointsOp,const int *gridType,const double *gridMetric,
     const double *jacobianDeterminant,const double *bcParams,
     const double *gasParams,const double *rhoBuffer,const double *rhoVBuffer,
     const double *rhoEBuffer,const int *numScalar,const double *scalarBuffer,
     const double *scalarGrad,double *rhoRHS,double *rhoVRHS,
     double *rhoERHS,double *scalarRHS,const double *rhoTarget,
     const double *rhoVTarget,const double *rhoETarget,const double *scalarTarget,
     const double *muBuffer,const double *lambdaBuffer);

  void FC_MODULE(satutil,slip_adiabatic,SATUTIL,SLIP_ADIABATIC)
    (const size_t *eosInfo,const double *eosNonDimen,const double *eosGasParams, 
     const int *numDim,const size_t *bufferSizes,
     const size_t *numPointsBuffer, const int *patchNormalDir,const int *sconn, 
     const size_t *patchSizes,const size_t *numPointsPatch,
     const size_t *numPatchPointsOp,const size_t *patchPointsOp,const int *gridType,
     const double *gridMetric,const double *jacobianDeterminant,
     const double *bcParams,const double *gasParams,const double *rhoBuffer,
     const double *rhoVBuffer,const double *rhoEBuffer,const int *numScalar,
     const double *scalarBuffer,const double *scalarGrad,double *rhoRHS,
     double *rhoVRHS,double *rhoERHS,double *scalarRHS,const double *rhoTarget,
     const double *rhoVTarget,const double *rhoETarget,const double *scalarTarget);

//   void FC_MODULE(satutil,sat_form_roe_matrices,SATUTIL,SAT_FORM_ROE_MATRICES)
//                   (int *, double *, double *, double *, double *, double *, double *, double *);

  void FC_MODULE(satutil,dissipationweight,SATUTIL,DISSIPATIONWEIGHT)
    (const int *numDim,const size_t *dimSizes,const size_t *numPoints,
     const size_t *bufferInterval,const double *sigmaDissipation,
     const double *sigmaDilatation,const double *dilatationRamp,
     const double *dilatationCutoff,const double *divV,double *sigmaDiss);
  
}

#endif
