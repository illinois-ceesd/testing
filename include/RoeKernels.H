#ifndef __ROE_KERNELS_H__
#define __ROE_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(roe,pointroestate,ROE,POINTROESTATE)
    (const int *numDim, const int *numScalar, const double *uIn,
     const double *dvIn,const double *uOut, const double *dvOut,
     double *uRoe,double *dvRoe,double *dv2Roe);

  void FC_MODULE(roe,pointroestate2,ROE,POINTROESTATE2)
    (const int *numDim, const int *numScalar, const double *uL,
     const double *dvL,const double *uR, const double *dvR, double *uRoe);

  void FC_MODULE(roe,eigenmatrices,ROE,EIGENMATRICES)
    (const int *numDim,const int *numScalars,const double *qRoe,const double *dvRoe,
     const double *dp, const double *soundSpeed, const double *pointMetric,
     const double *metricEigenVectors,double *Pfor,double *Pinv,double *lambda);
  
  void FC_MODULE(roe,form_roe_state,ROE,FORM_ROE_STATE)
    (const int *numDim,const int *numScalars,const double *uLeft,const double *uRight,
     const double *gamma,const double *norm,double *uRoe,double *dvRoe,double *uCon,
     double *cRoe,double *theta);

  void FC_MODULE(roe,form_roe_matrices,ROE,FORM_ROE_MATRICES)
    (const int *numDim,const double *u_in,const double *u_out, const double *gamma,
     const double *norm,double *tmat,double *tinv,double *lambda);
};

#endif

