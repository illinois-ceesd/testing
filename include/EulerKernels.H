#ifndef __EULER_KERNELS_H__
#define __EULER_KERNELS_H__

#include "FC.h"

extern "C" {

  // Full curvilinear support inviscid flux routine
  void FC_MODULE(euler,flux1d,EULER,FLUX1D)
    (const int *numDim,const size_t *numPoints,const size_t *gridSizes,const size_t *opInterval,
     const int *fluxDir,const int *gridType,const double *gridMetric,const double *rhoBuffer,
     const double *rhoVBuffer,const double *rhoEBuffer,const double *velHat,
     const double *pressureBuffer,double *fluxBuffer);

  // Scalar transport flux
  void FC_MODULE(euler,scalarflux1d,EULER,SCALARFLUX1D)
    (const int *numDim,const size_t *numPoints,const size_t *gridSizes, 
     const size_t *opInterval,const int *numScalars,const double *scalarBuffer,
     const double *velHat,double *fluxBuffer);

  // Legacy uniform rectangular (UNIRECT) grids only
  void FC_MODULE(euler,uniformrhs,EULER,UNIFORMRHS)(int *,size_t *,size_t *,size_t *,size_t *,double *,
                                                    int *,int *,int *,int *, int *,double *,
                                                      int *,double *,double *,
                                                      double *,double *,double *,double *,double *,double *);

  void FC_MODULE(euler,uniformscalarrhs,EULER,UNIFORMSCALARRHS)(int *,size_t *,size_t *,size_t *,
                                                                int *,int *,int *,int *, int *,double *,
                                                                  int *,size_t *,size_t *,int *,
                                                                double *,double *,double *);

  void FC_MODULE(euler,uniformflux,EULER,UNIFORMFLUX)(int *,int *,size_t *,size_t *,size_t *,double *,
                                                      double *,double *,double *,double *,double *,double *,double *);
  
  void FC_MODULE(euler,uniformscalarflux,EULER,UNIFORMEULERFLUX)(int *,int *,size_t *,size_t *,size_t *,
                                                                 int *,double *,double *,double *);

};

#endif
