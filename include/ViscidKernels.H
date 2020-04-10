#ifndef __VISCID_KERNELS_H__
#define __VISCID_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(viscid,viscidstresstensor,VISCID,VISCIDSTRESSTENSOR)
    (int *,int *,size_t *,size_t *,size_t *,double *, double *,double *,
     double *, double *,double *,double *,double *,double *);

  void FC_MODULE(viscid,viscidstronguniformflux,VISCID,VISCIDSTRONGUNIFORMFLUX)
    (int *, int *,size_t *,size_t *,size_t *,double *, double *,double *,
     double *, double*, double*, double*, double*);

  void FC_MODULE(viscid,strongflux1d,VISCID,STRONGFLUX1D)
       (const int *numDim,const int *fluxDir,const size_t *gridSizes,const size_t *numPoints,
        const size_t *opInterval,const int *gridType,const double *gridMetric,
        const double *tauBuffer,const double *energyBuffer, double *fluxBuffer);
  
  void FC_MODULE(viscid,scalarflux1d,VISCID,SCALARFLUX1D)
    (const int *numDim,const int *fluxDir,const size_t *gridSizes,const size_t *numPoints,
     const size_t *opInterval,const int *gridType,const double *gridMetric,
     const double *gradScalar,double *fluxBuffer);
  
};

#endif
