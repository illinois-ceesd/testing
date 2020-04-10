#ifndef __NAVIERSTOKES_UTIL_KERNELS_H__
#define __NAVIERSTOKES_UTIL_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(nsutil,getpointstate,NSUTIL,GETPOINTSTATE)
    (const int *numDim,const int *numScalar,const size_t *numPointsBuffer,
     const double *rhoBuffer,const double *rhoVBuffer,const double *rhoEBuffer,
     const double *scalarBuffer,const size_t *pointID,double *pointState);
  
}

#endif
