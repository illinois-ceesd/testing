#ifndef __OPERATOR_KERNELS_H__
#define __OPERATOR_KERNELS_H__

#include "FC.h"

extern "C" {



  void FC_MODULE(operators,applyoperator,OPERATORS,APPLYOPERATOR)
    (const int *numDim,const size_t *bufferSizes,const int *numComponents,
     const size_t *numPointsBuffer,const int *opDir,const size_t *opInterval,
     const int *numStencils,const int *stencilSizes,const int *stencilStarts,
     const int *numStencilWeights,const double *stencilWeights,
     const int *stencilOffsets,const int *stencilID,
     const double *uBuffer,double *duBuffer);
  
  void FC_MODULE(operators,applyoperatorv,OPERATORS,APPLYOPERATORV)
    (const int *,const size_t *,const int *,const size_t *,const int *,
     const size_t *,const int *,const int *,const int *,const int *,
     const double *,const int *,const int *,const double *,double *);

  void FC_MODULE(operators,applyoperatorblobs,OPERATORS,APPLYOPERATORBLOBS)
    (const int *,const size_t *,const int *,const size_t *,const int *,
     const int *,const int *,const int *,const int *,const double *,const int *,
     const size_t *,const size_t *,const size_t *,const double *,double *);

  void FC_MODULE(operators,applysinglestencil,OPERATORS,APPLYSINGLESTENCIL)
    (const int *,const size_t *,const int *,const size_t *,const int *,const size_t *,
     const size_t *,const int *,const double *,const int *,const double *,double *);
  
};

#endif
