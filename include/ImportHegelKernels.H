#ifndef __IMPORTHEGEL_KERNELS_H__
#define __IMPORTHEGEL_KERNELS_H__

#include "FC.h"

extern "C"
{

  void FC_MODULE(importhegel,gethegelgridsize,IMPORTHEGEL,GETHEGELGRIDSIZE)
    (const char fname[256],int *gsX, int*gsY);

  void FC_MODULE(importhegel,readhegeldata,IMPORTHEGEL,READHEGELDATA)
    (const char fname1[256],const char fname2[256],
     const int *startEq,const int *numEq,const int *gsX,const int *gsY,
     double *cdX, double *cdY, double *outBuffer);
  
};

#endif
