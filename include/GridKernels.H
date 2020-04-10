#ifndef __GRID_KERNELS_H__
#define __GRID_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(grid,applyuniformgridmetric,GRID,APPLYUNIFORMGRIDMETRIC)(int *,size_t *,size_t *,size_t *,double *,double *,double *);
  void FC_MODULE(grid,applycartesiangridmetric,GRID,APPLYCARTESIANGRIDMETRIC)(int *,size_t *,size_t *,size_t *,double *,double *,double *);

};

#endif
