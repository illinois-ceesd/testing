#ifndef __RUNGEKUTTA_KERNELS_H__
#define __RUNGEKUTTA_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(rungekutta,rk4sum,RUNGEKUTTA,RK4SUM)(int *,size_t *,size_t *,size_t *,double *, double *,double *,double *,double *,double *);

}
#endif
