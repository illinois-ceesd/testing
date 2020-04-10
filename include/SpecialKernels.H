#ifndef __SPECIAL_KERNELS_H__
#define __SPECIAL_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(special,veclen,SPECIAL,VECLEN)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const int *numComp,const double *V,
     double *lenV);

  void FC_MODULE(special,determinant3x3,SPECIAL,DETERMINANT3X3)
    (size_t *numPoints,size_t *bufferSize,size_t *bufferInterval,
     double *inMatrix,double *matrixDeterminant);

  void FC_MODULE(special,determinant2x2,SPECIAL,DETERMINANT2X2)
    (size_t *numPoints,size_t *bufferSize,size_t *bufferInterval,
     double *inMatrix,double *matrixDeterminant);
  
  void FC_MODULE(special,metricsum4,SPECIAL,METRICSUM4)
    (int *numDim,size_t *numPoints,size_t *bufferSize,size_t *bufferInterval,
     double *buf1,double *buf2,double *buf3,double *buf4,double *buf5,
     double *buf6,double *buf7,double *metricSum);

}
