#ifndef __SIMPLE_KERNELS_H__
#define __SIMPLE_KERNELS_H__

#include "FC.h"

extern "C" {

  void ZAXPY_CUDA(const int *, const size_t *, const size_t *, const size_t *, 
                  const double *, const double *, const double *, double *);

  void FC_MODULE(simple,yavdotwpy,SIMPLE,YAVDOTWPY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *V,
     const double *W,double *Y);

  void FC_MODULE(simple,vasupv,SIMPLE,VASUPV)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *S,
     const double *U,double *V);

  void FC_MODULE(simple,yassmwdotupy,SIMPLE,YASSMWDOTUPY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *s1,
     const double *s2,const double *W,const double *U,double *Y);
  
  void FC_MODULE(simple,yasmwdotupy,SIMPLE,YASMWDOTUPY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *s1,
     const double *W,const double *U,double *Y);
  
  void FC_MODULE(simple,yaxpy,SIMPLE,YAXPY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *X,double *Y);
  
  void FC_MODULE(simple,ywxpy,SIMPLE,YWXPY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *opInterval,const double *w,const double *x, double *y);
  
  void FC_MODULE(simple,zaxpy,SIMPLE,ZAXPY)
    (const int *,const size_t *,const size_t *,const size_t *,const double *a,
     const double *x,const double *y,double *z);
  void FC_MODULE(simple,zaxpby,SIMPLE,ZAXPBY)
    (const int *,const size_t *,const size_t *,const size_t *,const double *a, 
     const double *x,const double *b,const double *y,double *z);
  void FC_MODULE(simple,yaxpby,SIMPLE,YAXPBY)
    (const int *,const size_t *,const size_t *,const size_t *,const double *a, 
     const double *x,const double *b,double *y);
  void FC_MODULE(simple,yax,SIMPLE,YAX)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a, const double *x,double *y);

  void FC_MODULE(simple,zxdoty,SIMPLE,ZXDOTY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const int *numComponents,const double *X,
     const double *Y,double *Z);

  void FC_MODULE(simple,xax,SIMPLE,XAX)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a, double *X);

  void FC_MODULE(simple,zxy,SIMPLE,ZXY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *X,const double *Y,double *Z);
  
  void FC_MODULE(simple,zaxy,SIMPLE,ZAXY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *X,const double *Y,double *Z);
  
  void FC_MODULE(simple,yxy,SIMPLE,YXY)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *X, double *Y);
  
  void FC_MODULE(simple,zwxpy,SIMPLE,ZWXPY)
    (int *,size_t *,size_t *,size_t *,double *, double *,double *,double *);

  void FC_MODULE(simple,zwmxpy,SIMPLE,ZMWXPY)
    (int *,size_t *,size_t *,size_t *,double *, double *,double *,double *);

  void FC_MODULE(simple,assignmentyx,SIMPLE,ASSIGNMENTYX)
    (const int *,const size_t *,const size_t *,const size_t *,const double *x, 
     double *y);
  void FC_MODULE(simple,assignmentxa,SIMPLE,ASSIGNMENTXA)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a, double *x);

  void FC_MODULE(simple,assignmentyabsx,SIMPLE,ASSIGNMENTYABSX)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,double *X, double *Y);

  void FC_MODULE(operators,xaxm1,OPERATORS,XAXM1)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a, double *X);
  
  void FC_MODULE(operators,xam1x,OPERATORS,XAM1X)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a, double *X);

  void FC_MODULE(operators,yaxm1,OPERATORS,YAXM1)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *a,const double *X,double *Y);

  void FC_MODULE(operators,zawpxy,OPERATORS,ZAWPXY)
    (int *numDim,size_t *numPoints,size_t *bufferSize,size_t *bufferInterval,
     double *a,double *W,double *X,double *Y,double *Z);

  void FC_MODULE(operators,zvwpxy,OPERATORS,ZVWPXY)
    (int *numDim,size_t *numPoints,size_t *bufferSize,size_t *bufferInterval,
     double *V,double *W,double *X,double *Y,double *Z);

  void FC_MODULE(operators,zxym1,OPERATORS,ZXYM1)
    (const int *numDim,const size_t *numPoints,const size_t *bufferSize,
     const size_t *bufferInterval,const double *X,const double *Y,double *Z);
  
  

}
