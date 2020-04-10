#ifndef __METRIC_KERNELS_H__
#define __METRIC_KERNELS_H__

#include "FC.h"

extern "C" {

  void FC_MODULE(metricops,gradijktogradxyz,METRICOPS,GRADIJKTOGRADXYZ)
    (const int *numDim,const size_t *numPoints,const size_t *gridSizes, 
     const size_t *opInterval,const int *gridType,const double *gridJacobian, 
     const double *gridMetric,const double *gradIJK,double *gradXYZ);

  void FC_MODULE(metricops,ijkgradtoxyzdiv,METRICOPS,IJKGRADTOXYZDIV)
    (const int *numDim,const size_t *numPoints,const size_t *gridSizes, 
     const size_t *opInterval,const int *gridType,const double *gridJacobian, 
     const double *gridMetric,const double *gradVBuffer,double *divBuffer);
  
  void FC_MODULE(metricops,vhatcomponent,METRICOPS,VHATCOMPONENT)
    (const int *numDim,const size_t *numPointsBuffer,const size_t *bufferSizes,
     const size_t *opInterval,const int *gridType,const double *gridMetric,
     const int *velDir,const double *velocity,double *velHat);

  void FC_MODULE(metricops,alphaweight,METRICOPS,ALPHAWEIGHT)
    (const int *numDim,const size_t *numPointsBuffer,const size_t *bufferSizes,
     const size_t *opInterval,const int *gridType,const double *gridMetric,
     const int *alphaDir,double *alphaWeight);

  void FC_MODULE(metricops,alphaweight2,METRICOPS,ALPHAWEIGHT2)
    (const int *numDim,const size_t *numPointsBuffer,const size_t *bufferSizes,
     const size_t *opInterval,const int *gridType,const double *gridMetric,
     const double *gridJacobianM1,const int *alphaDir,double *alphaWeight);

  void FC_MODULE(metricops,getpointmetric,METRICOPS,GETPOINTMETRIC)
    (const int *numDim,const size_t *numPointsBuffer,const size_t *pointID,const int *gridType,
     const double *gridMetric,double *pointMetric);

  void FC_MODULE(metricops,boundarypointmetric,METRICOPS,BOUNDARYPOINTMETRIC)
    (const int *numDim,const int *normDir,const size_t *numPointsBuffer,
     const size_t *pointID,const int *gridType,const double *gridMetric,double *pointMetric);

  void FC_MODULE(metricops,pointeigenvectors,METRICOPS,POINTEIGENVECTORS)
    (const int *numDim,const double *pointMetric,double *pointEigenVectors,double *metricMags);

  void FC_MODULE(metricops,pointeigenvectorsgs,METRICOPS,POINTEIGENVECTORSGS)
    ( const int *numDim, const double *pointMetric, double *eigenVectors,double *eigenComponents);
  
};

#endif
