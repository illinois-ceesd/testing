#ifndef __EOS_H__
#define __EOS_H__

// #include "PCPPTypes.H"
// #include "PCPPIO.H"
#include "FC.h"

#ifdef USE_OMP
#include <omp.h>
#endif

//#include "CanteraAgent.H"

// MJA, revisit this when autogeneration is working
#include "PrometheusInterface.h"

namespace eos {

#ifdef HAVE_CANTERA
  void SetupGlobalCantera(const std::vector<Cantera::IdealGasMix *> &inCantera);
#endif

#ifdef EOS_TIMERS
  void SetupEOSTimers();
  void GetEOSTimers(std::vector<double> &inTimers,std::vector<size_t> &inCounts);
#endif

#ifdef USE_HYDRA
#pragma omp declare target
#endif

  static const int MAXSPECIES = 200;
  static const double UNIVERSALGASCONSTANT        = 8.3144598;
  static const double CANTERAUNIVERSALGASCONSTANT = 8.3144621;

#ifdef USE_HYDRA
#pragma omp end declare target
#endif

  enum GASMODELS   {IDEAL,CANTERA_PG_MULTI,PG_CP_MULTI,PG_TP_MULTI,
                    PROMETHEUS_PG_MULTI,PLASMA_CANTERA,PLASMA_PROMETHEUS,NUM_GAS_MODELS};

  enum GASTYPE     {PERFECTSINGLE,PERFECTMIX,IDEALMIX,PLASMA,NUMGASTYPES};

  enum GASPARAM    {SPECIESCP,SPECIESCV,SPECIESHF,SPECIESMW,SPECIESCHARGEMW,SPECIESMAP,NUMGASPARAMS};

  enum INFO        {CONTEXT,TYPE,HANDLER,NUMSPECIES,PROMETHEUSMODEL,
                    ABUNDANTINDEX,ELECTRONINDEX,NUMINFOS};

  enum HANDLERTYPE {NATIVE,CANTERA,PROMETHEUS,NUMHANDLERTYPES};

  enum DIMEN       {REFLENGTH,REFDENSITY,REFSPEED,REFENERGY,REFPRESSURE,
                    REFTEMPERATURE,REFSOUNDSPEED,UNIVGASCONSTANT,SPECIFICGASCONSTANT,
                    DENSITYSCALE,VELOCITYSCALE,PRESSURESCALE,TEMPERATURESCALE,
                    ENERGYSCALE,TIMESCALE,GAMMA,NUMDIMENS};

  int GasModelId(const std::string &modelName);
  std::string GasModelName(int gasModel);

} // namespace eos

extern "C" 
{
  void FC_GLOBAL(gasdv,GASDV)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,double *dvIn);
  void FC_GLOBAL(gasdp,GASDP)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,const double *dvIn,double *dp);
  void FC_GLOBAL(gasisothermal,GASISOTHERMAL)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *isoT,double *qIn);  
  void FC_GLOBAL(gaseosroe,GASEOSROE)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,double *q,double *dv,double *dp,double *c);
  void FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,const double *dvIn,
     const double *dpIn,double *soundSpeed);
}

#endif


