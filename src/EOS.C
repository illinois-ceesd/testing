#include "EOS.H"

#include <iostream>
#include <iomanip>
#include <cmath>

namespace eos {

#ifdef HAVE_CANTERA
  static std::vector<Cantera::IdealGasMix *> globalCantera;
  void SetupGlobalCantera(const std::vector<Cantera::IdealGasMix *> &inCantera){
    globalCantera = inCantera;
  }
#endif

#ifdef EOS_TIMERS
  enum TIMERS {ROETIMER,DPTIMER,ISOTHERMALTIMER,TESTTIMER,NUMTIMERS};
  static std::vector<double> eosTimers;
  static std::vector<size_t> eosCallCounts;
  void SetupEOSTimers() {
    eosTimers.resize(TIMERS::NUMTIMERS,0);
    eosCallCounts.resize(TIMERS::NUMTIMERS,0);
  };
  void GetEOSTimers(std::vector<double> &inTimers,std::vector<size_t> &inCounts)
  { 
    inTimers = eosTimers;
    inCounts = eosCallCounts;
  };
#endif

}


extern "C" 
{

  // Calculate mixture tMix = {R,Hf,Cp}
  inline void CalculateCPMixture(int numDim,const size_t *gasInfo,const double *nonDimen,
                                 const double *gasParams,const double *qIn,double *tMix)
  {
    
    // species buffer contains rho*Yi
    // one species (the last) is untracked
    
    //
    // calculate mixture specific gas constant (R)
    // R = sum(Y_i*R_i,1:N) = R_N + sum(Y_i*(R_i-R_N,1:N-1)
    //
    // calculate enthalpy of formation
    // h^f = sum(Y_i*h^f_i,1:N) = h^f_N + sum(Y_*(h^f_i-h^f_N),1:N-1)
    //
    // calculate mixture heat capacity (Cp)
    // Cp = sum(Y_i*Cp,1:N) = Cp_N + sum(Yi*(Cp_i-Cp_N),1:N-1)
    //
    
    const double *specificDensity   = &qIn[numDim+2]; // rhoY_i
    double qSpecificVolume          = 1.0/qIn[0];
    double univGasConst             = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
    size_t numSpecies               = gasInfo[eos::INFO::NUMSPECIES];
    
    // gasParams is blocked by numSpecies
    const double *speciesMolecularWeights = &gasParams[eos::GASPARAM::SPECIESMW*numSpecies];
    const double *speciesHeatOfFormation  = &gasParams[eos::GASPARAM::SPECIESHF*numSpecies];
    const double *speciesHeatCapacityCp   = &gasParams[eos::GASPARAM::SPECIESCP*numSpecies];
    
    tMix[0] = univGasConst/speciesMolecularWeights[numSpecies-1];
    tMix[1] = speciesHeatOfFormation[numSpecies-1];
    tMix[2] = speciesHeatCapacityCp[numSpecies-1];
    
    for(size_t i=0;i<numSpecies-1;i++){
      tMix[0] += specificDensity[i]*qSpecificVolume*
        (univGasConst/speciesMolecularWeights[i]-univGasConst/speciesMolecularWeights[numSpecies-1]);
      tMix[1] += specificDensity[i]*qSpecificVolume*
        (speciesHeatOfFormation[i]-speciesHeatOfFormation[numSpecies-1]);
      tMix[2] += specificDensity[i]*qSpecificVolume*
        (speciesHeatCapacityCp[i]-speciesHeatCapacityCp[numSpecies-1]);
    }
  }

  // everything is a pointer to facilitate calling from fortran
  void FC_GLOBAL(gasdv,GASDV)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,double *dvIn)
  {
    double rho              =  qIn[0];
    const double *rhoV      = &qIn[1];
    int numDim              =  *inDimension;
    double rhoE             =  qIn[numDim+1];
    double rhoM1            =  1.0/rho;
    double refTemperature   = nonDimen[eos::DIMEN::REFTEMPERATURE];
    double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
    double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
    double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
    double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
    size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

    if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTSINGLE) {
      
      double gasCp       = gasParameters[0];
      double gasCv       = gasParameters[1];
      double gasCvM1     = 1.0/gasCv;
      double gamma       = gasCp*gasCvM1;
      double gammaMinus1 = gamma-1.0;
      
      
      double kineticEnergy = 0.0;
      for(int iDim = 0;iDim < numDim;iDim++){
        dvIn[2+iDim]   = rhoV[iDim]*rhoM1;
        kineticEnergy += dvIn[2+iDim]*dvIn[2+iDim];
      }
        
      kineticEnergy *= .5;
      double eVal = (rhoE-rho*kineticEnergy);
      //        *e = eVal;
        
      dvIn[0] = gammaMinus1*eVal;
      dvIn[1] = gasCvM1*eVal*rhoM1; // why not just (gamma * i.e. / rho)???

    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTMIX){
      
      // T = (h-e)/R
      // e is the internal energy, R is the specific (mixture) gas constant
      // h = h_f + Cp*(T-T_ref), where h_f and Cp are evaluted at the reference temperature T_ref
      // P = rho*R*T
      
      double internalEnergy = qIn[numDim+1];
      double kineticTerm = 0.0;
      double tMix[3];
      // Get tMix = {R,Hf,Cp}
      CalculateCPMixture(numDim,gasInfo,nonDimen,gasParameters,qIn,tMix);

      for(int iDim = 0;iDim < numDim;iDim++){
        dvIn[2+iDim] = qIn[1+iDim]*rhoM1;
        kineticTerm  += dvIn[2+iDim]*dvIn[2+iDim];
      }
      internalEnergy -= 0.5*rho*kineticTerm;
      
      dvIn[1] = (tMix[1] - tMix[2]*refTemperature-internalEnergy*energyScale*
                 rhoM1/densityScale)/(tMix[0]-tMix[2])/temperatureScale;

      dvIn[0] = rho*tMix[0]*dvIn[1]*densityScale*temperatureScale/pressureScale;
      //    *e = internalEnergy*rhom1;
      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::IDEALMIX) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];
        //        GetPointMassFractions(rho,specificDensity,massFractions);
        double totalMass=1.0;
        for(int i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV*rhoM1;
          dvIn[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;
        //        *e = internalEnergy;

        double Tnew = 300;
        double cp, R;
        double energy = rhoM1*internalEnergy*energyScale/densityScale;
        double density = rho*densityScale;

        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        prometheus::getTemperature(model,energy,&massFractions[0],Tnew,cp,R,true);

        dvIn[0] = density*R*Tnew/pressureScale;
        dvIn[1] = Tnew/temperatureScale;

      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){

#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif
        Cantera::IdealGasMix *locGasMixturePtr = globalCantera[threadId];

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];

        double totalMass=1.0;
        for(size_t i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;
        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          dvIn[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;
        //    *e = internalEnergy;
        double tolerance = 1e-10;
        locGasMixturePtr->setMassFractions(&massFractions[0]);
        locGasMixturePtr->setState_UV(rhoM1*internalEnergy*energyScale/densityScale, 
                                      rhoM1/densityScale,tolerance);
        
        dvIn[0] = locGasMixturePtr->pressure()/pressureScale;
        dvIn[1] = locGasMixturePtr->temperature()/temperatureScale;
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PLASMA) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];

        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV*rhoM1;
          dvIn[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;
        //        *e = internalEnergy;

        double Tnew = 300;
        double cp, R;
        double energy = rhoM1*internalEnergy*energyScale/densityScale;
        double density = rho*densityScale;

        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        prometheus::getTemperature(model,energy,&massFractions[0],Tnew,cp,R,true);

        dvIn[0] = density*R*Tnew/pressureScale;
        dvIn[1] = Tnew/temperatureScale;

      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){

#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif
        Cantera::IdealGasMix *locGasMixturePtr = globalCantera[threadId];

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];

        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          dvIn[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;
        //    *e = internalEnergy;
        double tolerance = 1e-10;
        locGasMixturePtr->setMassFractions(&massFractions[0]);
        locGasMixturePtr->setState_UV(rhoM1*internalEnergy*energyScale/densityScale, 
                                      rhoM1/densityScale,tolerance);
        
        dvIn[0] = locGasMixturePtr->pressure()/pressureScale;
        dvIn[1] = locGasMixturePtr->temperature()/temperatureScale;
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    }
  }

#ifdef USE_HYDRA
#pragma omp declare target
#endif
  // everything is a pointer to facilitate calling from fortran
  void FC_GLOBAL(gasdp,GASDP)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,const double *dvIn,double *dp)
  {
#ifdef EOS_TIMERS
    double entryTime = MPI_Wtime();
#endif
    double rho         =  qIn[0];
    const double *rhoV = &qIn[1];
    int numDim         =  *inDimension;
    double rhoE        =  qIn[numDim+1];
    double rhoM1       =  1.0/rho;

    double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
    double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
    double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
    double pressureScaleInv = 1/pressureScale;
    double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
    double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
    size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];
    
    if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTSINGLE) {
      
      double gasCp       = gasParameters[0];
      double gasCv       = gasParameters[1];
      double gasCvM1     = 1.0/gasCv;
      double gamma       = gasCp*gasCvM1;
      double gammaMinus1 = gamma-1.0;
      size_t numScalars = gasInfo[eos::INFO::NUMSPECIES];

      dp[0] = dvIn[0]*rhoM1;    // dp/d(rho)  = p/rho
      dp[1] = gammaMinus1*rho; // dp/de = (gamma - 1)*rho
      for(size_t iDim = 0;iDim < numScalars;iDim++){
        dp[iDim+2] = 0.0;
      }
      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTMIX){
      
      double internalEnergy = qIn[numDim+1];
      double kineticTerm = 0.0;
      double myTemperature = dvIn[1];
      double tMix[3];
      size_t numSpecies = gasInfo[eos::INFO::NUMSPECIES];
      const double *specificDensity   = &qIn[numDim+2]; // rhoY_i
      double referenceTemperature = nonDimen[eos::DIMEN::REFTEMPERATURE];

      // Gets tMix = {R,Hf,Cp} for the mixture
      CalculateCPMixture(numDim,gasInfo,nonDimen,gasParameters,qIn,tMix);

      for(int iDim = 0;iDim < numDim;iDim++){
        kineticTerm += 0.5*dvIn[2+iDim]*dvIn[2+iDim]*rho;
      }
      internalEnergy -= kineticTerm;
      
      double myGamma     = tMix[2]/(tMix[2]-tMix[0]); // Cp/(Cp - R) 

    
      // gasParams is blocked by numSpecies
      const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
      const double *speciesHeatOfFormation  = &gasParameters[eos::GASPARAM::SPECIESHF*numSpecies];
      const double *speciesHeatCapacityCp   = &gasParameters[eos::GASPARAM::SPECIESCP*numSpecies];
      
      dp[0] = ((tMix[1] - tMix[2]*referenceTemperature - 
                internalEnergy*rhoM1*energyScale/densityScale)/
               (1.0-tMix[2]/tMix[0]))*densityScale/pressureScale;
      dp[1] = rho*(myGamma - 1.0)*energyScale/pressureScale;

      double dpdy1 = 1.0/(1.0-tMix[2]/tMix[0]);

      for(int i=0;i<numSpecies-1;i++){
        dp[2+i] = speciesHeatOfFormation[i] - speciesHeatOfFormation[numSpecies-1] + 
          (speciesHeatCapacityCp[i]-speciesHeatCapacityCp[numSpecies-1])
          *(myTemperature*temperatureScale-referenceTemperature) 
          + tMix[2]*myTemperature*temperatureScale*
          (univGasConst/speciesMolecularWeights[numSpecies-1]-univGasConst/speciesMolecularWeights[i])/
          tMix[0];
        dp[2+i] *= dpdy1*densityScale/pressureScale;
      }

    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::IDEALMIX) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){
        double baseMassFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];
        double totalMass=1.0;
        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        //int numSpecies = prometheus::getNumSpecies(model);
        int numSpecies = gasInfo[eos::INFO::NUMSPECIES];
        for(int i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          baseMassFractions[i] = mass;
          totalMass-=mass;
        }
        baseMassFractions[numSpecies-1] = totalMass;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          ke += dvIn[iDim+2]*dvIn[iDim+2];
        }
        ke *= .5*rho;

        //double internalEnergy = qIn[numDim+1]-ke;
        double baseTemp = dvIn[1]*nonDimen[eos::DIMEN::TEMPERATURESCALE]; // dimensional Temperature
        double basePress = dvIn[0]*nonDimen[eos::DIMEN::PRESSURESCALE];   // dimensional Pressure

        double baseRho = rho*nonDimen[eos::DIMEN::DENSITYSCALE]; // dimensional values for use with Cantera
        double baseEnergy = qIn[numDim+1]-ke;

        //double cv = tempGas.cv_mole() / tempGas.meanMolecularWeight(); // J/kg-K
        /*
          double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
        */
        
        //std::vector<double> molecularWeight(prometheusPtr->molecularWeights());
        //std::vector<double> cpk(numSpecies, 0.0);
        //double molecularWeight[MAXSPECIES] = prometheusPtr->molecularWeights();  
        double cpk[eos::MAXSPECIES];
        prometheus::getSpecificHeats_R(model,baseTemp,cpk);

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        double cp=0.;
        double R=0.;
        double univGasConst = prometheus::UniversalGasConstant();

        for(int k = 0; k < numSpecies; ++k) {
          cpk[k] = 1000*univGasConst*cpk[k]/speciesMolecularWeights[k];
          cp += cpk[k] * baseMassFractions[k];
          R += baseMassFractions[k]/speciesMolecularWeights[k];
        }
        R*=univGasConst*1000;
        double cv = (cp-R)*baseRho; // J/m^3-K


        double speciesInternalEnergy[eos::MAXSPECIES]; 
        //std::vector<double> hk(numSpecies, 0.0);
        double hk[eos::MAXSPECIES];
        prometheus::getEnthalpies_RT(model,baseTemp,hk);
        for(int k = 0; k < numSpecies; ++k) { 
          speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
        }
        //tempGas.getIntEnergy_RT(speciesInternalEnergy); // unitless
        /*
          locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless
        */
        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= baseTemp*univGasConst*1000; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
        }
        
        double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
        for(int i=0;i<numSpecies;i++){
          dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
        }

        double dTdRhoE = 1.0/cv; // K-m^3/J

        double speciesGasConst[eos::MAXSPECIES]; // J/kg-K

        for(int i=0;i<numSpecies;i++){
          speciesGasConst[i] = 1000*univGasConst/speciesMolecularWeights[i];
        } // J/kg-K

        double sumRhoR=0;

        for(int i=0;i<numSpecies;i++){
          sumRhoR += baseRho*baseMassFractions[i]*speciesGasConst[i];} // J/m^3-K

        double dPdRhoI[eos::MAXSPECIES];

        for(int i=0;i<numSpecies;i++){
          dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2

        dp[0] = baseTemp*sumRhoR/baseRho*
          nonDimen[eos::DIMEN::DENSITYSCALE]/nonDimen[eos::DIMEN::PRESSURESCALE];
        dp[1] = baseRho*sumRhoR/cv;
        for(int i=0;i<numSpecies-1;i++){
          dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
          dp[2+i] *= nonDimen[eos::DIMEN::DENSITYSCALE]/nonDimen[eos::DIMEN::PRESSURESCALE];
        }
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif

        Cantera::IdealGasMix *locGasMixturePtr = globalCantera[threadId];
    
        const double *rhoY = &qIn[numDim+2];

        double massFractions[MAXSPECIES];
        double totalMass=1.0;
        for(size_t i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;

        double baseTemp = dvIn[1]*temperatureScale; // dimensional Temperature
        double basePress = dvIn[0]*pressureScale;   // dimensional Pressure

        locGasMixturePtr->setState_TPY(baseTemp,basePress,&massFractions[0]);
 
        double baseRho = locGasMixturePtr->density();
        double baseEnergy = locGasMixturePtr->intEnergy_mass();

        double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
    
        cv *= baseRho; // J/m^3-K


        double speciesInternalEnergy[eos::MAXSPECIES]; 
        locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];

        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= baseTemp*univGasConst*1000.; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
        }

        double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
        for(int i=0;i<numSpecies;i++){
          dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
        }

        double dTdRhoE = 1.0/cv; // K-m^3/J


        // Really need to do this?
        double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
        for(int i=0;i<numSpecies;i++){
          speciesGasConst[i] = 1000.*univGasConst/speciesMolecularWeights[i];} // J/kg-K

        double sumRhoR=0;
        
        for(int i=0;i<numSpecies;i++){
          sumRhoR += baseRho*massFractions[i]*speciesGasConst[i];} // J/m^3-K
        
        double dPdRhoI[eos::MAXSPECIES];
        
        for(int i=0;i<numSpecies;i++){
          dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2

        dp[0] = baseTemp*sumRhoR/baseRho*densityScale/pressureScale;
        dp[1] = baseRho*sumRhoR/cv;
        for(int i=0;i<numSpecies-1;i++){
          dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
          dp[2+i] *= densityScale/pressureScale;
        }
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PLASMA) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){
        double baseMassFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];
        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        int numSpecies = gasInfo[eos::INFO::NUMSPECIES];

        //double totalMass=1.0;
        ////int numSpecies = prometheus::getNumSpecies(model);
        //for(int i=0;i<numSpecies-1;i++){
          //double mass = std::max(0.,rhoY[i]*rhoM1);
          //baseMassFractions[i] = mass;
          //totalMass-=mass;
        //}
        //baseMassFractions[numSpecies-1] = totalMass;

        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          baseMassFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        baseMassFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        baseMassFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          ke += dvIn[iDim+2]*dvIn[iDim+2];
        }
        ke *= .5*rho;

        //double internalEnergy = qIn[numDim+1]-ke;
        double baseTemp = dvIn[1]*nonDimen[eos::DIMEN::TEMPERATURESCALE]; // dimensional Temperature
        double basePress = dvIn[0]*nonDimen[eos::DIMEN::PRESSURESCALE];   // dimensional Pressure

        double baseRho = rho*nonDimen[eos::DIMEN::DENSITYSCALE]; // dimensional values for use with Cantera
        double baseEnergy = qIn[numDim+1]-ke;

        //double cv = tempGas.cv_mole() / tempGas.meanMolecularWeight(); // J/kg-K
        /*
          double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
        */
        
        //std::vector<double> molecularWeight(prometheusPtr->molecularWeights());
        //std::vector<double> cpk(numSpecies, 0.0);
        //double molecularWeight[MAXSPECIES] = prometheusPtr->molecularWeights();  
        double cpk[eos::MAXSPECIES];
        prometheus::getSpecificHeats_R(model,baseTemp,cpk);

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        double cp=0.;
        double R=0.;
        double univGasConst = prometheus::UniversalGasConstant();

        for(int k = 0; k < numSpecies; ++k) {
          cpk[k] = 1000*univGasConst*cpk[k]/speciesMolecularWeights[k];
          cp += cpk[k] * baseMassFractions[k];
          R += baseMassFractions[k]/speciesMolecularWeights[k];
        }
        R*=univGasConst*1000;
        double cv = (cp-R)*baseRho; // J/m^3-K


        double speciesInternalEnergy[eos::MAXSPECIES]; 
        //std::vector<double> hk(numSpecies, 0.0);
        double hk[eos::MAXSPECIES];
        prometheus::getEnthalpies_RT(model,baseTemp,hk);
        for(int k = 0; k < numSpecies; ++k) { 
          speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
        }
        //tempGas.getIntEnergy_RT(speciesInternalEnergy); // unitless
        /*
          locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless
        */
        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= baseTemp*univGasConst*1000; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
        }
        
        double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
        for(int i=0;i<numSpecies;i++){
          dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
        }

        double dTdRhoE = 1.0/cv; // K-m^3/J

        double speciesGasConst[eos::MAXSPECIES]; // J/kg-K

        for(int i=0;i<numSpecies;i++){
          speciesGasConst[i] = 1000*univGasConst/speciesMolecularWeights[i];
        } // J/kg-K

        double sumRhoR=0;

        for(int i=0;i<numSpecies;i++){
          sumRhoR += baseRho*baseMassFractions[i]*speciesGasConst[i];} // J/m^3-K

        double dPdRhoI[eos::MAXSPECIES];

        for(int i=0;i<numSpecies;i++){
          dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2

        dp[0] = baseTemp*sumRhoR/baseRho*
          nonDimen[eos::DIMEN::DENSITYSCALE]/nonDimen[eos::DIMEN::PRESSURESCALE];
        dp[1] = baseRho*sumRhoR/cv;
        //for(int i=0;i<numSpecies-1;i++){
          //dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
          //dp[2+i] *= nonDimen[DIMEN::DENSITYSCALE]/nonDimen[DIMEN::PRESSURESCALE];
        //}
        for(int i=0;i<numSpecies-2;i++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+i]);
          dp[2+i] = dPdRhoI[chargeIndex] + 
            gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i]*
            dPdRhoI[gasInfo[eos::INFO::ELECTRONINDEX]] - 
            (1.0 + gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i])*
            dPdRhoI[gasInfo[eos::INFO::ABUNDANTINDEX]];
          dp[2+i] *= pressureScaleInv*densityScale;
        }
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif

        Cantera::IdealGasMix *locGasMixturePtr = eos::globalCantera[threadId];

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double pressureScaleInv = 1/pressureScale;
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        const double *rhoY = &qIn[numDim+2];

        double massFractions[eos::MAXSPECIES];
        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;

        double baseTemp = dvIn[1]*temperatureScale; // dimensional Temperature
        double basePress = dvIn[0]*pressureScale;   // dimensional Pressure

        locGasMixturePtr->setState_TPY(baseTemp,basePress,&massFractions[0]);
 
        double baseRho = locGasMixturePtr->density();
        double baseEnergy = locGasMixturePtr->intEnergy_mass();

        double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
    
        cv *= baseRho; // J/m^3-K


        double speciesInternalEnergy[eos::MAXSPECIES]; 
        locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];

        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= baseTemp*univGasConst*1000.; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
        }

        double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
        for(int i=0;i<numSpecies;i++){
          dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
        }

        double dTdRhoE = 1.0/cv; // K-m^3/J


        // Really need to do this?
        double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
        for(int i=0;i<numSpecies;i++){
          speciesGasConst[i] = 1000.*univGasConst/speciesMolecularWeights[i];} // J/kg-K

        double sumRhoR=0;
        
        for(int i=0;i<numSpecies;i++){
          sumRhoR += baseRho*massFractions[i]*speciesGasConst[i];} // J/m^3-K
        
        double dPdRhoI[eos::MAXSPECIES];
        
        for(int i=0;i<numSpecies;i++){
          dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2

        dp[0] = baseTemp*sumRhoR/baseRho*densityScale/pressureScale;
        dp[1] = baseRho*sumRhoR/cv;
        for(int i=0;i<numSpecies-2;i++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+i]);
          dp[2+i] = dPdRhoI[chargeIndex] + 
            gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i]*
            dPdRhoI[gasInfo[eos::INFO::ELECTRONINDEX]] - 
            (1.0 + gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i])*
            dPdRhoI[gasInfo[eos::INFO::ABUNDANTINDEX]];
          dp[2+i] *= pressureScaleInv*densityScale;
        }
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    }
#ifdef EOS_TIMERS
    double routineTime = MPI_Wtime() - entryTime;
    if(!eos::eosTimers.empty()){
      eos::eosTimers[eos::TIMERS::DPTIMER] += routineTime;
      eos::eosCallCounts[eos::TIMERS::DPTIMER]++;
    }
#endif
  }

#ifdef USE_HYDRA
#pragma omp end declare target
#endif
  // everything is a pointer to facilitate calling from fortran
  void FC_GLOBAL(gasisothermal,GASISOTHERMAL)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *isoT,double *qIn)
  {
#ifdef EOS_TIMERS
    double entryTime = MPI_Wtime();
#endif
    double rho         =  qIn[0];
    const double *rhoV = &qIn[1];
    int numDim         =  *inDimension;
    double rhoE        =  qIn[numDim+1];
    double rhoM1       =  1.0/rho;

    double referenceTemperature = nonDimen[eos::DIMEN::REFTEMPERATURE];
    double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
    double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
    double temperatureScale     = nonDimen[eos::DIMEN::TEMPERATURESCALE];
    double targetT     = *isoT/temperatureScale;
    
    if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTSINGLE) {
      
      double gasCp       = gasParameters[0];
      double gasCv       = gasParameters[1];
      double gasCvM1     = 1.0/gasCv;
      double gamma       = gasCp*gasCvM1;
      double gammaMinus1 = gamma-1.0;
      double specificGasConstant = nonDimen[eos::DIMEN::SPECIFICGASCONSTANT];
      
      qIn[numDim+1] = rho*targetT*specificGasConstant/gammaMinus1;
      double kineticEnergy = 0.0;
      for(int iDim = 0;iDim < numDim;iDim++){
        kineticEnergy += qIn[iDim+1]*qIn[iDim+1]*rhoM1;
      }
      kineticEnergy *= .5;
      qIn[numDim+1] += kineticEnergy;
      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTMIX){

      // ! internalEnergy = T_wall * (heatCapacityCpMix-specificGasConstMix) &
      // !                  +heatOfFormMix-heatCapacityCpMix*refTempCp
      // ! ! energyScale is rho*e
      // ! gI2(numDim+2) = internalEnergy/energyScale*densityScale*gI2(1) 

      double tMix[3];

      // Gets tMix = {R,Hf,Cp} for the mixture
      CalculateCPMixture(numDim,gasInfo,nonDimen,gasParameters,qIn,tMix);
      double tTarget = *isoT;
      double internalEnergy = tTarget*(tMix[2]-tMix[0])+(tMix[1]-tMix[2]*referenceTemperature);
      qIn[numDim+1] = internalEnergy/energyScale*densityScale*rho;

    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::IDEALMIX) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){

        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        double univGasConst = prometheus::UniversalGasConstant();
        int numSpecies = prometheus::getNumSpecies(model);
        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        //double speciesMolecularWeights[eos::MAXSPECIES];
        //prometheus::getMolecularWeights(model,&speciesMolecularWeights[0]);

        double baseMassFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];
        double totalMass=1.0;
        for(int i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          baseMassFractions[i] = mass;
          totalMass-=mass;
        }
        baseMassFractions[numSpecies-1] = totalMass;

        double tTarget = *isoT;

        double speciesInternalEnergy[eos::MAXSPECIES]; 
        //std::vector<double> hk(numSpecies, 0.0);
        double hk[eos::MAXSPECIES];
        prometheus::getEnthalpies_RT(model,tTarget,hk);
        for(int k = 0; k < numSpecies; ++k) { 
          speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
        }

        double energy = 0.;
        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= tTarget*univGasConst*1000; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          energy += speciesInternalEnergy[i]*baseMassFractions[i];
          //std::cout << "bassMassFractions[" << i << "] " << baseMassFractions[i] << std::endl;
        }
        
        //std::cout << "energy " << energy << std::endl;
        //std::cout << "energy2 " << energy/energyScale*rho << std::endl;
        
        qIn[numDim+1] = rho*nonDimen[eos::DIMEN::DENSITYSCALE]*energy/nonDimen[eos::DIMEN::ENERGYSCALE];

      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif
        Cantera::IdealGasMix *locGasMixturePtr = eos::globalCantera[threadId];
    
        double tTarget = *isoT;

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        const double *rhoY = &qIn[numDim+2];
        
        double massFractions[eos::MAXSPECIES];
        double totalMass=1.0;
        for(size_t i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;
        
        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;

        
        double specificGasConstant=univGasConst/speciesMolecularWeights[numSpecies-1];
        for(size_t i=0;i<numSpecies-1;i++){
          // we can remove a subtraction if we do it apriori when the eos is setup
          specificGasConstant += rhoY[i]/rho*
            (univGasConst/speciesMolecularWeights[i]-univGasConst/speciesMolecularWeights[numSpecies-1]);
        }

        specificGasConstant *= 1000.0;

        //double pressure = rho*specificGasConstant*tTarget*temperatureScale;
        double pressure = rho*densityScale*specificGasConstant*tTarget;

        //std::cout << "tTarget " << tTarget << std::endl;
        //std::cout << "tTarget2 " << tTarget/temperatureScale << std::endl;
        //std::cout << "pressure " << pressure << std::endl;
        //std::cout << "pressure2 " << pressure/pressureScale << std::endl;

        locGasMixturePtr->setState_TPY(tTarget,pressure,&massFractions[0]);
        double energy = locGasMixturePtr->intEnergy_mass();

        //std::cout << locGasMixturePtr->report();
        
        //std::cout << "rho " << rho*densityScale << std::endl;
        //std::cout << "rho2 " << rho << std::endl;
        //std::cout << "energy " << energy << std::endl;
        //std::cout << "energy2 " << energy/energyScale << std::endl;
        //std::cout << "energy3 " << rho*densityScale*energy/energyScale << std::endl;
        
        //for(int i=0;i<numSpecies;i++)
        //std::cout << "y[" << i << "] " << baseMassFractions[i] << std::endl;

        qIn[numDim+1] = rho*densityScale*energy/energyScale;
    
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PLASMA) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){

        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        double univGasConst = prometheus::UniversalGasConstant();
        int numSpecies = prometheus::getNumSpecies(model);
        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        //double speciesMolecularWeights[eos::MAXSPECIES];
        //prometheus::getMolecularWeights(model,&speciesMolecularWeights[0]);

        double baseMassFractions[eos::MAXSPECIES];
        const double *rhoY = &qIn[numDim+2];

        //double totalMass=1.0;
        //for(int i=0;i<numSpecies-1;i++){
          //double mass = std::max(0.,rhoY[i]*rhoM1);
          //baseMassFractions[i] = mass;
          //totalMass-=mass;
        //}
        //baseMassFractions[numSpecies-1] = totalMass;

        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          baseMassFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        baseMassFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        baseMassFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double tTarget = *isoT;

        double speciesInternalEnergy[eos::MAXSPECIES]; 
        //std::vector<double> hk(numSpecies, 0.0);
        double hk[eos::MAXSPECIES];
        prometheus::getEnthalpies_RT(model,tTarget,hk);
        for(int k = 0; k < numSpecies; ++k) { 
          speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
        }

        double energy = 0.;
        for(int i=0;i<numSpecies;i++){  
          speciesInternalEnergy[i] *= tTarget*univGasConst*1000; // J/kMol
          speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          energy += speciesInternalEnergy[i]*baseMassFractions[i];
          //std::cout << "bassMassFractions[" << i << "] " << baseMassFractions[i] << std::endl;
        }
        
        //std::cout << "energy " << energy << std::endl;
        //std::cout << "energy2 " << energy/energyScale*rho << std::endl;
        
        qIn[numDim+1] = rho*nonDimen[eos::DIMEN::DENSITYSCALE]*energy/nonDimen[eos::DIMEN::ENERGYSCALE];

      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif
        Cantera::IdealGasMix *locGasMixturePtr = eos::globalCantera[threadId];
    
        double tTarget = *isoT;

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        const double *rhoY = &qIn[numDim+2];
        
        double massFractions[eos::MAXSPECIES];
        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;
        
        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = qIn[1+iDim];
          double v = rhoV/rho;
          ke += v*v;
        }
        ke *= .5*rho;
        double internalEnergy = qIn[numDim+1]-ke;

        double specificGasConstant=univGasConst/speciesMolecularWeights[numSpecies-1];
        for(size_t i=0;i<numSpecies-1;i++){
          // we can remove a subtraction if we do it apriori when the eos is setup
          specificGasConstant += rhoY[i]/rho*
            (univGasConst/speciesMolecularWeights[i]-univGasConst/speciesMolecularWeights[numSpecies-1]);
        }

        specificGasConstant *= 1000.0;

        //double pressure = rho*specificGasConstant*tTarget*temperatureScale;
        double pressure = rho*densityScale*specificGasConstant*tTarget;

        //std::cout << "tTarget " << tTarget << std::endl;
        //std::cout << "tTarget2 " << tTarget/temperatureScale << std::endl;
        //std::cout << "pressure " << pressure << std::endl;
        //std::cout << "pressure2 " << pressure/pressureScale << std::endl;

        locGasMixturePtr->setState_TPY(tTarget,pressure,&massFractions[0]);
        double energy = locGasMixturePtr->intEnergy_mass();

        //std::cout << locGasMixturePtr->report();
        
        //std::cout << "rho " << rho*densityScale << std::endl;
        //std::cout << "rho2 " << rho << std::endl;
        //std::cout << "energy " << energy << std::endl;
        //std::cout << "energy2 " << energy/energyScale << std::endl;
        //std::cout << "energy3 " << rho*densityScale*energy/energyScale << std::endl;
        
        //for(int i=0;i<numSpecies;i++)
        //std::cout << "y[" << i << "] " << baseMassFractions[i] << std::endl;

        qIn[numDim+1] = rho*densityScale*energy/energyScale;
    
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    }
#ifdef EOS_TIMERS
    double routineTime = MPI_Wtime() - entryTime;
    if(!eos::eosTimers.empty()){
      eos::eosTimers[eos::TIMERS::ISOTHERMALTIMER] += routineTime;
      eos::eosCallCounts[eos::TIMERS::ISOTHERMALTIMER]++;
    }
#endif
  }

#ifdef USE_HYDRA
#pragma omp declare target
#endif
  // everything is a pointer to facilitate calling from fortran
  void FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,const double *qIn,const double *dvIn,
     const double *dpIn,double *soundSpeed)
  {
    double rho         =  qIn[0];
    const double *rhoV = &qIn[1];
    int numDim         =  *inDimension;
    double rhoE        =  qIn[numDim+1];
    double rhoM1       =  1.0/rho;
    
    if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTSINGLE) {
      
      double gasCp       = gasParameters[0];
      double gasCv       = gasParameters[1];
      double gasCvM1     = 1.0/gasCv;
      double gamma       = gasCp*gasCvM1;

      *soundSpeed = std::sqrt(gamma*dvIn[0]*rhoM1);
      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTMIX){
      
      double c2 = dpIn[0] + dpIn[1]*rhoM1 * (dvIn[0]*rhoM1);
      
      if(c2 < 0){
        #ifdef USE_HYDRA
        printf("CP EOS::GASSOUNDSPEED error!\n");
        #else
        std::cout << "CP EOS::GASSOUNDSPEED error!" << std::endl
                  << "P       = "   << dvIn[0]      << std::endl
                  << "rho     = "   << rho          << std::endl
                  << "dP/drho = "   << dpIn[0]      << std::endl
                  << "dP/de   = "   << dpIn[1]      << std::endl
                  << "c2      = "   << c2           << std::endl;
        exit(1);
        #endif
      }

      *soundSpeed = std::sqrt(c2);

      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::IDEALMIX) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){
        double c2 = dpIn[0] + dpIn[1]/rho * (dvIn[0]/rho);

        if(c2 < 0){
          #ifdef USE_HYDRA
          printf("PROMETHEUS EOS::GASSOUNDSPEED error!\n");
          #else
          std::cout << "PROMETHEUS EOS::GASSOUNDSPEED error!" << std::endl
                    << "P       = "   << dvIn[0] << std::endl
                    << "rho     = "   << rho     << std::endl
                    << "dP/drho = "   << dpIn[0] << std::endl
                    << "dP/de   = "   << dpIn[1] << std::endl
                    << "c2      = "   << c2      << std::endl;
          exit(1);
          #endif
        }

        *soundSpeed = std::sqrt(c2);
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        double c2 = dpIn[0] + dpIn[1]*rhoM1*(dvIn[0]*rhoM1);
        if(c2 < 0){
          std::cout << "CANTERA EOS::GASSOUNDSPEED error!" << std::endl
                    << "P       = "   << dvIn[0] << std::endl
                    << "rho     = "   << rho     << std::endl
                    << "dP/drho = "   << dpIn[0] << std::endl
                    << "dP/de   = "   << dpIn[1] << std::endl
                    << "c2      = "   << c2      << std::endl;
          exit(1);
        }
        *soundSpeed = std::sqrt(c2);
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PLASMA) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){
        double c2 = dpIn[0] + dpIn[1]/rho * (dvIn[0]/rho);

        if(c2 < 0){
          #ifdef USE_HYDRA
          printf("PROMETHEUS EOS::GASSOUNDSPEED error!\n");
          #else
          std::cout << "PROMETHEUS EOS::GASSOUNDSPEED error!" << std::endl
                    << "P       = "   << dvIn[0] << std::endl
                    << "rho     = "   << rho     << std::endl
                    << "dP/drho = "   << dpIn[0] << std::endl
                    << "dP/de   = "   << dpIn[1] << std::endl
                    << "c2      = "   << c2      << std::endl;
          exit(1);
          #endif
        }

        *soundSpeed = std::sqrt(c2);
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        double c2 = dpIn[0] + dpIn[1]*rhoM1*(dvIn[0]*rhoM1);
        if(c2 < 0){
          std::cout << "CANTERA EOS::GASSOUNDSPEED error!" << std::endl
                    << "P       = "   << dvIn[0] << std::endl
                    << "rho     = "   << rho     << std::endl
                    << "dP/drho = "   << dpIn[0] << std::endl
                    << "dP/de   = "   << dpIn[1] << std::endl
                    << "c2      = "   << c2      << std::endl;
          exit(1);
        }
        *soundSpeed = std::sqrt(c2);
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    }
  }


  // everything is a pointer to facilitate calling from fortran
  void FC_GLOBAL(gaseosroe,GASEOSROE)
    (const int *inDimension,const size_t *gasInfo,const double *gasParameters,
     const double *nonDimen,double *q,double *dv,double *dp,double *c)
  {
#ifdef EOS_TIMERS
    double entryTime = MPI_Wtime();
#endif
    double rho         =  q[0];
    const double *rhoV = &q[1];
    int numDim         =  *inDimension;
    double rhoE        =  q[numDim+1];
    double rhoM1       =  1.0/rho;
    
    if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTSINGLE) {
      
      double gasCp       = gasParameters[0];
      double gasCv       = gasParameters[1];
      double gasCvM1     = 1.0/gasCv;
      double gamma       = gasCp*gasCvM1;
      double gammaMinus1 = gamma-1.0;      
      double specificGasConstant = nonDimen[eos::DIMEN::SPECIFICGASCONSTANT];
      double H  = rhoE;
      size_t numScalars = gasInfo[eos::INFO::NUMSPECIES];
     
      double kineticEnergy = 0.0;
      for(int iDim = 0;iDim < numDim;iDim++){
        dv[2+iDim] = rhoV[iDim]*rhoM1;
        kineticEnergy += dv[2+iDim]*dv[2+iDim];
      }
      kineticEnergy *= .5;

      dv[1] = (H-kineticEnergy)/gasCp;
      dv[0] = rho*specificGasConstant*dv[1];

      // H = (rho*E+P)/rho -> rho*E = H*rho-P
      q[numDim+1] = q[numDim+1]*q[0]-dv[0];

      // we could replace these with the calls to the functions that compute them
      // but is that a performance hit?
      dp[0] = dv[0]*rhoM1;
      dp[1] = rho*gammaMinus1;
    
    // Set dp/dY = 0 for passive scalars [mtc]
      for(size_t iScalar = 0;iScalar < numScalars;iScalar++){
        dp[2+iScalar] = 0.0;
      }

      *c = std::sqrt(gamma*dv[0]*rhoM1);
      
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PERFECTMIX){

      double H = q[numDim+1];
      double kineticTerm = 0.0;
      double tMix[3];

      // Gets tMix = {R,Hf,Cp} for the mixture
      CalculateCPMixture(numDim,gasInfo,nonDimen,gasParameters,q,tMix);

      for(int iDim = 0;iDim < numDim;iDim++){
        dv[2+iDim] = q[1+iDim]*rhoM1;
        kineticTerm  += dv[2+iDim]*dv[2+iDim];
      }
      double ke = 0.5*rho*kineticTerm;
      H -= ke;
    
      dv[1] = ((H-kineticTerm-tMix[1])*nonDimen[eos::DIMEN::ENERGYSCALE]+
               tMix[2]*nonDimen[eos::DIMEN::REFTEMPERATURE])/
        tMix[2]/nonDimen[eos::DIMEN::TEMPERATURESCALE];

      dv[0] = rho*tMix[0]*dv[1]*nonDimen[eos::DIMEN::DENSITYSCALE]*
        nonDimen[eos::DIMEN::TEMPERATURESCALE]/nonDimen[eos::DIMEN::PRESSURESCALE];

      // H = (rho*E+P)/rho -> rho*E = H*rho-P
      q[numDim+1] = q[numDim+1]*rho-dv[0];
      
      
      //       FC_GLOBAL(gasdp,GASDP)
      //         (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp);
      {
        H = q[numDim+1] - ke;
        double myTemperature = dv[1];
        size_t numSpecies = gasInfo[eos::INFO::NUMSPECIES];
        const double *specificDensity   = &q[numDim+2]; // rhoY_i
        double univGasConst = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        double referenceTemperature = nonDimen[eos::DIMEN::REFTEMPERATURE];      
        double myGamma     = tMix[2]/(tMix[2]-tMix[0]); // Cp/(Cp - R) 
        
        
        // gasParams is blocked by numSpecies
        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        const double *speciesHeatOfFormation  = &gasParameters[eos::GASPARAM::SPECIESHF*numSpecies];
        const double *speciesHeatCapacityCp   = &gasParameters[eos::GASPARAM::SPECIESCP*numSpecies];
        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        
        dp[0] = ((tMix[1] - tMix[2]*referenceTemperature - 
                  H*rhoM1*energyScale/densityScale)/
                 (1.0-tMix[2]/tMix[0]))*densityScale/pressureScale;
        dp[1] = rho*(myGamma - 1.0)*energyScale/pressureScale;
        
        double dpdy1 = 1.0/(1.0-tMix[2]/tMix[0]);
        
        for(int i=0;i<numSpecies-1;i++){
          dp[2+i] = speciesHeatOfFormation[i] - speciesHeatOfFormation[numSpecies-1] + 
            (speciesHeatCapacityCp[i]-speciesHeatCapacityCp[numSpecies-1])
            *(myTemperature*temperatureScale-referenceTemperature) 
            + tMix[2]*myTemperature*temperatureScale*
            (univGasConst/speciesMolecularWeights[numSpecies-1]-univGasConst/speciesMolecularWeights[i])/
            tMix[0];
          dp[2+i] *= dpdy1*densityScale/pressureScale;
        }
        
      }
      //       FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
      //         (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp,c);
      {
        double c2 = dp[0] + dp[1]*rhoM1 * (dv[0]*rhoM1);
        
        if(c2 < 0){
          #ifdef USE_HYDRA
          #else
          printf("CP EOS::GASEOSROE error! = \n");
          std::cout << "CP EOS::GASEOSROE error!" << std::endl
                    << "P       = "   << dv[0]        << std::endl
                    << "rho     = "   << rho          << std::endl
                    << "dP/drho = "   << dp[0]        << std::endl
                    << "dP/de   = "   << dp[1]        << std::endl
                    << "c2      = "   << c2           << std::endl;
          exit(1);
          #endif
        }
        *c = std::sqrt(c2);
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::IDEALMIX) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        int model               = gasInfo[eos::INFO::PROMETHEUSMODEL];
        double univGasConst     = prometheus::UniversalGasConstant();
        int numSpecies          = prometheus::getNumSpecies(model);

        const double *speciesMolecularWeights = 
          &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
 
        //double speciesMolecularWeights[eos::MAXSPECIES];
        //prometheus::getMolecularWeights(model,&speciesMolecularWeights[0]);

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &q[numDim+2];
        double totalMass=1.0;
        for(int i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;


        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = q[1+iDim];
          double v = rhoV/rho;
          dv[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double enthalpy = q[numDim+1]-ke;
        enthalpy*=energyScale;

        // Assume value coming in contains initial guess for temperature
        double temperature = dv[1]*temperatureScale;
        double cp, R;
        double density = rho*densityScale;
        prometheus::getTemperature(model,enthalpy,&massFractions[0],temperature,cp,R,false);

        double basePress  = density*R*temperature;
        double baseTemp   = temperature;

        dv[0] = basePress/pressureScale;
        dv[1] = baseTemp/temperatureScale;

        q[numDim+1] = q[numDim+1]*q[0]-dv[0];
        double baseEnergy = q[numDim+1] - ke;
        
        //         FC_GLOBAL(gasdp,GASDP)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp);
        {
          cp=0.;
          R=0.;
          double workSpace[eos::MAXSPECIES];

          {
            //            double cpk[eos::MAXSPECIES];
            double *cpk = &workSpace[0];
            prometheus::getSpecificHeats_R(model,baseTemp,cpk);
            for(int k = 0; k < numSpecies; ++k) {
              cpk[k] = 1000*univGasConst*cpk[k]/speciesMolecularWeights[k];
              cp += cpk[k] * massFractions[k];
              R += massFractions[k]/speciesMolecularWeights[k];
            }
          }
          
          R*=univGasConst*1000;
          double cv = (cp-R)*density; // J/m^3-K

          double *speciesInternalEnergy = &workSpace[0];
          //          double speciesInternalEnergy[eos::MAXSPECIES]; 
          {
            double hk[eos::MAXSPECIES];
            prometheus::getEnthalpies_RT(model,baseTemp,hk);
            for(int k = 0; k < numSpecies; ++k) { 
              speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
            }
          }

          for(int i=0;i<numSpecies;i++){  
            speciesInternalEnergy[i] *= baseTemp*univGasConst*1000; // J/kMol
            speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          }
        
          double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
          for(int i=0;i<numSpecies;i++){
            dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
          }

          double dTdRhoE = 1.0/cv; // K-m^3/J
          
          //          double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
          double *speciesGasConst = &workSpace[0];
          for(int i=0;i<numSpecies;i++){
            speciesGasConst[i] = 1000*univGasConst/speciesMolecularWeights[i];
          } // J/kg-K
          
          double sumRhoR=0;
          
          for(int i=0;i<numSpecies;i++){
            sumRhoR += density*massFractions[i]*speciesGasConst[i]; // J/m^3-K
          }
          
          // double dPdRhoI[eos::MAXSPECIES];
          double *dPdRhoI = &workSpace[0];
          for(int i=0;i<numSpecies;i++){
            dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2
          
          dp[0] = baseTemp*sumRhoR/density*densityScale/pressureScale;
          dp[1] = density*sumRhoR/cv;
          for(int i=0;i<numSpecies-1;i++){
            dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
            dp[2+i] *= densityScale/pressureScale;
          }
        }

//         FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
//           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp,c);
        {
          double c2 = dp[0] + dp[1]/rho * (dv[0]/rho);
          
          if(c2 < 0){
#ifdef USE_HYDRA
            printf("CP EOS::GASEOSROE error! = \n");
#else
            std::cout << "PROMETHEUS EOS::GASEOSROE error!" << std::endl
                      << "P       = "   << dv[0]                << std::endl
                      << "rho     = "   << rho                  << std::endl
                      << "dP/drho = "   << dp[0]                << std::endl
                      << "dP/de   = "   << dp[1]                << std::endl
                      << "c2      = "   << c2                   << std::endl;
            exit(1);
            #endif
          }
          
          *c = std::sqrt(c2);
        }
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif

        Cantera::IdealGasMix *locGasMixturePtr = eos::globalCantera[threadId];
    

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        const double *rhoY = &q[numDim+2];

        double massFractions[eos::MAXSPECIES];
        double totalMass=1.0;
        for(size_t i=0;i<numSpecies-1;i++){
          double mass = std::max(0.,rhoY[i]*rhoM1);
          massFractions[i] = mass;
          totalMass-=mass;
        }
        massFractions[numSpecies-1] = totalMass;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = q[1+iDim];
          double v = rhoV/rho;
          dv[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double enthalpy = q[numDim+1]-ke;
        enthalpy*=energyScale;

        locGasMixturePtr->setMassFractions(&massFractions[0]);
    
        double specificGasConstant=univGasConst/speciesMolecularWeights[numSpecies-1];
        for(size_t i=0;i<numSpecies-1;i++){
          // we can remove a subtraction if we do it apriori when the eos is setup
          specificGasConstant+=rhoY[i]*rhoM1*
            (univGasConst/speciesMolecularWeights[i]-univGasConst/speciesMolecularWeights[numSpecies-1]);
        }
        
        specificGasConstant *= 1000.0;

        // bracket the problem with the minimum and maximum valid temperatures
        double minTemp = locGasMixturePtr->minTemp();
        double maxTemp = locGasMixturePtr->maxTemp();
        //std::cout << "minTemp " << minTemp << " maxTemp " << maxTemp << std::endl;
        
        double tempLo = minTemp;
        double pressureLo = rho*densityScale*specificGasConstant*tempLo;
        locGasMixturePtr->setState_TPY(tempLo,pressureLo,&massFractions[0]);
        double enthalpyLo = locGasMixturePtr->enthalpy_mass();
        
        double tempHi = maxTemp;
        double pressureHi = rho*densityScale*specificGasConstant*tempHi;
        locGasMixturePtr->setState_TPY(tempHi,pressureHi,&massFractions[0]);
        double enthalpyHi = locGasMixturePtr->enthalpy_mass();
        
        //std::cout << "EnthalpyLo " << enthalpyLo << " EnthalpyHi " << enthalpyHi << std::endl;
        //std::cout << "PressureLo " << pressureLo << " PressureHi " << pressureHi << std::endl;
        //std::cout << "TemperatureLo " << tempLo << " temperatureHi " << tempHi << std::endl;

        double toler = 1.e-9;
        int iter = 0;
        double error = 1.0;
        double tempGuess = tempLo + (enthalpy - enthalpyLo)*(tempHi-tempLo)/(enthalpyHi-enthalpyLo);
        //std::cout << "Goal enthalpy " << enthalpy << std::endl;
        //std::cout << "tempGuess " << tempGuess << std::endl;
        //std::cout << "toler " << toler << std::endl;
        while((std::abs(error/enthalpy) > toler) && (iter < 50)) {

          double pressureGuess = rho*densityScale*specificGasConstant*tempGuess;
          locGasMixturePtr->setState_TPY(tempGuess,pressureGuess,&massFractions[0]);
          double enthalpyGuess = locGasMixturePtr->enthalpy_mass();

          if(enthalpyGuess > enthalpy){
            enthalpyHi = enthalpyGuess;
            tempHi = tempGuess;
          } else {
            enthalpyLo = enthalpyGuess;
            tempLo = tempGuess;
          }
          // could we get cP here from Cantera for a better estimate?
          double dTdH = (tempHi-tempLo)/(enthalpyHi-enthalpyLo);
          error = (enthalpy-enthalpyGuess);
          double dT = dTdH*error;
          //tempGuess = tempLo + (enthalpy - enthalpyLo)*(tempHi-tempLo)/(enthalpyHi-enthalpyLo);
          tempGuess = tempGuess + dT;
          
          iter++; 
          //std::cout << iter << " error " << error/enthalpy
          //<< " enthalpyGuess " << enthalpyGuess 
          //<< " temperatureGuess " << tempGuess 
          //<< std::endl;
        }
        
        if(iter > 50) {
          std::cout << "Failed to converge in EOSRoe" << std::endl;
          std::cout << iter << " error " << error/enthalpy
                    << " enthalpyGoal " << enthalpy
                    << " temperatureGuess " << tempGuess << std::endl
                    << "minTemp " << minTemp << " maxTemp " << maxTemp
                    << std::endl;
        }
        
        double baseTemp = locGasMixturePtr->temperature();
        double basePress = locGasMixturePtr->pressure();
        dv[0] = basePress/pressureScale;
        dv[1] = baseTemp/temperatureScale;

        //std::cout << locGasMixturePtr->report() << std::endl;
        // H = (rho*E+P)/rho -> rho*E = H*rho-P
        q[numDim+1] = q[numDim+1]*q[0]-dv[0];

        //         FC_GLOBAL(gasdp,GASDP)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp);
        {
          
          double internalEnergy = q[numDim+1]-ke;
          
          locGasMixturePtr->setState_TPY(baseTemp,basePress,&massFractions[0]);
          
          double baseRho = locGasMixturePtr->density();
          double baseEnergy = locGasMixturePtr->intEnergy_mass();
          double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
    
          cv *= baseRho; // J/m^3-K

          
          double speciesInternalEnergy[eos::MAXSPECIES]; 
          locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless

          for(int i=0;i<numSpecies;i++){  
            speciesInternalEnergy[i] *= baseTemp*univGasConst*1000.; // J/kMol
            speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          }
          
          double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
          
          for(int i=0;i<numSpecies;i++){
            dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
          }
          
          double dTdRhoE = 1.0/cv; // K-m^3/J
          
          
          // Really need to do this?
          double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
          for(int i=0;i<numSpecies;i++){
            speciesGasConst[i] = 1000.*univGasConst/speciesMolecularWeights[i]; // J/kg-K
          }

          double sumRhoR=0;
        
          for(int i=0;i<numSpecies;i++){
            sumRhoR += baseRho*massFractions[i]*speciesGasConst[i]; // J/m^3-K
          }
          
          double dPdRhoI[eos::MAXSPECIES];
          
          for(int i=0;i<numSpecies;i++){
            dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i]; // m^2/s^2
          }
          
          dp[0] = baseTemp*sumRhoR/baseRho*densityScale/pressureScale;
          dp[1] = baseRho*sumRhoR/cv;
          for(int i=0;i<numSpecies-1;i++){
            dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
            dp[2+i] *= densityScale/pressureScale;
          } 
        }
        //         FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp,c);
        {
          double c2 = dp[0] + dp[1]*rhoM1*(dv[0]*rhoM1);
          if(c2 < 0){
            std::cerr << "GASSOUNDSPEED:Cantera:Fatal error c2 = " << c2 << std::endl;
            exit(1);
          }
          *c = std::sqrt(c2);
        }
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    } else if(gasInfo[eos::INFO::TYPE] == eos::GASTYPE::PLASMA) {
      if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::PROMETHEUS){
        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double pressureScaleInv = 1./pressureScale;
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        int model = gasInfo[eos::INFO::PROMETHEUSMODEL]; 
        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        //double speciesMolecularWeights[eos::MAXSPECIES];
        //prometheus::getMolecularWeights(model,&speciesMolecularWeights[0]);

        double massFractions[eos::MAXSPECIES];
        const double *rhoY = &q[numDim+2];
        //double totalMass=1.0;
        //for(int i=0;i<numSpecies-1;i++){
          //double mass = std::max(0.,rhoY[i]*rhoM1);
          //massFractions[i] = mass;
          //totalMass-=mass;
        //}
        //massFractions[numSpecies-1] = totalMass;

        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;


        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = q[1+iDim];
          double v = rhoV/rho;
          dv[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double enthalpy = q[numDim+1]-ke;
        enthalpy*=energyScale;

        double temperature = dv[1]*temperatureScale; // incoming guess
        double cp, R;
        double density = rho*densityScale;
        prometheus::getTemperature(model,enthalpy,&massFractions[0],temperature,cp,R,false);

        double basePress  = density*R*temperature;
        double baseTemp   = temperature;

        dv[0] = basePress/pressureScale;
        dv[1] = baseTemp/temperatureScale;

        q[numDim+1] = q[numDim+1]*q[0]-dv[0];
        double baseEnergy = q[numDim+1] - ke;
        
        //         FC_GLOBAL(gasdp,GASDP)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp);
        {
          double cpk[eos::MAXSPECIES];
          prometheus::getSpecificHeats_R(model,baseTemp,cpk);
        
          cp=0.;
          R=0.;

          for(int k = 0; k < numSpecies; ++k) {
            cpk[k] = 1000*univGasConst*cpk[k]/speciesMolecularWeights[k];
            cp += cpk[k] * massFractions[k];
            R += massFractions[k]/speciesMolecularWeights[k];
          }
          R*=univGasConst*1000;
          double cv = (cp-R)*density; // J/m^3-K


          double speciesInternalEnergy[eos::MAXSPECIES]; 
          //std::vector<double> hk(numSpecies, 0.0);
          double hk[eos::MAXSPECIES];
          prometheus::getEnthalpies_RT(model,baseTemp,hk);
          for(int k = 0; k < numSpecies; ++k) { 
            speciesInternalEnergy[k] = hk[k] - 1.0; // h/RT = e/RT + P/(rho*R*T)
          }
          for(int i=0;i<numSpecies;i++){  
            speciesInternalEnergy[i] *= baseTemp*univGasConst*1000; // J/kMol
            speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          }
        
          double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
        
          for(int i=0;i<numSpecies;i++){
            dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
          }

          double dTdRhoE = 1.0/cv; // K-m^3/J
          
          double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
          
          for(int i=0;i<numSpecies;i++){
            speciesGasConst[i] = 1000*univGasConst/speciesMolecularWeights[i];
          } // J/kg-K
          
          double sumRhoR=0;
          
          for(int i=0;i<numSpecies;i++){
            sumRhoR += density*massFractions[i]*speciesGasConst[i]; // J/m^3-K
          }
          
          double dPdRhoI[eos::MAXSPECIES];
          
          for(int i=0;i<numSpecies;i++){
            dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i];} // m^2/s^2
          
          dp[0] = baseTemp*sumRhoR/density*densityScale/pressureScale;
          dp[1] = density*sumRhoR/cv;
          //for(int i=0;i<numSpecies-1;i++){
            //dp[2+i] = dPdRhoI[i] - dPdRhoI[numSpecies-1];
            //dp[2+i] *= densityScale/pressureScale;
          //}
          for(int i=0;i<numSpecies-2;i++){
            int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+i]);
            dp[2+i] = dPdRhoI[chargeIndex] + 
              gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i]*dPdRhoI[eos::INFO::ELECTRONINDEX] - 
              (1.0 + gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i])*
              dPdRhoI[eos::INFO::ABUNDANTINDEX];
            dp[2+i] *= pressureScaleInv*densityScale;
          }
        }

//         FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
//           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp,c);
        {
          double c2 = dp[0] + dp[1]/rho * (dv[0]/rho);
          
          if(c2 < 0){
            #ifdef USE_HYDRA
            #else
            printf("CP EOS::GASEOSROE error! = \n");
            std::cout << "PROMETHEUS EOS::GASEOSROE error!" << std::endl
                      << "P       = "   << dv[0]                << std::endl
                      << "rho     = "   << rho                  << std::endl
                      << "dP/drho = "   << dp[0]                << std::endl
                      << "dP/de   = "   << dp[1]                << std::endl
                      << "c2      = "   << c2                   << std::endl;
            exit(1);
            #endif
          }
          
          *c = std::sqrt(c2);
        }
      } else if(gasInfo[eos::INFO::HANDLER] == eos::HANDLERTYPE::CANTERA){
#ifdef HAVE_CANTERA
#ifdef USE_CANTERA
        int threadId = 0;
#ifdef USE_OMP
        threadId = omp_get_thread_num();
#endif

        Cantera::IdealGasMix *locGasMixturePtr = eos::globalCantera[threadId];
    

        double energyScale      = nonDimen[eos::DIMEN::ENERGYSCALE];
        double densityScale     = nonDimen[eos::DIMEN::DENSITYSCALE];
        double pressureScale    = nonDimen[eos::DIMEN::PRESSURESCALE];
        double pressureScaleInv = 1./pressureScale;
        double temperatureScale = nonDimen[eos::DIMEN::TEMPERATURESCALE];
        double univGasConst     = nonDimen[eos::DIMEN::UNIVGASCONSTANT];
        size_t numSpecies       = gasInfo[eos::INFO::NUMSPECIES];

        const double *speciesMolecularWeights = &gasParameters[eos::GASPARAM::SPECIESMW*numSpecies];
        const double *rhoY = &q[numDim+2];

        double massFractions[eos::MAXSPECIES];
        double totalMass=1.0;
        double YE=0.0;
        for(int iSpec=0;iSpec<numSpecies-2;iSpec++){
          int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+iSpec]);
          double mass = std::max(0.,rhoY[iSpec]*rhoM1);
          massFractions[chargeIndex] = mass;
          YE += (mass*gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+iSpec]);
          totalMass -= mass;
        }
        massFractions[gasInfo[eos::INFO::ABUNDANTINDEX]] = std::max(0.0,totalMass-YE);
        massFractions[gasInfo[eos::INFO::ELECTRONINDEX]] = YE;

        double ke = 0.0;
        for(int iDim = 0;iDim < numDim;iDim++){
          double rhoV = q[1+iDim];
          double v = rhoV/rho;
          dv[iDim+2] = v;
          ke += v*v;
        }
        ke *= .5*rho;
        double enthalpy = q[numDim+1]-ke;
        enthalpy*=energyScale;

        locGasMixturePtr->setMassFractions(&massFractions[0]);
    
        double specificGasConstant=univGasConst/speciesMolecularWeights[numSpecies-1];
        for(size_t i=0;i<numSpecies-1;i++){
          // we can remove a subtraction if we do it apriori when the eos is setup
          specificGasConstant+=rhoY[i]*rhoM1*
            (univGasConst/speciesMolecularWeights[i]-univGasConst/speciesMolecularWeights[numSpecies-1]);
        }
        
        specificGasConstant *= 1000.0;

        // bracket the problem with the minimum and maximum valid temperatures
        double minTemp = locGasMixturePtr->minTemp();
        double maxTemp = locGasMixturePtr->maxTemp();
        //std::cout << "minTemp " << minTemp << " maxTemp " << maxTemp << std::endl;
        
        double tempLo = minTemp;
        double pressureLo = rho*densityScale*specificGasConstant*tempLo;
        locGasMixturePtr->setState_TPY(tempLo,pressureLo,&massFractions[0]);
        double enthalpyLo = locGasMixturePtr->enthalpy_mass();
        
        double tempHi = maxTemp;
        double pressureHi = rho*densityScale*specificGasConstant*tempHi;
        locGasMixturePtr->setState_TPY(tempHi,pressureHi,&massFractions[0]);
        double enthalpyHi = locGasMixturePtr->enthalpy_mass();
        
        //std::cout << "EnthalpyLo " << enthalpyLo << " EnthalpyHi " << enthalpyHi << std::endl;
        //std::cout << "PressureLo " << pressureLo << " PressureHi " << pressureHi << std::endl;
        //std::cout << "TemperatureLo " << tempLo << " temperatureHi " << tempHi << std::endl;

        double toler = 1.e-9;
        int iter = 0;
        double error = 1.0;
        double tempGuess = tempLo + (enthalpy - enthalpyLo)*(tempHi-tempLo)/(enthalpyHi-enthalpyLo);
        //std::cout << "Goal enthalpy " << enthalpy << std::endl;
        //std::cout << "tempGuess " << tempGuess << std::endl;
        //std::cout << "toler " << toler << std::endl;
        while((std::abs(error/enthalpy) > toler) && (iter < 50)) {

          double pressureGuess = rho*densityScale*specificGasConstant*tempGuess;
          locGasMixturePtr->setState_TPY(tempGuess,pressureGuess,&massFractions[0]);
          double enthalpyGuess = locGasMixturePtr->enthalpy_mass();

          if(enthalpyGuess > enthalpy){
            enthalpyHi = enthalpyGuess;
            tempHi = tempGuess;
          } else {
            enthalpyLo = enthalpyGuess;
            tempLo = tempGuess;
          }
          // could we get cP here from Cantera for a better estimate?
          double dTdH = (tempHi-tempLo)/(enthalpyHi-enthalpyLo);
          error = (enthalpy-enthalpyGuess);
          double dT = dTdH*error;
          //tempGuess = tempLo + (enthalpy - enthalpyLo)*(tempHi-tempLo)/(enthalpyHi-enthalpyLo);
          tempGuess = tempGuess + dT;
          
          iter++; 
          //std::cout << iter << " error " << error/enthalpy
          //<< " enthalpyGuess " << enthalpyGuess 
          //<< " temperatureGuess " << tempGuess 
          //<< std::endl;
        }
        
        if(iter > 50) {
          std::cout << "Failed to converge in EOSRoe" << std::endl;
          std::cout << iter << " error " << error/enthalpy
                    << " enthalpyGoal " << enthalpy
                    << " temperatureGuess " << tempGuess << std::endl
                    << "minTemp " << minTemp << " maxTemp " << maxTemp
                    << std::endl;
        }
        
        double baseTemp = locGasMixturePtr->temperature();
        double basePress = locGasMixturePtr->pressure();
        dv[0] = basePress/pressureScale;
        dv[1] = baseTemp/temperatureScale;

        //std::cout << locGasMixturePtr->report() << std::endl;
        // H = (rho*E+P)/rho -> rho*E = H*rho-P
        q[numDim+1] = q[numDim+1]*q[0]-dv[0];

        //         FC_GLOBAL(gasdp,GASDP)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp);
        {
          
          double internalEnergy = q[numDim+1]-ke;
          
          locGasMixturePtr->setState_TPY(baseTemp,basePress,&massFractions[0]);
          
          double baseRho = locGasMixturePtr->density();
          double baseEnergy = locGasMixturePtr->intEnergy_mass();
          double cv = locGasMixturePtr->cv_mole() / locGasMixturePtr->meanMolecularWeight(); // J/kg-K
    
          cv *= baseRho; // J/m^3-K

          
          double speciesInternalEnergy[eos::MAXSPECIES]; 
          locGasMixturePtr->getIntEnergy_RT(speciesInternalEnergy); // unitless

          for(int i=0;i<numSpecies;i++){  
            speciesInternalEnergy[i] *= baseTemp*univGasConst*1000.; // J/kMol
            speciesInternalEnergy[i] = speciesInternalEnergy[i]/speciesMolecularWeights[i]; // J/kg
          }
          
          double dTdRhoI[eos::MAXSPECIES]; // K-m^3/kg
          
          for(int i=0;i<numSpecies;i++){
            dTdRhoI[i] = -speciesInternalEnergy[i]/cv;
          }
          
          double dTdRhoE = 1.0/cv; // K-m^3/J
          
          
          // Really need to do this?
          double speciesGasConst[eos::MAXSPECIES]; // J/kg-K
          for(int i=0;i<numSpecies;i++){
            speciesGasConst[i] = 1000.*univGasConst/speciesMolecularWeights[i]; // J/kg-K
          }

          double sumRhoR=0;
        
          for(int i=0;i<numSpecies;i++){
            sumRhoR += baseRho*massFractions[i]*speciesGasConst[i]; // J/m^3-K
          }
          
          double dPdRhoI[eos::MAXSPECIES];
          
          for(int i=0;i<numSpecies;i++){
            dPdRhoI[i] = speciesGasConst[i]*baseTemp + sumRhoR*dTdRhoI[i]; // m^2/s^2
          }
          
          dp[0] = baseTemp*sumRhoR/baseRho*densityScale/pressureScale;
          dp[1] = baseRho*sumRhoR/cv;
          for(int i=0;i<numSpecies-2;i++){
            int chargeIndex = static_cast<int>(gasParameters[eos::GASPARAM::SPECIESMAP*numSpecies+i]);
            dp[2+i] = dPdRhoI[chargeIndex] + 
              gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i]*dPdRhoI[eos::INFO::ELECTRONINDEX] - 
              (1.0 + gasParameters[eos::GASPARAM::SPECIESCHARGEMW*numSpecies+i])*
              dPdRhoI[eos::INFO::ABUNDANTINDEX];
            dp[2+i] *= pressureScaleInv*densityScale;
          }
        }
        //         FC_GLOBAL(gassoundspeed,GASSOUNDSPEED)
        //           (inDimension,gasInfo,gasParameters,nonDimen,q,dv,dp,c);
        {
          double c2 = dp[0] + dp[1]*rhoM1*(dv[0]*rhoM1);
          if(c2 < 0){
            std::cerr << "GASSOUNDSPEED:Cantera:Fatal error c2 = " << c2 << std::endl;
            exit(1);
          }
          *c = std::sqrt(c2);
        }
#endif
#endif
      } else {
        // ERROR - Unknown handler for this gas type
      }
    }
#ifdef EOS_TIMERS
    double routineTime = MPI_Wtime() - entryTime;
    if(!eos::eosTimers.empty()){
      eos::eosTimers[eos::TIMERS::ROETIMER] += routineTime;
      eos::eosCallCounts[eos::TIMERS::ROETIMER]++;
    }
#endif
  }
#ifdef USE_HYDRA
#pragma omp end declare target
#endif
} // Fortran-compatible API

  //  --------- end of single point interfaces ---------------

namespace eos {


  std::string GasModelName(int gasModel)
  {
    if(gasModel == IDEAL)
      return(std::string("Ideal"));
    if(gasModel == CANTERA_PG_MULTI)
      return(std::string("CanteraIdealMixture"));
    if(gasModel == PG_CP_MULTI)
      return(std::string("CaloricallyPerfectMixture"));
    if(gasModel == PG_TP_MULTI)
      return(std::string("ThermallyPerfectMixture"));
    if(gasModel == PLASMA_CANTERA)
      return(std::string("CanteraPlasma"));
    return(std::string("Unknown"));
  }

  int GasModelId(const std::string &modelName)
  {
    if(modelName == "Ideal")
      return(IDEAL);
    if(modelName == "CanteraIdealMixture")
      return(CANTERA_PG_MULTI);
    if(modelName == "CaloricallyPerfectMixture")
      return(PG_CP_MULTI);
    if(modelName == "ThermallyPerfectMixture")
      return(PG_TP_MULTI);
    if(modelName == "CanteraPlasma")
      return(PLASMA_CANTERA);
    return(-1);
  }


} // namespace eos

