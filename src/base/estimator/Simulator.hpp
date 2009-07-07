//$Id$
//------------------------------------------------------------------------------
//                         Simulator
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/30
//
/**
 * Definition for the class used to generate simulated measurement data
 */
//------------------------------------------------------------------------------


#ifndef Simulator_hpp
#define Simulator_hpp

#include "Solver.hpp"
#include "GmatState.hpp"
#include "MeasurementManager.hpp"
#include "PropSetup.hpp"

/// The Measurement simulator
class Simulator : public Solver
{
public:
   Simulator(const std::string &name);
   virtual ~Simulator();
   Simulator(const Simulator& sim);
   Simulator* operator=(const Simulator& sim);

   virtual GmatBase* Clone() const;
   virtual Integer SetSolverResults(Real*, const std::string&, const std::string&); // noop
   virtual void SetResultValue(Integer, Real, const std::string&);                  // noop
   virtual void WriteToTextFile(Solver::SolverState);

   virtual bool        Initialize();
   virtual SolverState AdvanceState();
   virtual bool        Finalize();

   Real GetTimeStep();
   
   // methods overridden from GmatBase
   virtual std::string  GetParameterText(const Integer id) const;
   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value);

   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const Integer id,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;

protected:
   
   enum
   {
      MEASUREMENTS = SolverParamCount,
      PROPAGATOR,
      INITIAL_EPOCH_FORMAT,
      INITIAL_EPOCH,
      FINAL_EPOCH_FORMAT,
      FINAL_EPOCH,
      MEASUREMENT_TIME_STEP,
      SimulatorParamCount
   };
   
   static const std::string    PARAMETER_TEXT[SimulatorParamCount -
                                              SolverParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[SimulatorParamCount -
                                              SolverParamCount];
   
   Solver::SolverState currentState;
   PropSetup           *propagator;
   std::string         propagatorName;

   GmatState           *simState;
   GmatEpoch           simulationStart;
   GmatEpoch           simulationEnd;
   GmatEpoch           nextSimulationEpoch;
   GmatEpoch           currentEpoch;
   
   std::string         initialEpochFormat;
   std::string         initialEpoch;      // ??? is this simulationStart
   std::string         finalEpochFormat;
   std::string         finalEpoch;      // ??? is this simulationEnd

   Real                simulationStep;
   Real                timestep;

   MeasurementManager  measManager;
   StringArray         measList;   // temporary - may get list from MeasManager;

   // State machine methods
   void CompleteInitialization();
   void FindTimeStep();
   void CalculateData();
   // void ProcessEvent();
   void SimulateData();
   void RunComplete();

   // Helper methods
   void FindNextSimulationEpoch();

   // Reporting method
   void ReportProgress();
};

#endif /* Simulator_hpp */
