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
// Author: Darrel J. Conway, Thinking Systems, Inc. & Wendy Shoan/GSFC/GSSB
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
   Simulator(const Simulator& sim);
   Simulator& operator=(const Simulator& sim);
   virtual ~Simulator();

   virtual GmatBase* Clone() const;
   virtual Integer SetSolverResults(Real*, const std::string&, const std::string&); // noop
   virtual void SetResultValue(Integer, Real, const std::string&);                  // noop
   virtual void WriteToTextFile(SolverState stateToUse = UNDEFINED_STATE);
   
   virtual bool        Initialize();
   virtual SolverState AdvanceState();
   virtual bool        Finalize();

   Real                GetTimeStep();
   
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

   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");

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
   /// The propagator configured for simulation
   PropSetup           *propagator;
   /// Name of the propagator
   std::string         propagatorName;

   /// The state vector, used to buffer state information during event location
   GmatState           *simState;   // (This piece is stuill in flux -- do we do
                                    // it like this, or differently?)
   /// The initial epoch for the simulation
   GmatEpoch           simulationStart;
   /// The target epoch for the end of the simulation
   GmatEpoch           simulationEnd;
   /// The epoch of the next measurement calculation
   GmatEpoch           nextSimulationEpoch;
   /// The current epoch (typically as determined via propagation)
   GmatEpoch           currentEpoch;
   
   /// Format of the scripted simulation start time
   std::string         initialEpochFormat;
   /// The string specifying the simulation start time
   std::string         initialEpoch;      // ??? is this simulationStart
   /// Format of the scripted simulation end time
   std::string         finalEpochFormat;
   /// The string specifying the simulation end time
   std::string         finalEpoch;      // ??? is this simulationEnd

   /// Timestep from current measurement epoch to the epoch of the next measurement
   Real                simulationStep;
   /**
    *  The timestep that gets returned for the next propagation
    *
    *  timeStep will be the same as simulationStep when the state machine is in
    *  the PROPAGATING state.  When in the LOCATING state, timeStep is the time
    *  from the base epoch to the next attempt at finding the event that is
    *  being located.
    */
   Real                timeStep;

   /// The simulator's measurement manager
   MeasurementManager  measManager;
   /// The list of measurement models that the measurement manager is managing
   StringArray         measList;   // temporary - may get list from MeasManager;

   // State machine methods
   void                   CompleteInitialization();
   void                   FindTimeStep();
   void                   CalculateData();
   // void ProcessEvent();
   void                   SimulateData();
   void                   RunComplete();

   // Helper methods
   void                   FindNextSimulationEpoch();
   // progress string for reporting
   virtual std::string    GetProgressString();
};

#endif /* Simulator_hpp */
