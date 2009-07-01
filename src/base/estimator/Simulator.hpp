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

/// The Measurement simulator
class Simulator : public Solver
{
public:
   Simulator(const std::string &name);
   virtual ~Simulator();
   Simulator(const Simulator& sim);
   Simulator* operator=(const Simulator& sim);

   virtual GmatBase* Clone() const;
   virtual Integer SetSolverResults(Real*, const std::string&, const std::string&);
   virtual void SetResultValue(Integer, Real, const std::string&);
   virtual void WriteToTextFile(Solver::SolverState);

   virtual bool        Initialize();
   virtual SolverState AdvanceState();
   virtual bool        Finalize();

   Real GetTimeStep();

protected:
   Solver::SolverState currentState;

   GmatState *simState;
   GmatEpoch simulationStart;
   GmatEpoch simulationEnd;
   GmatEpoch nextSimulationEpoch;
   GmatEpoch currentEpoch;

   Real simulationStep;
   Real timestep;

   MeasurementManager measManager;

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
