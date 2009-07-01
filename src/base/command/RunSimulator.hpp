//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#ifndef RunSimulator_hpp
#define RunSimulator_hpp

#include "RunSolver.hpp"

#include "PropSetup.hpp"
#include "Simulator.hpp"

/// Descriptor here
class RunSimulator : public RunSolver
{
public:
   RunSimulator();
   virtual ~RunSimulator();
   RunSimulator(const RunSimulator& rs);
   RunSimulator& operator=(const RunSimulator& rs);

   virtual GmatBase* Clone() const;

   virtual bool Initialize();
   virtual bool Execute();

protected:
   PropSetup      *thePropagator;
   Simulator      *theSimulator;

   void PrepareToEstimate();
   void Propagate();
   void Calculate();
   void LocateEvent();
   void Simulate();
   void Finalize();
};

#endif /* RunSimulator_hpp */
