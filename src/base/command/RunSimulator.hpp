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

   virtual std::string GetRefObjectName(const Gmat::ObjectType type) const;
   virtual bool SetRefObjectName(const Gmat::ObjectType type,
         const std::string &name);
   virtual bool RenameRefObject(const Gmat::ObjectType type,
         const std::string &oldName, const std::string &newName);
   virtual const std::string& GetGeneratingString(Gmat::WriteMode mode,
         const std::string &prefix, const std::string &useName);

   virtual bool Initialize();
   virtual bool Execute();
   virtual void RunComplete();
   virtual GmatCommand* GetNext();

protected:
   /// The simulator that drives this process
   Simulator      *theSimulator;
   /// Flag indicating if command execution is started
   bool commandRunning;
//   /// Flag indicating if propagation is running an needs reentrance
//   bool commandPropagating;
   /// Flag indicating if command execution is done
   bool commandComplete;

   void PrepareToSimulate();
   void Propagate();
   void Calculate();
   void LocateEvent();
   void Simulate();
   void Finalize();
};

#endif /* RunSimulator_hpp */
