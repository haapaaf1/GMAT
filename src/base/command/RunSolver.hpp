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


#ifndef RunSolver_hpp
#define RunSolver_hpp

#include "PropagationEnabledCommand.hpp"

/**
 * Base class for the commands that drive the solvers.
 *
 * This base class manages the single-command versions of the solver state
 * machine commands.  The SolverBranchCommand class handles the solver commands
 * that use a solver control sequence.
 */
class RunSolver : public PropagationEnabledCommand
{
public:
   RunSolver(const std::string &typeStr);
   virtual ~RunSolver();
   RunSolver(const RunSolver& rs);
   RunSolver& operator=(const RunSolver& rs);

   virtual bool         InterpretAction();

   virtual bool         Initialize();
//   virtual bool Execute();

protected:
   /// Name of a configured Solver to run.  Pointer is set in derived classes.
   std::string    solverName;
};

#endif /* RunSolver_hpp */
