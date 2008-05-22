//$Header$
//------------------------------------------------------------------------------
//                            SolverBranchCommand
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway
// Created: 2006/10/20
//
/**
 * Definition for the Solver loop command base class (Target, Optimize and 
 * Iterate).
 */
//------------------------------------------------------------------------------


#ifndef SolverBranchCommand_hpp
#define SolverBranchCommand_hpp

#include "BranchCommand.hpp"
#include "Solver.hpp"


class SolverBranchCommand : public BranchCommand
{
public:
        SolverBranchCommand(const std::string &typeStr);
        virtual ~SolverBranchCommand();
   SolverBranchCommand(const SolverBranchCommand& sbc);

   SolverBranchCommand&    operator=(const SolverBranchCommand& sbc);
   
   virtual GmatCommand*    GetNext();
   virtual bool            TakeAction(const std::string &action, 
                                      const std::string &actionData = "");
   
   // Parameter access methods
   virtual std::string GetParameterText(const Integer id) const;
   virtual Integer     GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                       GetParameterType(const Integer id) const;
   virtual std::string GetParameterTypeString(const Integer id) const;
   
   virtual bool        SetStringParameter(const Integer id, 
                                          const std::string &value);
   virtual std::string GetStringParameter(const Integer id) const; 
   virtual std::string GetStringParameter(const std::string &label) const;
   virtual const StringArray& 
                       GetStringArrayParameter(const Integer id) const; 
   virtual const StringArray& 
                       GetStringArrayParameter(const std::string &label) const;

   
protected:
   // Mode definitions for the state machine overrrides
   enum solverStartMode
   {
      RUN_INITIAL_GUESS,
      RUN_AND_SOLVE,
      RUN_SOLUTION
   };
   
   // Mode definitions used on convergence
   enum solverExitMode
   {
      DISCARD_AND_CONTINUE,
      SAVE_AND_CONTINUE
   };
   
   solverStartMode     startMode;
   solverExitMode      exitMode;
   Solver::SolverState specialState;
   
   /// Modes used in the targeter, filled in the derived classes
   StringArray         solverModes;    

// THESE GO IN THE SOLVER:
//   // States available during a mission run, for reporting purposes
//   enum
//   {
//      READY = 0,
//      CONVERGED,
//      EXCEEDED_ITERATIONS,
//      RAN_INITIAL_VALUES,
//      FAILED
//   };
   
   /// Local store of the objects that we'll need to reset
   ObjectArray         localStore;

   // Methods used to save the starting point for the loops
   virtual void        StoreLoopData();
   virtual void        ResetLoopData();
   virtual void        FreeLoopData();
   
   virtual void        ApplySolution();
   
   enum
   {
      SOLVER_SOLVE_MODE  = BranchCommandParamCount,
      SOLVER_EXIT_MODE,
      SOLVER_SOLVE_MODE_OPTIONS,
      SOLVER_EXIT_MODE_OPTIONS,
      SolverBranchCommandParamCount
   };
   
   
};

#endif /*SOLVERBRANCHCOMMAND_HPP_*/
