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


#include "RunSolver.hpp"

#include <sstream>
#include "MessageInterface.hpp"


//#define DEBUG_RUNSOLVER_PARSING


RunSolver::RunSolver(const std::string &typeStr) :
   PropagationEnabledCommand  (typeStr)
{
}

RunSolver::~RunSolver()
{
}

RunSolver::RunSolver(const RunSolver & rs) :
   PropagationEnabledCommand  (rs)
{
}

RunSolver & RunSolver::operator =(const RunSolver& rs)
{
   if (&rs != this)
   {
      PropagationEnabledCommand::operator=(rs);
   }

   return *this;
}

//------------------------------------------------------------------------------
// bool RunSolver::InterpretAction()
//------------------------------------------------------------------------------
/**
 * Parser for the RunSolver commands
 *
 * The RunSolver commands all have a simple enough structure that the generic
 * command parsers should be able to handle them.  However, that functionality
 * no longer works as designed, so an implementation is provided here.  This
 * implementation assumes that the command syntax is
 *
 *    CommandKeyword SolverName
 *
 * If you need more specific text in the command scripting, override this method
 * and implement your command specific details.
 *
 * @return true if the command parsed the generating string successfully; false
 *         otherwise.
 */
//------------------------------------------------------------------------------
bool RunSolver::InterpretAction()
{
   bool retval = false;

   #ifdef DEBUG_RUNSOLVER_PARSING
      MessageInterface::ShowMessage(
            "RunSolver::InterpretAction() called for string \"%s\"\n",
            generatingString.c_str());
   #endif

   std::stringstream stringToParse;

   stringToParse << generatingString;

   std::string temp;

   stringToParse >> temp;

   #ifdef DEBUG_RUNSOLVER_PARSING
      MessageInterface::ShowMessage("   Cmd keyword: \"%s\"\n", temp.c_str());
   #endif
   stringToParse >> temp;

   #ifdef DEBUG_RUNSOLVER_PARSING
      MessageInterface::ShowMessage("   Solver Name: \"%s\"\n", temp.c_str());
   #endif

   if (temp != "")
   {
      solverName = temp;
      retval = true;
   }

   return retval;
}


bool RunSolver::Initialize()
{
   bool retval = PropagationEnabledCommand::Initialize();

   #ifdef DEBUG_RUNSOLVER_INITIALIZATION
      MessageInterface::ShowMessage("   PEC returned \"%s\"\n",
            (retval ? "true" : "false"));
   #endif

   return retval;
}
