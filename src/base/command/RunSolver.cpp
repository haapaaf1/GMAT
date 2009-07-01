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

RunSolver::RunSolver(const std::string &typeStr) :
   GmatCommand          (typeStr)
{
}

RunSolver::~RunSolver()
{
}

RunSolver::RunSolver(const RunSolver & rs) :
   GmatCommand          (rs)
{
}

RunSolver & RunSolver::operator =(const RunSolver& rs)
{
   if (&rs != this)
   {

   }

   return *this;
}

bool RunSolver::Initialize()
{
   return false;
}

//bool RunSolver::Execute()
//{
//   return false;
//}
