//$Id$
//------------------------------------------------------------------------------
//                                 CallMatlabFunction
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Allison Greene
// Created: 2004/09/22
//
/**
 * Definition for the CallMatlabFunction command class
 */
//------------------------------------------------------------------------------
#ifndef CallMatlabFunction_hpp
#define CallMatlabFunction_hpp

#include "CallFunction.hpp"
#include "MatlabInterface.hpp"

class GMAT_API CallMatlabFunction : public CallFunction
{
public:
   CallMatlabFunction();
   virtual ~CallMatlabFunction();
   
   CallMatlabFunction(const CallMatlabFunction& cf);
   CallMatlabFunction&  operator=(const CallMatlabFunction& cf);
   
   std::string          FormEvalString();
   
   // override GmatCommand methods
   virtual bool         Initialize();
   virtual bool         Execute();
   virtual void         RunComplete();
   
   // override GmatBase methods
   virtual GmatBase*    Clone() const;

protected:
   
   MatlabInterface *matlabIf;
   
   bool ExecuteMatlabFunction();
   void SendInParam(Parameter *param);
   void GetOutParams();
   void EvalMatlabString(std::string evalString);
   void ClearInputParameters();
   void ClearOutputParameters();
   void UpdateObject(GmatBase *obj, char *buffer);
   
};

#endif // CallMatlabFunction_hpp
