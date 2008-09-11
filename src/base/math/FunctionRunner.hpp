//$Id$
//------------------------------------------------------------------------------
//                              FunctionRunner
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CCA54C
//
// Author: Wendy Shoan
// Created: 2008.04.21
//
/**
 * Declares FunctionRunner class.
 */
//------------------------------------------------------------------------------
#ifndef FunctionRunner_hpp
#define FunctionRunner_hpp

#include "MathFunction.hpp"
#include "FunctionManager.hpp"
#include "SolarSystem.hpp"
#include "PhysicalModel.hpp"
#include "RealUtilities.hpp"


class GMAT_API FunctionRunner : public MathFunction
{
public:
   FunctionRunner(const std::string &nomme);
   virtual ~FunctionRunner();
   FunctionRunner(const FunctionRunner &copy);
   
   void                 SetFunctionName(const std::string &fname);
   void                 SetFunction(Function *function);
   void                 AddFunctionInput(const std::string &name);
   void                 AddFunctionOutput(const std::string &name);
   void                 SetFunctionInputs();
   void                 SetFunctionOutputs();
   const StringArray&   GetInputs();
   virtual void         SetCallingFunction(FunctionManager *fm);
   
   // for setting objects to FunctionManager
   virtual void         SetObjectMap(ObjectMap *map);
   virtual void         SetGlobalObjectMap(ObjectMap *map);
   void                 SetSolarSystem(SolarSystem *ss);
   void                 SetInternalCoordSystem(CoordinateSystem *cs);
   void                 SetTransientForces(std::vector<PhysicalModel*> *tf);
   
   // inherited from MathFunction
   virtual void         GetOutputInfo(Integer &type, Integer &rowCount, 
                                      Integer &colCount);
   virtual bool         ValidateInputs(); 
   virtual Real         Evaluate();
   virtual Rmatrix      MatrixEvaluate();
   virtual void         Finalize();
   
   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   
protected:
   
   FunctionManager theFunctionManager;
   ObjectMap       *theObjectMap;
   ObjectMap       *theGlobalObjectMap;
   std::string     theFunctionName;
   Function        *theFunction;
   StringArray     theInputNames;
   StringArray     theOutputNames;
   
   FunctionManager *callingFunction;
   
   GmatBase* FindObject(const std::string &name);
};

#endif // FunctionRunner_hpp
