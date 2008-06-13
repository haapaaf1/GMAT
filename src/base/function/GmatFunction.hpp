//$Id$
//------------------------------------------------------------------------------
//                                  GmatFunction
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Allison Greene
// Created: 2004/12/16
//
/**
 * Definition for the GmatFunction class.
 */
//------------------------------------------------------------------------------


#ifndef GmatFunction_hpp
#define GmatFunction_hpp

#include "Function.hpp"

class GMAT_API GmatFunction : public Function
{
public:
   GmatFunction(const std::string &nomme);
   
   virtual ~GmatFunction(void);
   
   GmatFunction(const GmatFunction &copy);
   GmatFunction& operator=(const GmatFunction &right);

   // inherited from Function
   virtual bool         Initialize();
   virtual bool         Execute();
   virtual void         Finalize();

   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* orig);
   
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
protected:
   enum
   {
      GmatFunctionParamCount = FunctionParamCount  /// Count of the parameters for this class
   };
   
//   static const std::string
//      PARAMETER_TEXT[GmatFunctionParamCount - FunctionParamCount];
//   static const Gmat::ParameterType
//      PARAMETER_TYPE[GmatFunctionParamCount - FunctionParamCount];

};

#endif
