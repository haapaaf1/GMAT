//$Id$
//------------------------------------------------------------------------------
//                                 CallFunction
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
 * Definition for the CallFunction command class
 */
//------------------------------------------------------------------------------



#ifndef CallFunction_hpp
#define CallFunction_hpp


#include "GmatCommand.hpp"
#include "Function.hpp"
#include "FunctionManager.hpp"
#include "Parameter.hpp"
#include "Array.hpp"
#include "StringVar.hpp"

//#include <map>
#include <iostream>
#include <iomanip>

#ifdef __USE_MATLAB__
#include "MatlabInterface.hpp"
#endif

// Forward references for GMAT core objects
class Publisher;


class CallFunction : public GmatCommand
{
public:
   CallFunction();
   virtual ~CallFunction();

   CallFunction(const CallFunction& cf);
   CallFunction&        operator=(const CallFunction& cf);
   
   std::string          FormEvalString();
   bool                 AddInputParameter(const std::string &paramName, Integer index);
   bool                 AddOutputParameter(const std::string &paramName, Integer index);
   
   // override GmatCommand methods
   virtual bool         Initialize();
   virtual bool         Execute();
   virtual void         RunComplete();
   virtual void         SetInternalCoordSystem(CoordinateSystem *cs);
   
   // override these to set on FunctionManager (and find function object in GOS)
   virtual void         SetPublisher(Publisher *pub);
   virtual void         SetObjectMap(std::map<std::string, GmatBase *> *map);
   virtual void         SetGlobalObjectMap(std::map<std::string, GmatBase *> *map);
   virtual bool         HasAFunction();
   
   // override GmatBase methods
   virtual GmatBase*    Clone() const;
   virtual const std::string&
                        GetGeneratingString(Gmat::WriteMode mode,
                                            const std::string &prefix = "",
                                            const std::string &useName = "");
   
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");
   
   //SringArray          GetRefObjectNameArray(const Gmat::ObjectType type) const;

   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id, const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   virtual bool         SetStringParameter(const Integer id, const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;

protected:

private:
   ObjectArray objectArray;
   std::vector<Parameter*> mInputList;
   std::vector<Parameter*> mOutputList;
   // Changed 8/31/05, DJC
   //   ObjectArray callcmds;
   GmatCommand *callcmds;
   
   StringArray mInputNames;
   StringArray mOutputNames;
   
   Integer mNumInputParams;
   Integer mNumOutputParams;
   
   Function *mFunction;
   std::string mFunctionName;
   
   /// CoordinateSystem used internally
   // Added this to GmatCommand (loj: 2008.06.18)
   ////CoordinateSystem *internalCoordSys;
   
   /// the manager for the Function
   FunctionManager fm;
   
   bool isGmatFunction;
   bool isMatlabFunction;
   
   #if defined __USE_MATLAB__
   MatlabInterface *matlabIf;
   #endif
   
   bool ExecuteMatlabFunction();
   void SendInParam(Parameter *param);
   void GetOutParams();
   void EvalMatlabString(std::string evalString);
   void ClearInputParameters();
   void ClearOutputParameters();
   void UpdateObject(GmatBase *obj, char *buffer);
   
   enum
   {
      FUNCTION_NAME = GmatCommandParamCount,
      ADD_INPUT,
      ADD_OUTPUT,
      COMMAND_STREAM,
      CallFunctionParamCount  /// Count of the parameters for this class
   };
   
   
   static const std::string
      PARAMETER_TEXT[CallFunctionParamCount - GmatCommandParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[CallFunctionParamCount - GmatCommandParamCount];
   
   
};


#endif // CallFunction_hpp
