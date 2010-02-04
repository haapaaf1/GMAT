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
#include "CallFunction.hpp"
#include "BeginFunction.hpp"
#include "StringTokenizer.hpp"
#include "StringUtil.hpp"          // for Replace()
#include "FileManager.hpp"         // for GetAllMatlabFunctionPaths()
#include "MessageInterface.hpp"
#include <sstream>

#if defined __USE_MATLAB__
#include "MatlabInterface.hpp"     // for Matlab Engine functions
#endif

// if using MatlabInterface::EvalMatlabString() to send array to Matlab workspace
// if using MatlabInterface::EvalMatlabString() for getting string from Matlab workspace
//#define __USE_EVAL_STRING__

//#define DEBUG_CALL_FUNCTION_PARAM
//#define DEBUG_CALL_FUNCTION_INIT
//#define DEBUG_CALL_FUNCTION_EXEC
//#define DEBUG_SEND_PARAM
//#define DEBUG_UPDATE_VAR
//#define DEBUG_UPDATE_OBJECT
//#define DEBUG_SHOW_ARRAY
//#define DEBUG_GMAT_FUNCTION_INIT
//#define DEBUG_GET_OUTPUT
//#define DEBUG_OBJECT_MAP
//#define DEBUG_GLOBAL_OBJECT_MAP
//#define DEBUG_RUN_COMPLETE
//#define DEBUG_MATLAB_EXEC
//#define DEBUG_MATLAB_EVAL

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif
//#ifndef DEBUG_TRACE
//#define DEBUG_TRACE
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif
#ifdef DEBUG_TRACE
#include <ctime>                 // for clock()
#endif

//---------------------------------
// static data
//---------------------------------
const std::string
CallFunction::PARAMETER_TEXT[CallFunctionParamCount - GmatCommandParamCount] =
{
   "FunctionName",
   "AddInput",
   "AddOutput",
   "CommandStream",
};


const Gmat::ParameterType
CallFunction::PARAMETER_TYPE[CallFunctionParamCount - GmatCommandParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::OBJECT_TYPE,
};


//------------------------------------------------------------------------------
// CallFunction::CallFunction()
//------------------------------------------------------------------------------
CallFunction::CallFunction() :
   GmatCommand      ("CallFunction"),
   callcmds         (NULL),
   mFunction        (NULL),
   mFunctionName    (""),
   isGmatFunction   (false),
   isMatlabFunction (false)
{
   mNumInputParams = 0;
   mNumOutputParams = 0;
   
   parameterCount = CallFunctionParamCount;
   objectTypeNames.push_back("CallFunction");
}


//------------------------------------------------------------------------------
// ~CallFunction()
//------------------------------------------------------------------------------
CallFunction::~CallFunction()
{
   if (callcmds)
      delete callcmds;
}


//------------------------------------------------------------------------------
// CallFunction(const CallFunction& cf) :
//------------------------------------------------------------------------------
CallFunction::CallFunction(const CallFunction& cf) :
   GmatCommand     (cf),
   callcmds        (NULL),
   mFunction       (cf.mFunction),
   mFunctionName   (cf.mFunctionName),
   fm              (cf.fm)
{
   mNumInputParams = cf.mNumInputParams;
   mNumOutputParams = cf.mNumOutputParams;
   
   objectArray = cf.objectArray;
   mInputList = cf.mInputList;
   mOutputList = cf.mOutputList;
   callcmds = NULL;           // Commands must be reinitialized
   isGmatFunction = cf.isGmatFunction;
   isMatlabFunction = cf.isMatlabFunction;
   
   mInputNames  = cf.mInputNames;
   mOutputNames = cf.mOutputNames;
   
   parameterCount = CallFunctionParamCount;
}


//------------------------------------------------------------------------------
// CallFunction& operator=(const CallFunction& cf)
//------------------------------------------------------------------------------
CallFunction& CallFunction::operator=(const CallFunction& cf)
{
   if (this == &cf)
      return *this;
   
   GmatCommand::operator=(cf);
   
   mFunction = cf.mFunction;
   mFunctionName = cf.mFunctionName;
   mNumInputParams = cf.mNumInputParams;
   mNumOutputParams = cf.mNumOutputParams;
   
   objectArray = cf.objectArray;
   mInputList = cf.mInputList;
   mOutputList = cf.mOutputList;
   callcmds = NULL;           // Commands must be reinitialized
   isGmatFunction = cf.isGmatFunction;
   isMatlabFunction = cf.isMatlabFunction;
   
   mInputNames  = cf.mInputNames;
   mOutputNames = cf.mOutputNames;
   fm               = cf.fm;
   
   return *this;
}


//------------------------------------------------------------------------------
// std::string FormEvalString()
//  String format
//    [Out1, Out2] = FunctionName(In1, In2, In3);
//------------------------------------------------------------------------------
std::string CallFunction::FormEvalString()
{
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage
      ("CallFunction::FormEvalString() entered, mFunction=<%p>'%s'\n",
       mFunction, mFunction ? mFunction->GetName().c_str() : "NULL");
   #endif
   std::string evalString = "";
   
   // left hand side of evaluation string and equals (if necessary)
   if (mOutputList.size() > 1)
   {
      evalString = evalString + "[";
      Parameter *param = (Parameter *)mOutputList[0];
      evalString = evalString + param->GetName();
      
      for (unsigned int i=1; i<mOutputList.size(); i++)
      {
         param = (Parameter *)mOutputList[i];
         evalString = evalString +", " + param->GetName();
      }
      
      evalString = evalString + "] = ";
   }
   else if (mOutputList.size() == 1)
   {
      Parameter *param = (Parameter *)mOutputList[0];
      evalString = "[" + evalString + param->GetName() + "]";
      evalString = evalString +" = ";
   }
   else if (mOutputList.size() == 0)
   {
      // no left hand side
   }
   else
   {
      // need to throw an exception here
   }
   
   
   // right hand side of evaluation string
   // function name and left parenthesis
   evalString = evalString + mFunction->GetName().c_str() + "(";


   // input parameters
   if (mInputList.size() > 0)
   {
      Parameter *param = (Parameter *)mInputList[0];
      evalString = evalString + param->GetName();

      for (unsigned int i=1; i<mInputList.size(); i++)
      {
         param = (Parameter *)mInputList[i];
         evalString = evalString + ", " + param->GetName();
      }
   }
   
   // right parenthesis and semi-colon
   evalString = evalString + ");";
   
   return evalString;
}


//------------------------------------------------------------------------------
// bool AddInputParameter(const std::string &paramName, Integer index)
//------------------------------------------------------------------------------
bool CallFunction::AddInputParameter(const std::string &paramName, Integer index)
{
   if (paramName != "" && index == mNumInputParams)
   {
      mInputNames.push_back(paramName);
      mNumInputParams = mInputNames.size();
      mInputList.push_back(NULL);
      fm.AddInput(paramName);
      return true;
   }
   
   return false;
}


//------------------------------------------------------------------------------
// bool AddOutputParameter(const std::string &paramName, Integer index)
//------------------------------------------------------------------------------
bool CallFunction::AddOutputParameter(const std::string &paramName, Integer index)
{
   if (paramName != "" && index == mNumOutputParams)
   {
      mOutputNames.push_back(paramName);
      mNumOutputParams = mOutputNames.size();
      mOutputList.push_back(NULL);
      fm.AddOutput(paramName);      
      return true;
   }

   return false;
}


//------------------------------------------------------------------------------
//  void SetObjectMap(std::map<std::string, GmatBase *> *map)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the local asset store used by the GmatCommand
 * 
 * @param <map> Pointer to the local object map
 */
//------------------------------------------------------------------------------
void CallFunction::SetObjectMap(std::map<std::string, GmatBase *> *map)
{
   GmatCommand::SetObjectMap(map);
   fm.SetObjectMap(map);
}


//------------------------------------------------------------------------------
//  void SetGlobalObjectMap(std::map<std::string, GmatBase *> *map)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the global asset store used by the GmatCommand
 * 
 * @param <map> Pointer to the local object map
 */
//------------------------------------------------------------------------------
void CallFunction::SetGlobalObjectMap(std::map<std::string, GmatBase *> *map)
{
   #ifdef DEBUG_GLOBAL_OBJECT_MAP
   MessageInterface::ShowMessage
      ("CallFunction::SetGlobalObjectMap() entered, mFunctionName='%s', "
       "map=<%p>\n", mFunctionName.c_str(), map);
   #endif
   
   GmatCommand::SetGlobalObjectMap(map);
   
   // Now, find the function object
   GmatBase *mapObj = FindObject(mFunctionName);
   
   #ifdef DEBUG_GLOBAL_OBJECT_MAP
   MessageInterface::ShowMessage
      ("   mapObj=<%p><%s>'%s'\n", mapObj,
       mapObj ? mapObj->GetTypeName().c_str() : "NULL",
       mapObj ? mapObj->GetName().c_str() : "NULL");
   #endif
   
   if (mapObj == NULL)
   {
      //throw CommandException("CallFunction command cannot find Function " +
      //         mFunctionName + "\n");
      ; // leave NULL for now
   }
   else
   {
      mFunction = (Function *)mapObj;
      
      #ifdef DEBUG_GLOBAL_OBJECT_MAP
      MessageInterface::ShowMessage
         ("   mFunction=<%p><%s>\n", mFunction, mFunction->GetName().c_str());
      #endif
      
      // Set only GmatFunction to FunctionManager (loj: 2008.09.03)
      if (mapObj->GetTypeName() == "GmatFunction")
         fm.SetFunction(mFunction);
   }
   fm.SetGlobalObjectMap(map);
   
   #ifdef DEBUG_GLOBAL_OBJECT_MAP
   MessageInterface::ShowMessage("CallFunction::SetGlobalObjectMap() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// bool HasAFunction()
//------------------------------------------------------------------------------
bool CallFunction::HasAFunction()
{
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CallFunction.
 *
 * @return clone of the CallFunction.
 *
 */
//------------------------------------------------------------------------------
GmatBase* CallFunction::Clone() const
{
   return (new CallFunction(*this));
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string CallFunction::GetParameterText(const Integer id) const
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage("CallFunction::GetParameterText\n");
   #endif

   if (id >= GmatCommandParamCount && id < CallFunctionParamCount)
      return PARAMETER_TEXT[id - GmatCommandParamCount];
   else
      return GmatCommand::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  const std::string& GetGeneratingString()
//------------------------------------------------------------------------------
/**
 * Method used to retrieve the string that was parsed to build this GmatCommand.
 *
 * This method is used to retrieve the GmatCommand string from the script that
 * was parsed to build the GmatCommand.  It is used to save the script line, so
 * that the script can be written to a file without inverting the steps taken to
 * set up the internal object data.  As a side benefit, the script line is
 * available in the GmatCommand structure for debugging purposes.
 *
 * @param mode    Specifies the type of serialization requested.
 * @param prefix  Optional prefix appended to the object's name.  (Used to
 *                indent commands)
 * @param useName Name that replaces the object's name.  (Not used in
 *                commands)
 *
 * @return The script line that, when interpreted, defines this CallFunction.
 */
//------------------------------------------------------------------------------
const std::string& CallFunction::GetGeneratingString(Gmat::WriteMode mode,
                                                     const std::string &prefix,
                                                     const std::string &useName)
{
   std::string gen;
   
   // Build the local string
   if (mode != Gmat::NO_COMMENTS)
      gen = prefix + "GMAT ";
   
   if (mOutputNames.size() > 0)
   {
      gen += "[";
      for (StringArray::iterator i = mOutputNames.begin();
           i != mOutputNames.end(); ++i)
      {
         if (i != mOutputNames.begin())
            gen += ", ";
         gen += *i;
      }
      gen += "] = ";
   }
   
   gen += mFunctionName;
   
   if (mInputNames.size() > 0)
   {
      gen += "(";
      for (StringArray::iterator i = mInputNames.begin();
           i != mInputNames.end(); ++i)
      {
         if (i != mInputNames.begin())
            gen += ", ";
         gen += *i;
      }
      gen += ")";
   }
   
   generatingString = gen + ";";
   
   if (mode == Gmat::NO_COMMENTS)
      return generatingString;
   
   // Then call the base class method
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer CallFunction::GetParameterID(const std::string &str) const
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage("CallFunction::GetParameterID \n");
   #endif

   for (int i=GmatCommandParamCount; i<CallFunctionParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatCommandParamCount])
         return i;
   }
   
   return GmatCommand::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType CallFunction::GetParameterType(const Integer id) const
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage("CallFunction::GetParameterType\n");
   #endif

   if (id >= GmatCommandParamCount && id < CallFunctionParamCount)
      return PARAMETER_TYPE[id - GmatCommandParamCount];
   else
      return GmatCommand::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string CallFunction::GetParameterTypeString(const Integer id) const
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage("CallFunction::GetParameterTypeString\n");
   #endif

   if (id >= GmatCommandParamCount && id < CallFunctionParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id - GmatCommandParamCount)];
   else
      return GmatCommand::GetParameterTypeString(id);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string CallFunction::GetStringParameter(const Integer id) const
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage("CallFunction::GetStringParameter\n");
   #endif

   switch (id)
   {
   case FUNCTION_NAME:
      return fm.GetFunctionName();
      //return mFunctionName;
   default:
      return GmatCommand::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string CallFunction::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool CallFunction::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_CALL_FUNCTION_PARAM
      MessageInterface::ShowMessage
         ("CallFunction::SetStringParameter with id = %d and value = %s\n",
          id, value.c_str());
   #endif
      
   switch (id)
   {
   case FUNCTION_NAME:
      mFunctionName = value;
      fm.SetFunctionName(value);
      return true;
   case ADD_INPUT:
      return AddInputParameter(value, mNumInputParams);
   case ADD_OUTPUT:
      return AddOutputParameter(value, mNumOutputParams);
   default:
      return GmatCommand::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool CallFunction::SetStringParameter(const std::string &label,
                                const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const Integer id, const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool CallFunction::SetStringParameter(const Integer id, const std::string &value,
                                const Integer index)
{
   switch (id)
   {
   case ADD_INPUT:
      return AddInputParameter(value, index);
   case ADD_OUTPUT:
      return AddOutputParameter(value, index);
   default:
      return GmatCommand::SetStringParameter(id, value, index);
   }
}


//------------------------------------------------------------------------------
// virtual bool SetStringParameter(const std::string &label,
//                                 const std::string &value,
//                                 const Integer index)
//------------------------------------------------------------------------------
bool CallFunction::SetStringParameter(const std::string &label,
                                const std::string &value,
                                const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
const StringArray& CallFunction::GetStringArrayParameter(const Integer id) const
{
   switch (id)
   {
   case ADD_INPUT:
      return mInputNames;
   case ADD_OUTPUT:
      return mOutputNames;
   default:
      return GmatCommand::GetStringArrayParameter(id);
   }
}


//------------------------------------------------------------------------------
// StringArray& GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
const StringArray& CallFunction::GetStringArrayParameter(const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual bool TakeAction(const std::string &action,
//                         const std::string &actionData = "");
//------------------------------------------------------------------------------
/**
 * This method performs action.
 *
 * @param <action> action to perform
 * @param <actionData> action data associated with action
 * @return true if action successfully performed
 *
 */
//------------------------------------------------------------------------------
bool CallFunction::TakeAction(const std::string &action,
                        const std::string &actionData)
{
   if (action == "ClearInput")
   {
      ClearInputParameters();
      return true;
   }
   else if (action == "ClearOutput")
   {
      ClearOutputParameters();
      return true;
   }
   else if (action == "Clear")
   {
      ClearInputParameters();
      ClearOutputParameters();
      objectArray.clear();
      return true;
   }

   return GmatCommand::TakeAction(action, actionData);
}


//------------------------------------------------------------------------------
// StringArray GetRefObjectNameArray(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
const StringArray& CallFunction::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   
   switch (type) {
      case Gmat::PARAMETER:         // Input/Output
         for (unsigned int i=0; i<mInputNames.size(); i++)
            refObjectNames.push_back(mInputNames[i]);
         for (unsigned int i=0; i<mOutputNames.size(); i++)
            refObjectNames.push_back(mOutputNames[i]);
         return refObjectNames;
      default:
         break;
   }
   
   return refObjectNames;
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool CallFunction::RenameRefObject(const Gmat::ObjectType type,
                                   const std::string &oldName,
                                   const std::string &newName)
{
   #ifdef DEBUG_RENAME
   MessageInterface::ShowMessage
      ("CallFunction::RenameRefObject() type=%d, oldName='%s', newName='%s'\n",
       type, oldName.c_str(), newName.c_str());
   #endif
   
   if (type == Gmat::FUNCTION)
   {
      if (mFunctionName == oldName)
         mFunctionName = newName;
   }
   else if (type == Gmat::PARAMETER)
   {
      // parameters - go through input and output
      for (unsigned int i=0; i<mInputNames.size(); i++)
      {
         if (mInputNames[i] == oldName)
         {
            mInputNames[i] = newName;
            break;
         }
      }

      for (unsigned int i=0; i<mOutputNames.size(); i++)
      {
         if (mOutputNames[i] == oldName)
         {
            mOutputNames[i] = newName;
            break;
         }
      }
   }
   // Since parameter name is composed of spacecraftName.dep.paramType or
   // burnName.dep.paramType, check the type first
   else if (type == Gmat::SPACECRAFT || type == Gmat::BURN ||
            type == Gmat::COORDINATE_SYSTEM || type == Gmat::CALCULATED_POINT)
   {
      
      for (UnsignedInt i=0; i<mInputNames.size(); i++)
         if (mInputNames[i].find(oldName) != std::string::npos)
            mInputNames[i] =
               GmatStringUtil::Replace(mInputNames[i], oldName, newName);
      
      for (UnsignedInt i=0; i<mOutputNames.size(); i++)
         if (mOutputNames[i].find(oldName) != std::string::npos)
            mOutputNames[i] =
               GmatStringUtil::Replace(mOutputNames[i], oldName, newName);
   }

   return true;
}


// Reference object accessor methods
//------------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
GmatBase* CallFunction::GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name)
{
   switch (type)
   {
      case Gmat::PARAMETER:
         for (int i=0; i<mNumInputParams; i++)
         {
            if (mInputNames[i] == name)
               return mInputList[i];
         }
         
         for (int i=0; i<mNumOutputParams; i++)
         {
            if (mOutputNames[i] == name)
               return mOutputList[i];
         }
         
         throw GmatBaseException("ReportFile::GetRefObject() the object name: "
                           + name + "not found\n");
         
      case Gmat::FUNCTION:
         return mFunction;
         
      case Gmat::COMMAND:
         return callcmds;
         
      default:
         break;
   }

   // Not handled here -- invoke the next higher GetRefObject call
   return GmatCommand::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, ...
//------------------------------------------------------------------------------
/**
 * Sets reference object pointer.
 *
 * @return true if object successfully set, false otherwise
 */
//------------------------------------------------------------------------------
bool CallFunction::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                const std::string &name)
{
   #ifdef DEBUG_CALL_FUNCTION_REF_OBJ
   MessageInterface::ShowMessage
      ("CallFunction::SetRefObject() entered, obj=<%p><%s>'%s', type=%d, name='%s'\n",
       obj, obj ? obj->GetTypeName().c_str() : "NULL", obj ? obj->GetName().c_str() : "NULL",
       type, name.c_str());
   #endif
   
   if (obj == NULL)
      return false;
   
   switch (type)
   {
   case Gmat::PARAMETER:
      for (int i=0; i<mNumInputParams; i++)
      {
         if (mInputNames[i] == name)
         {
            mInputList[i] = (Parameter*)obj;
            return true;
         }
      }

      for (int i=0; i<mNumOutputParams; i++)
      {
         if (mOutputNames[i] == name)
         {
            mOutputList[i] = (Parameter*)obj;
            return true;
         }
      }
      
   case Gmat::FUNCTION:
      if (name == mFunctionName)
      {
         mFunction = (Function *)obj;
         if (mFunction && mFunction->GetTypeName() == "GmatFunction")
            fm.SetFunction(mFunction);
      }
      return true;
      
   case Gmat::COMMAND:
      if (callcmds)
         delete callcmds;
      callcmds = (GmatCommand*)obj;
      return true;
      
   default:
      break;
   }
   
   // Not handled here -- invoke the next higher SetRefObject call
   return GmatCommand::SetRefObject(obj, type, name);
}


//------------------------------------------------------------------------------
// virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
ObjectArray& CallFunction::GetRefObjectArray(const Gmat::ObjectType type)
{
   switch (type)
   {
   case Gmat::PARAMETER:
      objectArray.clear();

      for (unsigned int i=0; i<mInputList.size(); i++)
         objectArray.push_back(mInputList[i]);
      
      for (unsigned int i=0; i<mOutputList.size(); i++)
         objectArray.push_back(mOutputList[i]);
      
      return objectArray;
      
   default:
      break;
   }

   // Not handled here -- invoke the next higher SetReferenceObject call
   return GmatCommand::GetRefObjectArray(type);
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool CallFunction::Initialize()
{
   #ifdef DEBUG_CALL_FUNCTION_INIT
      MessageInterface::ShowMessage
         ("CallFunction::Initialize() this=<%p> entered, command = '%s'\n   "
          "function type is '%s', callingFunction is '%s'\n", this,
          GetGeneratingString(Gmat::NO_COMMENTS).c_str(), mFunction->GetTypeName().c_str(),
          callingFunction? (callingFunction->GetFunctionName()).c_str() : "NULL");
   #endif
   
   GmatCommand::Initialize();
   
   #ifdef DEBUG_OBJECT_MAP
   ShowObjectMaps("In CallFunction::Initialize()");
   #endif
   
   isGmatFunction = false;
   isMatlabFunction = false;
   
   bool rv = true;  // Initialization return value
   if (mFunction == NULL)
      throw CommandException("CallFunction::Initialize() the function pointer is NULL");
   
   if (mFunction->GetTypeName() == "GmatFunction")
      isGmatFunction = true;
   else if (mFunction->GetTypeName() == "MatlabFunction")
      isMatlabFunction = true;
   
   if (!isGmatFunction && !isMatlabFunction)
      throw CommandException
         ("CallFunction::Initialize() the function is neither GmatFunction nor MatlabFunction");
   
   // We need to add all Matlab paths to the bottom of the path using path(path, 'newpath')
   // since FileManager::GetAllMatlabFunctionPaths() returns in top to bottom order
   if (isMatlabFunction)
   {
#ifdef __USE_MATLAB__
      
      FileManager *fm = FileManager::Instance();
      StringArray paths = fm->GetAllMatlabFunctionPaths();
      
      // Open Matlab engine first
      matlabIf = MatlabInterface::Instance();
      if (!matlabIf->IsOpen())
         matlabIf->Open("GmatMatlab");
      
      #ifdef DEBUG_CALL_FUNCTION_INIT
      MessageInterface::ShowMessage("   Found %d matlab path\n", paths.size());
      #endif
      
      // Add path to the top of the Matlab path in reverse order(loj: 2008.10.16)
      std::string pathName;
      StringArray::reverse_iterator rpos = paths.rbegin();
      std::string addPath;
      while (rpos != paths.rend())
      {
         pathName = *rpos;
         if (pathName != "")
         {
            #ifdef DEBUG_CALL_FUNCTION_INIT
            MessageInterface::ShowMessage
               ("   Adding matlab path '%s' to the top\n", pathName.c_str());
            #endif
            addPath = "path('" + pathName + "', path)";
            matlabIf->EvalString(addPath);
         }
         rpos++;
      }
#else
      
      throw CommandException
         ("MATLAB Interface is disabled.  GMAT will not run if any CallFunction"
          " uses MATLAB function");
      
#endif
   }
   
   
   // Initialize input parameters only for MatlabFunction
   // Initialization of GmatFunctions are handled by FunctionManager during
   // execution
   if (isMatlabFunction)
   {
      // add input/output parameters
      mInputList.clear();
      GmatBase *mapObj;
      // need to initialize input parameters
      for (StringArray::iterator i = mInputNames.begin();
           i != mInputNames.end(); ++i)
      {
         if ((mapObj = FindObject(*i))  == NULL)
            throw CommandException("CallFunction command cannot find Parameter " +
                                   *i + " in script line\n   \"" +
                                   GetGeneratingString(Gmat::SCRIPTING) + "\"");
         
         #ifdef DEBUG_CALL_FUNCTION_INIT
         MessageInterface::ShowMessage("Adding input parameter %s\n", i->c_str());
         #endif
         
         mInputList.push_back((Parameter *)mapObj);
      }
      
      // need to initialize output parameters
      mOutputList.clear();
      
      for (StringArray::iterator i = mOutputNames.begin();
           i != mOutputNames.end();++i)
      {
         if ((mapObj = FindObject(*i))  == NULL)
            throw CommandException("CallFunction command cannot find Parameter " + (*i));
         
         #ifdef DEBUG_CALL_FUNCTION_INIT
         MessageInterface::ShowMessage("Adding output parameter %s\n", i->c_str());
         #endif
         
         mOutputList.push_back((Parameter *)mapObj);
      }
      
      if (mInputList.size() > 0)
         if (mInputList[0] == NULL)
         {
            MessageInterface::PopupMessage
               (Gmat::WARNING_,
                "CallFunction::Initialize() CallFunction will not be created.\n"
                "The first parameter selected as input for the CallFunction is NULL\n");
            return false;
         }
      
      if (mOutputList.size() > 0)
         if (mOutputList[0] == NULL)
         {
            MessageInterface::PopupMessage
               (Gmat::WARNING_,
                "CallFunction::Initialize() CallFunction will not be created.\n"
                "The first parameter selected as output for the CallFunction is NULL\n");
            return false;
         }
   }
   
   // Handle additional initialization for GmatFunctions
   if (isGmatFunction)
   {
      #ifdef DEBUG_GMAT_FUNCTION_INIT
         MessageInterface::ShowMessage
            ("CallFunction::Initialize: Initializing GmatFunction '%s'\n",
             mFunction->GetName().c_str());
      #endif
      fm.SetSolarSystem(solarSys);
      fm.SetTransientForces(forces);
      fm.SetGlobalObjectMap(globalObjectMap);
   }
   
   return rv;
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
bool CallFunction::Execute()
{
   bool status = false;
   
   if (mFunction == NULL)
      throw CommandException("Function is not defined for CallFunction");
   
   #ifdef DEBUG_TRACE
   static Integer callCount = 0;
   callCount++;      
   clock_t t1 = clock();
   MessageInterface::ShowMessage
      ("=== CallFunction::Execute() entered, '%s' Count = %d\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str(), callCount);
   #endif
   
   #ifdef DEBUG_CALL_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("CallFunction::Execute() this=<%p> entered, command = '%s'\n   "
          "function type is '%s', callingFunction is '%s'\n", this,
          GetGeneratingString(Gmat::NO_COMMENTS).c_str(), mFunction->GetTypeName().c_str(),
          callingFunction? (callingFunction->GetFunctionName()).c_str() : "NULL");
      #ifdef DEBUG_OBJECT_MAP
      ShowObjectMaps("object maps at the start");
      #endif
   #endif
      
   #ifdef __USE_MATLAB__
      if (isMatlabFunction)
      {
         #ifdef DEBUG_CALL_FUNCTION_EXEC
         MessageInterface::ShowMessage("   calling ExecuteMatlabFunction()\n");
         #endif
         
         status = ExecuteMatlabFunction();
         BuildCommandSummary(true);
         
         #ifdef DEBUG_CALL_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("CallFunction::Execute() MatlabFunction exiting with %d\n", status);
            #ifdef DEBUG_OBJECT_MAP
            ShowObjectMaps("object maps at the end");
            #endif
         #endif
         return status;
      }
   #endif
      
   if (isGmatFunction)
   {
      #ifdef DEBUG_CALL_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("   calling FunctionManager::Execute() with callingFunction='%s'\n",
          callingFunction ? (callingFunction->GetFunctionName()).c_str() : "NULL");
      #endif
      status = fm.Execute(callingFunction);
   }
   
   #ifdef DEBUG_CALL_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("   Executed command, about to build command summery\n");
   #endif
      
   BuildCommandSummary(true);
   
   #ifdef DEBUG_CALL_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("CallFunction::Execute() GmatFunction exiting with %d\n", status);
      #ifdef DEBUG_OBJECT_MAP
      ShowObjectMaps("object maps at the end");
      #endif
   #endif
      
   #ifdef DEBUG_TRACE
   clock_t t2 = clock();
   MessageInterface::ShowMessage
      ("=== CallFunction::Execute() exiting, '%s' Count = %d, Run Time: %f seconds\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str(), callCount, (Real)(t2-t1)/CLOCKS_PER_SEC);
   #endif
   
   return status;
}


//------------------------------------------------------------------------------
// void RunComplete()
//------------------------------------------------------------------------------
void CallFunction::RunComplete()
{
   #ifdef DEBUG_RUN_COMPLETE
   MessageInterface::ShowMessage
      ("CallFunction::RunComplete() entered for this=<%p> '%s',\n   "
       "FCS %sfinalized\n", this, GetGeneratingString(Gmat::NO_COMMENTS).c_str(),
       fm.IsFinalized() ? "already " : "NOT ");
   #endif
   
   if (!fm.IsFinalized())
   {
      #ifdef DEBUG_RUN_COMPLETE
      MessageInterface::ShowMessage("   calling FunctionManager::Finalize()\n");
      #endif
      fm.Finalize();
   }
   
   GmatCommand::RunComplete();
}


//------------------------------------------------------------------------------
// bool ExecuteMatlabFunction()
//------------------------------------------------------------------------------
bool CallFunction::ExecuteMatlabFunction()
{
#ifdef __USE_MATLAB__   
   
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage
      ("CallFunction::ExecuteMatlabFunction() entered, function='%s'\n",
       mFunctionName.c_str());
   #endif
   
   // open Matlab engine
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage(".....Opening Matlab Engine\n");
   #endif
   matlabIf->Open("GmatMatlab");
   
   // set format long so that we don't loose precision between string transmission
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage(".....Sending format long\n");
   #endif
   matlabIf->EvalString("format long");
   
   // Clear last errormsg
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage(".....Sending clear errormsg\n");
   #endif
   matlabIf->EvalString("clear errormsg");
   
   // add the path to the top of the path list using path('newpath', path)
   Integer pathId = mFunction->GetParameterID("FunctionPath");
   std::string thePath = mFunction->GetStringParameter(pathId);      
   
   if (thePath != "")
   {
      #ifdef DEBUG_MATLAB_EXEC
      MessageInterface::ShowMessage(".....Sending path to use\n");
      #endif
      std::string setPath = "path('" + thePath + "', path)";
      matlabIf->EvalString(setPath);
   }
   
   // send the in parameters
   for (unsigned int i=0; i<mInputList.size(); i++)
   {
      Parameter *param = (Parameter *)mInputList[i];
      #ifdef DEBUG_MATLAB_EXEC
      MessageInterface::ShowMessage
         (".....Sending input parameter <%p> '%s', %d out of %d\n", param,
          mInputNames[i].c_str(), i+1, mNumInputParams);
      #endif
      SendInParam(param);
   }
   
   //  Eval String
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage(".....Forming evaluate string\n");
   #endif
   std::string evalString = FormEvalString();
   EvalMatlabString(evalString);
   
   // get the value for the out parameters
   #ifdef DEBUG_MATLAB_EXEC
   MessageInterface::ShowMessage(".....Getting output parameters\n");
   #endif
   GetOutParams();
   
   return true;
   
#endif
   return false;
}


//------------------------------------------------------------------------------
// void SendInParam(Parameter *param)
//------------------------------------------------------------------------------
void CallFunction::SendInParam(Parameter *param)
{
#ifdef __USE_MATLAB__
   
   #ifdef DEBUG_SEND_PARAM
   MessageInterface::ShowMessage("CallFunction::SendInParam()");
   #endif
   
   if (param == NULL)
   {
      MessageInterface::ShowMessage("Parameter was null");
      return;
   }
   
   #ifdef DEBUG_SEND_PARAM
   MessageInterface::ShowMessage
      ("Parameter name=%s, type=%s\n", param->GetName().c_str(),
       param->GetTypeName().c_str());
   #endif
   
   if (param->GetTypeName() == "Array")
   {
      Array *array = (Array *)param;
      int numRows = array->GetIntegerParameter("NumRows");
      int numCols = array->GetIntegerParameter("NumCols");
      Rmatrix rmatrix = array->GetRmatrix();

      //----------------------------------------------------
      #ifdef __USE_EVAL_STRING__
      //----------------------------------------------------
      std::ostringstream os;
      os.precision(18);
      
      for (int j=0; j<numRows; j++)
      {
         os << "[";
         for (int k=0; k<numCols; k++)
            os << rmatrix(j, k) << ",";
         os << "], \n";
      }
      
      std::string inParamString = array->GetName() + " = [" +os.str() + "];";
      EvalMatlabString(inParamString);
      
      //----------------------------------------------------
      #else
      //----------------------------------------------------
      const Real *realArray = rmatrix.GetDataVector();
      
      #ifdef DEBUG_SEND_PARAM
      MessageInterface::ShowMessage
         (".....Putting RealArray data <%p> to Matlab workspace\n", realArray);
      #endif
      matlabIf->PutRealArray(param->GetName(), numRows, numCols, realArray);
      
      //----------------------------------------------------
      #endif
      //----------------------------------------------------
   }
   else if (param->GetTypeName() == "Variable")
   {
      //----------------------------------------------------
      #ifndef __USE_EVAL_STRING__
      //----------------------------------------------------
      std::ostringstream os;
      os.precision(18);
      os << param->EvaluateReal();
      
      std::string inParamString = param->GetName() +" = " + os.str() +";";
      
      #ifdef DEBUG_SEND_PARAM
      MessageInterface::ShowMessage
         ("Sent string %s to matlab\n", inParamString.c_str());
      #endif
      
      EvalMatlabString(inParamString);
      
      //----------------------------------------------------
      #else
      //----------------------------------------------------
      static Real *realVal = new Real[1];
      realVal[0] = param->EvaluateReal();
      
      #ifdef DEBUG_SEND_PARAM
      MessageInterface::ShowMessage
         (".....Putting Real data %p to Matlab workspace\n", realVal);
      #endif
      
      matlabIf->PutRealArray(param->GetName(), 1, 1, (const Real*)realVal);
      
      //----------------------------------------------------
      #endif
      //----------------------------------------------------
   }
   else if (param->GetTypeName() == "String")
   {
      StringVar *stringVar = (StringVar *)param;
      std::string inParamString = param->GetName() +" = '" +
         stringVar->GetString() +"';";
      
      EvalMatlabString(inParamString);
   }
   else // it is any object
   {
      if (param->GetTypeName() == "Spacecraft")
         param->TakeAction("UpdateEpoch");
      
      std::string inParamString =
         param->GetGeneratingString(Gmat::MATLAB_STRUCT);
      
      #ifdef DEBUG_SEND_PARAM
      MessageInterface::ShowMessage
         ("Generated param string :\n%s\n", inParamString.c_str());
      #endif
      
      EvalMatlabString(inParamString);
   }
   
#endif
}


//------------------------------------------------------------------------------
// void GetOutParams()
//------------------------------------------------------------------------------
void CallFunction::GetOutParams()
{
#ifdef __USE_MATLAB__
   try
   {
      for (unsigned int i=0; i<mOutputList.size(); i++)
      {
         Parameter *param = (Parameter *)mOutputList[i];
         std::string varName = param->GetName();
         
         #ifdef DEBUG_GET_OUTPUT
         MessageInterface::ShowMessage
            ("CallFunction::GetOutParams() OutParamType=%s, name=%s\n",
             param->GetTypeName().c_str(), varName.c_str());
         #endif
         
         if (param->GetTypeName() == "Array")
         {
            Array *array = (Array *)param;
            int numRows = array->GetIntegerParameter("NumRows");
            int numCols = array->GetIntegerParameter("NumCols");
            int totalCells = numRows * numCols;            
            double *outArray = new double[totalCells];
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (outArray, "outArray", "CallFunction::GetOutParams()",
                "*outArray = new double[totalCells]");
            //MessageInterface::ShowMessage
            //   ("+++ CallFunction::GetOutParams() double *outArray = new double[%d], <%p>\n",
            //    totalCells, outArray);
            #endif
            
            #ifdef DEBUG_GET_OUTPUT
            MessageInterface::ShowMessage
               ("CallFunction::GetOutParams() calling MI::GetRealArray()\n");
            #endif
            if (matlabIf->GetRealArray(varName, totalCells, outArray) == 0)
            {
               std::string msg =
                  "CallFunction::GetOutParams() error occurred in matlabIf->GetRealArray()";
               #ifdef DEBUG_GET_OUTPUT
               MessageInterface::ShowMessage(msg + "\n");
               #endif
               throw CommandException(msg);
            }
            
            // create rmatrix
            Rmatrix rmatrix = Rmatrix (numRows, numCols);
            // copy value
            for (int j=0; j<numCols; j++)
               for (int k=0; k<numRows; k++)
                  rmatrix(k, j) = outArray[(j*numRows) + k];
            
            #ifdef DEBUG_SHOW_ARRAY
            for (int j=0; j<numRows; j++)
            {
               for (int k=0; k<numCols; k++)
                  MessageInterface::ShowMessage("%f\t", rmatrix(j, k));
               MessageInterface::ShowMessage("\n");
            }
            #endif
            
            // assign rmatrix to array
            array->SetRmatrixParameter("RmatValue", rmatrix);
            
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (outArray, "outArray", "CallFunction::GetOutParams()",
                "deletinig outArray");
            //MessageInterface::ShowMessage
            //   ("--- CallFunction::GetOutParams() deleting outArray <%p>\n",
            //    outArray);
            #endif
            delete [] outArray;
            
         }
         else if (param->GetTypeName() == "Variable")
         {
            double *outArray = new double[1];
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (outArray, "outArray", "CallFunction::GetOutParams()",
                "*outArray = new double[1]");
            //MessageInterface::ShowMessage
            //   ("+++ CallFunction::GetOutParams() double *outArray = new double[1], <%p>\n",
            //    outArray);
            #endif
            
            #ifdef DEBUG_UPDATE_VAR
            MessageInterface::ShowMessage
               (".....Calling matlabIf->GetRealArray()\n");
            #endif
            matlabIf->GetRealArray(varName, 1, outArray);
            
            param->SetReal(outArray[0]);
            std::ostringstream ss;
            ss.precision(18);
            ss << outArray[0];
            param->SetStringParameter("Expression", ss.str());
            
            #ifdef DEBUG_UPDATE_VAR
            MessageInterface::ShowMessage
               ("The EvaluateReal is %f\n",  param->EvaluateReal());
            MessageInterface::ShowMessage
               ("The GetReal is %f\n", param->GetReal());
            #endif
            
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (outArray, "outArray", "CallFunction::GetOutParams()",
                "deletinig outArray");
            //MessageInterface::ShowMessage
            //   ("--- CallFunction::GetOutParams() deleting outArray <%p>\n",
            //    outArray);
            #endif
            delete [] outArray;
         }
         else if (param->GetTypeName() == "String")
         {
            //----------------------------------------------
            #ifdef __USE_EVAL_STRING__
            //----------------------------------------------            
            #ifdef DEBUG_UPDATE_VAR
            MessageInterface::ShowMessage
               (".....Calling matlabIf->SetOutputBuffer()\n");
            #endif
            
            // need to output string value to buffer
            int bufSize = matlabIf->SetOutputBuffer(512);
            EvalMatlabString(varName);
            
            char *outBuffer = matlabIf->GetOutputBuffer();
            
            // get rid of "var ="
            char *ptr = strtok((char *)outBuffer, "=");
            ptr = strtok(NULL, "\n");
            
            //----------------------------------------------
            #else
            //----------------------------------------------
            #ifdef DEBUG_UPDATE_VAR
            MessageInterface::ShowMessage
               (".....Calling matlabIf->GetString()\n");
            #endif
            std::string outStr;
            matlabIf->GetString(varName, outStr);
            param->SetStringParameter("Expression", outStr);
            
            //----------------------------------------------
            #endif
            //----------------------------------------------
         }
         else // objects
         {
            #ifdef DEBUG_UPDATE_OBJECT
            MessageInterface::ShowMessage
               (".....Calling matlabIf->SetOutputBuffer() for object\n");
            #endif
            
            char buffer[8192];
            int bufSize = matlabIf->SetOutputBuffer(8192);
            EvalMatlabString(varName);
            char *outBuffer = matlabIf->GetOutputBuffer();
            
            // copy output to local buffer
            strncpy(buffer, outBuffer, bufSize);
            
            #ifdef DEBUG_UPDATE_OBJECT
            MessageInterface::ShowMessage("   buffer=\n%s\n", buffer);
            #endif
            
            // assign new value to object
            UpdateObject(param, buffer);
         }
      }
   }
   catch (BaseException &e)
   {
      std::string moreMsg = e.GetFullMessage() + " in \n" +
         GetGeneratingString(Gmat::SCRIPTING);
      e.SetMessage("");
      e.SetDetails(moreMsg);
      throw;
   }
#endif
}


//------------------------------------------------------------------------------
// void EvalMatlabString(std::string evalString)
//------------------------------------------------------------------------------
void CallFunction::EvalMatlabString(std::string evalString)
{
#ifdef __USE_MATLAB__
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage
      ("CallFunction::EvalMatlabString() calling MI::RunMatlabString() with\n"
       "======================================================================\n"
       "%s\n\n", evalString.c_str());
   #endif
   
   matlabIf->RunMatlabString(evalString);
   
#endif
}


//------------------------------------------------------------------------------
// void ClearInputParameters()
//------------------------------------------------------------------------------
void CallFunction::ClearInputParameters()
{
   mInputList.clear();
   mInputNames.clear();
   mNumInputParams = 0;
}


//------------------------------------------------------------------------------
// void ClearOutputParameters()
//------------------------------------------------------------------------------
void CallFunction::ClearOutputParameters()
{
   mOutputList.clear();
   mOutputNames.clear();
   mNumOutputParams = 0;
}


//------------------------------------------------------------------------------
// void SetInternalCoordSystem(CoordinateSystem *cs)
//------------------------------------------------------------------------------
/**
 *  Sets the internal coordinate system used by the Sandbox.
 *
 *  @param <cs> The internal coordinate system.
 */
//------------------------------------------------------------------------------
void CallFunction::SetInternalCoordSystem(CoordinateSystem *cs)
{
   /// @todo Check initialization and cloning for the internal CoordinateSystem.
   //internalCoordSys = (CoordinateSystem*)(cs->Clone());
   internalCoordSys = cs;
   fm.SetInternalCoordinateSystem(internalCoordSys);
}


//------------------------------------------------------------------------------
// void SetPublisher(Publisher *pub)
//------------------------------------------------------------------------------
/**
 *  Passes the Publisher used by the Sandbox to FunctionManager
 *
 *  @param <pub> The publisher
 */
//------------------------------------------------------------------------------
void CallFunction::SetPublisher(Publisher *pub)
{
   #ifdef DEBUG_PUBLISHER
   MessageInterface::ShowMessage
      ("CallFunction::SetPublisher() setting publiser <%p> to FunctionManager\n", pub);
   #endif
   GmatCommand::SetPublisher(pub);
   fm.SetPublisher(pub);
}


//------------------------------------------------------------------------------
// void UpdateObject(GmatBase *obj, char *buffer)
//------------------------------------------------------------------------------
void CallFunction::UpdateObject(GmatBase *obj, char *buffer)
{
   StringTokenizer st(buffer, ": \n");
   StringArray tokens = st.GetAllTokens();

   #ifdef DEBUG_UPDATE_OBJECT
   for (unsigned int i=0; i<tokens.size(); i++)
      MessageInterface::ShowMessage("tokens[%d]=<%s>\n", i, tokens[i].c_str());
   #endif
   
   int id;
   Gmat::ParameterType type;
   std::string newstr;

   // actual parameter starts at 2
   for (unsigned int i=2; i<tokens.size(); i+=2)
   {
      //MessageInterface::ShowMessage("tokens[%d]=<%s>\n", i, tokens[i].c_str());
      id = obj->GetParameterID(tokens[i]);
      type = obj->GetParameterType(id);
      
      switch (type)
      {
      case Gmat::STRING_TYPE:
      {
         //MessageInterface::ShowMessage
         //   ("tokens[i+1]=<%s>, length=%d\n", tokens[i+1].c_str(),
         //    tokens[i+1].length());
                  
         if (((tokens[i+1].c_str())[0] == '\'')&&
            ((tokens[i+1].c_str())[tokens[i+1].length()-1] == '\''))
            newstr = tokens[i+1].substr(1, tokens[i+1].length()-2);
         else if ((tokens[i+1].c_str())[0] == '\'')
         {
            // assume it is a gregorian date then DD MMM YYYY hh:mm:ss.sss
            // this probably isn't the best way, but it will do for now...
            
            // DD
            newstr = tokens[i+1].substr(1, tokens[i+1].length()-1) + " ";
            i++;
            
            // MMM
            newstr = newstr + tokens[i+1].substr(0, tokens[i+1].length()) + " "; 
            i++;
            
            // YYYY
            newstr = newstr + tokens[i+1].substr(0, tokens[i+1].length()) + " "; 
            i++;
            
            // hh:
            newstr = newstr + tokens[i+1].substr(0, tokens[i+1].length()) + ":"; 
            i++;
            
            // mm:                          
            newstr = newstr + tokens[i+1].substr(0, tokens[i+1].length()) + ":"; 
            i++;

            // ss.sss
            newstr = newstr + tokens[i+1].substr(0, tokens[i+1].length()-1); 

         }
         else
            newstr = tokens[i+1].substr(1, tokens[i+1].length()-2);
            
         //MessageInterface::ShowMessage("newstr=<%s>\n", newstr.c_str());
         obj->SetStringParameter(id, newstr);
         break;
      }
      case Gmat::REAL_TYPE:
         obj->SetRealParameter(id, atof(tokens[i+1].c_str()));
         break;
      default:
         throw CommandException
            ("\nCurrently CallFunction cannot update output object for "
             "parameter type: " + GmatBase::PARAM_TYPE_STRING[type] + "\n");
      }
   }

   #ifdef DEBUG_UPDATE_OBJECT
   MessageInterface::ShowMessage
      ("new %s=\n%s\n", obj->GetName().c_str(),
       obj->GetGeneratingString(Gmat::MATLAB_STRUCT).c_str());
   #endif
}

