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
 * Implementation for the GmatFunction class.
 */
//------------------------------------------------------------------------------

#include "GmatFunction.hpp"
#include "FileManager.hpp"       // for GetGmatFunctionPath()
#include "FileUtil.hpp"          // for ParseFileName()
#include "StringUtil.hpp"        // for Trim()
#include "MessageInterface.hpp"

//#define DEBUG_GMAT_FUNCTION


//---------------------------------
// static data
//---------------------------------
//const std::string
//GmatFunction::PARAMETER_TEXT[GmatFunctionParamCount - FunctionParamCount] =
//{
//};
//
//const Gmat::ParameterType
//GmatFunction::PARAMETER_TYPE[GmatFunctionParamCount - FunctionParamCount] =
//{
//};




//------------------------------------------------------------------------------
// GmatFunction(std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor.
 *
 * @param <name> function name
 */
//------------------------------------------------------------------------------
GmatFunction::GmatFunction(const std::string &name) :
   Function("GmatFunction", name)
{
   // for initial function path, use FileManager
   FileManager *fm = FileManager::Instance();
   std::string pathname;
   
   try
   {
      // if there is a function name, try to locate it
      if (name != "")
      {
         // Get path of first it is located
         pathname = fm->GetGmatFunctionPath(name + ".gmf");
         
         // gmat function uses whole path name
         pathname = pathname + name + ".gmf";         
         functionPath = pathname;
         functionName = GmatFileUtil::ParseFileName(functionPath);
         
         // Remove path and .gmf
         functionName = GmatFileUtil::ParseFileName(functionPath);
         std::string::size_type dotIndex = functionName.find(".gmf");
         functionName = functionName.substr(0, dotIndex);
      }
      else
      {
         // gmat function uses whole path name
         functionPath = fm->GetFullPathname("GMAT_FUNCTION_PATH");    
      }
   }
   catch (GmatBaseException &e)
   {
      #ifdef DEBUG_GMAT_FUNCTION
      MessageInterface::ShowMessage(e.GetFullMessage());
      #endif
      
      // see if there is FUNCTION_PATH
      try
      {
         pathname = fm->GetFullPathname("FUNCTION_PATH");
         functionPath = pathname;
      }
      catch (GmatBaseException &e)
      {
         #ifdef DEBUG_GMAT_FUNCTION
         MessageInterface::ShowMessage(e.GetFullMessage());
         #endif
      }
   }
   
   #ifdef DEBUG_GMAT_FUNCTION
   MessageInterface::ShowMessage
      ("   Gmat functionPath=<%s>\n", functionPath.c_str());
   MessageInterface::ShowMessage
      ("   Gmat functionName=<%s>\n", functionName.c_str());
   #endif
}


//------------------------------------------------------------------------------
// ~GmatFunction()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
GmatFunction::~GmatFunction()
{
}


//------------------------------------------------------------------------------
// GmatFunction(const GmatFunction &copy)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <copy> object being copied
 */
//------------------------------------------------------------------------------
GmatFunction::GmatFunction(const GmatFunction &copy) :
   Function(copy)
{
}


//------------------------------------------------------------------------------
// GmatFunction& GmatFunction::operator=(const GmatFunction& right)
//------------------------------------------------------------------------------
/**
 * The assignment operator
 *
 * @param <right> object being copied
 *
 * @return reference to this object
 */
//------------------------------------------------------------------------------
GmatFunction& GmatFunction::operator=(const GmatFunction& right)
{
   if (this == &right)
      return *this;
   
   Function::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool GmatFunction::Initialize()
{
   #ifdef DEBUG_GMAT_FUNCTION
      MessageInterface::ShowMessage("Entering GmatFunction::Initialize for function %s\n",
            functionName.c_str());
      MessageInterface::ShowMessage("   and FCS is %s set.\n", (fcs? "correctly" : "NOT"));
      MessageInterface::ShowMessage("   Pointer for FCS is %p\n", fcs);
      MessageInterface::ShowMessage("   First command in fcs is %s\n",
            (fcs->GetTypeName()).c_str());         
   #endif
   if (!fcs) return false;
   // Initialize the Validator - I think I need to do this each time - or do I?
   validator.SetFunction(this);
   validator.SetSolarSystem(solarSys);
   std::map<std::string, GmatBase *>::iterator omi;
   //loj: moved this block inside the while loop
//    for (omi = objectStore->begin(); omi != objectStore->end(); ++omi)
//       validatorStore.insert(std::make_pair(omi->first, omi->second));
//    for (omi = globalObjectStore->begin(); omi != globalObjectStore->end(); ++omi)
//       validatorStore.insert(std::make_pair(omi->first, omi->second));
//    validator.SetObjectMap(&validatorStore);
   
   // add automatic objects to the FOS
   for (omi = automaticObjects.begin(); omi != automaticObjects.end(); ++omi)
   {
      if (objectStore->find(omi->first) == objectStore->end())
         objectStore->insert(std::make_pair(omi->first, omi->second));
   }
   // first, send all the commands the input wrappers
   
   GmatCommand *current = fcs;
   
   while (current)
   {
      #ifdef DEBUG_GMAT_FUNCTION
         if (!current)  MessageInterface::ShowMessage("Current is NULL!!!\n");
         else MessageInterface::ShowMessage("   Now about to initialize command of type %s\n",
               (current->GetTypeName()).c_str());         
      #endif
      current->SetObjectMap(objectStore);
      current->SetGlobalObjectMap(globalObjectStore);
      current->SetSolarSystem(solarSys);
      current->SetTransientForces(forces);      
      #ifdef DEBUG_GMAT_FUNCTION
         MessageInterface::ShowMessage("   Now about to send required wrappers to command of type %s\n",
               (current->GetTypeName()).c_str());         
      #endif
      // (Re)set object map on Validator (necessary because objects may have been added to the 
      // Local Object Store or Global Object Store during initialization of previous commands)
      validatorStore.clear();
      for (omi = objectStore->begin(); omi != objectStore->end(); ++omi)
         validatorStore.insert(std::make_pair(omi->first, omi->second));
      for (omi = globalObjectStore->begin(); omi != globalObjectStore->end(); ++omi)
         validatorStore.insert(std::make_pair(omi->first, omi->second));
      validator.SetObjectMap(&validatorStore);
      
      // Let's try to ValidateCommand here, this will validate the comand
      // and create wrappers also
      if (!validator.ValidateCommand(current, false, true))
         return false;
      if (!(current->Initialize()))
         return false;
      current = current->GetNext();
   }
   return true;
}


//------------------------------------------------------------------------------
// bool GmatFunction::Execute()
//------------------------------------------------------------------------------
bool GmatFunction::Execute()
{
   if (!fcs) return false;
   GmatCommand *current = fcs;
   bool        isFunction = false;
   while (current)
   {
      // Call to IsNextAFunction is necessary for branch commands in particular
      isFunction = current->HasAFunction();
      #ifdef DEBUG_GMAT_FUNCTION
            MessageInterface::ShowMessage("In GmatFunction execute loop and next command (%s) %s a function call.\n",
                  (current->GetTypeName()).c_str(), isFunction? "HAS" : "DOES NOT HAVE");
      #endif
      if (isFunction)
      {
         ; // @todo - call stack stuff
      }
      if (!(current->Execute()))
         return false;
      if (isFunction)
      {
         ; // @todo - call stack stuff
      }
      current = current->GetNext();
   }
   // create output wrappers and put into map
   GmatBase *obj;
   for (unsigned int jj = 0; jj < outputNames.size(); jj++)
   {
      if (!(obj = FindObject(outputNames.at(jj))))
      {
         std::string errMsg = "Function: Output \"" + outputNames.at(jj);
         errMsg += " not found for function \"" + functionName + "\"";
         throw FunctionException(errMsg);
      }
      ElementWrapper *outWrapper = validator.CreateElementWrapper(outputNames.at(jj), false, false);
      outWrapper->SetRefObject(obj); 
      outputArgMap[outputNames.at(jj)] = outWrapper;
      #ifdef DEBUG_GMAT_FUNCTION // --------------------------------------------------- debug ---
         MessageInterface::ShowMessage("GmatFunction: Output wrapper created for %s\n", (outputNames.at(jj)).c_str());
      #endif // -------------------------------------------------------------- end debug ---
   }
   return true; 
}


void GmatFunction::Finalize()
{
   ; // @todo - finalize anything else that needs it as well
   GmatCommand *current = fcs;
   while (current)
   {
      #ifdef DEBUG_FUNCTION
         if (!current)  MessageInterface::ShowMessage("Current is NULL!!!\n");
         else MessageInterface::ShowMessage("   Now about to finalize (call RunComplete on) command %s\n",
               (current->GetTypeName()).c_str());         
      #endif
      current->RunComplete();
      current = current->GetNext();
   }
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Clone of the GmatFunction.
 *
 * @return clone of the GmatFunction.
 *
 */
//------------------------------------------------------------------------------
GmatBase* GmatFunction::Clone() const
{
   return (new GmatFunction(*this));
}


//---------------------------------------------------------------------------
// void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void GmatFunction::Copy(const GmatBase* orig)
{
   operator=(*((GmatFunction *)(orig)));
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return If value of the parameter was set.
 */
//------------------------------------------------------------------------------
bool GmatFunction::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_FUNCTION_SET
   MessageInterface::ShowMessage
      ("GmatFunction::SetStringParameter() entered, id=%d, value=%s\n", id, value.c_str());
   #endif
   
   switch (id)
   {
   case FUNCTION_PATH:
      {
         FileManager *fm = FileManager::Instance();
         
         // Compose full path if it has relative path.
         // Assuming if first char has '.', it has relative path.
         std::string temp = GmatStringUtil::Trim(value);
         if (temp[0] == '.')
         {
            std::string currPath = fm->GetCurrentPath();
            
            #ifdef DEBUG_FUNCTION_SET
            MessageInterface::ShowMessage("   currPath=%s\n", currPath.c_str());
            #endif
            
            functionPath = currPath + temp.substr(1);
         }
         else
         {
            functionPath = value;
         }
         
         // add to GmatFunction path (loj: 2008.04.18)
         fm->AddGmatFunctionPath(functionPath);
         
         // Remove path
         functionName = GmatFileUtil::ParseFileName(functionPath);
         
         // Remove .gmf if GmatGmatFunction
         std::string::size_type dotIndex = functionName.find(".gmf");
         functionName = functionName.substr(0, dotIndex);
         
         #ifdef DEBUG_FUNCTION_SET
         MessageInterface::ShowMessage
            ("   functionPath=<%s>\n", functionPath.c_str());
         MessageInterface::ShowMessage
            ("   functionName=<%s>\n", functionName.c_str());
         #endif
         
         return true;
      }
   case FUNCTION_NAME:
      {
         // Remove path if it has one
         functionName = GmatFileUtil::ParseFileName(functionPath);
         
         // Remove .gmf if GmatGmatFunction
         std::string::size_type dotIndex = functionName.find(".gmf");
         functionName = functionName.substr(0, dotIndex);
         
         return true;
      }
   default:
      return Function::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
bool GmatFunction::SetStringParameter(const std::string &label,
                                      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}

