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
#include "Assignment.hpp"        // for Assignment::GetMathTree()
#include "FileManager.hpp"       // for GetGmatFunctionPath()
#include "FileUtil.hpp"          // for ParseFileName()
#include "StringUtil.hpp"        // for Trim()
#include "CommandUtil.hpp"       // for ClearCommandSeq()
#include "MessageInterface.hpp"

//#define DEBUG_FUNCTION
//#define DEBUG_FUNCTION_SET
//#define DEBUG_FUNCTION_INIT
//#define DEBUG_FUNCTION_EXEC
//#define DEBUG_FUNCTION_FINALIZE

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif
//#ifndef DEBUG_PERFORMANCE
//#define DEBUG_PERFORMANCE
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif
#ifdef DEBUG_PERFORMANCE
#include <ctime>                 // for clock()
#endif

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
   mIsNewFunction = false;
   
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
      #ifdef DEBUG_FUNCTION
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
         #ifdef DEBUG_FUNCTION
         MessageInterface::ShowMessage(e.GetFullMessage());
         #endif
      }
   }
   
   #ifdef DEBUG_FUNCTION
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
   #ifdef DEBUG_GMATFUNCTION
   MessageInterface::ShowMessage
      ("GmatFunction() destructor entered, this=<%p> '%s', fcs=<%p>\n", this,
       GetName().c_str(), fcs);
   #endif
   
   // delete function sequence including NoOp (loj: 2008.12.22)
   if (fcs)
      GmatCommandUtil::ClearCommandSeq(fcs, false);
   
   #ifdef DEBUG_GMATFUNCTION
   MessageInterface::ShowMessage("GmatFunction() destructor exiting\n");
   #endif
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
// bool IsNewFunction()
//------------------------------------------------------------------------------
/**
 * Return true if function was created but not saved to file.
 * FunctionSetupPanel uses this flag to open new editor or load existing function.
 */
//------------------------------------------------------------------------------
bool GmatFunction::IsNewFunction()
{
   return mIsNewFunction;
}


//------------------------------------------------------------------------------
// void SetNewFunction(bool flag)
//------------------------------------------------------------------------------
void GmatFunction::SetNewFunction(bool flag)
{
   mIsNewFunction = flag;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool GmatFunction::Initialize()
{
   #ifdef DEBUG_FUNCTION_INIT
      MessageInterface::ShowMessage
         ("======================================================================\n"
          "GmatFunction::Initialize() entered for function '%s'\n", functionName.c_str());
      MessageInterface::ShowMessage("   and FCS is %s set.\n", (fcs? "correctly" : "NOT"));
      MessageInterface::ShowMessage("   Pointer for FCS is %p\n", fcs);
      MessageInterface::ShowMessage("   First command in fcs is %s\n",
            (fcs->GetTypeName()).c_str());
      MessageInterface::ShowMessage("   internalCS is %p\n", internalCoordSys);
   #endif
   if (!fcs) return false;
   
   Function::Initialize();
   
   // Initialize the Validator - I think I need to do this each time - or do I?
   validator->SetFunction(this);
   validator->SetSolarSystem(solarSys);
   std::map<std::string, GmatBase *>::iterator omi;
   
   // add automatic objects to the FOS (well, actually, clones of them)
   for (omi = automaticObjectMap.begin(); omi != automaticObjectMap.end(); ++omi)
   {
      std::string objName = omi->first;
      
      // if name not found, clone it and add to map (loj: 2008.12.15)
      if (objectStore->find(omi->first) == objectStore->end())
      {
         GmatBase *autoObj = (omi->second)->Clone();
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (autoObj, objName, "GmatFunction::Initialize()",
             "autoObj = (omi->second)->Clone()");
         #endif
         objectStore->insert(std::make_pair(objName, autoObj));
      }
   }
   
   // first, send all the commands the object store, solar system, etc
   GmatCommand *current = fcs;
   
   while (current)
   {
      #ifdef DEBUG_FUNCTION_INIT
         if (!current)  MessageInterface::ShowMessage("Current is NULL!!!\n");
         else MessageInterface::ShowMessage("   =====> Current command is %s <%s>\n",
                 (current->GetTypeName()).c_str(),
                 current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
      #endif
      current->SetObjectMap(objectStore);
      current->SetGlobalObjectMap(globalObjectStore);
      current->SetSolarSystem(solarSys);
      current->SetInternalCoordSystem(internalCoordSys);
      current->SetTransientForces(forces);
      #ifdef DEBUG_FUNCTION_INIT
         MessageInterface::ShowMessage
            ("   Now about to set object map of type %s to Validator\n",
             (current->GetTypeName()).c_str());      
      #endif
      // (Re)set object map on Validator (necessary because objects may have been added to the 
      // Local Object Store or Global Object Store during initialization of previous commands)
      validatorStore.clear();
      for (omi = objectStore->begin(); omi != objectStore->end(); ++omi)
         validatorStore.insert(std::make_pair(omi->first, omi->second));
      for (omi = globalObjectStore->begin(); omi != globalObjectStore->end(); ++omi)
         validatorStore.insert(std::make_pair(omi->first, omi->second));
      validator->SetObjectMap(&validatorStore);
      
      #ifdef DEBUG_FUNCTION_INIT
      MessageInterface::ShowMessage
         ("   Now about to call Validator->ValidateCommand() of type %s\n",
          current->GetTypeName().c_str());
      #endif
      
      // Let's try to ValidateCommand here, this will validate the comand
      // and create wrappers also
      if (!validator->ValidateCommand(current, false, 2))
      {
         // get error message (loj: 2008.06.04)
         StringArray errList = validator->GetErrorList();
         std::string msg; // Check for empty errList (loj: 2009.03.17)
         if (errList.empty())
            msg = "Error occurred";
         else
            msg = errList[0];
         
         throw FunctionException(msg + " in the function \"" + functionPath + "\"");
      }
      
      #ifdef DEBUG_FUNCTION_INIT
      MessageInterface::ShowMessage
         ("   Now about to initialize command of type %s\n", current->GetTypeName().c_str());
      #endif
      
      // catch exception and add function name to message (loj: 2008.09.23)
      try
      {
         if (!(current->Initialize()))
         {
            #ifdef DEBUG_FUNCTION_INIT
            MessageInterface::ShowMessage
               ("Exiting  GmatFunction::Initialize for function '%s' with false\n",
                functionName.c_str());
            #endif
            return false;
         }
      }
      catch (BaseException &e)
      {
         throw FunctionException("Cannot continue due to " + e.GetFullMessage() +
                                 " in the function \"" + functionPath + "\"");
      }
      
      // Check to see if the command needs a server startup (loj: 2008.07.25)
      if (current->NeedsServerStartup())
         if (validator->StartServer(current) == false)
            throw FunctionException("Unable to start the server needed by the " +
                                   (current->GetTypeName()) + " command");
      
      current = current->GetNext();
   }
   fcsFinalized = false;
   #ifdef DEBUG_FUNCTION_INIT
   MessageInterface::ShowMessage
      ("GmatFunction::Initialize() exiting for function '%s' with true\n",
       functionName.c_str());
   #endif
   return true;
}


//------------------------------------------------------------------------------
// bool GmatFunction::Execute(ObjectInitializer *objInit, bool reinitialize)
//------------------------------------------------------------------------------
bool GmatFunction::Execute(ObjectInitializer *objInit, bool reinitialize)
{
   if (!fcs) return false;
   if (!objInit) return false;
   
   #ifdef DEBUG_PERFORMANCE
   static Integer callCount = 0;
   callCount++;      
   clock_t t1 = clock();
   MessageInterface::ShowMessage
      ("=== GmatFunction::Execute() entered, '%s' Count = %d\n",
       functionName.c_str(), callCount);
   #endif
   
   #ifdef DEBUG_FUNCTION_EXEC
   MessageInterface::ShowMessage
      ("======================================================================\n"
       "GmatFunction::Execute() entered for '%s'\n   internalCS is %p\n",
       functionName.c_str(), internalCoordSys);
   #endif
   
   GmatCommand *current = fcs;
   GmatCommand *last = NULL;
   
   // We want to initialize local objects with new object map,
   // so do it everytime (loj: 2008.09.26)
   // This causes to slow down function execution, so initialize if necessary
   if (reinitialize)
      objectsInitialized = false;
   
   while (current)
   {
      // Call to IsNextAFunction is necessary for branch commands in particular
      #ifdef DEBUG_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("......Function executing <%p><%s> [%s]\n", current, current->GetTypeName().c_str(),
          current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
      MessageInterface::ShowMessage("      objectsInitialized=%d\n", objectsInitialized);
      #endif
      
      last = current;
      
      // Since we don't know where actual mission squence start, just check for command
      // that is not NoOp, Create, Global, and GMAT with equation.
      // Can we have simple command indicating beginning of the sequence,
      // such as BeginMission? (loj: 2008.06.19)
      Function *func = current->GetCurrentFunction();
      bool isEquation = false;
      std::string cmdType = current->GetTypeName();
      if (func && cmdType == "GMAT")
         if (((Assignment*)current)->GetMathTree() != NULL)
            isEquation = true;
      
      if (!objectsInitialized)
      {
         if (cmdType != "NoOp" && cmdType != "Create" && cmdType != "Global")
         {
            bool beginInit = true;            
            if (cmdType == "GMAT" && !isEquation)
               beginInit = false;
            
            if (beginInit)
            {
               objectsInitialized = true;
               
               #ifdef DEBUG_FUNCTION_EXEC
               MessageInterface::ShowMessage
                  ("Now at command \"%s\"\n\n",
                   current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
               MessageInterface::ShowMessage
                  ("\n============================ Begin initialization of local objects\n");
               #endif
               
               // Why internal coordinate syste is empty in ObjectInitializer?
               // Set internal coordinate system (added (loj: 2008.10.07)
               objInit->SetInternalCoordinateSystem(internalCoordSys);
               
               // Let's try initialzing local objects using ObjectInitializer (2008.06.19)
               // We need to add subscribers to publisher, so pass true
               try
               {
                  if (!objInit->InitializeObjects(true))
                     // Should we throw an exception instead?
                     return false;
               }
               catch (BaseException &e)
               {
                  // We need to ignore exception thrown for the case Object is
                  // created after it is used, such as
                  // GMAT DefaultOpenGL.ViewPointReference = EarthSunL1;
                  // Create LibrationPoint EarthSunL1;
                  #ifdef DEBUG_FUNCTION_EXEC
                  MessageInterface::ShowMessage
                     ("objInit->InitializeObjects() threw an exception:\n'%s'\n"
                      "   So ignoring...\n", e.GetFullMessage().c_str());
                  #endif
               }
               
               #ifdef DEBUG_FUNCTION_EXEC
               MessageInterface::ShowMessage
                  ("============================ End   initialization of local objects\n");
               #endif
            }
         }
      }
      
      try
      {
         #ifdef DEBUG_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("Now calling <%p>[%s]->Execute()\n", current->GetTypeName().c_str(),
             current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
         #endif
         
         if (!(current->Execute()))
            return false;
      }
      catch (BaseException &e)
      {
         // If it is user interrupt, rethrow (loj: 2008.10.16)
         // How can we tell if it is thrown by Stop command?
         // For now just find the phrase "interrupted by Stop command"
         std::string msg = e.GetFullMessage();
         if (msg.find("interrupted by Stop command") != msg.npos)
            throw;
         
         // Let's try initialzing local objects here again (2008.10.14)
         // We need to add subscribers to publisher, so pass true               
         #ifdef DEBUG_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("current->Execute() threw an exception:\n'%s'\n   So re-initializing "
             "local objects\n", e.GetFullMessage().c_str());
         MessageInterface::ShowMessage
            ("\n============================ Begin initialization of local objects\n");
         #endif
         if (!objInit->InitializeObjects(true))
            return false;
         
         #ifdef DEBUG_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("============================ End   initialization of local objects\n");
         #endif
         
         #ifdef DEBUG_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("......Function re-executing <%p><%s> [%s]\n", current,
             current->GetTypeName().c_str(),
             current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
         #endif
         if (!(current->Execute()))
            return false;
      }
      
      // If current command is BranchCommand and still executing, continue to next
      // command in the branch (LOJ: 2009.03.24)
      if (current->IsOfType("BranchCommand") && current->IsExecuting())
      {
         #ifdef DEBUG_FUNCTION_EXEC
         MessageInterface::ShowMessage
            ("In Function '%s', still executing current command is <%p><%s>\n",
             functionName.c_str(), current, current ? current->GetTypeName().c_str() : "NULL");
         #endif
         
         continue;
      }
      
      current = current->GetNext();
      
      #ifdef DEBUG_FUNCTION_EXEC
      MessageInterface::ShowMessage
         ("In Function '%s', the next command is <%p><%s>\n", functionName.c_str(),
          current, current ? current->GetTypeName().c_str() : "NULL");
      #endif
   }
   
   // Set ObjectMap from the last command to Validator in order to create
   // valid output wrappers (loj: 2008.11.12)
   validator->SetObjectMap(last->GetObjectMap());
   
   #ifdef DEBUG_FUNCTION_EXEC
   MessageInterface::ShowMessage
      ("   Now about to create %d output wrapper(s) to set results, objectsInitialized=%d\n",
       outputNames.size(), objectsInitialized);
   #endif
   
   // create output wrappers and put into map
   GmatBase *obj;
   wrappersToDelete.clear();
   for (unsigned int jj = 0; jj < outputNames.size(); jj++)
   {
      if (!(obj = FindObject(outputNames.at(jj))))
      {
         std::string errMsg = "Function: Output \"" + outputNames.at(jj);
         errMsg += " not found for function \"" + functionName + "\"";
         throw FunctionException(errMsg);
      }
      std::string outName = outputNames.at(jj);
      ElementWrapper *outWrapper =
         validator->CreateElementWrapper(outName, false, false);
      #ifdef DEBUG_MORE_MEMORY
      MessageInterface::ShowMessage
         ("+++ GmatFunction::Execute() *outWrapper = validator->"
          "CreateElementWrapper(%s), <%p> '%s'\n", outName.c_str(), outWrapper,
          outWrapper->GetDescription().c_str());
      #endif
      
      outWrapper->SetRefObject(obj);
      
      // nested CallFunction crashes if old outWrappers are deleted here. (loj: 2008.11.24)
      // so collect here and delete when FunctionRunner completes.
      wrappersToDelete.push_back(outWrapper);         
      
      // Set new outWrapper
      outputArgMap[outName] = outWrapper;
      #ifdef DEBUG_FUNCTION_EXEC // --------------------------------------------------- debug ---
         MessageInterface::ShowMessage("GmatFunction: Output wrapper created for %s\n",
                                       (outputNames.at(jj)).c_str());
      #endif // -------------------------------------------------------------- end debug ---
   }
   
   #ifdef DEBUG_FUNCTION_EXEC
   MessageInterface::ShowMessage
      ("GmatFunction::Execute() exiting true for '%s'\n", functionName.c_str());
   #endif
   
   #ifdef DEBUG_PERFORMANCE
   clock_t t2 = clock();
   MessageInterface::ShowMessage
      ("=== GmatFunction::Execute() exiting, '%s' Count = %d, Run Time: %f seconds\n",
       functionName.c_str(), callCount, (Real)(t2-t1)/CLOCKS_PER_SEC);
   #endif
   
   return true; 
}


//------------------------------------------------------------------------------
// void Finalize()
//------------------------------------------------------------------------------
void GmatFunction::Finalize()
{
   #ifdef DEBUG_PERFORMANCE
   static Integer callCount = 0;
   callCount++;      
   clock_t t1 = clock();
   MessageInterface::ShowMessage
      ("=== GmatFunction::Finalize() entered, '%s' Count = %d\n",
       functionName.c_str(), callCount);
   #endif
   
   #ifdef DEBUG_FUNCTION_FINALIZE
   MessageInterface::ShowMessage
      ("======================================================================\n"
       "GmatFunction::Finalize() entered for '%s', FCS %sfinalized\n",
       functionName.c_str(), fcsFinalized ? "already " : "NOT ");
   #endif
   
   // Call RunComplete on each command in fcs
   if (!fcsFinalized)
   {
      fcsFinalized = true;
      GmatCommand *current = fcs;
      while (current)
      {
         #ifdef DEBUG_FUNCTION_FINALIZE
            if (!current)
               MessageInterface::ShowMessage
                  ("   GmatFunction:Finalize() Current is NULL!!!\n");
            else
               MessageInterface::ShowMessage("   GmatFunction:Finalize() Now "
                  "about to finalize (call RunComplete on) command %s\n",
                  (current->GetTypeName()).c_str());
         #endif
         current->RunComplete();
         current = current->GetNext();
      }
   }
   
   Function::Finalize();
   
   #ifdef DEBUG_PERFORMANCE
   clock_t t2 = clock();
   MessageInterface::ShowMessage
      ("=== GmatFunction::Finalize() exiting, '%s' Count = %d, Run Time: %f seconds\n",
       functionName.c_str(), callCount, (Real)(t2-t1)/CLOCKS_PER_SEC);
   #endif
   
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
         
         // Add to GmatFunction path
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

