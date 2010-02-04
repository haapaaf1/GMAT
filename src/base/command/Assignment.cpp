//$Id$
//------------------------------------------------------------------------------
//                                Assignment
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway
// Created: 2004/02/03
//
/**
 * Implementation for the Assignment command class.
 * 
 * The assignment class is the Command class that handles commands of the form
 * 
 *     GMAT object.parameter = value;
 *     GMAT sat.thruster1.ThrustDirection1 = value; (Setting to Parameter)
 *     GMAT variable = parameter;
 *     GMAT variable = equation;
 *
 */
//------------------------------------------------------------------------------


#include "Assignment.hpp"
#include "MathParser.hpp"
#include "StringUtil.hpp"
#include "TextParser.hpp"
#include "NumberWrapper.hpp"
#include "ArrayWrapper.hpp"
#include "FunctionManager.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_RENAME
//#define DEBUG_EVAL_RHS
//#define DEBUG_ASSIGNMENT_IA
//#define DEBUG_ASSIGNMENT_SET
//#define DEBUG_ASSIGNMENT_INIT
//#define DEBUG_ASSIGNMENT_EXEC
//#define DEBUG_EQUATION 1
//#define DEBUG_WRAPPER_CODE
//#define DEBUG_FUNCTION 1
//#define DEBUG_OBJECT_MAP
//#define DEBUG_ASSIGN_CALLING_FUNCTION

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

//------------------------------------------------------------------------------
//  Assignment()
//------------------------------------------------------------------------------
/**
 * Constructs the Assignment command (default constructor).
 */
//------------------------------------------------------------------------------
Assignment::Assignment  () :
   GmatCommand          ("GMAT"),
   lhs                  (""),
   rhs                  ("Not_Set"),
   lhsWrapper           (NULL),
   rhsWrapper           (NULL),
   mathTree             (NULL)
{
   objectTypeNames.push_back("GMAT");
   objectTypeNames.push_back("Assignment");
}


//------------------------------------------------------------------------------
//  ~Assignment()
//------------------------------------------------------------------------------
/**
 * Destroys the Assignment command (destructor).
 */
//------------------------------------------------------------------------------
Assignment::~Assignment()
{
   if (mathTree)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (mathTree, mathTree->GetName(), "Assignment::~Assignment()", "deleting mathTree");
      #endif
      delete mathTree;
   }
   
   mathTree = NULL;
   ClearWrappers();
}


//------------------------------------------------------------------------------
//  Assignment(const Assignment& a)
//------------------------------------------------------------------------------
/**
 * Constructs the Assignment command (copy constructor).
 *
 * @param <a> The instance that is copied.
 */
//------------------------------------------------------------------------------
Assignment::Assignment  (const Assignment& a) :
   GmatCommand          (a),
   lhs                  (a.lhs),
   rhs                  (a.rhs),
   lhsWrapper           (NULL),
   rhsWrapper           (NULL),
   mathTree             (a.mathTree)
{
}


//------------------------------------------------------------------------------
//  Assignment& operator=(const Assignment& a)
//------------------------------------------------------------------------------
/**
 * Assignment assignment operator.
 *
 * @param <a> The instance that is copied.
 *
 * @return This instance, set to match the input instance.
 */
//------------------------------------------------------------------------------
Assignment& Assignment::operator=(const Assignment& a)
{
   if (this == &a)
      return *this;
   
   lhs        = a.lhs;
   rhs        = a.rhs;
   lhsWrapper = NULL;
   rhsWrapper = NULL;
   mathTree   = a.mathTree;
   
   return *this;
}


//------------------------------------------------------------------------------
// MathTree* GetMathTree()
//------------------------------------------------------------------------------
MathTree* Assignment::GetMathTree()
{
   return mathTree;
}

//------------------------------------------------------------------------------
// bool HasAFunction()
//------------------------------------------------------------------------------
bool Assignment::HasAFunction()
{
   const StringArray fNames = GetGmatFunctionNames();
   if (fNames.empty()) return false;
   return true;
}

//------------------------------------------------------------------------------
// const StringArray& GetGmatFunctionNames()
//------------------------------------------------------------------------------
const StringArray& Assignment::GetGmatFunctionNames()
{
   #ifdef DEBUG_FUNCTION
   MessageInterface::ShowMessage
      ("Assignment::GetGmatFunctionNames() entered\n");
   #endif
   static StringArray functionNames;
   functionNames.clear();
   
   if (mathTree)
   {
      functionNames = mathTree->GetGmatFunctionNames();
      #ifdef DEBUG_FUNCTION
      MessageInterface::ShowMessage
         ("Assignment::GetGmatFunctionNames() returning %d GmatFunctions "
          " from the MathTree\n", functionNames.size());
      #endif
      return functionNames;
   }
   else
   {
      #ifdef DEBUG_FUNCTION
      MessageInterface::ShowMessage
         ("Assignment::GetGmatFunctionNames() returning 0 GmatFunctions, "
          "It's not a MathTree\n");
      #endif
      return functionNames;
   }
}


//------------------------------------------------------------------------------
// void SetMathWrappers()
//------------------------------------------------------------------------------
void Assignment::SetMathWrappers()
{
   // Set math Wrapper map
   if (mathTree)
      mathTree->SetMathWrappers(&mathWrapperMap);
}


//------------------------------------------------------------------------------
// virtual void SetFunction(Function *function)
//------------------------------------------------------------------------------
void Assignment::SetFunction(Function *function)
{
   #ifdef DEBUG_FUNCTION
   MessageInterface::ShowMessage
      ("Assignment::SetFunction() function=%p\n", function);
   #endif
   
   if (mathTree)
      mathTree->SetFunction(function);
   
   #ifdef DEBUG_FUNCTION
   MessageInterface::ShowMessage("Assignment::SetFunction() returning\n");
   #endif
}


//------------------------------------------------------------------------------
// std::vector<Function*> GetFunctions()
//------------------------------------------------------------------------------
std::vector<Function*> Assignment::GetFunctions() const
{
   if (mathTree)
      return mathTree->GetFunctions();
   
   std::vector<Function*> gf;
   return gf;
}


//------------------------------------------------------------------------------
// void SetPublisher(Publisher *pub)
//------------------------------------------------------------------------------
/**
 *  Passes the Publisher used by the Sandbox to FunctionManager through
 *  MathTree's FunctionRunner node.
 *
 *  @param <pub> The publisher
 */
//------------------------------------------------------------------------------
void Assignment::SetPublisher(Publisher *pub)
{
   GmatCommand::SetPublisher(pub);
   if (mathTree)
   {
      #ifdef DEBUG_PUBLISHER
      MessageInterface::ShowMessage
         ("Assignment::SetPublisher() setting publiser <%p> to MathTree\n");
      #endif
      mathTree->SetPublisher(pub);
   }
}


//------------------------------------------------------------------------------
//  void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the local solar system for the GmatCommand
 * 
 * @param <ss> Pointer to the local solar system
 */
//------------------------------------------------------------------------------
void Assignment::SetSolarSystem(SolarSystem *ss)
{
   GmatCommand::SetSolarSystem(ss);
   
   if (mathTree)
      mathTree->SetSolarSystem(ss);
}


//------------------------------------------------------------------------------
// virtual void SetInternalCoordSystem(CoordinateSystem *cs)
//------------------------------------------------------------------------------
/*
 * @see GmatCommand
 */
//------------------------------------------------------------------------------
void Assignment::SetInternalCoordSystem(CoordinateSystem *cs)
{
   if (cs == NULL)
      return;
   
   GmatCommand::SetInternalCoordSystem(cs);
   
   if (mathTree)
   {
      #ifdef DEBUG_ASSIGNMENT_SET
      MessageInterface::ShowMessage
         ("Assignment::SetInternalCoordSystem() this=<%p>'%s'\n   internalCS=<%p>, "
          "mathTree=<%p>\n", this, GetGeneratingString(Gmat::NO_COMMENTS).c_str(),
          internalCoordSys, mathTree);
      #endif
      mathTree->SetInternalCoordSystem(internalCoordSys);
   }
}


//------------------------------------------------------------------------------
//  void SetTransientForces(std::vector<PhysicalModel*> *tf)
//------------------------------------------------------------------------------
/**
 * Passes the transient force vector into the commands that need them
 * 
 * @param tf The vector of transient forces
 * 
 * @note The default behavior in the GmatCommands is to ignore the vector.
 */
//------------------------------------------------------------------------------
void Assignment::SetTransientForces(std::vector<PhysicalModel*> *tf)
{
   GmatCommand::SetTransientForces(tf);
   
   if (mathTree)
      mathTree->SetTransientForces(forces);
}


//------------------------------------------------------------------------------
//  void SetObjectMap(ObjectMap *map)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the local asset store used by the GmatCommand
 * 
 * @param <map> Pointer to the local object map
 */
//------------------------------------------------------------------------------
void Assignment::SetObjectMap(ObjectMap *map)
{
   GmatCommand::SetObjectMap(map);
   
   if (mathTree)
      mathTree->SetObjectMap(map);
}


//------------------------------------------------------------------------------
//  void SetGlobalObjectMap(ObjectMap *map)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the global asset store used by the GmatCommand
 * 
 * @param <map> Pointer to the global object map
 */
//------------------------------------------------------------------------------
void Assignment::SetGlobalObjectMap(ObjectMap *map)
{
   #ifdef DEBUG_GLOBAL_OBJECT_MAP
   MessageInterface::ShowMessage
      ("Assignment::SetGlobalObjectMap() entered, map=<%p>\n", map);
   #endif
   
   GmatCommand::SetGlobalObjectMap(map);
   
   if (mathTree)
      mathTree->SetGlobalObjectMap(map);
   
   #ifdef DEBUG_GLOBAL_OBJECT_MAP
   MessageInterface::ShowMessage("Assignment::SetGlobalObjectMap() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
/**
 * Parses the command string and builds the corresponding command structures.
 *
 * Assignment lines have the form
 *
 *    GMAT Sat.Element1 = 7654.321;
 *
 * or
 *
 *    GMAT object1 = object2;
 *
 * This method breaks this line into its constituent pieces.
 *
 * @return true on successful parsing of the command.
 *
 * @todo: Determine if Assignment::InterpretAction can be moved into the
 *        Interpreter
 */
//------------------------------------------------------------------------------
bool Assignment::InterpretAction()
{
   #ifdef DEBUG_ASSIGNMENT_IA
   MessageInterface::ShowMessage
      ("\nAssignment::InterpretAction() entered, currentFunction=<%p> '%s'\n",
       currentFunction,
       currentFunction ? currentFunction->GetFunctionPathAndName().c_str() : "NULL");
   #endif
   
   StringArray chunks = InterpretPreface();
   
   #ifdef DEBUG_ASSIGNMENT_IA
      MessageInterface::ShowMessage("Preface chunks as\n");
      for (StringArray::iterator i = chunks.begin(); i != chunks.end(); ++i)
         MessageInterface::ShowMessage("   \"%s\"\n", i->c_str());
   #endif
      
   lhs = chunks[0];
   rhs = chunks[1]; 
   
   // check if there is single quote in LHS(loj: 2008.07.22)
   if (lhs.find("'") != lhs.npos)
      throw CommandException("An assignment command is not allowed to contain "
                             "single quote on the left-hand-side"); 
   
   if (!GmatStringUtil::HasNoBrackets(lhs,true))
      throw CommandException("An assignment command is not allowed to contain "
         "brackets, braces, or parentheses (except to indicate an array element)"
         " on the left-hand-side");
   
   // check for unexpected commas on the left-hand-side
   Integer commaPos = -1;
   if (lhs.find(',') != lhs.npos)
   {
      GmatStringUtil::GetArrayCommaIndex(lhs, commaPos);
      if (commaPos == -1)
         throw CommandException("Command contains an unexpected comma on left-hand-side");
   }
   
   // check for single quotes in rhs and remove before process further (LOJ: 2009.10.09)
   // so that mp.IsEquation() will not think as transpose
   if (rhs.find("{") != rhs.npos && rhs.find("'") != rhs.npos)
   {
      // Single quote is allowed if it is paired, such as {'FuelTank1'}
      if (GmatStringUtil::StartsWith(rhs, "{") && GmatStringUtil::EndsWith(rhs, "}") ||
          GmatStringUtil::IsEnclosedWith(rhs, "'"))
      {
         rhs = GmatStringUtil::RemoveAll(rhs, '\'');
      }
   }
   
   // it there is still ; then report error since ; should have been removed
   if (rhs.find(";") != rhs.npos)
      throw CommandException("Is there a missing \"%\" for inline comment?");
   
   // check for common use of ./ (path) in GmatFunction to avoid creating MathTree(loj: 2008.09.08)
   if (rhs.find("./") != rhs.npos)
   {
      if (currentFunction != NULL &&
          (!GmatStringUtil::IsEnclosedWith(rhs, "'")))
         throw CommandException("The string literal \"" + rhs + "\" must be "
                                "enclosed with single quotes");
   }
   
   // Check if rhs is an equation
   MathParser mp = MathParser();
   if (mp.IsEquation(rhs))
   {
      // Parse RHS if equation
      #ifdef DEBUG_EQUATION
      MessageInterface::ShowMessage
         ("Assignment::InterpretAction() %s is an equation\n", rhs.c_str());
      #endif
      
      MathNode *topNode = mp.Parse(rhs);
      
      #ifdef DEBUG_EQUATION
      if (topNode)
         MessageInterface::ShowMessage
            ("   topNode=%s\n", topNode->GetTypeName().c_str());
      #endif
      
      // check if sting has missing start quote (loj: 2008.07.23)
      // it will be an error only if rhs with blank space removed matches with
      // any GmatFunction name without letter case
      std::string str1 = rhs;
      if (GmatStringUtil::EndsWith(str1, "'"))
      {
         #ifdef DEBUG_EQUATION
         MessageInterface::ShowMessage("   <%s> ends with '\n", str1.c_str());
         #endif
         
         str1 = GmatStringUtil::RemoveLastString(str1, "'");
         str1 = GmatStringUtil::RemoveAll(str1, ' ');
         StringArray gmatFnNames = mp.GetGmatFunctionNames();
         bool isError = false;
         for (UnsignedInt i=0; i<gmatFnNames.size(); i++)
         {
            if (GmatStringUtil::ToUpper(str1) == GmatStringUtil::ToUpper(gmatFnNames[i]))
            {
               isError = true;
               break;
            }
         }
         if (isError)
            throw CommandException("Found missing start quote on the right-hand-side"
                                   " of an Assignment command");
      }
      
      mathTree = new MathTree("MathTree", rhs);
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (mathTree, rhs, "Assignment::InterpretAction()", "mathTree = new MathTree()");
      #endif
      
      mathTree->SetTopNode(topNode);
      mathTree->SetGmatFunctionNames(mp.GetGmatFunctionNames());
   }
   else // if not an equation, check for unexpected commas on the right-hand-side
   {
      // Braces are allowed for lists of names, but brackets shouldn't be allowed
      // Assignment command should handle something like: 
      // "plot.ViewPointVector = [ 0 0 30000];", so commented out (loj: 2008.06.05)
      //if ((rhs.find('[') != rhs.npos) || (rhs.find(']') != rhs.npos))
      //   throw CommandException(
      //      "An assignment command is not allowed to contain brackets on the right-hand side"); 
      //if (!GmatStringUtil::AreAllBracketsBalanced(rhs, "({)}"))
      if (!GmatStringUtil::AreAllBracketsBalanced(rhs, "[({])}"))
         throw CommandException(
            "Parentheses or braces are unbalanced on the right-hand-side of an assignment command"); 
      
      // We want to allow the following scripts in the Assignment command.
      //    Create Formation Formation1;
      //    GMAT Formation1.Add = {Spacecraft1, Spacecraft2};
      // So commented out (loj: 2008.03.24)
      //if (rhs.find(',') != rhs.npos)
      //{
      //   GmatStringUtil::GetArrayCommaIndex(rhs, commaPos);
      //   if (commaPos == -1)
      //      throw CommandException("Command contains an unexpected comma on right-hand-side");
      //}
   }
   
   #ifdef DEBUG_ASSIGNMENT_IA
   MessageInterface::ShowMessage("Assignment::InterpretAction() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Performs the initialization needed to run the Assignment command.
 *
 * @return true if the GmatCommand is initialized, false if an error occurs.
 */
//------------------------------------------------------------------------------
bool Assignment::Initialize()
{
   #ifdef DEBUG_ASSIGNMENT_INIT
   MessageInterface::ShowMessage
      ("Assignment::Initialize() entered for <%s>, It's%s a math equation\n",
       generatingString.c_str(), (mathTree == NULL ? " not" : ""));
   MessageInterface::ShowMessage
      ("   lhsWrapper=<%p><%s>\n   rhsWrapper=<%p><%s>\n",
       lhsWrapper, lhsWrapper ? lhsWrapper->GetDescription().c_str() : "NULL",
       rhsWrapper, rhsWrapper ? rhsWrapper->GetDescription().c_str() : "NULL");
   #endif
   #ifdef DEBUG_OBJECT_MAP
   ShowObjectMaps();
   #endif
   
   if (lhsWrapper == NULL || (mathTree == NULL && rhsWrapper == NULL))
      return false;
   
   if (GmatCommand::Initialize() == false)
      return false;
   
   // if rhs is not an equation, set ref obj on both lhs and rhs wrappers
   if (mathTree == NULL)
   {      
      // Set references for the wrappers   
      if (SetWrapperReferences(*lhsWrapper) == false)
         return false;
      
      if (SetWrapperReferences(*rhsWrapper) == false)
         return false;
   }
   else
   {
      // Set references for the lhs wrapper
      if (SetWrapperReferences(*lhsWrapper) == false)
         return false;
      
      std::map<std::string, ElementWrapper *>::iterator ewi;
      
      #ifdef DEBUG_ASSIGNMENT_INIT
      for (ewi = mathWrapperMap.begin(); ewi != mathWrapperMap.end(); ++ewi)
         MessageInterface::ShowMessage
            ("   name=<%s>, wrapper=<%p>, type=%d\n", (ewi->first).c_str(), ewi->second,
             (ewi->second)->GetWrapperType());
      #endif
      
      // Set references for the rhs math element wrappers
      for (ewi = mathWrapperMap.begin(); ewi != mathWrapperMap.end(); ++ewi)
      {
         if (SetWrapperReferences(*(ewi->second)) == false)
            return false;
      }
      
      // Initialize mathTree
      MathNode *topNode = mathTree->GetTopNode();
      
      #ifdef DEBUG_ASSIGNMENT_INIT
      MessageInterface::ShowMessage
         ("Assignment::Initialize() Initializing topNode=%s, %s\n",
          topNode->GetTypeName().c_str(), topNode->GetName().c_str());
      #endif
      
      std::string fnMsg;
      if (currentFunction != NULL)
      {
         fnMsg = currentFunction->GetFunctionPathAndName();
         fnMsg = "\n(In Function \"" + fnMsg + "\")";
      }
      
      try
      {
         if (mathTree->Initialize(objectMap, globalObjectMap))
         {
            if (!topNode->ValidateInputs())
               throw CommandException("Failed to validate equation inputs in\n   \"" +
                                      generatingString + "\"" + fnMsg);
         }
         else
         {
            throw CommandException("Failed to initialize equation in\n   \"" +
                                   generatingString + "\"" + fnMsg);
         }
      }
      catch (BaseException &e)
      {
         CommandException ce;
         ce.SetDetails("%s in \n   \"%s\"%s\n", e.GetDetails().c_str(),
                      generatingString.c_str(), fnMsg.c_str());
         throw ce;
      }
   }
   
   #ifdef DEBUG_ASSIGNMENT_INIT
   MessageInterface::ShowMessage("Assignment::Initialize() returning true\n");
   #endif
   
   return true;
   
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
/**
 * The method that is fired to perform the command.
 *
 * Derived classes implement this method to perform their actions on 
 * GMAT objects.
 *
 * @return true if the GmatCommand runs to completion, false if an error 
 *         occurs. 
 */
//------------------------------------------------------------------------------
bool Assignment::Execute()
{
   #ifdef DEBUG_TRACE
   static Integer callCount = 0;
   callCount++;      
   clock_t t1 = clock();
   MessageInterface::ShowMessage
      ("=== Assignment::Execute() entered, '%s' Count = %d\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str(), callCount);
   #endif
   
   #ifdef DEBUG_ASSIGNMENT_EXEC
   MessageInterface::ShowMessage
      ("\nAssignment::Execute() this=<%p> entered, for \"%s\"\n   "
       "callingFunction='%s', internalCS=<%p>\n", this,
       GetGeneratingString(Gmat::NO_COMMENTS).c_str(),
       callingFunction? (callingFunction->GetFunctionName()).c_str() : "NULL",
       internalCoordSys);
   #endif
   
   if (lhsWrapper == NULL || (rhsWrapper == NULL && mathTree == NULL))
   {
      CommandException ce;
      ce.SetMessage("");
      std::string msg = "Assignment::Execute() failed, LHS or/and RHS wrappers are NULL";
      ce.SetDetails("%s in\n   \"%s\"\n", msg.c_str(), generatingString.c_str());
      throw ce;
   }
   
   ElementWrapper *outWrapper = NULL;
   
   try
   {
      bool retval = false;
      
      // In attempt to fix Func_LibrationSE.script for nested (loj: 2008.10.24)
      // function which doesn't give correct results.
      // We need to determine if ref object shoud be set to lhs to handle setting
      // spacecraft's CoordinateSystem which is not NULL and
      // CoordinateSystem is defined later in the GmatFunction script.
      // GMAT Sat1.CoordinateSystem = EarthSunL1_MJ2000Eq;
      // Since Spacecraft converts initial state to given CoordinateSyatem
      // when CoordinateSyatem is not NULL for nested GmatFunction,
      // we don't want to convert until initial state is set.
      // So pass setRefObj to true when this command is not in GmatFunction and
      // if both lhs and rhs are whole objects.
      
      // @todo This code broke routine test script APT_Func_allPropagates on 2nd run,
      // so set setRefObj to true for now
      
      //bool setRefObj = false;
      //if (currentFunction == NULL)
      //   setRefObj = true;
      //else
      //   if (lhsWrapper->GetWrapperType() == Gmat::OBJECT &&
      //       lhsWrapper->GetWrapperType() == rhsWrapper->GetWrapperType())
      //      setRefObj = true;      
      //MessageInterface::ShowMessage("===> setRefObj=%d\n", setRefObj);
      
      bool setRefObj = true;
      
      // Use ElementWrapper static method SetValue() (loj: 2008.06.20)
      if (mathTree == NULL)
      {
         retval = ElementWrapper::SetValue(lhsWrapper, rhsWrapper, solarSys, objectMap,
                                           globalObjectMap, setRefObj);

         // Check if setting spacecraft property
         if (lhsWrapper->GetWrapperType() == Gmat::OBJECT_PROPERTY_WT ||
             lhsWrapper->GetWrapperType() == Gmat::OBJECT_WT)
            HandleScPropertyChange(lhsWrapper);
      }
      else
      {
         #ifdef DEBUG_ASSIGNMENT_EXEC_OBJECT_MAP
         ShowObjectMaps("object maps at the start");
         #endif
         
         outWrapper = RunMathTree();
         retval = ElementWrapper::SetValue(lhsWrapper, outWrapper, solarSys, objectMap,
                                           globalObjectMap, setRefObj);
      }
      
      #ifdef DEBUG_ASSIGNMENT_EXEC
      MessageInterface::ShowMessage("   ElementWrapper::SetValue() returned %d\n", retval);
      #endif
      
      if (!retval)
      {
         if (outWrapper)
         {
            GmatBase *refObj = outWrapper->GetRefObject();
            if (refObj)
            {
               #ifdef DEBUG_MEMORY
               MemoryTracker::Instance()->Remove
                  (refObj, refObj->GetName(), "Assignment::Execute()",
                   //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting outWrapper's refObj");
                   " deleting outWrapper's refObj");
               #endif
               delete refObj;
            }
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (outWrapper, outWrapper->GetDescription(), "Assignment::Execute()",
                //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting outWrapper");
                " deleting outWrapper");
            #endif
            delete outWrapper;
         }
         
         throw CommandException("Assignment::Execute() failed");
      }
   }
   catch (BaseException &e)
   {
      // To make error message format consistent, just add "Command Exception:"
      std::string msg = e.GetFullMessage();
      if (msg.find("Exception") == msg.npos && msg.find("exception") == msg.npos)
         msg = "Command Exception: " + msg;
      
      if (callingFunction != NULL)
      {
         std::string funcPath = callingFunction->GetFunction()->GetFunctionPathAndName();
         msg = msg + "\n(In Function \"" + funcPath + "\")";
         MessageInterface::ShowMessage(msg + "\n");
         throw;
      }
      
      if (currentFunction != NULL)
      {
         MessageInterface::ShowMessage(msg + "\n");
      }
      else
      {
         if (outWrapper)
         {
            GmatBase *refObj = outWrapper->GetRefObject();
            if (refObj)
               delete refObj;
            delete outWrapper;
         }
         
         CommandException ce;
         ce.SetMessage("");
         ce.SetDetails("%s in\n   \"%s\"\n", msg.c_str(), generatingString.c_str());
         throw ce;
      }
   }
   
   #ifdef DEBUG_ASSIGNMENT_EXEC
   MessageInterface::ShowMessage("Assignment::Execute() returning true\n");
   #endif
   
   if (outWrapper)
   {
      GmatBase *refObj = outWrapper->GetRefObject();
      if (refObj)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (refObj, refObj->GetName(), "Assignment::Execute()",
             //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting outWrapper's refObj");
             " deleting outWrapper's refObj");
         #endif
         delete refObj;
         refObj = NULL;
      }
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (outWrapper, outWrapper->GetDescription(), "Assignment::Execute()",
          //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting outWrapper");
          " deleting outWrapper");
      #endif
      delete outWrapper;
      outWrapper = NULL;
   }
   
   #ifdef DEBUG_TRACE
   clock_t t2 = clock();
   MessageInterface::ShowMessage
      ("=== Assignment::Execute() exiting, '%s' Count = %d, Run Time: %f seconds\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str(), callCount,
       (Real)(t2-t1)/CLOCKS_PER_SEC);
   #endif
   return true;
}


//------------------------------------------------------------------------------
// void RunComplete()
//------------------------------------------------------------------------------
void Assignment::RunComplete()
{
   if (mathTree)
   {
      #ifdef DEBUG_RUN_COMPLETE
      MessageInterface::ShowMessage
         ("RunComplete::RunComplete() calling MathTree::Finalize()\n");
      #endif
      
      mathTree->Finalize();
   }
   
   GmatCommand::RunComplete();
}


//------------------------------------------------------------------------------
//  bool SkipInterrupt()
//------------------------------------------------------------------------------
/**
 * Allows select commands to avoid polling for user interrupts.
 * 
 * @return true if the command can skip polling; false if polling is needed.
 */
bool Assignment::SkipInterrupt()
{
   return true;
}

//------------------------------------------------------------------------------
// void SetCallingFunction();
//------------------------------------------------------------------------------
void Assignment::SetCallingFunction(FunctionManager *fm)
{
   #ifdef DEBUG_ASSIGN_CALLING_FUNCTION
      MessageInterface::ShowMessage("Assignment::SetCallingFunction - fm is %s NULL\n",
            fm? "NOT" : "really");
      MessageInterface::ShowMessage("   and mathTree DOES %s exist\n",
            mathTree? "really" : "NOT");
   #endif
   GmatCommand::SetCallingFunction(fm);
   if (mathTree)
      mathTree->SetCallingFunction(fm);
}


//------------------------------------------------------------------------------
// const StringArray& Vary::GetWrapperObjectNameArray()
//------------------------------------------------------------------------------
/*
 * Returns wrapper object names.
 */
//------------------------------------------------------------------------------
const StringArray& Assignment::GetWrapperObjectNameArray()
{
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("Assignment::GetWrapperObjectNameArray() lhs=<%s>, rhs=<%s>\n",
       lhs.c_str(), rhs.c_str());
   #endif
   
   wrapperObjectNames.clear();
   
   // If rhs is not an equation, just add rhs
   if (mathTree == NULL)
   {
      // If LHS has more than 1 dot add to the list and Interpreter::ValidateCommand()
      // will figure out if it is settable Parameter or not.(LOJ: 2009.12.22)
      if (GmatStringUtil::NumberOfOccurrences(lhs, '.') > 1)
         wrapperObjectNames.push_back(lhs);
      
      if (rhs != "")
      {
         wrapperObjectNames.push_back(rhs);
      }
   }
   else
   {
      // Add math node elements to wrapper object names
      StringArray tmpArray = mathTree->GetRefObjectNameArray(Gmat::PARAMETER);
      if (tmpArray.size() > 0)
         wrapperObjectNames.insert(wrapperObjectNames.end(),
                                   tmpArray.begin(), tmpArray.end());
      
      #ifdef DEBUG_WRAPPER_CODE
      MessageInterface::ShowMessage("   Got the following from the MathTree:\n");
      #endif
      for (UnsignedInt i=0; i<wrapperObjectNames.size(); i++)
      {
         mathWrapperMap[wrapperObjectNames[i]] = NULL;
         
         #ifdef DEBUG_WRAPPER_CODE
         MessageInterface::ShowMessage
            ("   Math element %d: '%s'\n", i, wrapperObjectNames[i].c_str());
         #endif
      }
   }
   
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("Assignment::GetWrapperObjectNameArray() returning %d wrapper elements\n",
       wrapperObjectNames.size());
   for (UnsignedInt i=0; i<wrapperObjectNames.size(); i++)
      MessageInterface::ShowMessage("   '%s'\n", wrapperObjectNames[i].c_str());
   #endif
   
   return wrapperObjectNames;
   
}


//------------------------------------------------------------------------------
// bool SetElementWrapper(ElementWrapper *toWrapper, const std::string &withName)
//------------------------------------------------------------------------------
bool Assignment::SetElementWrapper(ElementWrapper *toWrapper, 
                                   const std::string &withName)
{
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("Assignment::SetElementWrapper() toWrapper=<%p>, name='%s'\n   lhs='%s'\n   rhs='%s', "
       "mathTree=<%p>\n", toWrapper,withName.c_str(), lhs.c_str(), rhs.c_str(), mathTree);
   #endif
   
   if (toWrapper == NULL)
   {
      #ifdef DEBUG_WRAPPER_CODE
      MessageInterface::ShowMessage
         ("Assignment::SetElementWrapper() returning false, toWrapper is NULL\n");
      #endif
      return false;
   }
   
   bool retval = false;
   
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("   Setting wrapper \"%s\" of data type \"%d\" and of wrapper type \"%d\" "
       "on Assignment\n      \"%s\"\n", withName.c_str(), (Integer) (toWrapper->GetDataType()), 
       (Integer) (toWrapper->GetWrapperType()), GetGeneratingString(Gmat::NO_COMMENTS).c_str());
   #endif
   
   ElementWrapper *lhsOldWrapper = NULL;
   ElementWrapper *rhsOldWrapper = NULL;
   ElementWrapper *rhsNewWrapper = NULL;
   
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("   lhsWrapper=<%p>, rhsWrapper=<%p>\n", lhsWrapper, rhsWrapper);
   #endif
   
   if (withName == lhs)
   {
      // All lhs object property wrapper are settable, so check first
      if (withName.find(".") == withName.npos ||
          (withName.find(".") != withName.npos &&
           toWrapper->GetWrapperType() == Gmat::OBJECT_PROPERTY_WT))
      {
         // to handle Count = Count + 1, old lhsWrapper cannot be deleted here
         if (lhsWrapper != toWrapper)
         {
            lhsOldWrapper = lhsWrapper;
            lhsWrapper = toWrapper;
         }
         retval = true;
      }
      else if (withName.find(".") != withName.npos)
      {
         // Some lhs Parameters are settable such as Sat.Thruster1.FuelMass, so check here
         Parameter *param = (Parameter*)toWrapper->EvaluateObject();
         if (param && param->IsSettable())
         {
            if (lhsWrapper != toWrapper)
            {
               lhsOldWrapper = lhsWrapper;
               lhsWrapper = toWrapper;
            }
            retval = true;
         }
      }
   }
   
   if (mathTree == NULL)
   {
      if (withName == rhs)
      {
         if (rhsWrapper != toWrapper)
         {
            rhsOldWrapper = rhsWrapper;
            rhsNewWrapper = toWrapper;
            rhsWrapper = toWrapper;
         }
         retval = true;
      }
   }
   else
   {
      // if name found in the math wrapper map
      if (mathWrapperMap.find(withName) != mathWrapperMap.end())
      {
         // rhs should always be parameter wrapper, so check first
         if (withName.find(".") == withName.npos ||
             (withName.find(".") != withName.npos &&
              toWrapper->GetWrapperType() == Gmat::PARAMETER_WT))
         {
            if (mathWrapperMap[withName] != toWrapper)
            {
               rhsOldWrapper = mathWrapperMap[withName];
               rhsNewWrapper = toWrapper;
               mathWrapperMap[withName] = toWrapper;
            }
            retval = true;
         }
      }
   }
   
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("   lhsOldWrapper=<%p>, rhsOldWrapper=<%p>\n", lhsOldWrapper, rhsOldWrapper);
   MessageInterface::ShowMessage
      ("   lhsWrapper=<%p>, rhsWrapper=<%p>, rhsNewWrapper=<%p>\n",
       lhsWrapper, rhsWrapper, rhsNewWrapper);
   #endif
   
   // now delete old wrappers
   if (lhsOldWrapper == rhsOldWrapper && lhsOldWrapper != NULL)
   {
      // delete only lhs old wrapper
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (lhsOldWrapper, lhsOldWrapper->GetDescription(), "Assignment::SetElementWrapper()",
          //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting lhsOldWrapper");
          " deleting lhsOldWrapper");
      #endif
      delete lhsOldWrapper;
      lhsOldWrapper = NULL;
   }
   else
   {
      // check lhs wrapper
      if (lhsOldWrapper != NULL)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (lhsOldWrapper, lhsOldWrapper->GetDescription(), "Assignment::SetElementWrapper()",
             //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting lhsOldWrapper");
             " deleting lhsOldWrapper");
         #endif
         delete lhsOldWrapper;     
         lhsOldWrapper = NULL;
      }
      
      // check rhs wrapper
      if (rhsOldWrapper != NULL)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (rhsOldWrapper, rhsOldWrapper->GetDescription(), "Assignment::SetElementWrapper()",
             //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting rhsOldWrapper");
             " deleting rhsOldWrapper");
         #endif
         delete rhsOldWrapper;
         rhsOldWrapper = NULL;
      }
   }
   
   #ifdef DEBUG_WRAPPER_CODE
   MessageInterface::ShowMessage
      ("Assignment::SetElementWrapper() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// void ClearWrappers()
//------------------------------------------------------------------------------
void Assignment::ClearWrappers()
{
   ElementWrapper* lhsEw = lhsWrapper;
   ElementWrapper* rhsEw = rhsWrapper;
   
   if (rhsEw)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (rhsEw, rhsEw->GetDescription(), "Assignment::ClearWrappers()",
          //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting rhs wrapper");
          " deleting rhs wrapper");
      #endif
      delete rhsEw;
      rhsEw = NULL;
   }
   
   // clear rhs math wrapper map
   std::map<std::string, ElementWrapper *>::iterator ewi;
   for (ewi = mathWrapperMap.begin(); ewi != mathWrapperMap.end(); ++ewi)
   {
      if (ewi->second != NULL)
      {
         // if it is not the same as lhs wrapper, delete
         if (ewi->second != lhsEw)
         {
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (ewi->second, (ewi->second)->GetDescription(),
                "Assignment::ClearWrappers()",
                //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting math node wrapper");
                " deleting math node wrapper");
            #endif
            delete ewi->second;
            ewi->second = NULL;
         }
      }
   }
   
   if (lhsEw && lhsEw != rhsWrapper)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (lhsEw, lhsEw->GetDescription(), "Assignment::ClearWrappers()",
          //GetGeneratingString(Gmat::NO_COMMENTS) + " deleting lhs wrapper");
          " deleting lhs wrapper");
      #endif
      delete lhsEw;
      lhsEw = NULL;
   }
   
   lhsWrapper = NULL;
   rhsWrapper = NULL;
   
   mathWrapperMap.clear();
}


//------------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Renames referenced objects.
 *
 * @param <type> Type of the object that is renamed.
 * @param <oldName> The current name for the object.
 * @param <newName> The name the object has when this operation is complete.
 *
 * @return true on success.
 */
//------------------------------------------------------------------------------
bool Assignment::RenameRefObject(const Gmat::ObjectType type,
                                 const std::string &oldName,
                                 const std::string &newName)
{
   #ifdef DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Assignment::RenameRefObject() entered <%s>\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str());
   MessageInterface::ShowMessage
      ("   type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   // Go through lhs and rhs
   if (lhs.find(oldName) != lhs.npos)
      lhs = GmatStringUtil::ReplaceName(lhs, oldName, newName);
   
   if (rhs.find(oldName) != rhs.npos)
      rhs = GmatStringUtil::ReplaceName(rhs, oldName, newName);

   // Go through wrappers
   if (lhsWrapper)
      lhsWrapper->RenameObject(oldName, newName);
   
   if (rhsWrapper)
      rhsWrapper->RenameObject(oldName, newName);

   // Go through math tree
   if (mathTree)
      mathTree->RenameRefObject(type, oldName, newName);
   
   // Update generatingString
   GetGeneratingString();
   
   #ifdef DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Assignment::RenameRefObject() leaving <%s>\n",
       GetGeneratingString(Gmat::NO_COMMENTS).c_str());
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Assignment.
 *
 * @return clone of the Assignment.
 */
//------------------------------------------------------------------------------
GmatBase* Assignment::Clone() const
{
   return (new Assignment(*this));
}


//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id,
//                                            const Integer index) const
//------------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param id The integer ID for the parameter.
 * @param index The index when multiple StringArrays are supported.
 *
 * @return The requested StringArray.
 */
//------------------------------------------------------------------------------
const std::string& Assignment::GetGeneratingString(Gmat::WriteMode mode,
                                                   const std::string &prefix,
                                                   const std::string &useName)
{
   if (mode == Gmat::NO_COMMENTS)
   {
      generatingString = lhs + " = " + rhs + ";";
      return generatingString;
   }
   
   
   std::string gen = prefix + "GMAT " + lhs + " = " + rhs + ";";
   
   #ifdef DEBUG_ASSIGNMENT_SCRIPTING
   MessageInterface::ShowMessage("Assignment command generator is \"%s\"\n",
                                 gen.c_str());
   #endif
   
   generatingString = gen;
   
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}

//---------------------------------
// protected
//---------------------------------

//------------------------------------------------------------------------------
// ElementWrapper* RunMathTree()
//------------------------------------------------------------------------------
/*
 * Executes MathTree and creates output ElemenetWrapper.
 */
//------------------------------------------------------------------------------
ElementWrapper* Assignment::RunMathTree()
{
   if (mathTree == NULL)
      return NULL;
   
   Gmat::ParameterType lhsDataType = lhsWrapper->GetDataType();
   ElementWrapper *outWrapper = NULL;
   
   // Evalute math tree
   Integer returnType;
   Integer numRow;
   Integer numCol;
   
   MathNode *topNode = mathTree->GetTopNode();
   
   if (topNode)
   {
      #ifdef DEBUG_EQUATION
      MessageInterface::ShowMessage
         ("Assignment::RunMathTree() topNode=%s, %s\n", topNode->GetTypeName().c_str(),
          topNode->GetName().c_str());
      #endif
      
      topNode->GetOutputInfo(returnType, numRow, numCol);
      
      #ifdef DEBUG_ASSIGNMENT_EXEC
      MessageInterface::ShowMessage("   returnType=%d\n", returnType);
      #endif
      
      if (lhsDataType != returnType)
      {
         std::string lhsTypeStr = GmatBase::PARAM_TYPE_STRING[lhsDataType];
         CommandException ce;
         ce.SetDetails("Cannot set type \"%s\" to type \"%s\"",
                       GmatBase::PARAM_TYPE_STRING[returnType].c_str(),
                       lhsTypeStr.c_str());
         throw ce;
      }
      
      // @note We need to set description before setting the value to output wrapper
      
      switch (returnType)
      {
      case Gmat::REAL_TYPE:
         {
            #ifdef DEBUG_ASSIGNMENT_EXEC
            MessageInterface::ShowMessage("   Calling topNode->Evaluate()\n");
            #endif
            
            Real rval = -9999.9999;
            rval = topNode->Evaluate();
            
            #ifdef DEBUG_ASSIGNMENT_EXEC
            MessageInterface::ShowMessage("   Returned %f\n", rval);
            MessageInterface::ShowMessage("   Creating NumberWrapper for output\n");
            #endif
            
            outWrapper = new NumberWrapper();
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (outWrapper, GmatStringUtil::ToString(rval), "Assignment::RunMathTree()",
                "outWrapper = new NumberWrapper()");
            #endif
            
            outWrapper->SetDescription(GmatStringUtil::ToString(rval));
            outWrapper->SetReal(rval);
            break;
         }
      case Gmat::RMATRIX_TYPE:
         {
            #ifdef DEBUG_ASSIGNMENT_EXEC
            MessageInterface::ShowMessage("   Calling topNode->MatrixEvaluate()\n");
            #endif
            
            Rmatrix rmat;
            rmat.SetSize(numRow, numCol);
            rmat = topNode->MatrixEvaluate();
            // create Array, this array will be deleted when ArrayWrapper is deleted
            Array *outArray = new Array("ArrayOutput");
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (outArray, "outArray", "Assignment::RunMathTree()",
                "outArray = new Array()");
            #endif
            outArray->SetSize(numRow, numCol);
            outArray->SetRmatrix(rmat);
            
            #ifdef DEBUG_ASSIGNMENT_EXEC
            MessageInterface::ShowMessage("   Creating ArrayWrapper for output\n");
            #endif
            
            outWrapper = new ArrayWrapper();
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (outWrapper, "outWrapper", "Assignment::RunMathTree()",
                "outWrapper = new ArrayWrapper()");
            #endif
            outWrapper->SetDescription("ArrayOutput");
            outWrapper->SetRefObject(outArray);
            break;
         }
      default:
         CommandException ce;
         ce.SetDetails("Cannot set \"%s\" to \"%s\". The return type of "
                       "equation is unknown", rhs.c_str(), lhs.c_str());
         throw ce;
      }
   }
   else
   {
      throw CommandException("RHS is an equation, but top node is NULL\n");
   }
   
   return outWrapper;
   
}


//------------------------------------------------------------------------------
// void HandleScPropertyChange(ElementWrapper *lhsWrapper)
//------------------------------------------------------------------------------
void Assignment::HandleScPropertyChange(ElementWrapper *lhsWrapper)
{
   GmatBase *obj = lhsWrapper->GetRefObject();
   if (obj != NULL)
   {
      if (obj->IsOfType(Gmat::SPACECRAFT))
      {
         publisher->SetScPropertyChanged(this, obj->GetRealParameter("A1Epoch"),
                                         obj->GetName(), lhs + " = " + rhs);
      }
   }
}

