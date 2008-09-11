//$Id$
//------------------------------------------------------------------------------
//                                  Interpreter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/08/28
// Rework:  2006/09/27 by Linda Jun (NASA/GSFC)
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Class implementation for the Interpreter base class
 */
//------------------------------------------------------------------------------

#include "Interpreter.hpp"    // class's header file
#include "Moderator.hpp"
#include "StringTokenizer.hpp"
#include "ConditionalBranch.hpp"
#include "StringUtil.hpp"     // for ToReal()
#include "Array.hpp"
#include "Assignment.hpp"     // for GetLHS(), GetRHS()
#include "Validator.hpp"
#include "ElementWrapper.hpp"
#include "MessageInterface.hpp"
#include "FileUtil.hpp"       // for DoesFileExist()
#include "GmatGlobal.hpp"     // for GetMatlabFuncNameExt()
#include <stack>              // for checking matching begin/end control logic
#include <fstream>            // for checking GmatFunction declaration
#include <sstream>            // for checking GmatFunction declaration

//#define __DO_NOT_USE_OBJ_TYPE_NAME__

//#define DEBUG_HANDLE_ERROR
//#define DEBUG_INIT
//#define DEBUG_COMMAND_LIST
//#define DEBUG_OBJECT_LIST
//#define DEBUG_ARRAY_GET
//#define DEBUG_CREATE_OBJECT
//#define DEBUG_CREATE_PARAM
//#define DEBUG_CREATE_ARRAY
//#define DEBUG_CREATE_COMMAND
//#define DEBUG_MAKE_ASSIGNMENT
//#define DEBUG_ASSEMBLE_COMMAND
//#define DEBUG_ASSEMBLE_CREATE
//#define DEBUG_ASSEMBLE_FOR
//#define DEBUG_ASSEMBLE_CALL_FUNCTION
//#define DEBUG_ASSEMBLE_REPORT_COMMAND
//#define DEBUG_SET
//#define DEBUG_SET_FORCE_MODEL
//#define DEBUG_SET_SOLAR_SYS
//#define DEBUG_CHECK_OBJECT
//#define DEBUG_CHECK_BRANCH
//#define DEBUG_SPECIAL_CASE
//#define DEBUG_VALIDATE_COMMAND
//#define DEBUG_WRAPPERS
//#define DEBUG_PARSE_REPORT
//#define DEBUG_OBJECT_MAP
//#define DEBUG_FIND_OBJECT
//#define DEBUG_FIND_PROP_ID
//#define DEBUG_FUNCTION
//#define DEBUG_VAR_EXPRESSION
//#define DEBUG_MATH_TREE
//#define DBGLVL_FUNCTION_DEF 2
//#define DBGLVL_FINAL_PASS 1

//------------------------------------------------------------------------------
// Interpreter(SolarSystem *ss = NULL, ObjectMap *objMap = NULL)
//------------------------------------------------------------------------------
/**
 * Default constructor.
 *
 * @param  ss  The solar system to be used for findnig bodies
 * @param  objMap  The object map to be used for finding object 
 */
//------------------------------------------------------------------------------
Interpreter::Interpreter(SolarSystem *ss, ObjectMap *objMap)
{
   initialized = false;
   continueOnError = true;
   parsingDelayedBlock = false;
   ignoreError = false;
   inFunctionMode = false;
   hasFunctionDefinition = false;
   currentFunction = NULL;
   theSolarSystem = NULL;
   theObjectMap = NULL;
   
   theModerator  = Moderator::Instance();
   theReadWriter = ScriptReadWriter::Instance();
   theValidator = Validator::Instance();
   // Set Interpreter to singleton Validator
   theValidator->SetInterpreter(this);
   
   if (ss)
   {
      theSolarSystem = ss;
      theValidator->SetSolarSystem(ss);
   }
   
   if (objMap)
   {
      theObjectMap = objMap;
      theValidator->SetObjectMap(objMap);
      #ifdef DEBUG_OBJECT_MAP
      MessageInterface::ShowMessage
         ("Interpreter setting object map <%p> to Validator\n", theObjectMap);
      #endif
   }
   
   #ifdef DEBUG_INTERP
   MessageInterface::ShowMessage
      ("Interpreter::Interpreter() initialized=%d, theModerator=%p, theReadWriter=%p, "
       "theValidator=%p\n", initialized, theModerator, theReadWriter, theValidator);
   #endif
   
}


//------------------------------------------------------------------------------
// Interpreter()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
Interpreter::~Interpreter()
{
}


//------------------------------------------------------------------------------
// void Initialize()
//------------------------------------------------------------------------------
/**
 * Builds core lists of available objects.
 */
//------------------------------------------------------------------------------
void Interpreter::Initialize()
{
   #ifdef DEBUG_INIT
   MessageInterface::ShowMessage
      ("Interpreter::Initialize() initialized=%d\n", initialized);
   #endif
   
   errorList.clear();
   delayedBlocks.clear();
   delayedBlockLineNumbers.clear();
   inCommandMode = false;
   parsingDelayedBlock = false;
   ignoreError = false;
   
   if (initialized)
   {
      #ifdef DEBUG_INIT
      MessageInterface::ShowMessage
         ("Interpreter::Initialize() already initialized so just returning\n");
      #endif
      return;
   }
   
   BuildCreatableObjectMaps();
   
   // Register aliases used in scripting.  Plugins cannot use aliases, so this 
   // piece is performed outside of the creatable object map definitions.
   RegisterAliases();
   
   // Initialize TextParser command list
   theTextParser.Initialize(commandList);
   
   initialized = true;
}


//------------------------------------------------------------------------------
// void BuildCreatableObjectMaps()
//------------------------------------------------------------------------------
/**
 * Constructs the lists of object type names available in the Factories.  
 * 
 * This method is called whenever factories are registered with the 
 * FactoryManager.  During system startup, the Moderator makes this call after 
 * registering the default factories.  The call is reissued whenever a user
 * created factory is registered using the plug-in interfaces.
 */
//------------------------------------------------------------------------------
void Interpreter::BuildCreatableObjectMaps()
{
   // Build a mapping for all of the defined commands
   commandList.clear();
   StringArray cmds = theModerator->GetListOfFactoryItems(Gmat::COMMAND);
   copy(cmds.begin(), cmds.end(), back_inserter(commandList));
   
   #ifdef DEBUG_INIT
   MessageInterface::ShowMessage("Number of commands = %d\n", cmds.size());
   #endif
   
   #ifdef DEBUG_COMMAND_LIST
      std::vector<std::string>::iterator pos1;
      
      MessageInterface::ShowMessage("\nCommands:\n   ");      
      for (pos1 = cmds.begin(); pos1 != cmds.end(); ++pos1)
         MessageInterface::ShowMessage(*pos1 + "\n   ");
      
   #endif
      
   if (cmds.size() == 0)
   {
      throw InterpreterException("Command list is empty.");
   }
   
   // Build a mapping for all of the defined objects
   atmosphereList.clear();
   StringArray atms = theModerator->GetListOfFactoryItems(Gmat::ATMOSPHERE);
   copy(atms.begin(), atms.end(), back_inserter(atmosphereList));
   
   attitudeList.clear();
   StringArray atts = theModerator->GetListOfFactoryItems(Gmat::ATTITUDE);
   copy(atts.begin(), atts.end(), back_inserter(attitudeList));
   
   axisSystemList.clear();
   StringArray axes = theModerator->GetListOfFactoryItems(Gmat::AXIS_SYSTEM);
   copy(axes.begin(), axes.end(), back_inserter(axisSystemList));
   
   burnList.clear();
   StringArray burns = theModerator->GetListOfFactoryItems(Gmat::BURN);
   copy(burns.begin(), burns.end(), back_inserter(burnList));
   
   calculatedPointList.clear();
   StringArray cals = theModerator->GetListOfFactoryItems(Gmat::CALCULATED_POINT);
   copy(cals.begin(), cals.end(), back_inserter(calculatedPointList));
   
   functionList.clear();
   StringArray fns = theModerator->GetListOfFactoryItems(Gmat::FUNCTION);
   copy(fns.begin(), fns.end(), back_inserter(functionList));
   
   hardwareList.clear();
   StringArray hws = theModerator->GetListOfFactoryItems(Gmat::HARDWARE);
   copy(hws.begin(), hws.end(), back_inserter(hardwareList));
   
   parameterList.clear();
   StringArray parms = theModerator->GetListOfFactoryItems(Gmat::PARAMETER);
   copy(parms.begin(), parms.end(), back_inserter(parameterList));
   
   propagatorList.clear();
   StringArray props = theModerator->GetListOfFactoryItems(Gmat::PROPAGATOR);
   copy(props.begin(), props.end(), back_inserter(propagatorList));
   
   physicalModelList.clear();
   StringArray forces = theModerator->GetListOfFactoryItems(Gmat::PHYSICAL_MODEL);
   copy(forces.begin(), forces.end(), back_inserter(physicalModelList));
   
   solverList.clear();
   StringArray solvers = theModerator->GetListOfFactoryItems(Gmat::SOLVER);
   copy(solvers.begin(), solvers.end(), back_inserter(solverList));
   
   stopcondList.clear();
   StringArray stops = theModerator->GetListOfFactoryItems(Gmat::STOP_CONDITION);
   copy(stops.begin(), stops.end(), back_inserter(stopcondList));
   
   subscriberList.clear();
   StringArray subs = theModerator->GetListOfFactoryItems(Gmat::SUBSCRIBER);
   copy(subs.begin(), subs.end(), back_inserter(subscriberList));
   
   
   #ifdef DEBUG_OBJECT_LIST
      std::vector<std::string>::iterator pos;
      
      MessageInterface::ShowMessage("\nAtmosphereModel:\n   ");
      for (pos = atms.begin(); pos != atms.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\nAttitudes:\n   ");
      for (pos = atts.begin(); pos != atts.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\nAxisSystems:\n   ");
      for (pos = axes.begin(); pos != axes.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nBurns:\n   ");
      for (pos = burns.begin(); pos != burns.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nCalculatedPoints:\n   ");
      for (pos = cals.begin(); pos != cals.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nFunctions:\n   ");
      for (pos = fns.begin(); pos != fns.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nHardwares:\n   ");
      for (pos = hws.begin(); pos != hws.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nPhysicalModels:\n   ");
      for (pos = forces.begin(); pos != forces.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nParameters:\n   ");
      for (pos = parms.begin();  pos != parms.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");
      
      MessageInterface::ShowMessage("\nPropagators:\n   ");
      for (std::vector<std::string>::iterator pos = props.begin();
           pos != props.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\nSolvers:\n   ");
      for (pos = solvers.begin(); pos != solvers.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\nStopConds:\n   ");
      for (pos = stops.begin(); pos != stops.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\nSubscribers:\n   ");
      for (pos = subs.begin(); pos != subs.end(); ++pos)
         MessageInterface::ShowMessage(*pos + "\n   ");

      MessageInterface::ShowMessage("\n");
   #endif
   
}


//------------------------------------------------------------------------------
// StringArray GetCreatableList(Gmat::ObjectType type, Integer subType)
//------------------------------------------------------------------------------
/**
 * Returns the list of objects of a given type that can be built.
 * 
 * This method returns the list of object types supported by the current Factory 
 * system.  A future build will allow specification of a subtype -- for example,
 * for solvers, subtypes could be targeters, optimizers, iterators, and 
 * odSolvers.  The subType parameter is included to support this feature when it
 * becomes available.
 * 
 * @param type The Gmat::ObjectType requested.
 * @param subType The subtype.
 * 
 * @return The list of creatable objects.
 * 
 * @note The current implementation only supports the types in the Interpreter's
 *       lists of objects.  A future implementation should call 
 *       Moderator::GetListOfFactoryItems() instead. 
 */
//------------------------------------------------------------------------------
StringArray Interpreter::GetCreatableList(Gmat::ObjectType type, 
      Integer subType)
{
   StringArray clist;
   
   switch (type)
   {
      case Gmat::ATMOSPHERE:
         clist = atmosphereList;
         break;
         
      case Gmat::ATTITUDE:
         clist = attitudeList;
         break;
         
      case Gmat::AXIS_SYSTEM:
         clist = axisSystemList;
         break;
         
      case Gmat::BURN:
         clist = burnList;
         break;
         
      case Gmat::CALCULATED_POINT:
         clist = calculatedPointList;
         break;
         
      case Gmat::COMMAND:
         clist = commandList;
         break;
         
      case Gmat::FUNCTION:
         clist = functionList;
         break;
         
      case Gmat::HARDWARE:
         clist = hardwareList;
         break;
         
      case Gmat::PARAMETER:
         clist = parameterList;
         break;
         
      case Gmat::PROPAGATOR:
         clist = propagatorList;
         break;
         
      case Gmat::PHYSICAL_MODEL:
         clist = physicalModelList;
         break;
         
      case Gmat::SOLVER:
         clist = solverList;
         break;
         
      case Gmat::STOP_CONDITION:
         clist = stopcondList;
         break;
         
      case Gmat::SUBSCRIBER:
         clist = subscriberList;
         break;
         
      // These are all intentional fall-throughs:
      case Gmat::SPACECRAFT:
      case Gmat::FORMATION:
      case Gmat::SPACEOBJECT:
      case Gmat::GROUND_STATION:
      case Gmat::IMPULSIVE_BURN:
      case Gmat::FINITE_BURN:
      case Gmat::FORCE_MODEL:
      case Gmat::TRANSIENT_FORCE:
      case Gmat::INTERPOLATOR:
      case Gmat::SOLAR_SYSTEM:
      case Gmat::SPACE_POINT:
      case Gmat::CELESTIAL_BODY:
      case Gmat::LIBRATION_POINT:
      case Gmat::BARYCENTER:
      case Gmat::PROP_SETUP:
      case Gmat::FUEL_TANK:
      case Gmat::THRUSTER:
      case Gmat::COORDINATE_SYSTEM:
      case Gmat::MATH_NODE:
      case Gmat::MATH_TREE:
      case Gmat::UNKNOWN_OBJECT:
      default:
         break;
   }
   
   return clist;
}

//------------------------------------------------------------------------------
// void SetInputFocus()
//------------------------------------------------------------------------------
/*
 * Some GMAT UiInterpreters need to be able to obtain focus for message 
 * processing.  This method is overridden to perform run complete actions for 
 * those interpreters.
 */
//------------------------------------------------------------------------------
void Interpreter::SetInputFocus()
{}

//------------------------------------------------------------------------------
// void NotifyRunCompleted()
//------------------------------------------------------------------------------
/*
 * Some GMAT UiInterpreters need to know when a run is finished.  This method is
 * overridden to perform run complete actions for those interpreters.
 */
//------------------------------------------------------------------------------
void Interpreter::NotifyRunCompleted()
{} 

//------------------------------------------------------------------------------
// void NotifyRunCompleted(Integer type)
//------------------------------------------------------------------------------
/*
 * Some GMAT UiInterpreters need to update their view into the configured 
 * objects.  This method is overridden to perform those updates.  The parameter
 * maps to the following values:
 * 
 *  1   Configured objects
 *  2   Commands
 *  3   Commands and configured objects
 *  4   Outputs
 *  5   Outputs and configured objects
 *  6   Commands and Outputs
 *  7   Everything (Commands, outputs, configured objects)
 * 
 * The default value is 7. 
 */
//------------------------------------------------------------------------------
void Interpreter::UpdateView(Integer type)
{}


//------------------------------------------------------------------------------
// void SetInputFocus()
//------------------------------------------------------------------------------
/*
 * Some GMAT UiInterpreters need to take actions when a project is closed.  This
 * method tells them to take those actions.
 */
//------------------------------------------------------------------------------
void Interpreter::CloseCurrentProject()
{}

//------------------------------------------------------------------------------
// void StartServer()
//------------------------------------------------------------------------------
/*
 * Some GMAT Interpreters can start external servers -- for example, the MATLAB
 * server.  This method is overridden to perform that startup.
 */
//------------------------------------------------------------------------------
void Interpreter::StartServer()
{
   throw InterpreterException(
         "This Interpreter cannot start the external server");
}

//------------------------------------------------------------------------------
// void RegisterAliases()
//------------------------------------------------------------------------------
/*
 * Some GMAT script identifiers can be accessed using multiple text string.
 * This method creates a mapping for these strings so that scripts can be parsed
 * correctly.
 */
//------------------------------------------------------------------------------
void Interpreter::RegisterAliases()
{
   ForceModel::SetScriptAlias("PrimaryBodies", "GravityField");
   ForceModel::SetScriptAlias("Gravity", "GravityField");
   ForceModel::SetScriptAlias("PointMasses", "PointMassForce");
   ForceModel::SetScriptAlias("Drag", "DragForce");
   ForceModel::SetScriptAlias("SRP", "SolarRadiationPressure");
}


//------------------------------------------------------------------------------
// const StringArray& GetListOfObjects(Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Returns names of all configured items of object type.
 *
 * @param  type  object type
 *
 * @return array of configured item names; return empty array if none
 */
//------------------------------------------------------------------------------
const StringArray& Interpreter::GetListOfObjects(Gmat::ObjectType type)
{
   return theModerator->GetListOfObjects(type);
}


//------------------------------------------------------------------------------
// GmatBase* GetConfiguredObject(const std::string &name)
//------------------------------------------------------------------------------
GmatBase* Interpreter::GetConfiguredObject(const std::string &name)
{
   return theModerator->GetConfiguredObject(name);
}


//------------------------------------------------------------------------------
// GmatBase* CreateObject(const std::string &type, const std::string &name,
//                        Integer manage)
//------------------------------------------------------------------------------
/**
 * Calls the Moderator to build core objects and put them in the ConfigManager.
 *  
 * @param  type  Type for the requested object.
 * @param  name  Name for the object
 * @param  manage   0, if parameter is not managed
 *                  1, if parameter is added to configuration (default)
 *                  2, if Parameter is added to function object map
 *
 * @return object pointer on success, NULL on failure.
 */
//------------------------------------------------------------------------------
GmatBase* Interpreter::CreateObject(const std::string &type,
                                    const std::string &name,
                                    Integer manage)
{
   #ifdef DEBUG_CREATE_OBJECT
   MessageInterface::ShowMessage
      ("Interpreter::CreateObject() type=<%s>, name=<%s>, manage=%d\n",
       type.c_str(), name.c_str(), manage);
   #endif
   
   debugMsg = "In CreateObject()";
   GmatBase *obj = NULL;
   
   // if object to be managed and has non-blank name, and name is not valid, handle error
   if (manage == 1 && name != "")
   {
      bool isValid = false;
      
      // if type is Array, set flag to ignore bracket
      if (type == "Array")
         isValid = GmatStringUtil::IsValidName(name, true);
      else
         isValid = GmatStringUtil::IsValidName(name, false);
      
      if (!isValid)
      {
         InterpreterException ex
            (type + " object can not be named to \"" + name + "\"");
         HandleError(ex);
         return NULL;
      }
   }
   
   // Go through more checking if name is not blank
   if (name != "")
   {
      // object name cannot be any of command names
      if (IsCommandType(name))
      {
         InterpreterException ex
            (type + " object can not be named to Command \"" + name + "\"");
         HandleError(ex);
         return NULL;
      }
      
      #ifdef __DO_NOT_USE_OBJ_TYPE_NAME__
      // object name cannot be any of object types
      if (IsObjectType(name))
      {
         InterpreterException ex
            (type + " object can not be named to Object Type \"" + name + "\"");
         HandleError(ex);
         return NULL;
      }
      #endif
      
      // If object to be managed, give warning if name already exist
      if (manage == 1)
      {
         if ((name != "EarthMJ2000Eq") && 
             (name != "EarthMJ2000Ec") && 
             (name != "EarthFixed"))
         {
            obj = FindObject(name);
            // Since System Parameters are created automatically as they are referenced,
            // do not give warning if creating a system parameter
            if (obj != NULL && ((obj->GetType() != Gmat::PARAMETER) ||
                                (obj->GetType() == Gmat::PARAMETER &&
                                 (!obj->IsOfType("SystemParameter")))))
            {
               InterpreterException ex("");
               ex.SetDetails("%s object named \"%s\" already exist",
                             type.c_str(), name.c_str());
               HandleError(ex, true, true);
               return obj;
            }
         }
      }
   }
   
   
   if (type == "Spacecraft") 
      obj = (GmatBase*)theModerator->CreateSpacecraft(type, name);
   
   else if (type == "Formation") 
      obj = (GmatBase*)theModerator->CreateSpacecraft(type, name);
   
   else if (type == "PropSetup") 
      obj = (GmatBase*)theModerator->CreatePropSetup(name);
   
   else if (type == "ForceModel") 
      obj = (GmatBase*)theModerator->CreateForceModel(name, manage);
   
   else if (type == "CoordinateSystem") 
      obj = (GmatBase*)theModerator->CreateCoordinateSystem(name, true);
   
   else
   {
      // Handle Propagator
      if (find(propagatorList.begin(), propagatorList.end(), type) != 
          propagatorList.end())
         obj = (GmatBase*)theModerator->CreatePropagator(type, name);
      
      // Handle AxisSystem
      else if (find(axisSystemList.begin(), axisSystemList.end(), type) != 
               axisSystemList.end())
         obj =(GmatBase*) theModerator->CreateAxisSystem(type, name);
      
      // Handle Atmosphere Model
      else if (find(atmosphereList.begin(), atmosphereList.end(), type) != 
               atmosphereList.end())
         obj = (GmatBase*)theModerator->CreateAtmosphereModel(type, name);
      
      // Handle Attitude
      else if (find(attitudeList.begin(), attitudeList.end(), type) != 
               attitudeList.end())
         obj = (GmatBase*)theModerator->CreateAttitude(type, name);
      
      // Handle Burns
      else if (find(burnList.begin(), burnList.end(), type) != 
               burnList.end())
         obj = (GmatBase*)theModerator->CreateBurn(type, name);
      
      // Handle CalculatedPoint (Barycenter, LibrationPoint)
      // Creates default Barycentor or LibrationPoint
      else if (find(calculatedPointList.begin(), calculatedPointList.end(), type) != 
               calculatedPointList.end())
         obj =(GmatBase*) theModerator->CreateCalculatedPoint(type, name, true);
      
      // Handle Functions
      else if (find(functionList.begin(), functionList.end(), type) != 
               functionList.end())
         obj = (GmatBase*)theModerator->CreateFunction(type, name, manage);
      
      // Handle Hardware (tanks, thrusters, etc.)
      else if (find(hardwareList.begin(), hardwareList.end(), type) != 
               hardwareList.end())
         obj = (GmatBase*)theModerator->CreateHardware(type, name);
      
      // Handle Parameters
      else if (find(parameterList.begin(), parameterList.end(), type) != 
               parameterList.end())
         obj = (GmatBase*)CreateParameter(type, name, "", "");
      
      // Handle PhysicalModel
      else if (find(physicalModelList.begin(), physicalModelList.end(), type) != 
               physicalModelList.end())
         obj = (GmatBase*)theModerator->CreatePhysicalModel(type, name);
      
      // Handle Solvers
      else if (find(solverList.begin(), solverList.end(), type) != 
               solverList.end())
         obj = (GmatBase*)theModerator->CreateSolver(type, name);
      
      // Handle Subscribers
      else if (find(subscriberList.begin(), subscriberList.end(), type) != 
               subscriberList.end())
         obj = (GmatBase*)theModerator->CreateSubscriber(type, name);
   
   }
   
   //@note
   // Do throw exception if obj == NULL, since caller uses return pointer
   // to test further.
   
   
   #ifdef DEBUG_CREATE_OBJECT
   if (obj != NULL)
   {
      MessageInterface::ShowMessage
         ("Interpreter::CreateObject() type=<%s>, name=<%s> successfully created\n",
          obj->GetTypeName().c_str(), obj->GetName().c_str());
   }
   #endif
   
   return obj;
}


//------------------------------------------------------------------------------
// void SetSolarSystemInUse(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 * Sets a current solar system in use.
 *
 * @param <ss> Pointer to the solar system
 *
 */
//------------------------------------------------------------------------------
void Interpreter::SetSolarSystemInUse(SolarSystem *ss)
{
   #ifdef DEBUG_SET_SOLAR_SYS
   MessageInterface::ShowMessage
      ("Interpreter::SetSolarSystemInUse() ss=<%p>\n", ss);
   #endif
   
   if (ss != NULL)
   {
      theSolarSystem = ss;
      theValidator->SetSolarSystem(ss);
   }
}


//------------------------------------------------------------------------------
// SolarSystem* GetSolarSystemInUse()
//------------------------------------------------------------------------------
/**
 * Retrieves a current solar system in use.
 *
 * @return a default solar system object pointer
 */
//------------------------------------------------------------------------------
SolarSystem* Interpreter::GetSolarSystemInUse()
{
   return theSolarSystem;
}


//------------------------------------------------------------------------------
// void SetObjectMap(ObjectMap *objMap, bool forFunction)
//------------------------------------------------------------------------------
/**
 * Sets object map to be used for finding objects.
 * 
 * @param <objMap> Pointer to the object map
 * @param <forFunction> True if setting object map for function (false)
 */
//------------------------------------------------------------------------------
void Interpreter::SetObjectMap(ObjectMap *objMap, bool forFunction)
{
   #ifdef DEBUG_OBJECT_MAP
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectMap() objMap=<%p>, forFunction=%d\n", objMap,
       forFunction);
   #endif
   
   if (objMap != NULL)
   {
      if (forFunction)
      {
         #ifdef DEBUG_OBJECT_MAP
         MessageInterface::ShowMessage
            ("Interpreter::SetObjectMap() Here is the current object map <%p>, "
             "it has %d objects\n", objMap, objMap->size());
         for (std::map<std::string, GmatBase *>::iterator i = objMap->begin();
              i != objMap->end(); ++i)
         {
            MessageInterface::ShowMessage
               ("   %30s  <%p><%s>\n", i->first.c_str(), i->second,
                i->second == NULL ? "NULL" : (i->second)->GetTypeName().c_str());
         }
         #endif
      }
      
      theObjectMap = objMap;
      theValidator->SetObjectMap(objMap);
   }
}


//------------------------------------------------------------------------------
// ObjectMap* GetObjectMap()
//------------------------------------------------------------------------------
/**
 * @return a current object map in use.
 */
//------------------------------------------------------------------------------
ObjectMap* Interpreter::GetObjectMap()
{
   return theObjectMap;
}


//------------------------------------------------------------------------------
// void SetFunction(Function *func)
//------------------------------------------------------------------------------
/*
 * Sets Function pointer for function mode interpreting and to the Validator.
 *
 * @param  func  The Function pointer to set
 */
//------------------------------------------------------------------------------
void Interpreter::SetFunction(Function *func)
{
   #ifdef DEBUG_FUNCTION
   MessageInterface::ShowMessage
      ("Interpreter::SetFunction() function=<%p>'%s'\n", func,
       func ? func->GetName().c_str() : "NULL");
   #endif
   
   currentFunction = func;
   theValidator->SetFunction(func);
}


//------------------------------------------------------------------------------
// Function* GetFunction()
//------------------------------------------------------------------------------
/*
 * Retrives Function pointer currently set for function mode interpreting.
 */
Function* Interpreter::GetFunction()
{
   return currentFunction;
}


//------------------------------------------------------------------------------
// bool CheckUndefinedReference(GmatBase *obj, bool writeLine)
//------------------------------------------------------------------------------
/*
 * This method checks if reference object of given object exist through
 * the Validator.
 *
 * @param  obj  input object of undefined reference object to be checked
 * @param  writeLine  flag indicating whether or not line number should be written
 *                    for ther error message
 */
//------------------------------------------------------------------------------
bool Interpreter::CheckUndefinedReference(GmatBase *obj, bool writeLine)
{
   debugMsg = "In CheckUndefinedReference()";
   bool isValid = theValidator->CheckUndefinedReference(obj, continueOnError);
   
   // Handle error messages here
   if (!isValid)
   {
      StringArray errList = theValidator->GetErrorList();
      for (UnsignedInt i=0; i<errList.size(); i++)
         HandleError(InterpreterException(errList[i]), writeLine);
   }
   
   return isValid;
}


//------------------------------------------------------------------------------
// bool ValidateCommand(GmatCommand *cmd)
//------------------------------------------------------------------------------
/**
 * Checks the input command to make sure it wrappers are set up for it
 * correctly through the Validator, if necessary.
 *
 * @param  cmd  the command to validate
 */
//------------------------------------------------------------------------------
bool Interpreter::ValidateCommand(GmatCommand *cmd)
{
   #ifdef DEBUG_VALIDATE_COMMAND
   MessageInterface::ShowMessage
      ("Interpreter::ValidateCommand() cmd=<%p><%s>, inFunctionMode=%d\n", cmd,
       cmd->GetTypeName().c_str(), inFunctionMode);
   #endif
   
   debugMsg = "In ValidateCommand()";
   
   // Check if any Parameters need to be created
   StringArray names = cmd->GetWrapperObjectNameArray();
   
   #ifdef DEBUG_VALIDATE_COMMAND
   WriteStringArray("RefParameterNames for ", cmd->GetTypeName(), names);
   #endif
   
   // Create Parameters
   for (UnsignedInt i=0; i<names.size(); i++)
   {
      CreateSystemParameter(names[i]);
   }
   
   // If in function mode, just return true,
   // ValidateCommand() is called from GmatFunction::Initialize()
   if (inFunctionMode)
   {
      #ifdef DEBUG_VALIDATE_COMMAND
      MessageInterface::ShowMessage
         ("Interpreter::ValidateCommand() in function mode, so just returning true\n");
      #endif
      return true;
   }
   
   bool isValid = theValidator->ValidateCommand(cmd, continueOnError, 1);
   
   // Handle error messages here
   if (!isValid)
   {
      StringArray errList = theValidator->GetErrorList();
      for (UnsignedInt i=0; i<errList.size(); i++)
         HandleError(InterpreterException(errList[i]));
   }
   
   #ifdef DEBUG_VALIDATE_COMMAND
   MessageInterface::ShowMessage
      ("Interpreter::ValidateCommand() returning %d\n", isValid);
   #endif
   
   return isValid;
   
} // ValidateCommand()


//------------------------------------------------------------------------------
// bool ValidateSubscriber(GmatBase *obj)
//------------------------------------------------------------------------------
/**
 * Checks the input subscriber to make sure it wrappers are set up for it
 * correctly, if necessary.
 *
 * @param <obj> the subscriber to validate
 */
//------------------------------------------------------------------------------
bool Interpreter::ValidateSubscriber(GmatBase *obj)
{
   if (obj == NULL)
      throw InterpreterException("The subscriber object to be validated is NULL");
   
   // Now continue validation
   #ifdef DEBUG_WRAPPERS
   MessageInterface::ShowMessage
      ("Interpreter::ValidateSubscriber() obj=<%p><%s>\n", obj,
       obj->GetName().c_str());
   #endif
   
   debugMsg = "In ValidateSubscriber()";
   
   // This method can be called from other than Interpreter, so check if
   // object is SUBSCRIBER type
   if (obj->GetType() != Gmat::SUBSCRIBER)
   {
      InterpreterException ex
         ("ElementWrapper for \"" + obj->GetName() + "\" of type \"" +
          obj->GetTypeName() + "\" cannot be created.");
      HandleError(ex);
      return false;
   }
   
   Subscriber *sub = (Subscriber*)obj;
   sub->ClearWrappers();
   const StringArray wrapperNames = sub->GetWrapperObjectNameArray();
   
   #ifdef DEBUG_WRAPPERS
   MessageInterface::ShowMessage
      ("In ValidateSubscriber, has %d wrapper names:\n", wrapperNames.size());
   for (Integer ii=0; ii < (Integer) wrapperNames.size(); ii++)
      MessageInterface::ShowMessage("   %s\n", wrapperNames[ii].c_str());
   #endif
   
   for (StringArray::const_iterator i = wrapperNames.begin();
        i != wrapperNames.end(); ++i)
   {
      try
      {
         ElementWrapper *ew = theValidator->CreateElementWrapper(*i, true);
         
         if (sub->SetElementWrapper(ew, *i) == false)
         {
            InterpreterException ex
               ("ElementWrapper for \"" + (*i) +
                "\" cannot be created for the Subscriber \"" + obj->GetName() + "\"");
            HandleError(ex, false);
            return false;
         }
      }
      catch (BaseException &ex)
      {
         HandleError(ex);
         return false;
      }
   }
   
   return true;
   
} // ValidateSubscriber()


//---------------------------------
// protected
//---------------------------------

//------------------------------------------------------------------------------
// bool FindPropertyID(GmatBase *obj, const std::string &chunk, GmatBase **owner,
//                     Integer &id, Gmat::ParameterType &type)
//------------------------------------------------------------------------------
/*
 * Finds property ID for given property. If property not found in the obj,
 * it tries to find proerty from the owned objects.
 *
 * @param  obj    Object to find proerty
 * @param  chunk  String contains property
 * @param  owner  Address of new owner pointer to be returned
 * @param  id     Property ID to return (-1 if property not found)
 * @param  type   Property type to return
 *                (Gmat::UNKNOWN_ParameterType if property not found)
 *
 * @return true if property found
 *
 * For example, From "FM.Gravity.Earth.Model"
 *   obj is FM pointer, chunk is "Gravity.Earth.Model"
 */
//------------------------------------------------------------------------------
bool Interpreter::FindPropertyID(GmatBase *obj, const std::string &chunk,
                                 GmatBase **owner, Integer &id,
                                 Gmat::ParameterType &type)
{
   #ifdef DEBUG_FIND_PROP_ID
   MessageInterface::ShowMessage
      ("Interpreter::FindPropertyID() obj=<%p><%s>, chunk=<%s>\n", obj,
       (obj == NULL ? "NULL" : obj->GetName().c_str()), chunk.c_str());
   #endif
   
   if (obj == NULL)
      return false;
   
   bool retval = false;
   StringArray parts = theTextParser.SeparateDots(chunk);
   Integer count = parts.size();
   std::string prop = parts[count-1];
   
   #ifdef DEBUG_FIND_PROP_ID
   MessageInterface::ShowMessage("   property=<%s>\n", prop.c_str());
   #endif
   
   // Set initial output id and type
   id = -1;
   type = Gmat::UNKNOWN_PARAMETER_TYPE;
   
   try
   {
      id = obj->GetParameterID(prop);
      type = obj->GetParameterType(id);
      *owner = obj;
      retval = true;
   }
   catch (BaseException &e)
   {
      if (FindOwnedObject(obj, prop, owner, id, type))
         retval = true;
   }
   
   #ifdef DEBUG_FIND_PROP_ID
   MessageInterface::ShowMessage
      ("Interpreter::FindPropertyID() returning owner=<%p><%s><%s>, retval=%d\n",
       *owner, ((*owner) == NULL ? "NULL" : (*owner)->GetTypeName().c_str()),
       ((*owner) == NULL ? "NULL" : (*owner)->GetName().c_str()), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// GmatBase* FindObject(const std::string &name, const std::string &ofType = "")
//------------------------------------------------------------------------------
/**
 * Finds the object from the current object map.
 * (old method: Calls the Moderator to find a configured object.)
 *
 * @param  name    Name of the object.
 * @param  ofType  Type of object required; leave blank for no checking
 *
 * @return  object pointer found
 */
//------------------------------------------------------------------------------
GmatBase* Interpreter::FindObject(const std::string &name, 
                                  const std::string &ofType)
{
   return theValidator->FindObject(name, ofType);
}


//------------------------------------------------------------------------------
// bool IsCommandType(const std::string &type)
//------------------------------------------------------------------------------
/*
 * Returns true if input string is one of Command type that can be created.
 */
//------------------------------------------------------------------------------
bool Interpreter::IsCommandType(const std::string &type)
{
   if (find(commandList.begin(), commandList.end(), type) == commandList.end())
      return false;
   
   return true;
}


//------------------------------------------------------------------------------
// GmatCommand* CreateCommand(const std::string &type, const std::string &desc)
//------------------------------------------------------------------------------
GmatCommand* Interpreter::CreateCommand(const std::string &type,
                                        const std::string &desc, bool &retFlag,
                                        GmatCommand *inCmd)
{
   #ifdef DEBUG_CREATE_COMMAND
   MessageInterface::ShowMessage
      ("Interpreter::CreateCommand() type=<%s>, inCmd=<%p>, \n   desc=<%s>\n",
       type.c_str(), inCmd, desc.c_str());
   MessageInterface::ShowMessage
      ("   inFunctionMode=%d, hasFunctionDefinition=%d\n", inFunctionMode,
       hasFunctionDefinition);
   #endif
   
   GmatCommand *cmd = NULL;
   std::string desc1 = desc;
   bool commandFound = false;
   
   // handle blank type
   std::string type1 = type;
   if (type == "")
   {
      std::string::size_type index = desc.find("(");
      type1 = desc.substr(0, index);
   }
   
   if (IsCommandType(type1))
      commandFound = true;
   
   // Check for CallFunction
   if (type[0] == '[')
   {
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("Interpreter::CreateCommand() detecting [ and creating CallFunction ...\n");
      #endif
      cmd = AppendCommand("CallFunction", retFlag, inCmd);
      desc1 = type +  "=" + desc;
      cmd->SetGeneratingString(desc1);
   }
   /// @TODO: This is a work around for a call function
   /// without any return parameters.  It should be updated in
   /// the design to handle this situation.
   else if ((desc1.find("=") == desc1.npos) && (desc != "")
            && (!commandFound))
   {
      StringArray parts = theTextParser.SeparateSpaces(desc1);
      
      #ifdef DEBUG_CREATE_COMMAND
      WriteStringArray("Calling IsObjectType()", "", parts);
      #endif
      
      if (IsObjectType(parts[0]))
      {
         InterpreterException ex("Found invalid command \"" + type + "\"");
         HandleError(ex);
      }
      else if (!GmatStringUtil::IsValidName(type + desc, true))
      {
         InterpreterException ex
            ("Found invalid function name \"" + type + desc + "\"");
         HandleError(ex);
      }
      else
      {
         cmd = AppendCommand("CallFunction", retFlag, inCmd);
         desc1 = "[] =" + type + desc;
         cmd->SetGeneratingString(desc1);
      }
   }
   else
   {
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("   Now creatinig <%s> command and setting GenString to <%s>\n",
          type.c_str(), std::string(type + " " + desc).c_str());
      #endif
      cmd = AppendCommand(type, retFlag, inCmd);
      cmd->SetGeneratingString(type + " " + desc);
   }
   
   if (cmd == NULL)
   {
      retFlag = false;
      return NULL;
   }
      
   #ifdef DEBUG_CREATE_COMMAND
   if (inCmd == NULL)
      MessageInterface::ShowMessage
         ("   => '%s' created.\n", cmd->GetTypeName().c_str());
   else
      MessageInterface::ShowMessage
         ("   => '%s' created and appended to '%s'.\n",
          cmd->GetTypeName().c_str(), inCmd->GetTypeName().c_str());
   #endif
   
   
   // Now assemble command
   try
   {
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("   => Now calling %s->InterpretAction()\n", type.c_str());
      #endif
      
      // Set current function to command 
      cmd->SetCurrentFunction(currentFunction);
      
      // if command has its own InterpretAction(), just return cmd
      if (cmd->InterpretAction())
      {
         // if command is Assignment, check if GmatFunction needs to be created
         if (type == "GMAT" && ((Assignment*)cmd)->GetMathTree() != NULL)
            HandleMathTree(cmd);
         
         #ifdef DEBUG_CREATE_COMMAND
         MessageInterface::ShowMessage("   => Now calling ValidateCommand()\n");
         #endif
         retFlag  = ValidateCommand(cmd);
         
         #ifdef DEBUG_CREATE_COMMAND
         MessageInterface::ShowMessage
            ("   ===> %s has own InterpretAction() returning %p\n", type.c_str(), cmd);
         #endif
         
         return cmd;
      }
   }
   catch (BaseException &e)
   {
      HandleError(e);
      retFlag = false;
      
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("CreateCommand() leaving creating %s, cmd=<%p>, retFlag=%d\n", type.c_str(),
          cmd, retFlag);
      #endif
      
      // Return cmd since command already created
      return cmd;
   }
   
   if (desc1 != "")
   {
      bool retval3 = true;
      bool retval1  = AssembleCommand(cmd, desc1);
      
      if (retval1)
         retval3 = ValidateCommand(cmd);
      
      retFlag = retval1 && retval3;
      
   }
   
   #ifdef DEBUG_CREATE_COMMAND
   MessageInterface::ShowMessage
      ("CreateCommand() leaving creating %s, cmd=<%p>, retFlag=%d\n", type.c_str(),
       cmd, retFlag);
   #endif
   
   return cmd;;
}


//------------------------------------------------------------------------------
//GmatCommand* AppendCommand(const std::string &type, bool &retFlag,
//                           GmatCommand *inCmd)
//------------------------------------------------------------------------------
GmatCommand* Interpreter::AppendCommand(const std::string &type, bool &retFlag,
                                        GmatCommand *inCmd)
{
   #ifdef DEBUG_CREATE_COMMAND
   MessageInterface::ShowMessage
      ("AppendCommand() type=<%s>, inCmd=<%p>\n", type.c_str(), inCmd);
   #endif
   
   GmatCommand *cmd = NULL;
   
   if (inCmd == NULL)
   {
      cmd = theModerator->AppendCommand(type, "", retFlag);
      
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("===> Appending command <%s> to the last command\n",
          cmd->GetTypeName().c_str());
      #endif
   }
   else
   {
      cmd = theModerator->CreateCommand(type, "", retFlag);
      inCmd->Append(cmd);
      
      #ifdef DEBUG_CREATE_COMMAND
      MessageInterface::ShowMessage
         ("===> Appending command <%s> to <%s>\n", cmd->GetTypeName().c_str(),
          inCmd->GetTypeName().c_str());
      #endif
   }
   
   #ifdef DEBUG_CREATE_COMMAND
   MessageInterface::ShowMessage("AppendCommand() returning <%p>\n", cmd);
   #endif
   
   return cmd;
}


//------------------------------------------------------------------------------
//bool AssembleCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleCommand(GmatCommand *cmd, const std::string &desc)
{
   bool retval = false;
   std::string type = cmd->GetTypeName();
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   MessageInterface::ShowMessage
      ("Interpreter::AssembleCommand() cmd='%s'\n   desc=<%s>\n",
       type.c_str(), desc.c_str());
   #endif
   
   if (type == "For")
      retval = AssembleForCommand(cmd, desc);
   else if (type == "CallFunction")
      retval = AssembleCallFunctionCommand(cmd, desc);
   else if (cmd->IsOfType("ConditionalBranch"))
      retval = AssembleConditionalCommand(cmd, desc);
   else
      retval = AssembleGeneralCommand(cmd, desc);
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   MessageInterface::ShowMessage
      ("AssembleCommand() leaving assembling %s, retval=%d\n",
       type.c_str(), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
//bool AssembleCallFunctionCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleCallFunctionCommand(GmatCommand *cmd,
                                              const std::string &desc)
{
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage
      ("Interpreter::AssembleCallFunctionCommand() cmd='%s'\n   desc=<%s>\n",
       cmd->GetTypeName().c_str(), desc.c_str());
   #endif
   
   debugMsg = "In AssembleCallFunctionCommand()";
   bool retval = true;
   
   // Output
   std::string::size_type index1 = 0;
   std::string lhs;
   StringArray outArray;
   
   // get output arguments if there was an equal sign
   if (GmatStringUtil::IsThereEqualSign(desc))
   {
      index1 = desc.find("=");
      lhs = desc.substr(0, index1);
      outArray = theTextParser.SeparateBrackets(lhs, "[]", " ,", true);
      index1 = index1 + 1;
   }
   
   // Function Name, Input
   StringArray inArray;
   std::string funcName;
   std::string::size_type index2 = desc.find("(", index1);
   
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage
      ("   Starting index=%u, open parenthesis index=%u\n", index1, index2);
   #endif
   
   if (index2 == desc.npos)
   {      
      funcName = desc.substr(index1);
   }
   else
   {
      funcName = desc.substr(index1, index2-index1);
      std::string rhs = desc.substr(index2);
      rhs = GmatStringUtil::RemoveOuterString(rhs, "(", ")");
      
      #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
      MessageInterface::ShowMessage("   rhs=\"%s\"\n", rhs.c_str());
      #endif
      
      // check if single quote found
      inArray = GmatStringUtil::SeparateByComma(rhs);
      
      #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
      MessageInterface::ShowMessage("   inArray.size()=%d\n", inArray.size());
      #endif
   }
   
   funcName = GmatStringUtil::Trim(funcName);
   
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage
      ("   Checking function name '%s'\n", funcName.c_str());
   #endif
   
   // Check for blank name
   if (funcName == "")
   {
      InterpreterException ex("Found blank function name");
      HandleError(ex);
      return false;
   }
   
   // Check for valid name
   if (!GmatStringUtil::IsValidName(funcName))
   {
      InterpreterException ex("Found invalid function name \"" + funcName + "\"");
      HandleError(ex);
      return false;
   }
   
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage("   Setting funcName '%s'\n", funcName.c_str());
   #endif
   
   // Special case for MatlabFunction
   // If in functin mode and function name is found from tempObjectNames,
   // add an extension
   std::string newFuncName = funcName;
   
   if (inFunctionMode)
   {
      if (find(tempObjectNames.begin(), tempObjectNames.end(), funcName) !=
          tempObjectNames.end())
      {
         GmatGlobal *global = GmatGlobal::Instance();
         newFuncName = funcName + global->GetMatlabFuncNameExt();
         
         #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
         MessageInterface::ShowMessage
            ("   '%s' found in tempObjectNames, so setting '%s' as function "
             "name\n", funcName.c_str(), newFuncName.c_str());
         #endif
      }
   }
   
   // Set function name to CallFunction
   retval = cmd->SetStringParameter("FunctionName", newFuncName);
   
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage("   Setting input\n");
   WriteStringArray("CallFunction Input", "", inArray);
   #endif
   
   // Set input to CallFunction
   bool validInput = false;
   Real rval;
   
   if (inArray.size() == 0) //if no inputs, set validInput to true
      validInput = true;
   
   for (UnsignedInt i=0; i<inArray.size(); i++)
   {            
      // If input is single item, set it to CallFunction, otherwise set "" (loj: 2008.08.22)
      // Should we do this here? Just hold off for now
      //==============================================================
      // The old way
      //==============================================================
      #if 0
      retval = cmd->SetStringParameter("AddInput", inArray[i]);
      #endif
      //==============================================================
      
      
      //==============================================================
      // The new way
      //==============================================================
      #if 1
      std::string input = inArray[i];
      if (GmatStringUtil::IsEnclosedWith(inArray[i], "'"))
         retval = cmd->SetStringParameter("AddInput", input);
      else
      {
         StringArray varNames = GmatStringUtil::GetVarNames(input);
         if (varNames.size() > 1)
            input = "";
         #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
         MessageInterface::ShowMessage
            ("   Setting <%s> as input to CallFunction\n", input.c_str());
         #endif      
         retval = cmd->SetStringParameter("AddInput", input);
      }
      #endif
      //==============================================================
      
      // if input parameter is a system Parameter then create
      validInput = false;      
      if (GmatStringUtil::IsEnclosedWith(inArray[i], "'")) // String literal
      {
         validInput = true;
      }
      else if (inArray[i].find('.') != std::string::npos)  // Parameter or Object property
      {
         if (IsParameterType(inArray[i]))
         {
            Parameter *param = CreateSystemParameter(inArray[i]);
            if (param != NULL)
               validInput = true;
         }
      }
      // Numbers will be allowed later
      else if (GmatStringUtil::ToReal(inArray[i], rval)) // Number
      {
         validInput = true;
      }
      else // whole object
      {
         GmatBase *obj = FindObject(inArray[i]);
         if (obj != NULL)
            validInput = true;
      }
      
      #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
      MessageInterface::ShowMessage
         ("   <%s> is %svalid input\n", inArray[i].c_str(), validInput ? "" : "not ");
      #endif
      
      // if in function mode, ignore invalid parameter
      if (inFunctionMode)
         validInput = true;
      
      // if not in function mode, throw exception if invalid parameter
      if (!validInput)
      {
         InterpreterException ex
            ("Nonexistent or disallowed CallFunction Input Parameter: \"" +
             inArray[i] +  "\"");
         HandleError(ex);
         return false;
      }
   }
   
   if (!retval || !validInput)
   {
      #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
      MessageInterface::ShowMessage
         ("Interpreter::AssembleCallFunctionCommand() returning false, "
          "retval=%d, validInput=%d\n", retval, validInput);
      #endif
      return false;
   }
   
   // Set output to CallFunction
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage("   Setting output\n");
   WriteStringArray("CallFunction Output", "", outArray);
   #endif
   
   for (UnsignedInt i=0; i<outArray.size(); i++)
      retval = cmd->SetStringParameter("AddOutput", outArray[i]);
   
   // if in function mode, just return retval
   if (inFunctionMode)
   {
      #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
      MessageInterface::ShowMessage
         ("Interpreter::AssembleCallFunctionCommand() returning %d, it's in "
          "function mode\n", retval);
      #endif
      return retval;
   }
   
   // See if Function is MatlabFunction since all MatlabFunctions are created
   // before mission sequence, if not, create as GmatFunction.
   // Changed to call FindObject()
   //GmatBase *func = GetConfiguredObject(funcName);
   GmatBase *func = FindObject(funcName);
   if (func == NULL)
      func = CreateObject("GmatFunction", funcName);
   
   // Set function pointer to CallFunction command
   cmd->SetRefObject(func, Gmat::FUNCTION, funcName);
   
   #ifdef DEBUG_ASSEMBLE_CALL_FUNCTION
   MessageInterface::ShowMessage
      ("Interpreter::AssembleCallFunctionCommand() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
//bool AssembleConditionalCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleConditionalCommand(GmatCommand *cmd,
                                             const std::string &desc)
{
   debugMsg = "In AssembleConditionalCommand()";
   bool retval = true;
   std::string type = cmd->GetTypeName();
   std::string opStr = "~<=>&|";
   
   // conditional commands, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(desc))
   {
      std::string msg = 
         "A conditional command is not allowed to contain brackets, braces, or "
         "parentheses (except to indicate an array element)";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   // This really becomes moot ...  wcs 2007.09.12
   // Remove enclosed parenthesis first
   Integer length = desc.size();
   std::string str1 = desc;
   if (desc[0] == '(' && desc[length-1] == ')')
   {
      str1 = desc.substr(1, length-2);
   }
   else
   {
      if (!GmatStringUtil::IsParenBalanced(desc))
      {
         InterpreterException ex("The Command has unbalanced parentheses");
         HandleError(ex);
         return false;
      }
   }
   
   std::string::size_type start = 0;
   std::string::size_type right = 0;
   std::string::size_type op = 0;
   bool done = false;
   StringArray parts;
   std::string str2;
   
   // Parse conditions
   while (!done)
   {
      op = str1.find_first_of(opStr, start);
      if (op == str1.npos)
      {
         // Add final right of operator, if not blank
         str2 = GmatStringUtil::Trim(str1.substr(start));
         if (str2 != "")
            parts.push_back(str2);
         break;
      }
      
      // Add left of operator
      str2 = GmatStringUtil::Trim(str1.substr(start, op-start));
      parts.push_back(str2);
      
      // Add operator
      right = str1.find_first_not_of(opStr, op);      
      str2 = GmatStringUtil::Trim(str1.substr(op, right-op));
      parts.push_back(str2);
      
      start = op + 1;
      op = str1.find_first_of(opStr, start);
      
      // check for double ops (such as: == ~= >= <=)
      if (op != str1.npos && op == start)
         start = op + 1;
   }
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   WriteStringArray("After parsing conditions()", "", parts);
   #endif
   
   Integer count = parts.size();
   for (Integer ii = 0; ii < count; ii++)
   {
      if (GmatStringUtil::IsBlank(parts.at(ii)))
      {
         InterpreterException ex("Missing field or operator in command");
         HandleError(ex);
         return false;
      }
      std::string strUpper = GmatStringUtil::ToUpper(parts.at(ii));         
      if (strUpper.find(" OR ") != strUpper.npos)
      {
         InterpreterException ex("\"OR\" is not a valid relational operator");
         HandleError(ex);
         return false;
      }
      if (strUpper.find(" AND ") != strUpper.npos)
      {
         InterpreterException ex("\"AND\" is not a valid relational operator");
         HandleError(ex);
         return false;
      }
   }
   
   // assuming there is no boolean argument
   if (count < 3 || ((count-3)%4) != 0)
   {
      InterpreterException ex("The Command has an invalid number of conditions");
      HandleError(ex);
      return false;
   }
   
   // Added try/catch block so that function name can be added to the error message
   try
   {
      ConditionalBranch *cb = (ConditionalBranch*)cmd;
      
      for (int i=0; i<count; i+=4)
      {
         #ifdef DEBUG_ASSEMBLE_COMMAND
         MessageInterface::ShowMessage
            ("   lhs:<%s>, op:<%s>, rhs:<%s>\n", parts[i].c_str(), parts[i+1].c_str(),
             parts[i+2].c_str());
         #endif
         
         // Try to create a parameter first if system parameter
         std::string type, ownerName, depObj;
         GmatStringUtil::ParseParameter(parts[i], type, ownerName, depObj);
         #ifdef DEBUG_ASSEMBLE_COMMAND // --------------------------------- debug ----
         MessageInterface::ShowMessage
            ("   lhs: type = %s, ownerName = %s, depObj = %s\n", 
             type.c_str(), ownerName.c_str(), depObj.c_str());
         #endif // ------------------------------------------------- end debug ----
         
         if (theModerator->IsParameter(type))
            CreateParameter(type, parts[i], ownerName, depObj);
         
         GmatStringUtil::ParseParameter(parts[i+2], type, ownerName, depObj);
         #ifdef DEBUG_ASSEMBLE_COMMAND // --------------------------------- debug ----
         MessageInterface::ShowMessage
            ("   rhs: type = %s, ownerName = %s, depObj = %s\n", 
             type.c_str(), ownerName.c_str(), depObj.c_str());
         #endif // ------------------------------------------------- end debug ----
         
         if (theModerator->IsParameter(type))
            CreateParameter(type, parts[i+2], ownerName, depObj);
         
         cb->SetCondition(parts[i], parts[i+1], parts[i+2]);
         
         if (count > i+3)
         {
            #ifdef DEBUG_ASSEMBLE_COMMAND
            MessageInterface::ShowMessage("   logOp=<%s>\n", parts[i+3].c_str());
            #endif
            
            cb->SetConditionOperator(parts[i+3]);
         }
      }
   }
   catch (BaseException &e)
   {
      InterpreterException ex(e.GetFullMessage());
      HandleError(ex);
      return false;
   }
   
   return retval;
}


//------------------------------------------------------------------------------
//bool AssembleForCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
/* Parses For loop control expression
 *    It's syntax is 
 *       For index = start:increment:end
 */
//------------------------------------------------------------------------------
bool Interpreter::AssembleForCommand(GmatCommand *cmd, const std::string &desc)
{
   #ifdef DEBUG_ASSEMBLE_FOR
   MessageInterface::ShowMessage
      ("Interpreter::AssembleForCommand() desc=<%s>\n", desc.c_str());
   #endif
   
   debugMsg = "In AssembleForCommand()";
   
   // For loop commands, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(desc))
   {
      std::string msg = 
         "A For command is not allowed to contain brackets, braces, or "
         "parentheses (except to indicate an array element)";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   bool retval = true;
   std::string::size_type equalSign = desc.find("=");
   
   if (equalSign == desc.npos)
   {
      InterpreterException ex("Cannot find equal sign (=) for For loop control");
      HandleError(ex);
      return false;
   }
   
   std::string index = desc.substr(0, equalSign);
   index = GmatStringUtil::Trim(index);
   
   std::string substr = desc.substr(equalSign+1);
   if (substr.find(':') == substr.npos)
   {
      InterpreterException ex("Missing colon (:) for For loop control");
      HandleError(ex);
      return false;
   }
   
   StringArray parts = theTextParser.SeparateBy(substr, ":");
   int count = parts.size();
   Integer numColons = 0;
   for (unsigned int ii = 0; ii < substr.size(); ii++)
      if (substr.at(ii) == ':') numColons++;
   if (numColons >= (Integer) count)
   {
      InterpreterException ex("Too many colons (:) for For loop control");
      HandleError(ex);
      return false;
   }
   #ifdef DEBUG_ASSEMBLE_FOR
   MessageInterface::ShowMessage
      ("Interpreter::AssembleForCommand() After SeparateBy, parts = \n");
   for (Integer ii=0;ii<count;ii++)
      MessageInterface::ShowMessage("   <%s>\n", parts[ii].c_str());
   #endif
   
   if (count < 2)
   {
      InterpreterException ex("Missing field, colon (:), or equal sign (=) for For loop control");
      HandleError(ex);
      return false;
   }
   
   std::string start = parts[0];
   std::string end = parts[1];
   std::string step = "1";
   
   if (count > 2)
   {
      step = parts[1];
      end = parts[2];
   }
   
   
   #ifdef DEBUG_ASSEMBLE_FOR
   MessageInterface::ShowMessage
      ("Interpreter::AssembleForCommand() index=<%s>, start=<%s>, end=<%s>, "
       "step=<%s>\n", index.c_str(), start.c_str(), end.c_str(), step.c_str());
   #endif
   
   cmd->SetStringParameter("IndexName", index);
   cmd->SetStringParameter("StartName", start);
   cmd->SetStringParameter("EndName", end);
   cmd->SetStringParameter("IncrementName", step);
   
   #ifdef DEBUG_ASSEMBLE_FOR
   MessageInterface::ShowMessage
      ("Interpreter::AssembleForCommand() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
//bool AssembleGeneralCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleGeneralCommand(GmatCommand *cmd,
                                         const std::string &desc)
{
   bool retval = true;
   std::string type = cmd->GetTypeName();
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   MessageInterface::ShowMessage
      ("AssembleGeneralCommand() cmd='%s', desc=<%s>\n", cmd->GetTypeName().c_str(),
       desc.c_str());
   #endif
   
   if (type == "Target" || type == "Report" || type == "BeginFiniteBurn" ||
       type == "EndFiniteBurn" || type == "Optimize")
   {
      // first item is ref. object name
      
      if (type == "Target")
         retval = AssembleTargetCommand(cmd, desc);
      else if (type == "Optimize")
         retval = AssembleOptimizeCommand(cmd, desc);
      else if (type == "Report")
         retval = AssembleReportCommand(cmd, desc);
      else
         retval = AssembleFiniteBurnCommand(cmd, desc);
   }
   else if (type == "Create")
      retval = AssembleCreateCommand(cmd, desc);
   else if (type == "Save" || type == "Global")
      retval = SetCommandRefObjects(cmd, desc);
   else
      retval = false;
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   MessageInterface::ShowMessage
      ("AssembleGeneralCommand() leaving assemblilng %s, retval=%d\n",
       type.c_str(), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool AssembleTargetCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleTargetCommand(GmatCommand *cmd, const std::string &desc)
{
   debugMsg = "In AssembleTargetCommand()";
   
   // This command, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(desc, false))
   {
      std::string msg = 
         "The Target command is not allowed to contain brackets, braces, or "
         "parentheses";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   bool retval = true;
   StringArray parts = theTextParser.Decompose(desc, "()");
   cmd->SetRefObjectName(Gmat::SOLVER, parts[0]);
   
   // Make sure there is only one thing on the line
   if (parts.size() > 1)
   {
      InterpreterException ex
         ("Unexpected text at end of Target command");
      HandleError(ex);
      retval = false;
   }
   
   // Check if the Solver exist if not in Function mode
   if (!inFunctionMode)
   {
      GmatBase *obj = FindObject(parts[0], "Solver");
      if (obj == NULL)
      {
         InterpreterException ex
            ("Cannot find the Solver \"" + parts[0] + "\"");
         HandleError(ex);
         retval = false;
      }
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// bool AssembleOptimizeCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleOptimizeCommand(GmatCommand *cmd, const std::string &desc)
{
   debugMsg = "In AssembleOptimizeCommand()";
   
   // This command, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(desc, false))
   {
      std::string msg = 
         "The Optimize command is not allowed to contain brackets, braces, or "
         "parentheses";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   bool retval = true;
   StringArray parts = theTextParser.Decompose(desc, "()");
   cmd->SetRefObjectName(Gmat::SOLVER, parts[0]);
   
   // Make sure there is only one thing on the line
   if (parts.size() > 1)
   {
      InterpreterException ex
         ("Unexpected text at end of Optimize command");
      HandleError(ex);
      retval = false;
   }
   
   // Check if the Solver exist if not in Function mode
   if (!inFunctionMode)
   {
      GmatBase *obj = FindObject(parts[0], "Solver");
      if (obj == NULL)
      {
         InterpreterException ex
            ("Cannot find the Solver \"" + parts[0] + "\"");
         HandleError(ex);
         retval = false;
      }
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// bool AssembleFiniteBurnCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleFiniteBurnCommand(GmatCommand *cmd, const std::string &desc)
{
   #ifdef DEBUG_ASSEMBLE_COMMAND
   MessageInterface::ShowMessage("Begin/EndFiniteBurn being processed ...\n");
   #endif
   
   bool retval = true;
   debugMsg = "In AssembleFiniteBurnCommand()";
   
   // Note:
   // Begin/EndFiniteBurn has the syntax: BeginFiniteBurn burn1(sat1 sat2)
   // First, check for errors in brackets
   if ((desc.find("[") != desc.npos) || (desc.find("]") != desc.npos))
   {
      InterpreterException ex
         ("Brackets not allowed in " + cmd->GetTypeName()+ " command");
      HandleError(ex);
      retval = false;
   }
   
   if (!GmatStringUtil::AreAllBracketsBalanced(desc, "({)}"))
   {
      InterpreterException ex
         ("Parentheses, braces, or brackets are unbalanced or incorrectly placed");
      HandleError(ex);
      retval = false;
   }
   
   // Get FiniteBurn name
   StringArray parts = theTextParser.Decompose(desc, "()", false);
   
   #ifdef DEBUG_ASSEMBLE_COMMAND
   std::string type = cmd->GetTypeName();
   WriteStringArray(type, "", parts);
   #endif
   
   if (parts.size() < 2)
   {
      InterpreterException ex
         ("Missing " + cmd->GetTypeName() + " parameter. Expecting "
          "\"FiniteBurnName(SpacecraftName)\"");
      HandleError(ex);
      retval = false;
   }
   else
   {
      cmd->SetRefObjectName(Gmat::FINITE_BURN, parts[0]);
      
      // Get Spacecraft names
      StringArray subParts = theTextParser.SeparateBrackets(parts[1], "()", ",");
      
      #ifdef DEBUG_ASSEMBLE_COMMAND
      WriteStringArray(type, "", subParts);
      #endif
      
      Integer count = subParts.size();
      if (count == 0)
      {
         InterpreterException ex
            (cmd->GetTypeName() + " command must contain at least one spacecraft name");
         HandleError(ex);
         retval = false;
      }
      Integer numCommas = GmatStringUtil::NumberOfOccurrences(parts[1],',');
      if (count != (numCommas + 1))
      {
         InterpreterException ex
            ("Missing spacecraft name in " + cmd->GetTypeName() + " command");
         HandleError(ex);
         retval = false;
      }
      for (int i=0; i<count; i++)
      {
         if (GmatStringUtil::IsBlank(subParts[i]))
         {
            InterpreterException ex
               ("Missing spacecraft name in " + cmd->GetTypeName() + " command");
            HandleError(ex);
            retval = false;
         }
         cmd->SetRefObjectName(Gmat::SPACECRAFT, subParts[i]);
      }
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// bool AssembleReportCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleReportCommand(GmatCommand *cmd, const std::string &desc)
{
   #ifdef DEBUG_ASSEMBLE_REPORT_COMMAND
   MessageInterface::ShowMessage
      ("AssembleReportCommand() cmd='%s', desc=<%s>\n", cmd->GetTypeName().c_str(),
       desc.c_str());
   #endif
   
   debugMsg = "In AssembleReportCommand()";
   bool retval = true;
   
   // This command, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(desc, true))
   {
      std::string msg = 
         "The Report command is not allowed to contain brackets, braces, or "
         "parentheses (except to indicate array elements)";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   // we only want to separate by spaces - commas are not allowed, 
   // not even in arrays (for this command)
   StringArray parts = GmatStringUtil::SeparateBy(desc, " ", true);
   Integer count = parts.size();
   
   #ifdef DEBUG_ASSEMBLE_REPORT_COMMAND 
   WriteStringArray("Parsing Report", "", parts);
   #endif
   
   // checking items to report
   if (count < 2)
   {
      InterpreterException ex ("There are no ReportFile or items to Report");
      HandleError(ex);
      return false;
   }
   
   // Set ReportFile name
   cmd->SetStringParameter("ReportFile", parts[0]);
   
   // Set reporting Parameter names
   for (int i=1; i<count; i++)
      cmd->SetStringParameter("Add", parts[i]);
   
   GmatBase *obj = NULL;
   
   // See if we can set ReportFile pointer
   // We can skip checking for configured object if in Function mode
   if (!inFunctionMode)
   {
      obj = FindObject(parts[0]);
      
      if (obj == NULL)
      {
         InterpreterException ex
            ("Cannot find the ReportFile \"" + parts[0] + "\"");
         HandleError(ex);
         return false;
      }
      
      // Set ReportFile pointer
      cmd->SetRefObject(obj, Gmat::SUBSCRIBER, parts[0], 0);
   }
   
   // Create Parameters to report
   for (int i=1; i<count; i++)
   {
      obj = (GmatBase*)CreateSystemParameter(parts[i]);
      
      if (!inFunctionMode)
      {
         if (obj != NULL)
         {
            cmd->SetRefObject(obj, Gmat::PARAMETER, parts[i], 0);
         }
         else
         {
            InterpreterException ex
               ("Nonexistent or disallowed Report Variable: \"" + parts[i] +
                "\";\nIt will not be added to Report");
            HandleError(ex);
            retval = false;
         }
      }
   }
   
   #ifdef DEBUG_ASSEMBLE_REPORT_COMMAND 
   MessageInterface::ShowMessage("AssembleReportCommand() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool AssembleCreateCommand(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::AssembleCreateCommand(GmatCommand *cmd, const std::string &desc)
{   
   #ifdef DEBUG_ASSEMBLE_CREATE
   MessageInterface::ShowMessage
      ("AssembleCreateCommand() Create command desc=<%s>\n", desc.c_str());
   #endif
   
   debugMsg = "In AssembleCreateCommand()";
   std::string::size_type typeIndex = desc.find_first_of(" ");
   std::string objTypeStr = desc.substr(0, typeIndex);
   std::string objNameStr = desc.substr(typeIndex+1);
   
   #ifdef DEBUG_ASSEMBLE_CREATE
   MessageInterface::ShowMessage("   Create object type=<%s>\n", objTypeStr.c_str());
   MessageInterface::ShowMessage("   Create object name=<%s>\n", objNameStr.c_str());
   #endif
   
   // check if object type is valid
   if (!IsObjectType(objTypeStr))
   {
      InterpreterException ex
         ("Unknown object type \"" + objTypeStr + "\" found in " +
          cmd->GetTypeName() + " command");
      HandleError(ex);
      return false;
   }
   
   //-----------------------------------------------------------------
   // check if comma is allowed in Create command (loj: 2008.08.29)
   //-----------------------------------------------------------------
   #ifdef __DISALLOW_COMMA_IN_CREATE__
   // check for comma, if comman is not allowed in Create command
   if (objNameStr.find(",") != objNameStr.npos)
   {
      InterpreterException ex
         ("Comma is not allowed in " + cmd->GetTypeName() + " command");
      HandleError(ex);
      return false;
   }
   StringArray objNames = GmatStringUtil::SeparateBy(objNameStr, " ", true);
   #else
   StringArray objNames = GmatStringUtil::SeparateBy(objNameStr, ", ", true);
   #endif
   
   
   #ifdef DEBUG_ASSEMBLE_CREATE
   WriteStringArray("Create object names", "", objNames);
   #endif
   
   if (objNames.size() == 0)
   {
      InterpreterException ex
         ("Missing object name found in " + cmd->GetTypeName() + " command");
      HandleError(ex);
      return false;
   }
   
   std::string objTypeStrToUse = objTypeStr;
   // Special case for Propagator
   if (objTypeStr == "Propagator")
      objTypeStrToUse = "PropSetup";
   
   try
   {
      // if object is MatlabFunction make sure we add .m extenstion to avoid
      // automatically creating GmatFunction in the Sandbox::HandleGmatFunction()
      cmd->SetStringParameter("ObjectType", objTypeStrToUse);
      for (UnsignedInt i=0; i<objNames.size(); i++)
         cmd->SetStringParameter("ObjectNames", objNames[i]);
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage(e.GetFullMessage());
      throw;
   }
   
   //-------------------------------------------------------------------
   // Create an unmanaged object and set to command
   // Note: Generally unnamed object will not be added to configuration,
   //       but we need name for Array for syntax checking, so pass name
   //       and set false to unmanage Array objects
   //-------------------------------------------------------------------
   std::string name;
   if (objTypeStrToUse == "Variable" || objTypeStrToUse == "Array")
      name = objNames[0];
   
   #ifdef DEBUG_ASSEMBLE_CREATE
      MessageInterface::ShowMessage
         ("   About to create reference object of '%s' for Create command\n",
          objTypeStrToUse.c_str());
   #endif
      
   GmatBase *obj = CreateObject(objTypeStrToUse, name, false);
   
   #ifdef DEBUG_ASSEMBLE_CREATE
   MessageInterface::ShowMessage("   %s created\n", obj->GetTypeName().c_str());
   #endif
   
   if (obj == NULL)
   {
      #ifdef DEBUG_ASSEMBLE_CREATE
         MessageInterface::ShowMessage("Reference object for Create command is NULL??\n");
      #endif
      return false;
   }
   
   // Send the object to the Create command
   //cmd->SetRefObject(obj, Gmat::UNKNOWN_OBJECT, obj->GetName());
   cmd->SetRefObject(obj, GmatBase::GetObjectType(objTypeStrToUse), obj->GetName());
   
   // Special case for MatlabFunction
   // Since CallFunction does not know whether the function is Gmat or Matlab function,
   // add an extention to indicate it is MatlabFunction so that Sandbox can create
   // proper functions. Add the name to tempObjectNames so that when creating
   // CallFunction or Assignment command, it can look in the array to figure out
   // whether it is MatlabFunction or not.
   if (objTypeStrToUse == "MatlabFunction")
   {
      for (UnsignedInt i=0; i<objNames.size(); i++)
         tempObjectNames.push_back(objNames[i]);
      
      #ifdef DEBUG_ASSEMBLE_CREATE
      MessageInterface::ShowMessage
         ("   tempObjectNames.size()=%d\n", tempObjectNames.size());
      #endif
   }
   
   #ifdef DEBUG_ASSEMBLE_CREATE
   MessageInterface::ShowMessage
      ("AssembleCreateCommand() returning true, created obj=<%p>, objType=<%s>, "
       "objName=<%s>\n", obj, obj->GetTypeName().c_str(), obj->GetName().c_str());
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// bool SetCommandRefObjects(GmatCommand *cmd, const std::string &desc)
//------------------------------------------------------------------------------
bool Interpreter::SetCommandRefObjects(GmatCommand *cmd, const std::string &desc)
{
   #ifdef DEBUG_ASSEMBLE_COMMAND   
   MessageInterface::ShowMessage
      ("Interpreter::SetCommandRefObjects() cmd=<%s>, desc=<%s>\n",
       cmd->GetTypeName().c_str(), desc.c_str());
   #endif

   debugMsg = "In SetCommandRefObjects()";
   
   // Save, Global commands, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces.
   // Since Create command can have "Create Array vec[3,1]", so do not check.
   if (!GmatStringUtil::HasNoBrackets(desc, false))
   {
      std::string msg = 
         "The " + cmd->GetTypeName() + " command is not allowed to contain "
         "brackets, braces, or parentheses";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   // we only want to separate by spaces - commas are not allowed, 
   // not even in arrays (for this command)
   StringArray parts = GmatStringUtil::SeparateBy(desc, " ", true);
   unsigned int numParts = parts.size();
   bool isOk = true;
   
   if (numParts == 0)
   {
      std::string msg = 
         "The " + cmd->GetTypeName() + " command has missing object names";
      InterpreterException ex(msg);
      HandleError(ex);
      return false;
   }
   
   #ifdef DEBUG_ASSEMBLE_COMMAND   
   WriteStringArray("object name parts", "", parts);
   #endif
   
   for (unsigned int i=0; i<numParts; i++)
   {
      if (parts[i].find(',') != parts[i].npos)
      {
         std::string msg = 
            "The " + cmd->GetTypeName() + " command is not allowed to contain commas - "
            "separate objects by spaces";
         InterpreterException ex(msg);
         HandleError(ex);
         isOk = false;
      }
      else if (!GmatStringUtil::IsValidName(parts[i]))
      {
         std::string msg = 
            "\"" + parts[i] + "\" is an invalid object name in " +
            cmd->GetTypeName() + " command";
         InterpreterException ex(msg);
         HandleError(ex);
         isOk = false;
      }
      else
      {
         cmd->SetStringParameter("ObjectNames", parts[i]);
      }
   }
   
   return isOk;
}


//------------------------------------------------------------------------------
//GmatCommand* CreateAssignmentCommand(const std::string &lhs,
//                                     const std::string &rhs, bool &retFlag,
//                                     GmatCommand *inCmd)
//------------------------------------------------------------------------------
GmatCommand* Interpreter::CreateAssignmentCommand(const std::string &lhs,
                                                  const std::string &rhs,
                                                  bool &retFlag, GmatCommand *inCmd)
{
   #ifdef DEBUG_CREATE_COMMAND
   MessageInterface::ShowMessage
      ("Interpreter::CreateAssignmentCommand() lhs=<%s>, rhs=<%s>\n", lhs.c_str(),
       rhs.c_str());
   #endif
   
   debugMsg = "In CreateAssignmentCommand()";
   
   // First check if it is really assignment by checking blank in the lhs.
   // (The lhs must be Variable, String, Array, or object property and this is
   //  validated in the Assignment command)
   std::string::size_type index = lhs.find_last_of(" ");
   if (index != lhs.npos)
   {
      std::string cmd = lhs.substr(0, index);
      
      // See if it is an Array since array index can have blanks
      index = lhs.find("(");
      if (index != lhs.npos)
      {
         if (!IsArrayElement(lhs))
         {
            InterpreterException ex("\"" + cmd + "\" is not a valid Command");
            HandleError(ex);
            return NULL;
         }
      }
   }
   
   std::string desc = lhs + " = " + rhs;
   return CreateCommand("GMAT", desc, retFlag, inCmd);
}


//------------------------------------------------------------------------------
// Parameter* CreateSystemParameter(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Creates a system Parameter from the input parameter name. If the name contains
 * dots, it consider it as a system parameter.  If it is not a system Parameter
 * it checks if object by given name is a Parameter.
 *
 * @param  name   parameter name to be parsed for Parameter creation
 *                Such as, sat1.Earth.ECC, sat1.SMA
 *
 * @return Created Paramteter pointer or pointer of the Parameter by given name
 *         NULL if it is not a system Parameter nor named object is not a Parameter
 *
 */
//------------------------------------------------------------------------------
Parameter* Interpreter::CreateSystemParameter(const std::string &str)
{
   #ifdef DEBUG_CREATE_PARAM
   MessageInterface::ShowMessage
      ("Interpreter::CreateSystemParameter() entered, str='%s', inFunctionMode=%d\n",
       str.c_str(), inFunctionMode);
   #endif
   
   Integer manage = 1;
   if (inFunctionMode)
      manage = 0;
   
   bool paramCreated = false;
   Parameter *param = theValidator->CreateSystemParameter(paramCreated, str, manage);
   
   #ifdef DEBUG_CREATE_PARAM
   MessageInterface::ShowMessage
      ("Interpreter::CreateSystemParameter() returning <%p><%s>'%s'\n", param,
       (param == NULL) ? "NULL" : param->GetTypeName().c_str(),
       (param == NULL) ? "NULL" : param->GetName().c_str());
   #endif
   
   return param;
}


//------------------------------------------------------------------------------
// Parameter* CreateParameter(const std::string &type, const std::string &name,
//                            const std::string &ownerName, const std::string &depName)
//------------------------------------------------------------------------------
/**
 * Calls the Moderator to create a Parameter.
 * 
 * @param  type       Type of parameter requested
 * @param  name       Name for the parameter.
 * @param  ownerName  object name of parameter requested ("")
 * @param  depName    Dependent object name of parameter requested ("")
 * 
 * @return Pointer to the constructed Parameter.
 */
//------------------------------------------------------------------------------
Parameter* Interpreter::CreateParameter(const std::string &type, 
                                        const std::string &name,
                                        const std::string &ownerName,
                                        const std::string &depName)
{
   #ifdef DEBUG_CREATE_PARAM
   MessageInterface::ShowMessage
      ("Interpreter::CreateParameter() type='%s', name='%s', ownerName='%s', "
       "depName='%s', inFunctionMode=%d\n", type.c_str(), name.c_str(),
       ownerName.c_str(), depName.c_str(), inFunctionMode);
   #endif
   
   return theValidator->CreateParameter(type, name, ownerName, depName, !inFunctionMode);
}


//------------------------------------------------------------------------------
// Parameter* GetArrayIndex(const std::string &arrayStr, Integer &row, Integer &col)
//------------------------------------------------------------------------------
/**
 * Retrives array index from the configured array.
 *
 * @param  arrayStr  String form of array (A(1,3), B(2,j), etc)
 *
 * @note Array name must be created and configured before acces.
 */
//------------------------------------------------------------------------------
Parameter* Interpreter::GetArrayIndex(const std::string &arrayStr,
                                      Integer &row, Integer &col)
{
   debugMsg = "In GetArrayIndex()";
   std::string name, rowStr, colStr;
   
   // parse array name and index
   GmatStringUtil::GetArrayIndex(arrayStr, rowStr, colStr, row, col, name);
   
   // Remove - sign from the name
   if (name[0] == '-')
      name = name.substr(1);
   
   #ifdef DEBUG_ARRAY_GET
   MessageInterface::ShowMessage
      ("Interpreter::GetArrayIndex() arrayStr=<%s>, name=<%s>, rowStr=<%s>, "
       "colStr=<%s>, row=%d, col=%d\n", arrayStr.c_str(), name.c_str(),
       rowStr.c_str(), colStr.c_str(), row, col);
   #endif
   
   Parameter *param = (Parameter*)FindObject(name);
   
   // Note:
   // To catch errors as much as possible, limited return statement used
   // even when error found
   
   if (param == NULL)
   {
      InterpreterException ex("Array named \"" + name + "\" is undefined");
      HandleError(ex);
   }
   else
   {
      if (param->GetTypeName() != "Array")
      {
         InterpreterException ex("\"" + name + "\" is not an Array");
         HandleError(ex);
         return NULL;
      }
      
      if (rowStr == "0" || colStr == "0" ||rowStr == "-1" || colStr == "-1")
      {
         InterpreterException ex("Index exceeds matrix dimensions");
         HandleError(ex);
         return NULL;
      }
      
      // get row value
      if (row == -1 && rowStr != "-1")
      {
         Parameter *rowParam = (Parameter*)FindObject(rowStr);
         if (rowParam == NULL)
         {
            InterpreterException ex
               ("Array row index named \"" + rowStr + "\" is undefined");
            HandleError(ex);
         }
         else
         {
            if (rowParam->GetReturnType() == Gmat::REAL_TYPE)
            {
               row = (Integer)rowParam->GetReal() - 1; // index starts at 0
            }
            else
            {
               InterpreterException ex
                  ("Cannot handle row index of Array named \"" + name + "\"");
               HandleError(ex);
            }
         }
      }
      
      // get column value
      if (col == -1 && colStr != "-1")
      {
         Parameter *colParam = (Parameter*)FindObject(colStr);
         if (colParam == NULL)
         {
            InterpreterException ex
               ("Column index named \"" + colStr + "\" is undefined");
            HandleError(ex);
         }
         else
         {
            if (colParam->GetReturnType() == Gmat::REAL_TYPE)
            {
               col = (Integer)colParam->GetReal() - 1; // index starts at 0
            }
            else
            {
               InterpreterException ex
                  ("Cannot handle column index of Array named \"" + name + "\"");
               HandleError(ex);
            }
         }
      }
   }
   
   #ifdef DEBUG_ARRAY_GET
   MessageInterface::ShowMessage
      ("   GetArrayIndex() row=%d, col=%d\n", row, col);
   #endif
   
   if (param == NULL || row == -1 || col == -1)
      return NULL;
   else
      return param;
}


//------------------------------------------------------------------------------
// GmatBase* MakeAssignment(const std::string &lhs, const std::string &rhs)
//------------------------------------------------------------------------------
/*
 * Sets rhs to lhs.
 *
 * @param  lhs  Left hand side component
 * @param  rhs  Right hand side component
 *
 * @return return LHS object pointer
 */
//------------------------------------------------------------------------------
GmatBase* Interpreter::MakeAssignment(const std::string &lhs, const std::string &rhs)
{
   #ifdef DEBUG_MAKE_ASSIGNMENT
   MessageInterface::ShowMessage
      ("Interpreter::MakeAssignment() lhs=<%s>, rhs=<%s>\n", lhs.c_str(), rhs.c_str());
   MessageInterface::ShowMessage
      ("   inFunctionMode=%d, hasFunctionDefinition=%d\n", inFunctionMode,
       hasFunctionDefinition);
   #endif

   debugMsg = "In MakeAssignment()";
   bool retval = false;
   
   // Separate dots
   StringArray lhsParts = theTextParser.SeparateDots(lhs);
   Integer lhsPartCount = lhsParts.size();
   StringArray rhsParts = theTextParser.SeparateDots(rhs);
   Integer rhsPartCount = rhsParts.size();
   std::string::size_type dot;
   std::string lhsObjName, rhsObjName;
   std::string lhsPropName, rhsPropName;
   GmatBase *lhsObj = NULL;
   GmatBase *rhsObj = NULL;
   bool isLhsObject = false;
   bool isRhsObject = false;
   bool isLhsArray = false;
   bool isRhsArray = false;
   currentBlock = lhs + " = " + rhs;
   
   #ifdef DEBUG_MAKE_ASSIGNMENT
   WriteStringArray("lhs parts", "", lhsParts);
   WriteStringArray("rhs parts", "", rhsParts);
   #endif
   
   // check LHS
   if (lhsPartCount > 1)
   {
      lhsObjName = lhsParts[0];
      lhsObj = FindObject(lhsObjName);
      
      if (lhsObj == NULL)
      {
         if (lhs == "")
         {
            InterpreterException ex("Object field assignment is imcomplelte");
            HandleError(ex);
         }
         else
         {
            InterpreterException ex
               ("Cannot find LHS object named \"" + lhsObjName + "\"");
            HandleError(ex);
         }
         return NULL;
      }
      
      dot = lhs.find('.');
      if (dot == lhs.npos)
         lhsPropName = lhsParts[1];
      else
         lhsPropName = lhs.substr(dot+1);
   }
   else
   {
      lhsObj = FindObject(lhs);
      
      if (lhsObj)
      {
         if (IsArrayElement(lhs))
            isLhsArray = true;
         else
            isLhsObject = true;
      }
      else
      {
         if (lhs == "")
         {
            InterpreterException ex("Missing equal sign in object field assignment");
            HandleError(ex);
         }
         else
         {
            InterpreterException ex("Cannot find LHS object named \"" + lhs + "\"");
            HandleError(ex);
         }
         return NULL;
      }
   }
   
   #ifdef DEBUG_MAKE_ASSIGNMENT
   MessageInterface::ShowMessage
      ("   isLhsObject=%d, isLhsArray=%d, lhsPropName=<%s>, lhsObj=<%p><%s>\n",
       isLhsObject, isLhsArray, lhsPropName.c_str(), lhsObj,
       (lhsObj == NULL) ? "NULL" : lhsObj->GetName().c_str() );
   #endif
   
   // check RHS
   if (rhsPartCount > 1)
   {
      rhsObjName = rhsParts[0];
      std::string objTypeStr = "";
      // Check if RHS has open paren, then it should be an Array (loj: 2008.08.15)
      if (rhsObjName.find_first_of("(") != rhsObjName.npos)
         objTypeStr = "Array";
      rhsObj = FindObject(rhsObjName, objTypeStr);
      
      if (rhsObj == NULL)
      {
         //throw InterpreterException("Cannot find RHS object: " + rhsObjName + "\n");
         
         #ifdef DEBUG_MAKE_ASSIGNMENT
         MessageInterface::ShowMessage
            ("   Cannot find RHS object '%s' of type <%s>. It may be a string value\n",
             rhsObjName.c_str(), objTypeStr.c_str());
         #endif
      }
      else
      {
         // Note: Do not set rhsObj to true here since it needs to create
         // a Parameter if needed.
         
         #ifdef DEBUG_MAKE_ASSIGNMENT
         MessageInterface::ShowMessage
            ("   Found rhs object <%s>'%s', now checking for dot\n",
             rhsObj->GetTypeName().c_str(), rhsObj->GetName().c_str());
         #endif
         
         // Check if it is CallFunction first
         dot = rhs.find('.');
         if (dot == rhs.npos)
         {
            rhsPropName = rhsParts[1];
         }
         else
         {
            // check if it is property first
            std::string afterDot = rhs.substr(dot+1);
            GmatBase *toObj = NULL;
            Integer toId = -1;
            Gmat::ParameterType toType;
            if (FindPropertyID(rhsObj, afterDot, &toObj, toId, toType))
               rhsPropName = afterDot;
            else
               rhsPropName = rhsParts[1];
         }
      }
   }
   else
   {
      // If firist RHS char is "-" sign, use without it in finding name.
      // This is due to backward propagation. For example,
      // Propagate -prop(Sat1, Sat2, {Sat1.Periapsis})
      std::string newName = rhs;
      
      if (rhs[0] == '-')
         newName = rhs.substr(1);
      
      rhsObj = FindObject(newName);
      
      if (rhsObj)
      {
         if (IsArrayElement(rhs))
            isRhsArray = true;
         else
         {
            // @note
            // We want to allow user to create object and name it with one of
            // ObjectTypes. e.g. Create Spacecraft Spacecraft.
            // So if name found in configuration and not an ObjectType, except
            // calculated PARAMETER, it will considered as string value.
            if (IsObjectType(newName) && rhsObj->GetType() != Gmat::PARAMETER)
               isRhsObject = false;
            else
               isRhsObject = true;
         }
      }
   }
   
   #ifdef DEBUG_MAKE_ASSIGNMENT
   MessageInterface::ShowMessage
      ("   isRhsObject=%d, isRhsArray=%d, rhsPropName=<%s>, rhsObj=<%p><%s>\n",
       isRhsObject, isRhsArray, rhsPropName.c_str(), rhsObj,
       (rhsObj == NULL) ? "NULL" : rhsObj->GetName().c_str() );
   #endif
   
   if (isLhsObject)
   {
      if (isRhsObject)
         retval = SetObjectToObject(lhsObj, rhsObj);
      else if (rhsPropName != "")
         retval = SetPropertyToObject(lhsObj, rhsObj, rhsPropName);
      else if (isRhsArray)
         retval = SetArrayToObject(lhsObj, rhs);
      else
         retval = SetValueToObject(lhsObj, rhs);
   }
   else if (lhsPropName != "")
   {
      if (isRhsObject)
         retval = SetObjectToProperty(lhsObj, lhsPropName, rhsObj);
      else if (rhsPropName != "")
         retval = SetPropertyToProperty(lhsObj, lhsPropName, rhsObj, rhsPropName);
      else if (isRhsArray)
         retval = SetArrayToProperty(lhsObj, lhsPropName, rhs);
      else
         retval = SetValueToProperty(lhsObj, lhsPropName, rhs);
   }
   else if (isLhsArray)
   {
      if (isRhsObject)
         retval = SetObjectToArray(lhsObj, lhs, rhsObj);
      else if (rhsPropName != "")
         retval = SetPropertyToArray(lhsObj, lhs, rhsObj, rhsPropName);
      else if (isRhsArray)
         retval = SetArrayToArray(lhsObj, lhs, rhsObj, rhs);
      else
         retval = SetValueToArray(lhsObj, lhs, rhs);
   }
   else
   {
      InterpreterException ex
         ("Interpreter::MakeAssignment() Internal error if it reached here.");
      HandleError(ex);
   }
   
   #ifdef DEBUG_MAKE_ASSIGNMENT
   MessageInterface::ShowMessage
      ("Interpreter::MakeAssignment() returning lhsObj=%p\n", lhsObj);
   #endif
   
   if (retval)
      return lhsObj;
   else
      return NULL;
}


//-------------------------------------------------------------------------------
// bool SetObjectToObject(GmatBase *toObj, GmatBase *fromObj)
//-------------------------------------------------------------------------------
bool Interpreter::SetObjectToObject(GmatBase *toObj, GmatBase *fromObj)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectToObject() to=%s, from=%s\n",
       toObj->GetName().c_str(), fromObj->GetName().c_str());
   #endif
   
   debugMsg = "In SetObjectToObject()";
   
   // Copy object
   if (toObj->GetTypeName() == fromObj->GetTypeName())
   {
      toObj->Copy(fromObj);
   }
   else
   {
      InterpreterException ex("Object type of LHS and RHS are not the same.");
      HandleError(ex);
      return false;
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectToObject() returning true\n");
   #endif
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetPropertyToObject(GmatBase *toObj, GmatBase *fromOwner,
//                          const std::string &fromProp)
//-------------------------------------------------------------------------------
bool Interpreter::SetPropertyToObject(GmatBase *toObj, GmatBase *fromOwner,
                                      const std::string &fromProp)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("SetPropertyToObject() toObj=%s, fromOwner=%s, fromProp=%s\n",
       toObj->GetName().c_str(), fromOwner->GetName().c_str(), fromProp.c_str());
   #endif
   
   debugMsg = "In SetPropertyToObject()";
   std::string rhs = fromOwner->GetName() + "." + fromProp;
   Integer fromId = -1;
   Gmat::ParameterType fromType = Gmat::UNKNOWN_PARAMETER_TYPE;
   Parameter *rhsParam = NULL;
   
   if (toObj->GetTypeName() != "Variable" && toObj->GetTypeName() != "String")
   {
      InterpreterException ex
         ("Setting \"" + fromProp + "\" to an object \"" + toObj->GetName() +
          "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   try
   {
      fromId = fromOwner->GetParameterID(fromProp);
      fromType = fromOwner->GetParameterType(fromId);
   }
   catch (BaseException &e)
   {
      // try if fromProp is a system Parameter
      rhsParam = CreateSystemParameter(rhs);
      
      // it is not a Parameter, so handle error
      if (rhsParam == NULL)
      {
         // Try setting as Variable expression (loj: 2008.08.05)
         // to handle var = sat.A1ModJulian - 21545 prior to mission sequence
         // It also shows correct expression in the GUI
         if (ParseVariableExpression((Parameter*)toObj, rhs))
            return true;
         else
         {
            HandleError(e);
            return false;
         }
      }
      
      fromType = rhsParam->GetReturnType();
      
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("SetPropertyToObject() rhs:%s is a parameter\n", rhs.c_str());
      #endif
   }
   
   Parameter *toParam = (Parameter*)toObj;
   Gmat::ParameterType toType = toParam->GetReturnType();
   
   if (fromType == toType)
   {
      if (fromId == -1)
      {
         // LHS is a Variable or String, RHS is a Parameter
         if (toType == Gmat::STRING_TYPE || toType == Gmat::ENUMERATION_TYPE)
            toObj->SetStringParameter("Value", rhsParam->GetString());
         else if (toType == Gmat::REAL_TYPE)
            ParseVariableExpression(toParam, rhs);
      }
      else
      {
         // LHS is a Variable or String, RHS is an ObjectProperty
         if (toType == Gmat::STRING_TYPE || toType == Gmat::ENUMERATION_TYPE)
            toObj->SetStringParameter("Value", fromOwner->GetStringParameter(fromId));
         else if (toType == Gmat::REAL_TYPE)
         {
            // Check to see if fromProp is also a system Parameter, (loj: 2008.08.06)
            // if so Parameter takes higher presendence over ObjectProperty.
            rhsParam = CreateSystemParameter(rhs);
            if (rhsParam != NULL)
            {
               ParseVariableExpression(toParam, rhs);
            }
            else
               toObj->SetRealParameter("Value", fromOwner->GetRealParameter(fromId));
         }
      }
   }
   else
   {
      InterpreterException ex
         ("Setting \"" + fromProp + "\" to an object \"" + toObj->GetName() +
          "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetArrayToObject(GmatBase *toObj, const std::string &fromArray)
//-------------------------------------------------------------------------------
bool Interpreter::SetArrayToObject(GmatBase *toObj, const std::string &fromArray)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetArrayToObject() toObj=%s, fromArray=%s\n",
       toObj->GetName().c_str(), fromArray.c_str());
   #endif

   debugMsg = "In SetArrayToObject()";
   
   if (toObj->GetTypeName() != "Variable")
   {
      InterpreterException ex
         ("Setting \"" + fromArray + "\" to an object \"" + toObj->GetName() +
          "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   Integer row, col;
   Parameter *param = GetArrayIndex(fromArray, row, col);
   if (param == NULL)
      return false;
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   SetArrayToObject() row=%d, col=%d\n", row, col);
   #endif
   
   // Check for array index
   if (row == -1 || col == -1)
   {
      InterpreterException ex("Invalid array index: " + fromArray);
      HandleError(ex);
      return false;
   }
   
   Real rval = GetArrayValue(fromArray, row, col);
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage("   SetArrayToObject() rval=%f\n", rval);
   #endif

   try
   {
      toObj->SetRealParameter("Value", rval);
   }
   catch (BaseException &e)
   {
      HandleError(e);
      return false;
   }
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetValueToObject(GmatBase *toObj, const std::string &value)
//-------------------------------------------------------------------------------
bool Interpreter::SetValueToObject(GmatBase *toObj, const std::string &value)
{
   debugMsg = "In SetValueToObject()";
   std::string toObjType = toObj->GetTypeName();
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetValueToObject() toObjType=<%s>, toObjName=%s, value=<%s>\n",
       toObjType.c_str(), toObj->GetName().c_str(), value.c_str());
   #endif
   
   if (toObjType != "Variable" && toObjType != "String")
   {
      InterpreterException ex
         ("Setting a String value \"" + value + "\" to an object \"" + toObj->GetName() +
          "\" of type \"" + toObjType + "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   if (toObjType == "String")
   {
      // check for unpaired single quotes
      if (GmatStringUtil::HasMissingQuote(value, "'"))
      {
         InterpreterException ex("The String \"" + value + "\" has missing single quote");
         HandleError(ex);
         return false;
      }
      
      std::string valueToUse = GmatStringUtil::RemoveEnclosingString(value, "'");
      
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("   Calling %s->SetStringParameter(Expression, %s)\n", toObj->GetName().c_str(),
          valueToUse.c_str());
      MessageInterface::ShowMessage
         ("   Calling %s->SetStringParameter(Value, %s)\n", toObj->GetName().c_str(),
          valueToUse.c_str());
      #endif
      
      toObj->SetStringParameter("Expression", valueToUse);
      toObj->SetStringParameter("Value", valueToUse);
   }
   else if (toObjType == "Variable")
   {
      Real rval;

      try
      {
         if (GmatStringUtil::ToReal(value, rval, true))
         {      
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage("   SetValueToObject() rval=%f\n", rval);
            #endif
            
            toObj->SetRealParameter("Value", rval);
         }
         else
         {
            if (!ParseVariableExpression((Parameter*)toObj, value))
            {
               InterpreterException ex
                  ("Setting \"" + value + "\" to a Variable \"" + toObj->GetName() +
                   "\" is not allowed");
               HandleError(ex);
               return false;
            }
         }
      }
      catch (BaseException &e)
      {
         HandleError(e);
         return false;
      }
   }
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetObjectToProperty(GmatBase *toOwner, const std::string &toProp,
//                          GmatBase *fromObj)
//-------------------------------------------------------------------------------
bool Interpreter::SetObjectToProperty(GmatBase *toOwner, const std::string &toProp,
                                      GmatBase *fromObj)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectToProperty() ownerType=%s, toOwner=%s, toProp=%s, "
       "fromObj=%s\n", toOwner->GetTypeName().c_str(), toOwner->GetName().c_str(),
       toProp.c_str(), fromObj->GetName().c_str());
   #endif
   
   debugMsg = "In SetObjectToProperty()";
   
   if (toOwner->GetType() == Gmat::FORCE_MODEL)
   {
      std::string objName = fromObj->GetName();
      bool retval = SetForceModelProperty(toOwner, toProp, objName, fromObj);
      if (!retval)
      {
         InterpreterException ex
            ("The value of \"" + objName + "\" for field \"" + toProp +
             "\" on ForceModel \"" + toOwner->GetName() + "\" is not an allowed value");
         HandleError(ex);
         return false;
      }
      
      return true;
   }
   
   
   GmatBase *toObj = NULL;
   Integer toId = -1;
   Gmat::ParameterType toType;
   
   try
   {
      FindPropertyID(toOwner, toProp, &toObj, toId, toType);
      
      if (toObj == NULL)
      {
         if (parsingDelayedBlock)
         {
            InterpreterException ex
               ("The field name \"" + toProp + "\" on object " + toOwner->GetName() +
                " is not permitted");
            HandleErrorMessage(ex, lineNumber, currentLine, true);
            return false;
         }
         
         delayedBlocks.push_back(currentBlock);
         std::string lineNumStr = GmatStringUtil::ToString(theReadWriter->GetLineNumber());
         delayedBlockLineNumbers.push_back(lineNumStr);
         
         #ifdef DEBUG_SET
         MessageInterface::ShowMessage
            ("   ===> added to delayed blocks: line:%s, %s\n", lineNumStr.c_str(),
             currentBlock.c_str());
         #endif
         
         return true;
      }
   }
   catch (BaseException &e)
   {
      if (parsingDelayedBlock)
         return false;
      
      delayedBlocks.push_back(currentBlock);
      
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("   ===> added to delayed blocks: %s\n", currentBlock.c_str());
      #endif
      
      return true;
   }
   
   toType = toObj->GetParameterType(toId);
   
   // Let's treat enumeration type as string type
   if (toType == Gmat::ENUMERATION_TYPE)
      toType = Gmat::STRING_TYPE;
   
   try
   {
      std::string fromTypeName = fromObj->GetTypeName();
      
      if (fromObj->GetType() == Gmat::PARAMETER)
      {
         Gmat::ParameterType fromType = ((Parameter*)fromObj)->GetReturnType();
         
         #ifdef DEBUG_SET
         MessageInterface::ShowMessage
            ("   From object is a Parameter, toId=%d, fromType=%d, toType=%d\n",
             toId, fromType, toType);
         #endif
         
         if (fromType == toType)
         {
            if (toType == Gmat::STRING_TYPE)
               toObj->SetStringParameter(toId, fromObj->GetStringParameter("Value"));
            else if (toType == Gmat::REAL_TYPE)
               toObj->SetRealParameter(toId, fromObj->GetRealParameter("Value"));
         }
         else
         {
            bool errorCond = false;
            if (fromTypeName == "String")
            {
               if (toType == Gmat::STRING_TYPE || toType == Gmat::STRINGARRAY_TYPE)
                  toObj->SetStringParameter(toId, fromObj->GetStringParameter("Value"));
               else if (toType == Gmat::OBJECT_TYPE || toType == Gmat::OBJECTARRAY_TYPE)
                  toObj->SetStringParameter(toId, fromObj->GetName());               
               else
                  errorCond = true;
            }
            else if (fromTypeName == "Variable")
            {
               if (toType == Gmat::REAL_TYPE)
                  toObj->SetRealParameter(toId, fromObj->GetRealParameter("Value"));
               // Added to fix GMAT XYPlot1.IndVar = Var; (loj: 2008.08.01)
               else if (toType == Gmat::OBJECT_TYPE && toObj->IsOfType("XYPlot"))
                  toObj->SetStringParameter(toId, fromObj->GetName());
               else
                  errorCond = true;
            }
            else
            {
               if (toType == Gmat::OBJECT_TYPE || toType == Gmat::OBJECTARRAY_TYPE)
                  toObj->SetStringParameter(toId, fromObj->GetName());
               else
                  errorCond = true;
            }
            
            if (errorCond)
            {
               InterpreterException ex
                  ("The value of \"" + fromObj->GetName() + "\" for field \"" + toProp +
                   "\" on object " + "\"" + toOwner->GetName() + "\" is not an allowed value");
               HandleError(ex);
               return false;
            }
         }
      }
      else
      {
         #ifdef DEBUG_SET
         MessageInterface::ShowMessage
            ("   Setting objType=%s, objName=%s\n", fromTypeName.c_str(),
             fromObj->GetName().c_str());
         #endif
         
         toObj->SetStringParameter(toProp, fromObj->GetName());
         if (toObj->IsOwnedObject(toId))
            toObj->SetRefObject(fromObj, fromObj->GetType(), fromObj->GetName());
      }
   }
   catch (BaseException &ex)
   {
      HandleError(ex);
      return false;
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectToProperty() returning true\n");
   #endif
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetPropertyToProperty(GmatBase *toOwner, const std::string &toProp,
//                            GmatBase *fromOwner, const std::string &fromProp)
//-------------------------------------------------------------------------------
bool Interpreter::SetPropertyToProperty(GmatBase *toOwner, const std::string &toProp,
                                        GmatBase *fromOwner, const std::string &fromProp)
{
   debugMsg = "In SetPropertyToProperty()";
   bool retval = true;
   errorMsg1 = "";
   errorMsg2 = "";
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("SetPropertyToProperty() toOwner=%s<%s>, toProp=<%s>, fromOwner=<%s>, fromProp=<%s>\n",
       toOwner->GetName().c_str(), toOwner->GetTypeName().c_str(), toProp.c_str(),
       fromOwner->GetName().c_str(), fromProp.c_str());
   #endif
   
   Integer toId = -1;
   Gmat::ParameterType toType = Gmat::UNKNOWN_PARAMETER_TYPE;
   std::string lhs = toOwner->GetName() + "." + toProp;
   std::string rhs = fromOwner->GetName() + "." + fromProp;
   std::string value;
   Parameter *lhsParam = NULL;
   Parameter *rhsParam = NULL;
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage("   lhs=%s, rhs=%s\n", lhs.c_str(), rhs.c_str());
   #endif

   //-----------------------------------
   // try LHS property
   //-----------------------------------
   
   try
   {
      GmatBase *toObj = NULL;
      FindPropertyID(toOwner, toProp, &toObj, toId, toType);
   }
   catch (BaseException &e)
   {
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("   Parameter ID of '%s' not found. So create a parameter '%s'\n",
          toProp.c_str(), lhs.c_str());
      #endif
      lhsParam = CreateSystemParameter(lhs);
   }
   
   //-----------------------------------
   // try RHS property
   //-----------------------------------
   // try create parameter first if rhs type is OBJECT_TYPE
   if (toType == Gmat::OBJECT_TYPE)
      rhsParam = CreateSystemParameter(rhs);
   
   Integer fromId = -1;
   Gmat::ParameterType fromType = Gmat::UNKNOWN_PARAMETER_TYPE;
   bool isRhsProperty = true;
   
   try
   {
      fromId = fromOwner->GetParameterID(fromProp);   
      fromType = fromOwner->GetParameterType(fromId);
   }
   catch (BaseException &e)
   {
      isRhsProperty = false;
      fromType = Gmat::STRING_TYPE;
   }
   
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   toId=%d, toType=%d, fromId=%d, fromType=%d, lhsParam=%p, rhsParam=%p\n",
       toId, toType, fromId, fromType, lhsParam, rhsParam);
   #endif
   
   //-----------------------------------
   // now set value
   //-----------------------------------
   
   if (lhsParam != NULL && rhsParam != NULL)
   {
      SetObjectToObject(lhsParam, rhsParam);
   }
   else if (lhsParam == NULL && rhsParam != NULL)
   {
      if (toType == rhsParam->GetReturnType())
      {
         value = rhsParam->ToString();
         retval = SetProperty(toOwner, toId, toType, value);
      }
      else
      {
         retval = SetProperty(toOwner, toId, toType, rhs);
      }
   }
   else if (lhsParam != NULL && rhsParam == NULL)
   {
      if (lhsParam->GetReturnType() == fromType)
      {
         value = GetPropertyValue(fromOwner, fromId);
         lhsParam->SetString(value); 
         retval = true;
      }
   }
   else if (lhsParam == NULL && rhsParam == NULL)
   {
      if (toType == fromType)
      {
         if (toType == Gmat::STRING_TYPE || toType == Gmat::ENUMERATION_TYPE)
         {
            if (isRhsProperty)
            {
               value = GetPropertyValue(fromOwner, fromId);
               retval = SetPropertyValue(toOwner, toId, toType, value);
            }
            else
            {
               retval = SetPropertyValue(toOwner, toId, toType, rhs);
            }
         }
         else
         {
            value = GetPropertyValue(fromOwner, fromId);
            retval = SetProperty(toOwner, toId, toType, value);
         }
      }
      else
      {
         retval = SetProperty(toOwner, toId, toType, rhs);
      }
   }
   
   if (!retval)
   {
      if (errorMsg1 == "")
      {
         InterpreterException ex
            ("The field name \"" + fromProp + "\" on object " + toOwner->GetName() +
             " is not permitted");
         HandleError(ex);
      }
      else
      {
         InterpreterException ex
            (errorMsg1 + "field \"" + toProp + "\" on object " + "\"" +
             toOwner->GetName() + "\" is not an allowed value." + errorMsg2);
         HandleError(ex);
      }
   }
   
   return retval;
}


//-------------------------------------------------------------------------------
// bool SetArrayToProperty(GmatBase *toOwner, const std::string &toProp,
//                         const std::string &fromArray)
//-------------------------------------------------------------------------------
bool Interpreter::SetArrayToProperty(GmatBase *toOwner, const std::string &toProp,
                                     const std::string &fromArray)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetArrayToProperty() toOwner=%s, toProp=%s, fromArray=%s\n",
       toOwner->GetName().c_str(), toProp.c_str(), fromArray.c_str());
   #endif
   
   debugMsg = "In SetArrayToProperty()";
   Integer toId = -1;
   Gmat::ParameterType toType = Gmat::UNKNOWN_PARAMETER_TYPE;
   
   // Check for property id
   try
   {
      toId = toOwner->GetParameterID(toProp);
      toType = toOwner->GetParameterType(toId);
   }
   catch (BaseException &ex)
   {
      HandleError(ex);
      return false;
   }
   
   // Property type must be Real type, so check
   if (toType != Gmat::REAL_TYPE)
   {
      InterpreterException ex
         ("The value of \"" + fromArray + "\" for field \"" + toProp +
          "\" on object " + "\"" + toOwner->GetName() + "\" is not an allowed value");
      HandleError(ex);
      return false;
   }
      
   // Now try to set array to property
   Integer row, col;
   Real rval = GetArrayValue(fromArray, row, col);
   
   try
   {
      toOwner->SetRealParameter(toId, rval);
   }
   catch (BaseException &e)
   {
      HandleError(e);
      return false;
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetArrayToProperty() exiting. rval=%f, row=%d, col=%d, \n",
       rval, row, col);
   #endif
   
   return true;
}


//-------------------------------------------------------------------------------
// bool SetValueToProperty(GmatBase *toOwner, const std::string &toProp,
//                         const std::string &value)
//-------------------------------------------------------------------------------
bool Interpreter::SetValueToProperty(GmatBase *toOwner, const std::string &toProp,
                                     const std::string &value)
{
   debugMsg = "In SetValueToProperty()";
   bool retval = false;
   errorMsg1 = "";
   errorMsg2 = "";
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetValueToProperty() objType=%s, objName=%s, toProp=%s, "
       "value=%s\n", toOwner->GetTypeName().c_str(), toOwner->GetName().c_str(),
       toProp.c_str(), value.c_str());
   #endif
   
   if (toOwner->GetType() == Gmat::FORCE_MODEL)
   {
      retval = SetForceModelProperty(toOwner, toProp, value, NULL);
   }
   else if (toOwner->GetType() == Gmat::SOLAR_SYSTEM)
   {
      retval = SetSolarSystemProperty(toOwner, toProp, value);
   }
   else
   {
      StringArray parts = theTextParser.SeparateDots(toProp);
      
      // if property has multiple dots, handle separately
      if (parts.size() > 1)
      {
         retval = SetComplexProperty(toOwner, toProp, value);
      }
      else
      {
         GmatBase *toObj = NULL;
         Integer toId = -1;
         Gmat::ParameterType toType;
         
         FindPropertyID(toOwner, toProp, &toObj, toId, toType);
         
         if (toObj == NULL)
         {
            if (parsingDelayedBlock)
            {
               InterpreterException ex
                  ("The field name \"" + toProp + "\" on object \"" + toOwner->GetName() +
                   "\" is not permitted");
               HandleErrorMessage(ex, lineNumber, currentLine, true);
               return false;
            }
            
            delayedBlocks.push_back(currentBlock);
            std::string lineNumStr = GmatStringUtil::ToString(theReadWriter->GetLineNumber());
            delayedBlockLineNumbers.push_back(lineNumStr);
            
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   ===> added to delayed blocks: line:%s, %s\n", lineNumStr.c_str(),
                currentBlock.c_str());
            #endif
            
            return true;
         }
         
         retval = SetProperty(toObj, toId, toType, value);
      }
   }
   
   if (retval == false && !ignoreError)
   {
      if (errorMsg1 == "")
      {
         InterpreterException ex
            ("The field name \"" + toProp + "\" on object " + "\"" +
             toOwner->GetName() + "\" is not permitted");
         HandleError(ex);
      }
      else
      {
         InterpreterException ex
            (errorMsg1 + "field \"" + toProp + "\" on object " + "\"" +
             toOwner->GetName() + "\" is not an allowed value." + errorMsg2);
         HandleError(ex);
      }
   }
   
   if (ignoreError)
      ignoreError = false;
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetValueToProperty() returning retval=%d\n", retval);
   #endif
   
   return retval;
}


//-------------------------------------------------------------------------------
// bool SetObjectToArray(GmatBase *toArrObj, const std::string &toArray,
//                       GmatBase *fromObj)
//-------------------------------------------------------------------------------
bool Interpreter::SetObjectToArray(GmatBase *toArrObj, const std::string &toArray,
                                   GmatBase *fromObj)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetObjectToArray() toArrObj=%s, toArray=%s, fromObj=%s\n",
       toArrObj->GetName().c_str(), toArray.c_str(), fromObj->GetName().c_str());
   #endif
   
   debugMsg = "In SetObjectToArray()";
   
   if (fromObj->GetTypeName() != "Variable")
   {
      //InterpreterException ex
      //   ("Cannot set object other than Variable or Array element.");
      InterpreterException ex
         ("Setting object \"" + fromObj->GetName() + "\" to an array \"" + toArray +
          "\" is not permitted.");
      HandleError(ex);
      return false;
   }
   
   Real rval = fromObj->GetRealParameter("Value");
   
   Integer row, col;
   Parameter *param = GetArrayIndex(toArray, row, col);
   if (param == NULL)
      return false;
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   SetObjectToArray()rval=%f, row=%d, col=%d\n", rval, row, col);
   #endif

   try
   {
      toArrObj->SetRealParameter("SingleValue", rval, row, col);
   }
   catch (BaseException &e)
   {
      HandleError(e);
      return false;
   }

   return true;
}


//-------------------------------------------------------------------------------
// bool SetPropertyToArray(GmatBase *toArrObj, const std::string &toArray,
//                         GmatBase *fromOwner, const std::string &fromProp)
//-------------------------------------------------------------------------------
bool Interpreter::SetPropertyToArray(GmatBase *toArrObj, const std::string &toArray,
                                     GmatBase *fromOwner, const std::string &fromProp)
{
   #ifdef DEBGU_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyToArray() toArrObj=%s, toArray=%s, fromOwner=%s, "
       "fromProp=%s\n", toArrObj->GetName().c_str(), toArray.c_str(),
       fromOwner->GetName().c_str(), fromProp.c_str());
   #endif
   
   debugMsg = "In SetPropertyToArray()";
   
   // get object parameter id
   Integer fromId = fromOwner->GetParameterID(fromProp);
   
   if (fromOwner->GetParameterType(fromId) != Gmat::REAL_TYPE)
   {
      InterpreterException ex
         ("Setting non-Real type of \"" + fromProp + "\" to an Array element \"" +
          toArray + "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   Real rval = fromOwner->GetRealParameter(fromId);
   
   Integer row, col;
   Parameter *param = GetArrayIndex(toArray, row, col);
   if (param == NULL)
      return false;
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   SetPropertyToArray()rval=%f, row=%d, col=%d\n", rval, row, col);
   #endif

   try
   {
      toArrObj->SetRealParameter("SingleValue", rval, row, col);
   }
   catch (BaseException &e)
   {
      HandleError(e);
      return false;
   }

   return true;
}


//-------------------------------------------------------------------------------
// bool SetArrayToArray(GmatBase *toArrObj, const std::string &toArray,
//                      GmatBase *fromArrObj, const std::string &fromArray)
//-------------------------------------------------------------------------------
bool Interpreter::SetArrayToArray(GmatBase *toArrObj, const std::string &toArray,
                                  GmatBase *fromArrObj, const std::string &fromArray)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetArrayToArray() toArrObj=%s, toArray=%s, "
       "fromArrObj=%s, fromArray=%s\n", toArrObj->GetName().c_str(),
       toArray.c_str(), fromArrObj->GetName().c_str(), fromArray.c_str());
   #endif
   
   debugMsg = "In SetArrayToArray()";
   Integer rowFrom, colFrom;
   Integer rowTo, colTo;
   
   Parameter *param = GetArrayIndex(toArray, rowTo, colTo);
   if (param == NULL)
      return false;
   
   param = GetArrayIndex(fromArray, rowFrom, colFrom);
   if (param == NULL)
      return false;
   
   Real rval = GetArrayValue(fromArray, rowFrom, colFrom);
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   SetArrayToArray() rval=%f, rowFrom=%d, colFrom=%d, \n",
       rval, rowFrom, colFrom);
   MessageInterface::ShowMessage
      ("   SetArrayToArray()rowTo=%d, colTo=%d\n", rowTo, colTo);
   #endif

   try
   {
      if (fromArray[0] == '-')
         toArrObj->SetRealParameter("SingleValue", rval, rowTo, colTo);
      else   
         toArrObj->SetRealParameter("SingleValue", -rval, rowTo, colTo);
   }
   catch (BaseException &e)
   {
      HandleError(e);
      return false;
   }

   return true;
}


//-------------------------------------------------------------------------------
// bool SetValueToArray(GmatBase *array, const std::string &toArray,
//                      const std::string &value)
//-------------------------------------------------------------------------------
bool Interpreter::SetValueToArray(GmatBase *array, const std::string &toArray,
                                  const std::string &value)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetValueToArray() array=%s, toArray=%s, value=%s\n",
       array->GetName().c_str(), toArray.c_str(), value.c_str());
   #endif
   
   debugMsg = "In SetValueToArray()";
   Integer row, col;
   Real rval;

   Parameter *param = GetArrayIndex(toArray, row, col);
   if (param == NULL)
      return false;
   
   if (GmatStringUtil::ToReal(value, rval, true))
   {
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("   SetValueToArray() rval=%f, row=%d, col=%d\n", rval, row, col);
      #endif

      try
      {
         array->SetRealParameter("SingleValue", rval, row, col);
      }
      catch (BaseException &e)
      {
         InterpreterException ex("Index exceeds matrix dimensions");
         HandleError(ex);
         return false;
      }
   }
   else
   {
      InterpreterException ex
         ("Setting \"" + value + "\" to an object \"" + toArray +
          "\" is not allowed");
      HandleError(ex);
      return false;
   }
   
   return true;
}


//------------------------------------------------------------------------------
// bool SetPropertyValue(GmatBase *obj, const Integer id,
//                       const Gmat::ParameterType type,
//                       const std::string &value, const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets parameters on GMAT objects.
 * 
 * @param  obj    Pointer to the object that owns the property.
 * @param  id     ID for the property.
 * @param  type   Type for the property.
 * @param  value  Value of the property.
 * @param  index  Index of the property in array.
 * 
 * @return true if the property is set, false otherwise.
 */
//------------------------------------------------------------------------------
bool Interpreter::SetPropertyValue(GmatBase *obj, const Integer id,
                                   const Gmat::ParameterType type,
                                   const std::string &value, const Integer index)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyValue() obj=<%s>, id=%d, type=%d, value=<%s>, index=%d\n",
       obj->GetName().c_str(), id, type, value.c_str(), index);
   #endif
   
   debugMsg = "In SetPropertyValue()";
   bool retval = false;
   std::string valueToUse = value;
   CheckForSpecialCase(obj, id, valueToUse);
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("   propertyType=%s\n",
       type == -1 ? "UNKNOWN_TYPE" : GmatBase::PARAM_TYPE_STRING[type].c_str());
   #endif
   
   if (type == -1)
      return false;
   
   switch (type)
   {
   case Gmat::OBJECT_TYPE:
   case Gmat::OBJECTARRAY_TYPE:
      {
         return SetPropertyObjectValue(obj, id, type, valueToUse, index);
      }
   case Gmat::ENUMERATION_TYPE:
   case Gmat::STRING_TYPE:
   case Gmat::STRINGARRAY_TYPE:
      {
         return SetPropertyStringValue(obj, id, type, valueToUse, index);
      }
   case Gmat::INTEGER_TYPE:
   case Gmat::UNSIGNED_INT_TYPE:
      {
         Integer ival;
         if (GmatStringUtil::ToInteger(valueToUse, ival))
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling '%s'->SetIntegerParameter(%d, %d)\n",
                obj->GetName().c_str(), id, ival);
            #endif
            
            obj->SetIntegerParameter(id, ival);
            retval = true;
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + valueToUse + "\" for ";
            errorMsg2 = " Only integer number is allowed";
         }
         break;
      }
   case Gmat::UNSIGNED_INTARRAY_TYPE:
      {
         Integer ival;
         if (GmatStringUtil::ToInteger(valueToUse, ival))
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling '%s'->SetUnsignedIntParameter(%d, %d, %d)\n",
                obj->GetName().c_str(), id, ival, index);
            #endif
            
            obj->SetUnsignedIntParameter(id, ival, index);
            retval = true;
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + valueToUse + "\" for ";
            errorMsg2 = " Only integer number is allowed";
         }
         break;
      }
   case Gmat::REAL_TYPE:
   case Gmat::RVECTOR_TYPE:
      {
         Real rval;
         if (GmatStringUtil::ToReal(valueToUse, rval, true))
         {
            #ifdef DEBUG_SET
            std::string rvalStr =
               GmatStringUtil::ToString(rval, false, false, 17, 16);
            MessageInterface::ShowMessage
               ("   Calling <%s>'%s'->SetRealParameter(%d, %s)\n", obj->GetTypeName().c_str(),
                obj->GetName().c_str(), id, rvalStr.c_str());
            #endif
            
            if (type == Gmat::REAL_TYPE)
               obj->SetRealParameter(id, rval);
            else
               obj->SetRealParameter(id, rval, index);
            
            retval = true;
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + valueToUse + "\" for ";
            errorMsg2 = " The allowed value is Real number";
         }
         break;
      }
   case Gmat::BOOLEAN_TYPE:
      {
         bool tf;
         if (GmatStringUtil::ToBoolean(value, tf))
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling '%s'->SetBooleanParameter(%d, %d)\n",
                obj->GetName().c_str(), id, tf);
            #endif
            
            obj->SetBooleanParameter(id, tf);
            retval = true;
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + valueToUse + "\" for ";
            errorMsg2 = " The allowed values are: [true false]";
         }
         break;
      }
   case Gmat::ON_OFF_TYPE:
      {
         #ifdef DEBUG_SET
         MessageInterface::ShowMessage
            ("   Calling '%s'->SetOnOffParameter(%d, %s)\n",
             obj->GetName().c_str(), id, valueToUse.c_str());
         #endif
         
         if (valueToUse == "On" || valueToUse == "Off")
         {
            retval = obj->SetOnOffParameter(id, valueToUse);
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + valueToUse + "\" for ";
            errorMsg2 = " The allowed values are: [On Off]";
         }
         break;
      }
   default:
      InterpreterException ex
         ("Interpreter::SetPropertyValue() Cannot handle the type: " +
          GmatBase::PARAM_TYPE_STRING[type] + " yet.\n");
      HandleError(ex);
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyValue() returning retval=%d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool SetPropertyObjectValue(GmatBase *obj, const Integer id, ...)
//------------------------------------------------------------------------------
bool Interpreter::SetPropertyObjectValue(GmatBase *obj, const Integer id,
                                         const Gmat::ParameterType type,
                                         const std::string &value,
                                         const Integer index)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyObjectValue() obj=%s, id=%d, type=%d, value=%s, "
       "index=%d\n", obj->GetName().c_str(), id, type, value.c_str(), index);
   #endif
   
   debugMsg = "In SetPropertyObjectValue()";
   Parameter *param = NULL;
   
   // Try creating Parameter first if it is not ObjectType
   if (!IsObjectType(value))
   {
      // It is not a one of object types, so create parameter
      param = CreateSystemParameter(value);
      
      #ifdef DEBUG_SET
      if (param)
         MessageInterface::ShowMessage
            ("   param=(%p)%s type=%s returnType=%d\n", param,
             param->GetName().c_str(), param->GetTypeName().c_str(),
             param->GetReturnType());
      #endif
   }
   else
   {
      // It is object type so get parameter (Bug 743 fix)
      param = theModerator->GetParameter(value);
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("   theModerator->GetParameter() returned %p\n", param);
      #endif
   }
   
   try
   {
      if (param != NULL)
      {
         // Other than Subscriber, it can only take STRING_TYPE parameter
         if (param->GetReturnType() == Gmat::STRING_TYPE ||
             obj->IsOfType(Gmat::SUBSCRIBER))
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling '%s'->SetStringParameter(%d, %s)\n",
                obj->GetName().c_str(), id, value.c_str());
            #endif
            
            // Let base code check for the invalid values
            obj->SetStringParameter(id, value);
         }
         else
         {
            errorMsg1 = errorMsg1 + "The value of \"" + value + "\" for ";
            errorMsg2 = "  The allowed value is Object Name";
            return false;
         }
      }
      else
      {
         // check if value is a number
         Real rval;
         Integer ival;
         if (GmatStringUtil::ToReal(value, rval, true) ||
             GmatStringUtil::ToInteger(value, ival, true))
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage("   It is a Real or Integer value\n");
            #endif
            
            // Handle special case for OpenGlPlot.
            // ViewPointReference, ViewPointVector, and ViewDirection can have 
            // both vector and object name.
            if (obj->GetTypeName() == "OpenGLPlot")
            {
               obj->SetStringParameter(id, value, index);
            }
            else
            {
               errorMsg1 = errorMsg1 + "The value of \"" + value + "\" for ";
               errorMsg2 = "  The allowed value is Object Name";
               return false;
            }
         }
         
         // check if value is an object name
         GmatBase *configObj = FindObject(value);
         if (configObj)
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Found the object type of %s\n", configObj->GetTypeName().c_str());
            #endif
            
            // Set as String parameter, so it can be validated in FinalPass()
            bool retval = true;
            if (index != -1)
            {
               #ifdef DEBUG_SET
               MessageInterface::ShowMessage
                  ("   Calling '%s'->SetStringParameter(%d, %s, %d)\n",
                   obj->GetName().c_str(), id, value.c_str(), index);
               #endif
               
               retval = obj->SetStringParameter(id, value, index);
            }
            
            // if it has no index or failed setting with index, try without index
            if (index == -1 || !retval)
            {
               #ifdef DEBUG_SET
               MessageInterface::ShowMessage
                  ("   Calling '%s'->SetStringParameter(%d, %s)\n",
                   obj->GetName().c_str(), id, value.c_str());
               #endif
               
               obj->SetStringParameter(id, value);
            }
         }
         else
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Object not found, so try creating owned object\n");
            #endif
            
            // Create Owned Object, if it is valid owned object type
            GmatBase *ownedObj = NULL;
            if (obj->IsOwnedObject(id))
               ownedObj = CreateObject(value, "");
            
            #ifdef DEBUG_SET
            if (ownedObj)
               MessageInterface::ShowMessage
                  ("   Created ownedObjType: %s\n", ownedObj->GetTypeName().c_str());
            #endif
            
            if (ownedObj)
            {
               #ifdef DEBUG_SET
               MessageInterface::ShowMessage
                  ("   Calling '%s'->SetRefObject(%s(%p), %d)\n", obj->GetName().c_str(),
                   ownedObj->GetTypeName().c_str(), ownedObj, ownedObj->GetType());
               #endif
               
               obj->SetRefObject(ownedObj, ownedObj->GetType(), ownedObj->GetName());
            }
            else
            {
               // Special case of InternalForceModel in script
               if (value != "InternalForceModel")
               {
                  // Set as String parameter, so it can be caught in FinalPass()
                  #ifdef DEBUG_SET
                  MessageInterface::ShowMessage
                     ("   Calling '%s'->SetStringParameter(%d, %s)\n",
                      obj->GetName().c_str(), id, value.c_str());
                  #endif
                  
                  obj->SetStringParameter(id, value);
               }
            }
         }
      }
      
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("Interpreter::SetPropertyObjectValue() returning true\n");
      #endif
      
      return true;
   }
   catch (BaseException &ex)
   {
      HandleError(ex);
      ignoreError = true;
      
      #ifdef DEBUG_SET
      MessageInterface::ShowMessage
         ("Interpreter::SetPropertyObjectValue() returning false\n");
      #endif
      
      return false;
   }
}


//------------------------------------------------------------------------------
// bool SetPropertyStringValue(GmatBase *obj, const Integer id, ...)
//------------------------------------------------------------------------------
bool Interpreter::SetPropertyStringValue(GmatBase *obj, const Integer id,
                                         const Gmat::ParameterType type,
                                         const std::string &value,
                                         const Integer index)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyStringValue() obj=%s, id=%d, type=%d, value=%s, "
       "index=%d\n", obj->GetName().c_str(), id, type, value.c_str(), index);
   #endif
   
   debugMsg = "In SetPropertyStringValue()";
   bool retval = true;
   std::string valueToUse = value;
   
   switch (type)
   {
   case Gmat::ENUMERATION_TYPE:
   case Gmat::STRING_TYPE:
      {
         // remove enclosing quotes if used
         valueToUse = GmatStringUtil::RemoveEnclosingString(valueToUse, "'");
         
         try
         {
            if (index >= 0)
            {
               #ifdef DEBUG_SET
               MessageInterface::ShowMessage
                  ("   Calling %s->SetStringParameter(%d, %s, %d)\n",
                   obj->GetName().c_str(), id, valueToUse.c_str(), index);
               #endif
               
               retval = obj->SetStringParameter(id, valueToUse, index);
            }
            else
            {
               #ifdef DEBUG_SET
               MessageInterface::ShowMessage
                  ("   Calling %s->SetStringParameter(%d, %s)\n",
                   obj->GetName().c_str(), id, valueToUse.c_str());
               #endif
               
               retval = obj->SetStringParameter(id, valueToUse);
            }
         }
         catch (BaseException &ex)
         {
            HandleError(ex);
            ignoreError = true;
            retval = false;
         }
         break;
      }
   case Gmat::STRINGARRAY_TYPE:
      {         
         try
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling %s->SetStringParameter(%d, %s)\n",
                obj->GetName().c_str(), id, valueToUse.c_str());
            #endif
            
            retval = obj->SetStringParameter(id, valueToUse);
         }
         catch (BaseException &e)
         {
            #ifdef DEBUG_SET
            MessageInterface::ShowMessage
               ("   Calling %s->SetStringParameter(%d, %s, %d)\n", id,
                valueToUse.c_str(), index);
            #endif
            
            // try with index
            retval = obj->SetStringParameter(id, valueToUse, index);
         }
         break;
      }
   default:
      break;
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetPropertyStringValue() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// std::string GetPropertyValue(GmatBase *obj, const Integer id)
//------------------------------------------------------------------------------
std::string Interpreter::GetPropertyValue(GmatBase *obj, const Integer id)
{
   std::string sval;
   
   Gmat::ParameterType type = obj->GetParameterType(id);
   
   if (type == Gmat::OBJECT_TYPE)
   {
      sval = obj->GetStringParameter(id);
   }
   else if (type == Gmat::INTEGER_TYPE)
   {
      sval = GmatStringUtil::ToString(obj->GetIntegerParameter(id));
   }
   else if (type == Gmat::UNSIGNED_INT_TYPE)
   {
      sval = GmatStringUtil::ToString(obj->GetIntegerParameter(id));
   }
//    else if (type == Gmat::UNSIGNED_INTARRAY_TYPE)
//    {
//       sval = GmatStringUtil::ToString(obj->GetIntegerParameter(id));
//    }
   else if (type == Gmat::REAL_TYPE)
   {
      sval = GmatStringUtil::ToString(obj->GetRealParameter(id));
   }
   else if (type == Gmat::STRING_TYPE || type == Gmat::ENUMERATION_TYPE)
   {
      sval = obj->GetStringParameter(id);
   }
//    else if (type == Gmat::STRINGARRAY_TYPE)
//    {
//       sval = obj->GetStringParameter(id));
//    }
   else if (type == Gmat::BOOLEAN_TYPE)
   {
      if (obj->GetBooleanParameter(id))
         sval = "true";
      else
         sval = "false";
   }
   else if (type == Gmat::ON_OFF_TYPE)
   {
      sval = obj->GetOnOffParameter(id);
   }
   
   return sval;
}


//------------------------------------------------------------------------------
// bool SetProperty(GmatBase *obj, const Integer id, const Gmat::ParameterType type
//                  const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets parameters on GMAT objects.
 * 
 * @param  obj    Pointer to the object that owns the property.
 * @param  id     property ID
 * @param  type   proerty Type
 * @param  value  Value of the property.
 * 
 * @return true if the property is set, false otherwise.
 */
//------------------------------------------------------------------------------
bool Interpreter::SetProperty(GmatBase *obj, const Integer id,
                              const Gmat::ParameterType type,
                              const std::string &value)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetProperty() obj=%s, id=%d, type=%d, value=%s\n",
       obj->GetName().c_str(), id, type, value.c_str());
   #endif
   
   bool retval = false;
   
   std::string valueToUse = value;
   CheckForSpecialCase(obj, id, valueToUse);
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage("   propertyType=%d\n", obj->GetParameterType(id));
   #endif
   
   StringArray rhsValues;
   Integer count = 0;
   
   // if value has braces, setting multiple values
   if (value.find("{") != value.npos || value.find("}") != value.npos)
      rhsValues = theTextParser.SeparateBrackets(value, "{}", " ,");
   else if (value.find("[") != value.npos || value.find("]") != value.npos)
      rhsValues = theTextParser.SeparateBrackets(value, "[]", " ,");
   
   count = rhsValues.size();
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage("   count=%d\n", count);
   #endif
   
   if (count > 0)
   {
      for (int i=0; i<count; i++)
         retval = SetPropertyValue(obj, id, type, rhsValues[i], i);
   }
   else
   {
      retval = SetPropertyValue(obj, id, type, value);
   }
   
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetProperty() returning retval=%d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool SetComplexProperty(GmatBase *obj, const std::string &prop,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool Interpreter::SetComplexProperty(GmatBase *obj, const std::string &prop,
                                     const std::string &value)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::SetComplexProperty() prop=%s, value=%s\n",
       prop.c_str(), value.c_str());
   #endif
   
   StringArray parts = theTextParser.SeparateDots(prop);

   if (obj->GetType() == Gmat::SPACECRAFT)
   {
      Spacecraft *sc = (Spacecraft*)obj;
      
      if (parts[0] == "Epoch")
      {
         sc->SetDateFormat(parts[1]);
         sc->SetEpoch(value);
      }
      else
      {
         return false;
      }
   }
   
   return true;
}


//------------------------------------------------------------------------------
// bool SetForceModelProperty(GmatBase *obj, const std::string &prop,
//                            const std::string &value, GmatBase *fromObj)
//------------------------------------------------------------------------------
bool Interpreter::SetForceModelProperty(GmatBase *obj, const std::string &prop,
                                        const std::string &value, GmatBase *fromObj)
{
   debugMsg = "In SetForceModelProperty()";
   bool retval = false;
   StringArray parts = theTextParser.SeparateDots(prop);
   Integer count = parts.size();
   std::string pmType = parts[count-1];
   Integer id;
   Gmat::ParameterType type;
   
   // Current ForceModel scripting, SRP is on for central body.
   //GMAT FM.CentralBody = Earth;
   //GMAT FM.PrimaryBodies = {Earth, Luna};
   //GMAT FM.PointMasses = {Sun, Jupiter}; 
   //GMAT FM.Drag = None;
   //GMAT FM.SRP = On;
   //GMAT FM.GravityField.Earth.Degree = 20;
   //GMAT FM.GravityField.Earth.Order = 20;
   //GMAT FM.GravityField.Earth.Model = JGM2.cof;
   //GMAT FM.GravityField.Luna.Degree = 4;
   //GMAT FM.GravityField.Luna.Order = 4;
   //GMAT FM.GravityField.Luna.PotentialFile = LP165P.cof;
   
   // For future scripting we want to specify body for Drag and SRP
   // e.g. FM.Drag.Earth = JacchiaRoberts;
   //      FM.Drag.Mars = MarsAtmos;
   //      FM.SRP.ShadowBodies = {Earth,Moon}
   
   ForceModel *forceModel = (ForceModel*)obj;
   std::string forceType = ForceModel::GetScriptAlias(pmType);
   std::string centralBodyName = forceModel->GetStringParameter("CentralBody");
   
   #ifdef DEBUG_SET_FORCE_MODEL
   MessageInterface::ShowMessage
      ("Interpreter::SetForceModelProperty() fm=%s, prop=%s, value=%s\n"
       "   pmType=%s, forceType=%s\n", obj->GetName().c_str(), prop.c_str(), value.c_str(),
       pmType.c_str(), forceType.c_str());
   #endif
   
   //------------------------------------------------------------
   // Set ForceModel CentralBody
   //------------------------------------------------------------
   if (pmType == "CentralBody")
   {
      id = obj->GetParameterID("CentralBody");
      type = obj->GetParameterType(id);
      retval = SetPropertyValue(obj, id, type, value);
      return retval;
   }
   
   //------------------------------------------------------------
   // Create ForceModel owned PhysicalModel
   //------------------------------------------------------------
   
   else if (pmType == "PrimaryBodies" || pmType == "PointMasses")
   {
      retval = true;
      StringArray bodies = theTextParser.SeparateBrackets(value, "{}", " ,");
      
      for (UnsignedInt i=0; i<bodies.size(); i++)
      {
         #ifdef DEBUG_SET_FORCE_MODEL
         MessageInterface::ShowMessage("   bodies[%d]=%s\n", i, bodies[i].c_str());
         #endif
         
         // We don't want to configure PhysicalModel, so set name after create
         PhysicalModel *pm = (PhysicalModel*)CreateObject(forceType, "");
         if (pm)
         {
            pm->SetName(forceType + "." + bodies[i]);
            
            if (!pm->SetStringParameter("BodyName", bodies[i]))
            {
               InterpreterException ex("Unable to set body for force " + bodies[i]);
               HandleError(ex);
            }
            
            #ifdef DEBUG_SET_FORCE_MODEL
            MessageInterface::ShowMessage
               ("   Adding type:<%s> name:<%s> to ForceModel:<%s>\n",
                pm->GetTypeName().c_str(), pm->GetName().c_str(),
                forceModel->GetName().c_str());
            #endif
            
            // Add force to ForceModel
            forceModel->AddForce(pm);
            
            // Use JGM2 for default Earth gravity file, in case it is not
            // specified in the script
            if (pmType == "PrimaryBodies" && bodies[i] == "Earth")
            {
               id = pm->GetParameterID("Model");
               type = pm->GetParameterType(id);
               retval = SetPropertyValue(pm, id, type, "JGM2");
            }
         }
      }
      
      #ifdef DEBUG_SET_FORCE_MODEL
      MessageInterface::ShowMessage
         ("Interpreter::SetForceModelProperty() returning %d\n", retval);
      #endif
      return retval;
   }
   else if (pmType == "SRP" || pmType == "Drag")
   {
      if (pmType == "SRP")
      {
         id = obj->GetParameterID("SRP");
         type = obj->GetParameterType(id);
         retval = SetPropertyValue(obj, id, type, value);
         
         if (retval && value != "On")
            return true;
         else if (!retval)
            return false;
      }
      
      if (pmType == "Drag" && value == "None")
         return true;
      
      // Create PhysicalModel
      PhysicalModel *pm = (PhysicalModel*)CreateObject(forceType, "");
      pm->SetName(pmType + "." + centralBodyName);
      
      // Special handling for Drag
      if (pmType == "Drag")
      {
         if (!pm->SetStringParameter("AtmosphereModel", value))
         {
            InterpreterException ex
               ("Unable to set AtmosphereModel for drag force");
            HandleError(ex);
            ignoreError = true;
            return false;
         }
         
         /// @todo Add the body name for drag at other bodies
         if (value != "BodyDefault")
         {
            pm->SetStringParameter("BodyName", centralBodyName);
            GmatBase *am = CreateObject(value, "");
            if (am)
               pm->SetRefObject(am, Gmat::ATMOSPHERE, am->GetName());
            else
            {
               InterpreterException ex
                  ("Unable to create AtmosphereModel \"" + value + "\" for drag force");
               HandleError(ex);
               ignoreError = true;
               return false;
            }
         }
      }
      else if (pmType == "SRP")
      {
         // Should we set SRP on ForceModel central body?
         pm->SetStringParameter("BodyName", centralBodyName);
      }
      
      #ifdef DEBUG_SET_FORCE_MODEL
      MessageInterface::ShowMessage
         ("   Adding type:<%s> name:<%s> to ForceModel:<%s>\n",
          pm->GetTypeName().c_str(), pm->GetName().c_str(),
          forceModel->GetName().c_str());
      #endif
      
      // Add force to ForceModel
      forceModel->AddForce(pm);
      
      #ifdef DEBUG_SET_FORCE_MODEL
      MessageInterface::ShowMessage("Interpreter::SetForceModelProperty() returning true\n");
      #endif
      return true;
   }
   // User defined forces
   else if (pmType == "UserDefined")
   {
      StringArray udForces = theTextParser.SeparateBrackets(value, "{}", " ,");
      
      for (UnsignedInt i=0; i<udForces.size(); i++)
      {
         #ifdef DEBUG_SET_FORCE_MODEL
            MessageInterface::ShowMessage("   User defined force[%d] = %s\n", 
                  i, udForces[i].c_str());
         #endif
         
         // We don't want to configure PhysicalModel, so set name after create
         PhysicalModel *pm = (PhysicalModel*)CreateObject(udForces[i], "");
         if (pm)
         {
            pm->SetName(udForces[i]);
            forceModel->AddForce(pm);
         }
         else
            throw InterpreterException(
                        "User defined force \"" + udForces[i] + 
                        "\" cannot be created\n");
      }

   }
   
   
   //------------------------------------------------------------
   // Set ForceModel owned object properties
   //------------------------------------------------------------
   
   pmType = parts[0];
   forceType = ForceModel::GetScriptAlias(pmType);
   std::string propName = parts[count-1];
   
   #ifdef DEBUG_SET_FORCE_MODEL
   MessageInterface::ShowMessage
      ("   Setting pmType=%s, forceType=%s, propName=%s\n", pmType.c_str(),
       forceType.c_str(), propName.c_str());
   #endif
   
   GmatBase *owner;
   Integer propId;
   Gmat::ParameterType propType;
   if (FindPropertyID(forceModel, propName, &owner, propId, propType))
   {
      id = owner->GetParameterID(propName);
      type = owner->GetParameterType(id);
      retval = SetPropertyValue(owner, id, type, value);
      if (fromObj != NULL)
         owner->SetRefObject(fromObj, fromObj->GetType(), value);
   }
   
   #ifdef DEBUG_SET_FORCE_MODEL
   MessageInterface::ShowMessage
      ("Interpreter::SetForceModelProperty() returning %d\n", retval);
   #endif
   return retval;
}


//------------------------------------------------------------------------------
// bool SetSolarSystemProperty(GmatBase *obj, const std::string &prop,
//                            const std::string &value)
//------------------------------------------------------------------------------
bool Interpreter::SetSolarSystemProperty(GmatBase *obj, const std::string &prop,
                                         const std::string &value)
{
   #ifdef DEBUG_SET_SOLAR_SYS
   MessageInterface::ShowMessage
      ("Interpreter::SetSolarSystemProperty() type=%s, name=%s, prop=%s, value=%s\n",
       obj->GetTypeName().c_str(), obj->GetName().c_str(), prop.c_str(), value.c_str());
   #endif
   
   debugMsg = "In SetSolarSystemProperty()";
   bool retval = false;
   StringArray parts = theTextParser.SeparateDots(prop);
   Integer count = parts.size();
   SolarSystem *solarSystem = (SolarSystem *)obj;
   
   if (count == 1)
   {
      if (prop == "Ephemeris")
      {
         StringArray ephems = theTextParser.SeparateBrackets(value, "{}", " ,");
      
         #ifdef DEBUG_SET_SOLAR_SYS
         for (StringArray::iterator i = ephems.begin(); i != ephems.end(); ++i)
            MessageInterface::ShowMessage("   Source = %s\n", i->c_str());
         #endif
         
         theModerator->SetPlanetarySourceTypesInUse(ephems);
         retval = true;
      }
      else
      {
         Integer id = obj->GetParameterID(prop);
         Gmat::ParameterType type = obj->GetParameterType(id);
         retval = SetPropertyValue(obj, id, type, value);
      }
   }
   else
   {
      // Script has the form of:
      // GMAT SolarSystem.Earth.NutationUpdateInterval = 60.0;
      // GMAT SolarSystem.Earth.UseTTForEphemeris = true;
      // GMAT SolarSystem.Earth.DateFormat  = TAIModJulian;
      // GMAT SolarSystem.Earth.StateType   = Keplerian;
      // GMAT SolarSystem.Earth.InitalEpoch = 21544.500371
      // GMAT SolarSystem.Earth.SMA         = 149653978.978377
      
      std::string bodyName = parts[0];
      std::string newProp = parts[count-1];
      
      #ifdef DEBUG_SET_SOLAR_SYS
      MessageInterface::ShowMessage
         ("   bodyName=%s, newProp=%s\n", bodyName.c_str(), newProp.c_str());
      #endif
      
      // Cannot use FindPropertyID() because SolarSystem bodies have the
      // same property name. So use GetBody() instead.
      GmatBase *body = (GmatBase*)solarSystem->GetBody(bodyName);
      
      if (body == NULL)
      {
         InterpreterException ex
            ("Body: " + bodyName + " not found in the SolarSystem\n");
         HandleError(ex);
      }
      
      try
      {
         Integer id = body->GetParameterID(newProp);
         Gmat::ParameterType type = body->GetParameterType(id);
         retval = SetPropertyValue(body, id, type, value);
      }
      catch (BaseException &e)
      {
         HandleError(e);
      }
   }
   
   #ifdef DEBUG_SET_SOLAR_SYS
   MessageInterface::ShowMessage
      ("Interpreter::SetSolarSystemProperty() prop=%s, retval=%d\n",
       prop.c_str(), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool FindOwnedObject(GmatBase *owner, const std::string toProp,
//                      GmatBase **ownedObj, Integer &id, Gmat::OBJECT_TYPE &type)
//------------------------------------------------------------------------------
/*
 * Finds owned object and its property.
 *
 * @param  owner    Owner object to find owned object for property
 * @param  toProp   Property name to find
 * @param  id       Output owned property id (-1 if property not found)
 * @param  type     Output owned property type
 *                  (Gmat::UNKNOWN_PARAMETER_TYPE if property not found)
 *
 * @return  true if property found from the owned object
 */
//------------------------------------------------------------------------------
bool Interpreter::FindOwnedObject(GmatBase *owner, const std::string toProp,
                                  GmatBase **ownedObj, Integer &id,
                                  Gmat::ParameterType &type)
{
   #ifdef DEBUG_FIND_OBJECT
   MessageInterface::ShowMessage
      ("Interpreter::FindOwnedObject() owner=<%s>, toProp=<%s>\n",
       owner->GetName().c_str(), toProp.c_str());
   #endif
   
   debugMsg = "In FindOwnedObject()";
   bool retval = false;
   Integer ownedObjCount = owner->GetOwnedObjectCount();
   Integer errorCount = 0;
   GmatBase *tempObj = NULL;
   
   // Initialize output parameters
   id = -1;
   type = Gmat::UNKNOWN_PARAMETER_TYPE;
   
   #ifdef DEBUG_FIND_OBJECT
   MessageInterface::ShowMessage("   ownedObjCount=%d\n", ownedObjCount);
   #endif
   
   if (ownedObjCount > 0)
   {
      for (int i=0; i<ownedObjCount; i++)
      {
         tempObj = owner->GetOwnedObject(i);
         if (ownedObj)
         {
            #ifdef DEBUG_FIND_OBJECT
            MessageInterface::ShowMessage
               ("   i=%d, ownedObj type=<%s>, name=<%s>\n", i,
                tempObj->GetTypeName().c_str(), tempObj->GetName().c_str());
            #endif
            
            try
            {
               id = tempObj->GetParameterID(toProp);
               type = tempObj->GetParameterType(id);
               *ownedObj = tempObj;
               retval = true;
               break;
            }
            catch (BaseException &e)
            {
               errorCount++;
               continue;
            }
         }
      }
      
      if (errorCount == ownedObjCount)
      {
         // Throw error only when parsing delayed block, so that
         // duplicated error message will not be shown
         if (parsingDelayedBlock)
         {
            //@todo
            // Currently SolarSystem parameter is handled by the Moderator,
            // so it is an exceptional case.
            // Eventually we want to move parameter handling to SolarSyatem.
            if (owner->GetName() != "SolarSystem")
            {
               InterpreterException ex
                  ("The field name \"" + toProp + "\" on object " + owner->GetName() +
                   " is not permitted");
               HandleErrorMessage(ex, lineNumber, currentLine, true);
            }
         }
      }
   }
   
   #ifdef DEBUG_FIND_OBJECT
   MessageInterface::ShowMessage
      ("   FindOwnedObject() returning retval=%d, ownedObj=%p\n", retval, *ownedObj);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// Real GetArrayValue(const std::string &arrayStr, Integer &row, Integer &col)
//------------------------------------------------------------------------------
/**
 * Retrives configured array value by row and col.
 *
 * @param  arrayStr  String form of array (A(1,3), B(2,j), etc)
 *
 * @note Array name must be created and configured before access.
 */
//------------------------------------------------------------------------------
Real Interpreter::GetArrayValue(const std::string &arrayStr,
                                Integer &row, Integer &col)
{
   #ifdef DEBUG_SET
   MessageInterface::ShowMessage
      ("Interpreter::GetArrayValue arrayStr=%s\n", arrayStr.c_str());
   #endif
   
   debugMsg = "In GetArrayValue()";
   Parameter *param = GetArrayIndex(arrayStr, row, col);
   
   if (row != -1 && col != -1)
      return param->GetRealParameter("SingleValue", row, col);
   else
   {
      InterpreterException ex("Invalid row and column index\n");
      HandleError(ex);
   }
   return 0.0;
}


//------------------------------------------------------------------------------
// bool IsArrayElement(const std::string &str)
//------------------------------------------------------------------------------
bool Interpreter::IsArrayElement(const std::string &str)
{
   bool retval = false;
   
   if (str.find("[") != str.npos)
   {
      InterpreterException ex("\"" + str + "\" is not a valid Array element");
      HandleError(ex);
   }
   
   retval = GmatStringUtil::IsParenPartOfArray(str);

   #ifdef DEBUG_ARRAY_GET
   MessageInterface::ShowMessage
      ("Interpreter::IsArrayElement() str=%s, array=%d\n", str.c_str(), retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool ParseVariableExpression(Parameter *var, const std::string &exp)
//------------------------------------------------------------------------------
bool Interpreter::ParseVariableExpression(Parameter *var, const std::string &exp)
{
   if (var == NULL)
   {
      InterpreterException ex
         ("Interpreter::ParseVariableExpression() The variable is NULL\n");
      HandleError(ex);
      return false;
   }
   
   #ifdef DEBUG_VAR_EXPRESSION
   MessageInterface::ShowMessage
      ("Interpreter::ParseVariableExpression() entered, var=<%p>'%s', exp='%s'\n",
       var, var->GetName().c_str(), exp.c_str());
   #endif
   
   // Check for invalid starting name such as 1(x) should give an error (loj: 2008.08.15)
   if (exp.find_first_of("(") != exp.npos)
   {
      if (!GmatStringUtil::IsValidName(exp, true))
      {
         #ifdef DEBUG_VAR_EXPRESSION
         MessageInterface::ShowMessage
            ("Interpreter::ParseVariableExpression() returning false, '%s' is not "
             "a valid name\n", exp.c_str());
         #endif
         return false;
      }
   }
   
   // Parse the Parameter
   StringTokenizer st(exp, "()*/+-^ ");
   StringArray tokens = st.GetAllTokens();
   Real rval;
   
   // Check if unexisting varibles used in expression
   for (unsigned int i=0; i<tokens.size(); i++)
   {
      #ifdef DEBUG_VAR_EXPRESSION
      MessageInterface::ShowMessage("   token:<%s> \n", tokens[i].c_str());
      #endif
      
      if (!GmatStringUtil::ToReal(tokens[i], rval))
      {
         #ifdef DEBUG_VAR_EXPRESSION
         MessageInterface::ShowMessage
            ("   It is not a number, so trying to create a Parameter\n");
         #endif
         
         Parameter *param = CreateSystemParameter(tokens[i]);
         if (param)
         {
            #ifdef DEBUG_VAR_EXPRESSION
            MessageInterface::ShowMessage
               ("   The Parameter '%s' found or created, so setting it to '%s' "
                "as ref object name\n",  param->GetName().c_str(), var->GetName().c_str());
            #endif
            // set parameter names used in expression
            var->SetRefObjectName(Gmat::PARAMETER, tokens[i]);
         }
         else
         {
            #ifdef DEBUG_VAR_EXPRESSION
            MessageInterface::ShowMessage
               ("Interpreter::ParseVariableExpression() returning false "
                "since '%s' is not allowed in the expression\n", tokens[i].c_str());
            #endif
            
            //InterpreterException ex
            //   ("The Variable \"" + tokens[i] + "\" does not exist. "
            //    "It must be created first");
            //HandleError(ex);
            return false;
         }
      }
   }
   
   var->SetStringParameter("Expression", exp);
   
   return true;
}


//------------------------------------------------------------------------------
// AxisSystem* CreateAxisSystem(std::string type, GmatBase *owner)
//------------------------------------------------------------------------------
AxisSystem* Interpreter::CreateAxisSystem(std::string type, GmatBase *owner)
{
   AxisSystem *axis = theValidator->CreateAxisSystem(type, owner);
   
   // Handle error messages here
   if (axis == NULL)
   {
      StringArray errList = theValidator->GetErrorList();
      for (UnsignedInt i=0; i<errList.size(); i++)
         HandleError(InterpreterException(errList[i]));
   }
   
   return axis;
}


//------------------------------------------------------------------------------
// void HandleError(const BaseException &e, bool writeLine, bool warning ...)
//------------------------------------------------------------------------------
void Interpreter::HandleError(const BaseException &e, bool writeLine, bool warning)
{
   if (writeLine)
   {
      lineNumber = GmatStringUtil::ToString(theReadWriter->GetLineNumber());
      currentLine = theReadWriter->GetCurrentLine();
      
      HandleErrorMessage(e, lineNumber, currentLine, writeLine, warning);
   }
   else
   {
      HandleErrorMessage(e, "", "", writeLine, warning);
   }
}


//------------------------------------------------------------------------------
// void HandleErrorMessage(const BaseException &e, const std::string &lineNumber...)
//------------------------------------------------------------------------------
void Interpreter::HandleErrorMessage(const BaseException &e,
                                     const std::string &lineNumber,
                                     const std::string &line,
                                     bool writeLine, bool warning)
{
   std::string currMsg = "";
   std::string msgKind = "**** ERROR **** ";
   if (warning)
      msgKind = "*** WARNING *** ";
   
   // Added function name in the message (loj: 2008.08.29)
   std::string fnMsg;
   if (currentFunction != NULL)
   {
      fnMsg = currentFunction->GetFunctionPathAndName();
      fnMsg = "(In Function \"" + fnMsg + "\")\n";
   }
   
   if (writeLine)
      currMsg = " in line:\n" + fnMsg + "   \"" + lineNumber + ": " + line + "\"\n";
   else
      currMsg = "\n" + fnMsg;
   
   std::string msg = msgKind + e.GetFullMessage() + currMsg;
   
   #ifdef DEBUG_HANDLE_ERROR
   MessageInterface::ShowMessage(debugMsg + "\n");
   #endif
   
   if (continueOnError)
   {
      errorList.push_back(msg);
      
      #ifdef DEBUG_HANDLE_ERROR
      MessageInterface::ShowMessage(msg + "\n");
      #endif
   }
   else
   {
      if (warning)
         MessageInterface::ShowMessage(msg);
      else
         throw InterpreterException(msg);
   }
}


//------------------------------------------------------------------------------
// bool IsBranchCommand(const std::string &str)
//------------------------------------------------------------------------------
bool Interpreter::IsBranchCommand(const std::string &str)
{
   StringArray parts = theTextParser.SeparateSpaces(str);
   
   if (parts[0] == "If" || parts[0] == "EndIf" ||
       parts[0] == "For" || parts[0] == "EndFor" ||
       parts[0] == "While" || parts[0] == "EndWhile" ||
       parts[0] == "Target" || parts[0] == "EndTarget" ||
       parts[0] == "Optimize" || parts[0] == "EndOptimize" ||
       parts[0] == "BeginScript" || parts[0] == "EndScript")
      return true;
   else
      return false;
   
}


//------------------------------------------------------------------------------
// bool CheckBranchCommands(const IntegerArray &lineNumbers,
//                          const StringArray &lines,)
//------------------------------------------------------------------------------
/**
 * Checks branch command matching end command.
 *
 * @return true if the all matches, false otherwise
 */
//------------------------------------------------------------------------------
bool Interpreter::CheckBranchCommands(const IntegerArray &lineNumbers,
                                      const StringArray &lines)
{
   #ifdef DEBUG_CHECK_BRANCH
   MessageInterface::ShowMessage("Interpreter::CheckBranchCommands()\n");
   for (UnsignedInt i=0; i<lines.size(); i++)
      MessageInterface::ShowMessage("%d: %s\n", lineNumbers[i], lines[i].c_str());
   #endif
   
   // Check for unbalaced branch commands
   
   debugMsg = "In CheckBranchCommands()";
   std::stack<std::string> controlStack;
   std::string expEndStr, str, str1;
   bool retval = true;
   
   #ifdef DEBUG_CHECK_BRANCH
   MessageInterface::ShowMessage("   Now start checking\n");
   #endif
   
   for (UnsignedInt i=0; i<lines.size(); i++)
   {
      str = lines[i];
      
      #ifdef DEBUG_CHECK_BRANCH
      MessageInterface::ShowMessage
         ("   line=%d, str=%s\n", lineNumbers[i], str.c_str());
      #endif
      
      if (GmatStringUtil::StartsWith(str, "End"))
      {
         if (controlStack.empty())
         {
            InterpreterException ex("Found too many \"" + str + "\"");
            HandleErrorMessage(ex, GmatStringUtil::ToString(lineNumbers[i]), str);
            retval = false;
            break;
         }
         
         str1 = controlStack.top();
         controlStack.pop();
         
         if (str1 == "BeginScript")
            expEndStr = "EndScript";
         else
            expEndStr = "End" + str1;
         
         if (expEndStr != str)
         {
            InterpreterException ex
               ("Expecting \"" + expEndStr + "\" but found \"" + str + "\"");
            HandleErrorMessage(ex, GmatStringUtil::ToString(lineNumbers[i]), str);
            retval = false;
            break;
         }
      }
      else
      {
         controlStack.push(str);
      }
   }
   
   
   if (retval == true)
   {
      if (!controlStack.empty())
      {
         InterpreterException ex
            ("Matching \"End" + controlStack.top() + "\" not found for \"" +
             controlStack.top() + "\"");
         HandleError(ex, false);
         retval = false;
      }
   }
   
   #ifdef DEBUG_CHECK_BRANCH
   MessageInterface::ShowMessage
      ("Interpreter::CheckBranchCommands() returning %d\n", retval);
   #endif

   return retval;
}


//------------------------------------------------------------------------------
// bool FinalPass()
//------------------------------------------------------------------------------
/**
 * Finishes up the Interpret call by setting internal references that are needed 
 * by the GUI.
 *
 * @return true if the references were set; false otherwise.
 *
 * @note: Most objects has reference objects already set in the SetObject*(),
 *        if paramter type is OBJECT_TYPE, so not requiring additional call to
 *        SetRefObject()
 */
//------------------------------------------------------------------------------
bool Interpreter::FinalPass()
{
   #if DBGLVL_FINAL_PASS
   MessageInterface::ShowMessage("Interpreter::FinalPass() entered\n");
   #endif
   
   debugMsg = "In FinalPass()";
   bool retval = true;
   GmatBase *obj = NULL;
   GmatBase *refObj;
   StringArray refNameList;
   std::string objName;
   StringArray objList;
   
   objList = theModerator->GetListOfObjects(Gmat::UNKNOWN_OBJECT);
   
   #if DBGLVL_FINAL_PASS > 0 //------------------------------ debug ----
   MessageInterface::ShowMessage("FinalPass:: All object list =\n");
   for (Integer ii = 0; ii < (Integer) objList.size(); ii++)
      MessageInterface::ShowMessage("   %s\n", (objList.at(ii)).c_str());
   #endif //------------------------------------------- end debug ----
   
   //----------------------------------------------------------------------
   // Check reference objects
   //----------------------------------------------------------------------
   for (StringArray::iterator i = objList.begin(); i != objList.end(); ++i)
   {
      obj = FindObject(*i);
      
      #if DBGLVL_FINAL_PASS > 1
      MessageInterface::ShowMessage
         ("Checking ref. object on %s:%s\n", obj->GetTypeName().c_str(),
          obj->GetName().c_str());
      #endif
      
      // check System Parameters seperately since it follows certain naming
      // convention.  "owner.dep.type" where owner can be either Spacecraft
      // or Burn for now
      
      if (obj->GetType() == Gmat::PARAMETER)
      {
         std::string type, owner, depObj;
         Parameter *param = (Parameter*)obj;
         
         if (param->GetKey() == GmatParam::SYSTEM_PARAM)
         {
            objName = obj->GetName();            
            GmatStringUtil::ParseParameter(objName, type, owner, depObj);
            
            // Since we can create a system parameter as: Create A1ModJulian Time,
            // we don't want to check if owner is blank.
            if (owner != "")
            {
               refObj = FindObject(owner);
               if (refObj == NULL)
               {
                  InterpreterException ex
                     ("Nonexistent object \"" + owner + "\" referenced in \"" +
                      obj->GetName() + "\"");
                  HandleError(ex, false);
                  retval = false;
               }
               else if (param->GetOwnerType() != refObj->GetType())
               {
                  InterpreterException ex
                     ("\"" + type + "\" is not property of \"" +
                      refObj->GetTypeName() + "\"");
                  HandleError(ex, false);
                  retval = false;
               }
            }
         }
      }
      
      // check Function seperately since it has inputs that can be any object type,
      // including Real number (1234.5678) and String literal ('abc')
      //
      // We don't want to check this in the FinalPass(), since it will be checked
      // when ScriptInterpreter::InterpretGmatFunction() is called
      else if (obj->GetType() == Gmat::FUNCTION)
      {
         // If GmatFunction, see if function file exist and the function name
         // matches the file name
         if (obj->GetTypeName() == "GmatFunction")
         {
            std::string funcPath = obj->GetStringParameter("FunctionPath");
            #if DBGLVL_FUNCTION_DEF > 0
            MessageInterface::ShowMessage
               ("Interpreter::FinalPass() calling CheckFunctionDefinition()\n");
            #endif
            retval = CheckFunctionDefinition(funcPath, obj, false);
         }
      }
      //
      //-----------------------------------------------------------------
      // Note: This section needs be modified as needed. 
      // GetRefObjectTypeArray() should be implemented if we want to
      // add to this list. This was added to write specific error messages.
      //-----------------------------------------------------------------
      
      else if (obj->GetType() == Gmat::BURN ||
               obj->GetType() == Gmat::SPACECRAFT ||
               obj->GetType() == Gmat::FORCE_MODEL ||
               obj->GetType() == Gmat::COORDINATE_SYSTEM ||
               obj->GetType() == Gmat::CALCULATED_POINT ||
               obj->GetType() == Gmat::SUBSCRIBER)
      {
         // Set return flag to false if any check failed
         try
         {
            bool retval1 = CheckUndefinedReference(obj, false);
            retval = retval && retval1;
            
            // Subscribers uses ElementWrapper to handle Parameter, Variable,
            // Array, Array elements, so create wrappers in ValidateSubscriber()
            if (retval && obj->GetType() == Gmat::SUBSCRIBER)
               retval = retval && ValidateSubscriber(obj);
         }
         catch (BaseException &ex)
         {
            HandleError(ex, false);
            retval = false;
         }
      }
      else
      {
         try
         {
            // Check referenced SpacePoint used by given objects
            refNameList = obj->GetRefObjectNameArray(Gmat::SPACE_POINT);
            
            for (UnsignedInt j = 0; j < refNameList.size(); j++)
            {
               refObj = FindObject(refNameList[j]);
               if ((refObj == NULL) || !(refObj->IsOfType(Gmat::SPACE_POINT)))
               {
                  #if DBGLVL_FINAL_PASS > 1
                  MessageInterface::ShowMessage
                     ("   refNameList[%d]=%s\n", j, refNameList[j].c_str());
                  #endif
                  
                  InterpreterException ex
                     ("Nonexistent SpacePoint \"" + refNameList[j] +
                      "\" referenced in \"" + obj->GetName() + "\"");
                  HandleError(ex, false);
                  retval = false;
               }
            }
         }
         catch (BaseException &e)
         {
            #if DBGLVL_FINAL_PASS
            MessageInterface::ShowMessage(e.GetFullMessage());
            #endif
         }
      }
   }
   
   //-------------------------------------------------------------------
   // Special check for LibrationPoint.
   // Since the order of setting primary and secondary bodies can be
   // different, it cannot check for the same bodies in the base code
   // LibrationPoint::SetStringParameter(). Instead the checking is done
   // in here.  This allows repeated setting of bodies as shown in the
   // following script.
   //    GMAT Libration1.Primary = Sun;
   //    GMAT Libration1.Secondary = Earth;
   //    GMAT Libration1.Primary = Earth;
   //    GMAT Libration1.Secondary = Luna;
   //-------------------------------------------------------------------
   objList = theModerator->GetListOfObjects(Gmat::CALCULATED_POINT);
   
   #if DBGLVL_FINAL_PASS > 1
   MessageInterface::ShowMessage("FinalPass:: CalculatedPoint list =\n");
   for (Integer ii = 0; ii < (Integer) objList.size(); ii++)
      MessageInterface::ShowMessage("   %s\n", (objList.at(ii)).c_str());
   #endif
   
   for (StringArray::iterator i = objList.begin(); i != objList.end(); ++i)
   {
      obj = FindObject(*i);
      refNameList = obj->GetRefObjectNameArray(Gmat::SPACE_POINT);
      
      if (obj->GetTypeName() == "LibrationPoint")
      {
         std::string primary = obj->GetStringParameter("Primary");
         std::string secondary = obj->GetStringParameter("Secondary");
         
         #if DBGLVL_FINAL_PASS > 1
         MessageInterface::ShowMessage
            ("   primary=%s, secondary=%s\n", primary.c_str(), secondary.c_str());
         #endif
         
         if (primary == secondary)
         {
            InterpreterException ex
               ("The Primay and Secondary bodies cannot be the same in the "
                "LibrationPoint \"" + obj->GetName() + "\"");
            HandleError(ex, false);
            retval = false;
         }
      }
      
      //----------------------------------------------------------------
      // Now set ref objects to CalculatedPoint objects
      //----------------------------------------------------------------
      
      #if DBGLVL_FINAL_PASS > 1
      MessageInterface::ShowMessage
         ("   Setting RefObject on obj=%s\n", obj->GetName().c_str());
      #endif
      for (UnsignedInt j = 0; j < refNameList.size(); j++)
      {
         #if DBGLVL_FINAL_PASS > 1
         MessageInterface::ShowMessage
            ("   refNameList[%d]=%s\n", j, refNameList[j].c_str());
         #endif
         
         refObj = FindObject(refNameList[j]);
         if (refObj)
            obj->SetRefObject(refObj, Gmat::SPACE_POINT, refObj->GetName());
      }
   }
   
   
   //----------------------------------------------------------------------
   // Initialize CoordinateSystem
   //----------------------------------------------------------------------
   objList = theModerator->GetListOfObjects(Gmat::COORDINATE_SYSTEM);
   
   #if DBGLVL_FINAL_PASS > 1//------------------------------ debug ----
   MessageInterface::ShowMessage("FinalPass:: CoordinateSystem list =\n");
   for (Integer ii = 0; ii < (Integer) objList.size(); ii++)
      MessageInterface::ShowMessage("    %s\n", (objList.at(ii)).c_str());
   #endif //------------------------------------------- end debug ----
   
   objList = theModerator->GetListOfObjects(Gmat::COORDINATE_SYSTEM);
   for (StringArray::iterator i = objList.begin(); i != objList.end(); ++i)
   {
      CoordinateSystem *cs = (CoordinateSystem*)FindObject(*i);
      #if DBGLVL_FINAL_PASS > 1
      MessageInterface::ShowMessage("Initializing CoordinateSystem '%s'\n",
                                    i->c_str());
      #endif
      refNameList = cs->GetRefObjectNameArray(Gmat::SPACE_POINT);
      for (UnsignedInt j = 0; j < refNameList.size(); j++)
      {
         #if DBGLVL_FINAL_PASS > 1
         MessageInterface::ShowMessage
            ("   refNameList[%d]=%s\n", j, refNameList[j].c_str());
         #endif
         
         refObj = FindObject(refNameList[j]);
         if ((refObj == NULL) || !(refObj->IsOfType(Gmat::SPACE_POINT)))
         {            
            InterpreterException ex
               ("Nonexistent SpacePoint \"" + refNameList[j] +
                "\" referenced in \"" + obj->GetName() + "\"");
            HandleError(ex, false);
            retval = false;
         }
         else
         {
            cs->SetRefObject(refObj, Gmat::SPACE_POINT, refObj->GetName());
         }
      }
      cs->Initialize();
   }
   
   //-------------------------------------------------------------------
   // Special case for Spacecraft, we need to set CoordinateSyatem
   // pointer in which initial state is represented.  So that
   // Spacecraft can convert initial state in user representation to
   // internal representation (EarthMJ2000Eq Cartesian).
   //-------------------------------------------------------------------
   objList = theModerator->GetListOfObjects(Gmat::SPACECRAFT);

   #if DBGLVL_FINAL_PASS > 1
   MessageInterface::ShowMessage("FinalPass:: Spacecraft list =\n");
   for (Integer ii = 0; ii < (Integer) objList.size(); ii++)
      MessageInterface::ShowMessage("   %s\n", (objList.at(ii)).c_str());
   #endif
   
   for (StringArray::iterator i = objList.begin(); i != objList.end(); ++i)
   {
      obj = FindObject(*i);
      
      std::string csName = obj->GetRefObjectName(Gmat::COORDINATE_SYSTEM);
      GmatBase *csObj = FindObject(csName);
      
      // To catch as many errors we can, continue with next object
      if (csObj == NULL)
         continue;
      
      #if DBGLVL_FINAL_PASS > 1
      MessageInterface::ShowMessage
         ("   Calling '%s'->SetRefObject(%s(%p), %d)\n", obj->GetName().c_str(),
          csObj->GetName().c_str(), csObj, csObj->GetType());
      #endif
      
      if (csObj->GetType() != Gmat::COORDINATE_SYSTEM)
      {
         InterpreterException ex
            ("The Spacecraft \"" + obj->GetName() + "\" failed to set "
             "\"CoordinateSystem\" to \"" + csName + "\"");
         HandleError(ex, false);
         retval = false;
         continue;
      }
      
      try
      {
         obj->SetRefObject(csObj, Gmat::COORDINATE_SYSTEM, csObj->GetName());
      }
      catch (BaseException &e)
      {
         InterpreterException ex
            ("The Spacecraft \"" + obj->GetName() + "\" failed to set "
             "CoordinateSystem: " + e.GetFullMessage());
         HandleError(ex, false);
         retval = false;
         continue;
      }
   }
   
   #if DBGLVL_FINAL_PASS
   MessageInterface::ShowMessage("Interpreter::FinalPass() returning %d\n", retval);
   #endif
   
   return retval;
}


//----------------------------------
// Private
//----------------------------------

//------------------------------------------------------------------------------
// bool IsObjectType(const std::string &type)
//------------------------------------------------------------------------------
/*
 * Returns true if input string is one of Object type that can be created.
 */
//------------------------------------------------------------------------------
bool Interpreter::IsObjectType(const std::string &type)
{
   if (type == "Spacecraft") 
      return true;
   
   if (type == "Formation") 
      return true;
   
   if (type == "Propagator") 
      return true;
   
   if (type == "ForceModel") 
      return true;
   
   if (type == "CoordinateSystem") 
      return true;
   
   if (theSolarSystem->IsBodyInUse(type))
      return true;
   
   if (find(propagatorList.begin(), propagatorList.end(), type) !=
       propagatorList.end())
      return true;
   
   if (find(axisSystemList.begin(), axisSystemList.end(), type) !=
       axisSystemList.end())
      return true;
   
   if (find(atmosphereList.begin(), atmosphereList.end(), type) !=
       atmosphereList.end())
      return true;
   
   if (find(attitudeList.begin(), attitudeList.end(), type) !=
       attitudeList.end())
      return true;
   
   if (find(burnList.begin(), burnList.end(), type) != burnList.end())
      return true;

   if (find(calculatedPointList.begin(), calculatedPointList.end(), type) != 
       calculatedPointList.end()) 
      return true;
   
   if (find(functionList.begin(), functionList.end(), type) != 
       functionList.end())
      return true;
   
   if (find(hardwareList.begin(), hardwareList.end(), type) != 
       hardwareList.end())
      return true;

   if (find(parameterList.begin(), parameterList.end(), type) != 
       parameterList.end())
      return true;
   
   if (find(physicalModelList.begin(), physicalModelList.end(), type) != 
       physicalModelList.end())
      return true;
   
   if (find(solverList.begin(), solverList.end(), type) != 
       solverList.end())
      return true;
   
   if (find(subscriberList.begin(), subscriberList.end(), type) != 
       subscriberList.end())
      return true;
   
   return false;
}


//------------------------------------------------------------------------------
// bool IsParameterType(const std::string &desc)
//------------------------------------------------------------------------------
/*
 * Checks if input description is a Parameter.
 * If desctiption has dots, it will parse the components into Object, Depdency,
 * and Type. If type is one of the system parameters, it will return true.
 *
 * @param  desc  Input string to check for Parameter type
 * @return  true  if type is a Parameter type
 */
//------------------------------------------------------------------------------
bool Interpreter::IsParameterType(const std::string &desc)
{
   return theValidator->IsParameterType(desc);
}


//------------------------------------------------------------------------------
// bool CheckForSpecialCase(GmatBase *obj, Integer id, std::string &value)
//------------------------------------------------------------------------------
/**
 * Handles special alias for gravity field type.
 * such as JGM2, JGM3, EGM96, LP165P, etc.
 *
 * @param  obj    Pointer to the object that owns the parameter.
 * @param  id     ID for the parameter.
 * @param  value  Input/Output value of the parameter.
 */
//------------------------------------------------------------------------------
bool Interpreter::CheckForSpecialCase(GmatBase *obj, Integer id, 
                                     std::string &value)
{
   bool retval = false;
   std::string val = value;
   
   #ifdef DEBUG_SPECIAL_CASE
   MessageInterface::ShowMessage
      ("Entered CheckForSpecialCase with \"" + value +
       "\" being set on parameter \"" + obj->GetParameterText(id) + 
       "\" for a \"" + obj->GetTypeName() + "\" object\n");
   #endif
   
   // JGM2, JGM3, EGM96, LP165P, etc.  are special strings in GMAT; handle them here
   if ((obj->GetTypeName() == "GravityField") &&
       (obj->GetParameterText(id) == "PotentialFile"))
   {
      val = theModerator->GetPotentialFileName(value);
      if (val.find("Unknown Potential File Type") == std::string::npos)
      {
         value = val;
         retval = true;
      }
   }
   
   #ifdef DEBUG_SPECIAL_CASE
   MessageInterface::ShowMessage
      ("Leaving CheckForSpecialCase() value=%s, retval=%d\n", value.c_str(),
       retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// void WriteStringArray(const std::string &title1, const std::string &title2,
//                       const StringArray &parts)
//------------------------------------------------------------------------------
void Interpreter::WriteStringArray(const std::string &title1,
                                   const std::string &title2,
                                   const StringArray &parts)
{
   MessageInterface::ShowMessage("   ========== %s%s, has %d parts\n",
                                 title1.c_str(), title2.c_str(), parts.size());
   for (UnsignedInt i=0; i<parts.size(); i++)
      MessageInterface::ShowMessage("   %d: '%s'\n", i, parts[i].c_str());
   MessageInterface::ShowMessage("\n");
}


//------------------------------------------------------------------------------
// bool CheckFunctionDefinition(const std::string &funcPath, GmatBase *function,
//                              bool fullCheck)
//------------------------------------------------------------------------------
/*
 * Opens function file and checks if it has valid function definition line.
 *
 * @param  funcPath  The full path and name of fuction file
 * @param  function  The Function pointer
 * @param  fullCheck set to true if checking fullly for input and output arguments
 *
 */
//------------------------------------------------------------------------------
bool Interpreter::CheckFunctionDefinition(const std::string &funcPath,
                                          GmatBase *function, bool fullCheck)
{
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage
      ("Interpreter::CheckFunctionDefinition() function=<%p>,\n   funcPath=<%s>\n",
       function, funcPath.c_str());
   #endif
   
   debugMsg = "In CheckFunctionDefinition()";
   bool retval = true;
   
   if (function == NULL)
   {
      MessageInterface::ShowMessage
         ("** INTERNAL ERROR ** Cannot check function definition. "
          "function pointer is NULL\n");
      retval = false;;
   }
   
   // check if function path exist
   if (!GmatFileUtil::DoesFileExist(funcPath))
   {
      InterpreterException ex
         ("Nonexistent GmatFunction file \"" + funcPath +
          "\" referenced in \"" + function->GetName() + "\"\n");
      HandleError(ex, false);
      retval = false;
   }
   
   // check for no extension of .gmf or wrong extenstion
   StringArray parts = GmatStringUtil::SeparateBy(funcPath, ".");
   if ((parts.size() == 1) ||
       (parts.size() == 2 && parts[1] != "gmf"))
   {
      InterpreterException ex
         ("The GmatFunction file \"" + funcPath + "\" has no or incorrect file "
          "extension referenced in \"" + function->GetName() + "\"\n");
      HandleError(ex, false);
      retval = false;
   }
   
   if (!retval || !fullCheck)
   {
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage
         ("Interpreter::CheckFunctionDefinition() returning false, since it's "
          "not full checking\n");
      #endif
      return retval;
   }
   
   // check function declaration
   std::ifstream inStream(funcPath.c_str());
   std::string line;
   StringArray outputArgs;
   
   while (!inStream.eof())
   {
      // Use cross-platform getline()
      if (!GmatFileUtil::GetLine(&inStream, line))
      {
         InterpreterException ex
            ("Error reading the GamtFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
         break;
      }
      
      #if DBGLVL_FUNCTION_DEF > 1
      MessageInterface::ShowMessage("   line=<%s>\n", line.c_str());
      #endif
      
      line = GmatStringUtil::Trim(line, GmatStringUtil::BOTH, true, true);
      
      // Skip empty line or comment line
      if (line[0] == '\0' || line[0] == '%')
         continue;
      
      //------------------------------------------------------
      // Parse function definition line
      //------------------------------------------------------
      bool hasOutput = false;
      if (line.find("=") != line.npos)
         hasOutput = true;
      
      StringArray parts;
      if (hasOutput)
         parts = GmatStringUtil::SeparateBy(line, "=", true);
      else
         parts = GmatStringUtil::SeparateBy(line, " ", true);
      
      StringArray::size_type numParts = parts.size();
      
      #if DBGLVL_FUNCTION_DEF > 1
      WriteStringArray("GmatFunction parts", "", parts);
      #endif
         
      StringArray lhsParts;
        
      try
      {
         lhsParts = theTextParser.Decompose(parts[0], "[]", false);
      }
      catch (BaseException &e)
      {
         InterpreterException ex
            ("Invalid output argument list found in the GamtFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
         break;
      }
      
      StringArray::size_type numLeft = lhsParts.size();
      
      #if DBGLVL_FUNCTION_DEF > 1
      WriteStringArray("GmatFunction lhsParts", "", lhsParts);
      #endif
      
      //------------------------------------------------------
      // Check if first part is "function"
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check if first part is function\n");
      #endif
      
      if (numLeft > 0 && lhsParts[0] != "function")
      {
         InterpreterException ex
            ("The \"function\" is missing in the GamtFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
         break;
      }
      
      //------------------------------------------------------
      // Check for valid output arguments
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check for output arguments\n");
      #endif
      
      if (hasOutput)
      {
         try
         {
            outputArgs =
               theTextParser.SeparateBrackets(lhsParts[1], "[]", ",");
            
            #if DBGLVL_FUNCTION_DEF > 1
            WriteStringArray("GmatFunction outputArgs", "", outputArgs);
            #endif
         }
         catch (BaseException &e)
         {
            InterpreterException ex
               ("Invalid output argument list found in the GamtFunction file \"" +
                funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
            HandleError(ex, false);
            retval = false;
            break;
         }
         
         
         if (outputArgs.size() == 0)
         {
            InterpreterException ex
               ("The output argument list is empty in the GamtFunction file \"" +
                funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
            HandleError(ex, false);
            retval = false;
            break;
         }
      }
      
      //------------------------------------------------------
      // Check for missing function name
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check for missing function name\n");
      MessageInterface::ShowMessage("   hasOutput=%d, numLeft=%d, numParts=%d\n",
                                    hasOutput, numLeft, numParts);
      #endif
         
      if (numParts <= 1)
      {
         InterpreterException ex
            ("The function name not found in the GamtFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
         break;
      }
      
      //------------------------------------------------------
      // check function name and input arguments
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check for input arguments\n");
      #endif
      
      StringArray rhsParts;
      try
      {
         rhsParts = theTextParser.Decompose(parts[1], "()", false);
         
         #if DBGLVL_FUNCTION_DEF > 1
         WriteStringArray("GmatFunction rhsParts", "", rhsParts);
         #endif         
      }
      catch (BaseException &e)
      {
         InterpreterException ex
            ("The invalid input argument list found in the GmatFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
         break;
      }
      
      //------------------------------------------------------
      // Check if function name matches the file name
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check if file has matching function name\n");
      #endif
      
      std::string fileFuncName = rhsParts[0];
      std::string funcName = function->GetStringParameter("FunctionName");
      
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage
         ("   fileFuncName=<%s>, funcName=<%s>\n", fileFuncName.c_str(), funcName.c_str());
      #endif
      
      if (fileFuncName != funcName)
      {
         InterpreterException ex
            ("The function name \"" + fileFuncName + "\" does not match with the "
             "GmatFunction file name \"" + funcPath + "\" referenced in \"" +
             function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
      }
      
      //------------------------------------------------------
      // Check for valid input arguments
      //------------------------------------------------------
      #if DBGLVL_FUNCTION_DEF > 0
      MessageInterface::ShowMessage("   Check for input arguments\n");
      #endif
      if (rhsParts.size() > 1)
      {
         StringArray inputArgs;
         try
         {
            inputArgs =
               theTextParser.SeparateBrackets(rhsParts[1], "()", ",");
            
            #if DBGLVL_FUNCTION_DEF > 1
            WriteStringArray("GmatFunction inputArgs", "", inputArgs);
            #endif
         }
         catch (BaseException &e)
         {
            InterpreterException ex
               ("Invalid input argument list found in the GamtFunction file \"" +
                funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
            HandleError(ex, false);
            retval = false;
            break;
         }
         
         if (inputArgs.size() == 0)
         {
            InterpreterException ex
               ("The input argument list is empty in the GamtFunction file \"" +
                funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
            HandleError(ex, false);
            retval = false;
            break;
         }
         
         // check for duplicate input list
         if (inputArgs.size() > 1)
         {
            StringArray multiples;
            // check for duplicate input names
            for (UnsignedInt i=0; i<inputArgs.size(); i++)
            {
               for (UnsignedInt j=0; j<inputArgs.size(); j++)
               {
                  if (i == j)
                     continue;
                  
                  if (inputArgs[i] == inputArgs[j])
                     if (find(multiples.begin(), multiples.end(), inputArgs[i]) == multiples.end())
                        multiples.push_back(inputArgs[i]);
               }
            }
            
            if (multiples.size() > 0)
            {
               std::string errMsg = "Duplicate input of";
               
               for (UnsignedInt i=0; i<multiples.size(); i++)
                  errMsg = errMsg + " \"" + multiples[i] + "\"";
               
               InterpreterException ex
                  (errMsg + " found in the GmatFunction file \"" +
                   funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
               HandleError(ex, false);
               retval = false;
               break;
            }
         }
      }
      
      break;
   }
   
   if (line == "")
   {
      InterpreterException ex
         ("The GmatFunction file \"" + funcPath + "\" referenced in \"" +
          function->GetName() + "\" is empty\n");
      HandleError(ex, false);
      retval = false;
   }
   
   // if function definition has been validated, check if all outputs are declared
   if (retval && outputArgs.size() > 0)
   {
      std::string errMsg;
      IntegerArray rowCounts, colCounts;
      WrapperTypeArray outputTypes =
         GmatFileUtil::GetFunctionOutputTypes(&inStream, outputArgs, errMsg,
                                              rowCounts, colCounts);
      
      if (errMsg != "")
      {
         InterpreterException ex
            (errMsg + " found in the GmatFunction file \"" +
             funcPath + "\" referenced in \"" + function->GetName() + "\"\n");
         HandleError(ex, false);
         retval = false;
      }
      else
      {
         ((Function*)function)->SetOutputTypes(outputTypes, rowCounts, colCounts);
      }
   }
   
   inStream.close();
   
   
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage
      ("Interpreter::CheckFunctionDefinition() returning true\n");
   #endif
   
   return retval;
   
} // CheckFunctionDefinition()


//------------------------------------------------------------------------------
// bool BuildFunctionDefinition(const std::string &str)
//------------------------------------------------------------------------------
/*
 * Sets function inputs and output to function from valid function definition
 * string.
 *
 * Note: This methods assumes that input string already has passed function
 *       validation check
 */
//------------------------------------------------------------------------------
bool Interpreter::BuildFunctionDefinition(const std::string &str)
{
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage
      ("Interpreter::BuildFunctionDefinition() str=<%s>\n", str.c_str());
   #endif
   
   std::string lhs;
   std::string rhs;
   StringArray parts = theTextParser.SeparateBy(str, "=");
   
   #if DBGLVL_FUNCTION_DEF > 1
   WriteStringArray("parts", "", parts);
   #endif
   
   // if function has no output
   if (parts.size() == 1)
   {
      std::string::size_type index = str.find_first_of(" ");
      lhs = str.substr(0, index);
      rhs = str.substr(index+1);
   }
   else
   {
      lhs = parts[0];
      rhs = parts[1];
   }
   
   StringArray lhsParts = theTextParser.Decompose(lhs, "[]", false);
   StringArray rhsParts = theTextParser.Decompose(rhs, "()", false);
   
   #if DBGLVL_FUNCTION_DEF > 1
   WriteStringArray("lhsParts", "", lhsParts);
   WriteStringArray("rhsParts", "", rhsParts);
   #endif
   
   std::string funcName;
   
   if (lhsParts[0] != "function")
      return false;
   
   if (!GmatStringUtil::IsValidName(rhsParts[0], false))
      return false;
   
   StringArray inputs, outputs;
   
   //------------------------------------------------------
   // parse inputs
   //------------------------------------------------------
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage("   parse inputs\n");
   #endif
   
   if (rhsParts.size() > 1)
   {
      inputs = theTextParser.SeparateBy(rhsParts[1], ", ()");
      
      #if DBGLVL_FUNCTION_DEF > 1
      WriteStringArray("function inputs", "", inputs);
      #endif
   }
   
   //------------------------------------------------------
   // parse outputs
   //------------------------------------------------------
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage("   parse outputs\n");
   #endif
   if (lhsParts.size() > 1)
   {
      outputs = theTextParser.SeparateBy(lhsParts[1], ", []");
      
      #if DBGLVL_FUNCTION_DEF > 1
      WriteStringArray("function outputs", "", outputs);
      #endif
   }
   
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage
      ("   inFunctionMode=%d, currentFunction=<%p>\n", inFunctionMode,
       currentFunction);
   #endif
   
   //------------------------------------------------------
   // set inputs and outputs to current function
   //------------------------------------------------------
   if (inFunctionMode && currentFunction != NULL)
   {
      for (UnsignedInt i=0; i<inputs.size(); i++)
         currentFunction->SetStringParameter("Input", inputs[i]);

      for (UnsignedInt i=0; i<outputs.size(); i++)
         currentFunction->SetStringParameter("Output", outputs[i]);
   }
   
   hasFunctionDefinition = true;
   
   #if DBGLVL_FUNCTION_DEF > 0
   MessageInterface::ShowMessage
      ("Interpreter::BuildFunctionDefinition() returning true\n");
   #endif
   
   return true;
   
} // BuildFunctionDefinition()


//------------------------------------------------------------------------------
// bool Interpreter::HandleMathTree(GmatCommand *cmd)
//------------------------------------------------------------------------------
bool Interpreter::HandleMathTree(GmatCommand *cmd)
{
   #ifdef DEBUG_MATH_TREE
   MessageInterface::ShowMessage
      ("Interpreter::HandleMathTree() '%s', It is a math equation\n",
       cmd->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
   #endif
   
   Assignment *equation = (Assignment*)cmd;
   std::string lhs = equation->GetLHS();
   std::string rhs = equation->GetRHS();
   
   // Handle GmatFunction in math
   StringArray gmatFuns = equation->GetGmatFunctionNames();
   
   #ifdef DEBUG_MATH_TREE
   MessageInterface::ShowMessage("   Found %d GmatFunctions\n", gmatFuns.size());
   #endif
   
   for (UnsignedInt i=0; i<gmatFuns.size(); i++)
   {
      GmatBase *func = FindObject(gmatFuns[i]);
      Integer manage = 1;
      
      // Do not manage function if creating in function mode
      if (inFunctionMode)
         manage = 0;
      
      if (func == NULL)
         func = CreateObject("GmatFunction", gmatFuns[i], manage);
      
      #ifdef DEBUG_MATH_TREE
      MessageInterface::ShowMessage
         ("   Setting GmatFunction '%s'<%p> to equation<%p>\n",
          func->GetName().c_str(), func, equation);
      #endif
      
      equation->SetFunction((Function*)func);
   }
   
   #ifdef DEBUG_MATH_TREE
   MessageInterface::ShowMessage("Interpreter::HandleMathTree() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// void Interpreter::ClearTempObjectNames()
//------------------------------------------------------------------------------
/*
 * Clears temporary object name array.
 * tempObjectNames is used for finding MatlabFunction names.
 * This method is called from the ScriptInterpreter::InterpretGmatFunction()
 */
//------------------------------------------------------------------------------
void Interpreter::ClearTempObjectNames()
{
   tempObjectNames.clear();
}

