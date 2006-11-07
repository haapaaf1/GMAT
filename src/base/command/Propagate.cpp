//$Header$
//------------------------------------------------------------------------------
//                                 Propagate
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Darrel J. Conway
// Created: 2003/09/23
//
/**
 * Implementation for the Propagate command class
 */
//------------------------------------------------------------------------------

#include "Propagate.hpp"
#include "Publisher.hpp"
#include "Moderator.hpp"
#include "Parameter.hpp"
#include "StringUtil.hpp" // for Trim()
#include "MessageInterface.hpp"

#include <sstream>
#include <cmath>

//#define DEBUG_PROPAGATE_ASSEMBLE 1
//#define DEBUG_PROPAGATE_OBJ 1
//#define DEBUG_PROPAGATE_INIT 1
//#define DEBUG_PROPAGATE_DIRECTION 1
//#define DEBUG_PROPAGATE_STEPSIZE 1
//#define DEBUG_PROPAGATE_EXE 1
//#define DEBUG_STOPPING_CONDITIONS 1
//#define DEBUG_RENAME 1
//#define DEBUG_PROP_PERFORMANCE
//#define DEBUG_FIRST_CALL


#define TIME_ROUNDOFF 1.0e-6

//---------------------------------
// static data
//---------------------------------
std::string Propagate::PropModeList[PropModeCount] =
{
   "", "Synchronized"
};

#ifdef DEBUG_FIRST_CALL
static bool firstStepFired = false;
#endif

//---------------------------------
// public members
//---------------------------------

//------------------------------------------------------------------------------
//  Propagate()
//------------------------------------------------------------------------------
/**
 * Constructs the Propagate Command (default constructor).
 */
//------------------------------------------------------------------------------
Propagate::Propagate() :
   GmatCommand                 ("Propagate"),
   currentPropMode             (""),
   interruptCheckFrequency     (30),
   inProgress                  (false),
   hasFired                    (false),
   epochID                     (-1),
   stopInterval                (0.0),
   stopTrigger                 (-1),
   hasStoppedOnce              (false),
   stepsTaken                  (0),
   state                       (NULL),
   pubdata                     (NULL),
   stopCondMet                 (false),
   stopEpoch                   (0.0),
   dim                         (0),
   singleStepMode              (false),
   currentMode                 (INDEPENDENT),
   stopCondEpochID             (-1),
   stopCondBaseEpochID         (-1),
   stopCondStopVarID           (-1),
   // Set the parameter IDs
   availablePropModesID        (parameterCount),
   propCoupledID               (parameterCount+1),
   interruptCheckFrequencyID   (parameterCount+2),
   satNameID                   (parameterCount+3),
   propNameID                  (parameterCount+4),
   stopWhenID                  (parameterCount+5)
{
   parameterCount += 5;
}


//------------------------------------------------------------------------------
//  ~Propagate()
//------------------------------------------------------------------------------
/**
 * Destroys the Propagate Command.
 */
//------------------------------------------------------------------------------
Propagate::~Propagate()
{
   /// @todo: clean memory for satName.push_back(new StringArray);
   
   EmptyBuffer();
 
   for (UnsignedInt i=0; i<stopWhen.size(); i++)
      delete stopWhen[i];
   if (pubdata)
      delete [] pubdata;
      
   // Remove PropSetup clones; these are not currently cloned but soon...
   for (std::vector<PropSetup*>::iterator ps = prop.begin(); ps != prop.end(); 
        ++ps)
   {
      PropSetup *oldPs = *ps;
      *ps = NULL;
      delete oldPs;
   }
}


//------------------------------------------------------------------------------
//  Propagate(const Propagate &prp)
//------------------------------------------------------------------------------
/**
 * Constructs a Propagate Command based on another instance (copy constructor).
 *
 * @param <p> Original we are copying
 */
//------------------------------------------------------------------------------
Propagate::Propagate(const Propagate &prp) :
   GmatCommand                 (prp),
   propName                    (prp.propName),
   direction                   (prp.direction),
   satName                     (prp.satName),
   currentPropMode             (prp.currentPropMode),
   interruptCheckFrequency     (prp.interruptCheckFrequency),
   inProgress                  (false),
   hasFired                    (false),
   epochID                     (prp.epochID),
   stopInterval                (0.0),
   stopTrigger                 (-1),
   stopSatNames                (prp.stopSatNames),
   objectArray                 (prp.objectArray),
   elapsedTime                 (prp.elapsedTime),
   currEpoch                   (prp.currEpoch),
   hasStoppedOnce              (false),
   stepsTaken                  (0),
   state                       (NULL),
   pubdata                     (NULL),
   stopCondMet                 (false),
   stopEpoch                   (prp.stopEpoch),
   dim                         (prp.dim),
   singleStepMode              (prp.singleStepMode),
   transientForces             (NULL),
   currentMode                 (prp.currentMode),
   stopCondEpochID             (prp.stopCondEpochID),
   stopCondBaseEpochID         (prp.stopCondBaseEpochID),
   stopCondStopVarID           (prp.stopCondStopVarID),
   // Set the parameter IDs
   availablePropModesID        (prp.availablePropModesID),
   propCoupledID               (prp.propCoupledID),
   interruptCheckFrequencyID   (prp.interruptCheckFrequencyID),
   satNameID                   (prp.satNameID),
   propNameID                  (prp.propNameID),
   stopWhenID                  (prp.stopWhenID)
{
   parameterCount = prp.parameterCount;
   initialized = false;
   baseEpoch.clear();
   prop.clear();
   sats.clear();
   stopWhen.clear();
   stopSats.clear();
   satBuffer.clear();
   formBuffer.clear();
   p.clear();
   fm.clear();
}


//------------------------------------------------------------------------------
//  Propagate& operator=(const Propagate &prp)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the Propagate Command.
 *
 * @param <p> Original we are copying
 *
 * @return reference to this copy
 */
//------------------------------------------------------------------------------
Propagate& Propagate::operator=(const Propagate &prp)
{
   if (&prp == this)
      return *this;

   // Call the base assignment operator
   GmatCommand::operator=(prp);
   
   propName                = prp.propName;
   direction               = prp.direction;
   satName                 = prp.satName;
   currentPropMode         = prp.currentPropMode;
   interruptCheckFrequency = prp.interruptCheckFrequency;
   inProgress              = false;
   hasFired                = false;   
   epochID                 = prp.epochID;   
   objectArray             = prp.objectArray;
   elapsedTime             = prp.elapsedTime;
   currEpoch               = prp.currEpoch;
   state                   = NULL;
   pubdata                 = NULL;
   stopCondMet             = false;
   stopEpoch               = prp.stopEpoch;
   dim                     = prp.dim;
   singleStepMode          = prp.singleStepMode;
   currentMode             = prp.currentMode;
   stopCondEpochID         = prp.stopCondEpochID;
   stopCondBaseEpochID     = prp.stopCondBaseEpochID;
   stopCondStopVarID       = prp.stopCondStopVarID;
   
   // Set the parameter IDs
//   availablePropModesID      = prp.availablePropModesID;
//   propCoupledID             = prp.propCoupledID;
//   interruptCheckFrequencyID = prp.interruptCheckFrequencyID;
//   satNameID                 = prp.satNameID;
//   propNameID                = prp.propNameID;
//   stopWhenID                = prp.stopWhenID;
   
   initialized             = false;
   hasStoppedOnce          = false;
   stepsTaken              = 0;
       
   baseEpoch.clear();
   prop.clear();
   sats.clear();
   stopWhen.clear();
   stopSats.clear();
   satBuffer.clear();
   formBuffer.clear();
   p.clear();
   fm.clear();
   
   return *this;
}


//------------------------------------------------------------------------------
// bool SetObject(const std::string &name, const Gmat::ObjectType type,
//         const std::string &associate, const Gmat::ObjectType associateType)
//------------------------------------------------------------------------------
/**
 * Sets objects referenced by the Propagate command
 *
 * @param <name> Name of the reference object.
 * @param <type> Type of the reference object.
 * @param <associate> Object associated with this reference object.
 * @param <associateType> Type of the associated object.
 *
 * @return true if the reference was set, false if not.
 */
//------------------------------------------------------------------------------
bool Propagate::SetObject(const std::string &name, const Gmat::ObjectType type,
                          const std::string &associate,
                          const Gmat::ObjectType associateType)
{
   Integer propNum = propName.size() - 1;
   
   switch (type) {
      case Gmat::SPACECRAFT:
      case Gmat::FORMATION:
         (satName[propNum])->push_back(name);
         return true;
   
      case Gmat::PROP_SETUP:
         propName.push_back(name);
         if (name[0] == '-')
            direction.push_back(-1.0);
         else
            direction.push_back(1.0);
         satName.push_back(new StringArray);
         return true;
   
      default:
         break;
   }

   // Not handled here -- invoke the next higher SetObject call
   return GmatCommand::SetObject(name, type, associate, associateType);
}


//------------------------------------------------------------------------------
// bool SetObject(GmatBase *obj, const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Sets objects referenced by the Propagate command
 *
 * @param <name> Name of the reference object.
 * @param <type> Type of the reference object.
 *
 * @return true if the reference was set, false if not.
 */
//------------------------------------------------------------------------------
bool Propagate::SetObject(GmatBase *obj, const Gmat::ObjectType type)
{
   switch (type)
   {
   case Gmat::STOP_CONDITION:
      stopWhen.push_back((StopCondition *)obj);
      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage("Adding stopping condition named %s\n",
            obj->GetName().c_str());
      #endif
      stopCondEpochID = obj->GetParameterID("Epoch");
      stopCondBaseEpochID = obj->GetParameterID("BaseEpoch");
      stopCondStopVarID = obj->GetParameterID("StopVar");
      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage(
            "Stopping condition IDs are [%d, %d, %d]\n",
            stopCondEpochID, stopCondBaseEpochID, stopCondStopVarID);
      #endif
      return true;
            
   default:
      break;
   }

   // Not handled here -- invoke the next higher SetObject call
   return GmatCommand::SetObject(obj, type);
}

//------------------------------------------------------------------------------
// void ClearObject(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Clears the lists of objects referenced by the Propagate command.
 *
 * @param <type> Type of the objects to clear.
 */
//------------------------------------------------------------------------------
void Propagate::ClearObject(const Gmat::ObjectType type)
{
   switch (type)
   {
   case Gmat::SPACECRAFT:
   case Gmat::FORMATION:
      satName.clear();
      break;
   case Gmat::STOP_CONDITION:
      stopWhen.clear();
      break;
            
   default:
      break;
   }
}


//------------------------------------------------------------------------------
// GmatBase* GetObject(const Gmat::ObjectType type, const std::string objName)
//------------------------------------------------------------------------------
/**
 * Accesses objects referenced by the Propagate command.
 *
 * @param <type> Type of the reference object.
 * @param <objName> Name of the reference object.
 *
 * @return true if the reference was set, false if not.
 */
//------------------------------------------------------------------------------
GmatBase* Propagate::GetObject(const Gmat::ObjectType type,
                               const std::string objName)
{
   if (type == Gmat::STOP_CONDITION)
      if (stopWhen.empty())
         return NULL;
      else
         return stopWhen[0];
            
   return GmatCommand::GetObject(type, objName);
}

//------------------------------------------------------------------------------
//  const std::string GetGeneratingString()
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
 * @param <mode>    Specifies the type of serialization requested.
 * @param <prefix>  Optional prefix appended to the object's name. (Used for
 *                  indentation)
 * @param <useName> Name that replaces the object's name (Not yet used
 *                  in commands).
 *
 * @return The script line that defines this GmatCommand.
 */
//------------------------------------------------------------------------------
const std::string& Propagate::GetGeneratingString(Gmat::WriteMode mode,
                                                  const std::string &prefix,
                                                  const std::string &useName)
{
   std::string gen = prefix + "Propagate";
   
   // Construct the generating string
   UnsignedInt index = 0;
   
   if (currentPropMode != "")
      gen += (" " + currentPropMode);
   for (StringArray::iterator prop = propName.begin(); prop != propName.end();
        ++prop) {
      gen += " " + (*prop) + "(";
      // Spaceobjects that are propagated by this PropSetup
      StringArray *sats = satName[index];
      for (StringArray::iterator sc = sats->begin(); sc != sats->end(); ++sc) {
         // Add a comma if needed
         if (sc != sats->begin())
            gen += ", ";
         gen += (*sc);
      }

      // Stopping conditions are now added at the end of the Propagate line, 
      // rather than internal to the PropSetups.
      //
      //if (stopWhen.size() > index) {
      //   gen += ", {";
      //
      //   std::stringstream stopCondDesc;
      //
      //   std::string stopName = 
      //      stopWhen[index]->GetStringParameter(stopCondStopVarID);
      //   stopCondDesc << stopName;
      //
      //   if ((stopName.find("Periapsis") == std::string::npos) &&
      //       (stopName.find(".Apoapsis") == std::string::npos))
      //      stopCondDesc << " = " 
      //                   << stopWhen[index]->GetStringParameter("Goal");
      //   
      //   gen += stopCondDesc.str();
      //
      //   gen += "}";
      //}

      gen += ")";
      ++index;
   }

   // Now the stopping conditions.  Note that stopping conditions are now shown
   // at teh end of the Propagate line, rather than inside of the PropSetup 
   // delimiters.  This is by design, based on e-mails 9/28 - 10/2/2006
   if (stopWhen.size() > 0) {
      gen += " {";
   
      for (std::vector<StopCondition*>::iterator stp = stopWhen.begin();
           stp != stopWhen.end(); ++stp) {
         std::stringstream stopCondDesc;
         if (stp != stopWhen.begin())
            gen += ", ";
   
         std::string stopName = (*stp)->GetStringParameter(stopCondStopVarID);
         stopCondDesc << stopName;
   
         if ((stopName.find(".Periapsis") == std::string::npos) &&
             (stopName.find(".Apoapsis") == std::string::npos))
            stopCondDesc << " = " << (*stp)->GetStringParameter("Goal");
   
         gen += stopCondDesc.str();
      }
      gen += "}";
   }

   generatingString = gen + ";";
   // Then call the base class method
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Propagate.
 *
 * @return clone of the Propagate.
 */
//------------------------------------------------------------------------------
GmatBase* Propagate::Clone() const
{
   return (new Propagate(*this));
}


//------------------------------------------------------------------------------
//  std::string GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
/**
 * Accessor used to find the names of referenced objects.
 *
 * @param <type> reference object type.
 *
 * @return The name of the reference object.
 */
//------------------------------------------------------------------------------
std::string Propagate::GetRefObjectName(const Gmat::ObjectType type) const
{
   /// @todo Figure out how to send back the arrays of names
   switch (type) {
      // Propagator setups
      case Gmat::PROP_SETUP:
         return propName[0];
      
      // Objects that get propagated
      case Gmat::SPACECRAFT:
      case Gmat::FORMATION:
         if (satName.size() > 0)
            return (*satName[0])[0];
      
      default:
         break;
   }
   
   return GmatCommand::GetRefObjectName(type);
}


//------------------------------------------------------------------------------
//  bool SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Accessor used to set the names of referenced objects.
 *
 * @param <type> type of the reference object.
 * @param <name> name of the reference object.
 *
 * @return success of the operation.
 */
//------------------------------------------------------------------------------
bool Propagate::SetRefObjectName(const Gmat::ObjectType type,
                                 const std::string &name)
{
   switch (type) {
      // Propagator setups
      case Gmat::PROP_SETUP:
         direction.push_back(1.0);
         propName.push_back(name);
         satName.push_back(new StringArray);
         return true;
      
      // Objects that get propagated
      case Gmat::SPACECRAFT:
      case Gmat::FORMATION: 
      {
         Integer propNum = propName.size()-1;
         satName[propNum]->push_back(name);
         return true;
      }
      
      default:
         break;
   }
   
   return GmatCommand::SetRefObjectName(type, name);
}

// Reference object accessor methods
//------------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name,
//                        const Integer index
//------------------------------------------------------------------------------
/**
 * Accessor for reference object pointers.
 *
 * @param <type> type of the reference object.
 * @param <name> name of the reference object.
 * @param <index> Index into the object array.
 *
 * @return reference object pointer.
 */
//------------------------------------------------------------------------------
GmatBase* Propagate::GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name, const Integer index)
{
   switch (type)
   {
   case Gmat::STOP_CONDITION:
      if (index < (Integer)stopWhen.size())
      {
         return stopWhen[index];
      }
      else
      {
         throw CommandException("Propagate::GetRefObject() invalid index\n");
      }
   default:
      break;
   }

   // Not handled here -- invoke the next higher GetRefObject call
   return GmatCommand::GetRefObject(type, name, index);
}

//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, ...
//------------------------------------------------------------------------------
/**
 * Sets reference object pointer.
 *
 * @param <obj> Pointer to the reference object.
 * @param <type> type of the reference object.
 * @param <name> name of the reference object.
 * @param <index> Index into the object array.
 *
 * @return true if object successfully set, false otherwise
 */
//------------------------------------------------------------------------------
bool Propagate::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                             const std::string &name, const Integer index)
{
   #if DEBUG_PROPAGATE_OBJ
      MessageInterface::ShowMessage
         ("Propagate::SetRefObject() type=%s name=%s, index=%d\n",
          obj->GetTypeName().c_str(), name.c_str(), index);
   #endif
   
   switch (type)
   {
      case Gmat::STOP_CONDITION:
         {
            std::string satName = obj->GetName();
            Integer strt = satName.find("StopOn") + 6;
            if (strt == (Integer)std::string::npos)
               strt = 0;
            Integer ndx = satName.find(".",0);
            if (ndx != (Integer)std::string::npos)
               satName = satName.substr(strt, ndx-strt);

            Integer size = stopWhen.size();

            if (stopWhen.empty() && index == 0)
            {
               stopWhen.push_back((StopCondition *)obj);
               stopSatNames.push_back(satName);
            }
            else if (index == size)
            {
               stopWhen.push_back((StopCondition *)obj);
               stopSatNames.push_back(satName);
            }
            else if (index < size)
            {
               stopWhen[index] = (StopCondition *)obj;
               stopSatNames[index] = satName;
            }
            else
            {
               MessageInterface::ShowMessage
                  ("Propagate::SetRefObject() index=%d is not next available "
                   "index=%d. Setting %s:%s failed\n", index, size,
                   obj->GetTypeName().c_str(), obj->GetName().c_str());
               return false;
            }

            #if DEBUG_PROPAGATE_OBJ
               for (UnsignedInt  j=0; j<stopSatNames.size(); j++)
                  MessageInterface::ShowMessage(
                     "Propagate::SetRefObject() stopSatNames=%s\n",
                     stopSatNames[j].c_str());
            #endif
            
            #ifdef DEBUG_STOPPING_CONDITIONS
               MessageInterface::ShowMessage(
                  "Adding stopping condition named %s\n",
                  obj->GetName().c_str());
            #endif
            stopCondEpochID = obj->GetParameterID("Epoch");
            stopCondBaseEpochID = obj->GetParameterID("BaseEpoch");
            stopCondStopVarID = obj->GetParameterID("StopVar");
            #ifdef DEBUG_STOPPING_CONDITIONS
               MessageInterface::ShowMessage(
                  "Stopping condition IDs are [%d, %d, %d]\n",
                  stopCondEpochID, stopCondBaseEpochID, stopCondStopVarID);
            #endif
            
            return true;
         }
         
      default:
         break;
   }

   // Not handled here -- invoke the next higher SetRefObject call
   return GmatCommand::SetRefObject(obj, type, name, index);
}

//------------------------------------------------------------------------------
// virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Obtains an array of GmatBase pointers by type.
 *
 * @param type The type of objects requested.
 *
 * @return Reference to the array.
 */
//------------------------------------------------------------------------------
ObjectArray& Propagate::GetRefObjectArray(const Gmat::ObjectType type)
{
   objectArray.clear();
   
   switch (type)
   {
      case Gmat::STOP_CONDITION:
         for (UnsignedInt i=0; i<stopWhen.size(); i++)
            objectArray.push_back(stopWhen[i]);
         return objectArray;
      
      default:
         break;
   }

   // Not handled here -- invoke the next higher SetReferenceObject call
   return GmatCommand::GetRefObjectArray(type);
}


// Parameter accessor methods

//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the description for the parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return String description for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string Propagate::GetParameterText(const Integer id) const
{
   if (id == propCoupledID)
      return "PropagateMode";

   if (id == interruptCheckFrequencyID)
      return "InterruptFrequency";

   if (id == satNameID)
      return "Spacecraft";
    
   if (id == propNameID)
      return "Propagator";
    
   if (id == stopWhenID)
      return "StoppingConditions";

   return GmatCommand::GetParameterText(id);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Retrieve the ID for the parameter given its description.
 *
 * @param <str> Description for the parameter.
 *
 * @return the parameter ID.
 */
//------------------------------------------------------------------------------
Integer Propagate::GetParameterID(const std::string &str) const
{
   if (str == "AvailablePropModes")
      return availablePropModesID;

   if (str == "PropagateMode")
      return propCoupledID;

   if (str == "InterruptFrequency")
      return interruptCheckFrequencyID;

   if (str == "Spacecraft")
      return satNameID;
    
   if (str == "Propagator")
      return propNameID;
    
   return GmatCommand::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the enumerated type of the object.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The enumeration for the type of the parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Propagate::GetParameterType(const Integer id) const
{
   if (id == availablePropModesID)
      return Gmat::STRINGARRAY_TYPE;

   if (id == propCoupledID)
      return Gmat::STRING_TYPE;

   if (id == interruptCheckFrequencyID)
      return Gmat::INTEGER_TYPE;

   if (id == satNameID)
      return Gmat::STRINGARRAY_TYPE;
    
   if (id == propNameID)
      return Gmat::STRING_TYPE;
    
   return GmatCommand::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the string associated with a parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return Text description for the type of the parameter.
 */
//------------------------------------------------------------------------------
std::string Propagate::GetParameterTypeString(const Integer id) const
{
   if (id == availablePropModesID)
      return PARAM_TYPE_STRING[Gmat::STRINGARRAY_TYPE];

   if (id == propCoupledID)
      return PARAM_TYPE_STRING[Gmat::STRING_TYPE];

   if (id == interruptCheckFrequencyID)
      return PARAM_TYPE_STRING[Gmat::INTEGER_TYPE];

   if (id == satNameID)
      return PARAM_TYPE_STRING[Gmat::STRINGARRAY_TYPE];

   if (id == propNameID)
      return PARAM_TYPE_STRING[Gmat::STRING_TYPE];

   return GmatCommand::GetParameterTypeString(id);
}


//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the value for an Integer parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The parameter's value.
 */
//------------------------------------------------------------------------------
Integer Propagate::GetIntegerParameter(const Integer id) const
{
   if (id == interruptCheckFrequencyID)
      return interruptCheckFrequency;

   return GmatCommand::GetIntegerParameter(id);
}


//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * Set the value for an Integer parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new parameter value.
 *
 * @return the parameter value at the end of this call.
 */
//------------------------------------------------------------------------------
Integer Propagate::SetIntegerParameter(const Integer id, const Integer value)
{
   if (id == interruptCheckFrequencyID) {
      if (value >= 0)
         interruptCheckFrequency = value;
      return interruptCheckFrequency;
   }

   return GmatCommand::SetIntegerParameter(id, value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
std::string Propagate::GetStringParameter(const Integer id) const
{
   if (id == propCoupledID)
      return currentPropMode;

   return GmatCommand::GetStringParameter(id);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored.
 */
//------------------------------------------------------------------------------
bool Propagate::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == propCoupledID) {
      const StringArray pmodes = GetStringArrayParameter(availablePropModesID);
      if (find(pmodes.begin(), pmodes.end(), value) != pmodes.end()) {
         currentPropMode = value;
         for (Integer i = 0; i < PropModeCount; ++i)
            if (value == pmodes[i]) {
               currentMode = (PropModes)i;
               return true;
            }
      }
   }
 
   if (id == satNameID) {
      Integer propNum = propName.size()-1;
      satName[propNum]->push_back(value);
      return true;
   }

   if (id == propNameID) {
      propName.push_back(value);
      direction.push_back(1.0);
      satName.push_back(new StringArray);
      return true;
   }
 
   return GmatCommand::SetStringParameter(id, value);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value,
//                         const Integer index)
//------------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param id The integer ID for the parameter.
 * @param value The new string for this parameter.
 * @param index Index for parameters in arrays.  Use -1 or the index free
 *              version to add the value to the end of the array.
 *
 * @return true if the string is stored, false if not.
 */
//------------------------------------------------------------------------------
bool Propagate::SetStringParameter(const Integer id, const std::string &value,
                                   const Integer index)
{   
   if (id == satNameID) {
      if (index < (Integer)propName.size())
         satName[index]->push_back(value);
      else
         throw CommandException("Propagate::SetStringParameter Attempting to "
                         "assign a spacecraft without an associated PropSetup");
      return true;
   }

   return GmatCommand::SetStringParameter(id, value, index);
}

//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The requested StringArray.
 */
//------------------------------------------------------------------------------
const StringArray& Propagate::GetStringArrayParameter(const Integer id) const
{
   static StringArray modeList;
   
   if (id == availablePropModesID) {
      modeList.clear();
      for (Integer i = 0; i < PropModeCount; ++i)
         modeList.push_back(PropModeList[i]);
      return modeList;
   }
 
   if (id == satNameID)
      return *satName[0];
 
   if (id == propNameID) {
      return propName;
   }
 
   return GmatCommand::GetStringArrayParameter(id);
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
const StringArray& Propagate::GetStringArrayParameter(const Integer id,
                                               const Integer index) const
{
   if (id == satNameID)
      return *satName[index];

   return GmatCommand::GetStringArrayParameter(id, index);
}


//------------------------------------------------------------------------------
// bool TakeAction(const std::string &action, const std::string &actionData)
//------------------------------------------------------------------------------
/**
 * Interface used to support user actions.
 *
 * @param <action> The string descriptor for the requested action.
 * @param <actionData> Optional data used for the action.
 *
 * @return true if the action was performed, false if not.
 */
//------------------------------------------------------------------------------
bool Propagate::TakeAction(const std::string &action,
                           const std::string &actionData)
{
   if (action == "Clear")
   {
      if (actionData == "Propagator")
      {
         for (Integer i = 0; i < (Integer)satName.size(); ++i)
         {
            delete satName[i];
//            satName[i] = NULL;  // Paranoia sets in!
         }
         satName.clear();

         propName.clear();
         prop.clear();
         sats.clear();
      }
      else if (actionData == "StopCondition")
      {
         stopWhen.clear();
         stopSats.clear();
         stopSatNames.clear();
         return true;
      }
   }
   else if (action == "SetStopSpacecraft")
   {
      stopSatNames.push_back(actionData);
      return true;
   }
   else if (action == "ResetLoopData")
   {
      for (std::vector<Propagator*>::iterator i = p.begin(); i != p.end(); ++i)
      {
         (*i)->ResetInitialData(); 
      }
      return true;
   }
   
   return GmatCommand::TakeAction(action, actionData);
}


//------------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Renames referenced objects.
 *
 * @param type Type of the object that is renamed.
 * @param oldName The current name for the object.
 * @param newName The name the object has when this operation is complete.
 *
 * @return true on success.
 */
//------------------------------------------------------------------------------
bool Propagate::RenameRefObject(const Gmat::ObjectType type,
                                const std::string &oldName,
                                const std::string &newName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Propagate::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   // Propagate needs to know about spacecraft or formation only
   if (type != Gmat::SPACECRAFT && type != Gmat::FORMATION &&
       type != Gmat::PROP_SETUP)
      return true;

   StringArray::iterator pos;
   
   if (type == Gmat::PROP_SETUP)
   {
      // rename PropSetup
      for (pos = propName.begin(); pos != propName.end(); ++pos)
         if (*pos == oldName)
            *pos = newName;
   }
   else
   {
      // rename space object name used in prop setup
      for (UnsignedInt prop = 0; prop < propName.size(); ++prop)
         for (pos = satName[prop]->begin(); pos != satName[prop]->end(); ++pos)
            if (*pos == oldName)
               *pos = newName;
      
      // rename space object name used in stopping condition
      for (UnsignedInt i = 0; i < stopSatNames.size(); ++i)
         if (stopSatNames[i] == oldName)
            stopSatNames[i] = newName;
      
      #if DEBUG_RENAME
      MessageInterface::ShowMessage
         ("Propagate::RenameConfiguredItem() Rename StopCondtion Ref. Object\n");
      #endif
      
      // rename stop condition parameter
      for (UnsignedInt i=0; i<stopWhen.size(); i++)
         stopWhen[i]->RenameRefObject(type, oldName, newName);
   }
   
   return true;
}

//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
/**
 * Parses the command string and builds the corresponding command structures.
 *
 * The Propagate command has the following syntax:
 *
 *     Propagate prop(Sat1, ... , {Sat1.ElapsedDays = 700}) ...;
 *
 * where prop is a PropSetup, "Sat1, ... ," is the list of SpaceObjects that are
 * propagated, and the items in curly braces are the (optional) stopping
 * conditions.  The Propagate command supports simultaneous propagation of
 * multiple spacecraft, either in a single PropSetup or in a list of PropSetups
 * on the same Propagate command line.
 *
 * This method breaks the script line into the corresponding pieces, and stores
 * the names of the PropSetups, SpaceObjects, and StoppingConditions so they can
 * be set to point to the correct objects during initialization.
 *
 * @return true on successful parsing of the command.
 */
//------------------------------------------------------------------------------
bool Propagate::InterpretAction()
{
   Integer loc = generatingString.find("Propagate", 0) + 9;
   const char *str = generatingString.c_str();
   
   if (generatingString.find("..") != generatingString.npos)
      throw CommandException("Propagate::InterpretAction: Can not parse command\n "
                                + generatingString);
   
   while (str[loc] == ' ')
      ++loc;

   // Check to see if there are optional parameters (e.g. "Synchronized")
   CheckForOptions(loc, generatingString);
   // Now fill in the list of propagators
   AssemblePropagators(loc, generatingString);
   
   return true;
}


//------------------------------------------------------------------------------
// void CheckForOptions(Integer &loc, std::string &generatingString)
//------------------------------------------------------------------------------
/**
 * Looks for propagator options that exist prior to any PropSetup names.
 *
 * @param <loc>               The current location in the generating string.
 * @param <generatingString>  The generating string.
 */
//------------------------------------------------------------------------------
void Propagate::CheckForOptions(Integer &loc, std::string &generatingString)
{
   std::string modeStr;
   currentMode = INDEPENDENT;
   
   for (Integer modeId = INDEPENDENT+1; modeId != PropModeCount; ++modeId) 
   {
      modeStr = PropModeList[modeId];
      modeStr += " ";
      
      #ifdef DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage("\nPropagate::CheckForOptions() looking"
                                " for \"%s\" starting at loc=%d\n in \n\"%s\"",
                                modeStr.c_str(), loc, generatingString.c_str());
      #endif
      Integer end = generatingString.find(modeStr, loc);
      if (end != (Integer)std::string::npos) 
      {
         currentMode = (PropModes)modeId;
         currentPropMode = PropModeList[modeId];
         #ifdef DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage("\nLocated at %d\n", end);
            MessageInterface::ShowMessage("Mode is now %d\n", currentMode);
         #endif

         if (end >= loc)
            loc = end + modeStr.length();
      }
   }
}


//------------------------------------------------------------------------------
// void AssemblePropagators(Integer &loc, std::string& generatingString)
//------------------------------------------------------------------------------
/**
 * Parses the PropSetup portion of the Propagate command.
 *
 * @param <loc>               The current location in the generating string.
 * @param <generatingString>  The generating string.
 */
//------------------------------------------------------------------------------
void Propagate::AssemblePropagators(Integer &loc, 
   std::string& generatingString)
{
   // First parse the pieces from the string, starting at loc
   StringArray setupStrings, stopStrings;
   
   FindSetupsAndStops(loc, generatingString, setupStrings, stopStrings);

   #ifdef DEBUG_PROPAGATE_ASSEMBLE
      // Output the chunks for debugging
      MessageInterface::ShowMessage("PropSetups:\n");
      for (StringArray::iterator i = setupStrings.begin(); 
           i != setupStrings.end(); ++i)
         MessageInterface::ShowMessage("   '%s'\n", i->c_str());
      MessageInterface::ShowMessage("StopConditions:\n");
      for (StringArray::iterator i = stopStrings.begin(); 
           i != stopStrings.end(); ++i)
         MessageInterface::ShowMessage("   '%s'\n", i->c_str());
   #endif
   
   // Now build the prop setups
   for (StringArray::iterator i = setupStrings.begin(); 
        i != setupStrings.end(); ++i)
      ConfigurePropSetup(*i);
   
   // and the stopping conditions
   for (StringArray::iterator i = stopStrings.begin(); 
        i != stopStrings.end(); ++i)
      ConfigureStoppingCondition(*i);

   // Finally, set the prop mode
   if (stopWhen.empty())
      singleStepMode = true;  // If not, run in single step mode
}


//------------------------------------------------------------------------------
// void Propagate::FindSetupsAndStops(Integer &loc, 
//   std::string& generatingString, StringArray &setupStrings, 
//   StringArray &stopStrings)
//------------------------------------------------------------------------------
/**
 * Breaks out the PropSetup object strings and the stopping condition strings.
 *
 * @param <loc>               The current location in the generating string.
 * @param <generatingString>  The generating string.
 * @param <setupStrings>      The container for the PropSetup strings.
 * @param <stopStrings>       The container for teh stopping condition strings.
 */
//------------------------------------------------------------------------------
void Propagate::FindSetupsAndStops(Integer &loc, 
   std::string& generatingString, StringArray &setupStrings, 
   StringArray &stopStrings)
{
   // First parse the pieces from the string, starting at loc
   std::string tempString, setupWithStop, oneStop;
   const char *str = generatingString.c_str();
   Integer currentLoc = loc, parmstart, end, commaLoc;

   bool scanning = true;
   
   // First find the PropSetups
   parmstart = generatingString.find("(", currentLoc);   
   while (scanning) 
   {
      end = generatingString.find(")", parmstart)+1;

      if (end == (Integer)std::string::npos)
         throw CommandException("Propagate::AssemblePropagators: Propagate"
                                " string does not identify propagator");
      
      if (generatingString[currentLoc] == '-') {
         direction.push_back(-1.0);
      }
      else
         direction.push_back(1.0);
         
      tempString = generatingString.substr(currentLoc, end-currentLoc);
      // Remove stop condition here
      if (tempString.find("{", 0) != std::string::npos)
      {
         setupWithStop = tempString;
         
         Integer braceStart = setupWithStop.find("{", 0),
                 braceEnd   = setupWithStop.find("}", 0);
                 
         if (braceEnd == (Integer)std::string::npos)
            throw CommandException("Propagate::AssemblePropagators: PropSetup"
                                  " string " + tempString +
                                  " starts a stopping condition, but does not"
                                  " have a closing brace.");
         // Now remove the bracketed chunk from the string
         tempString = setupWithStop.substr(0, braceStart);
         // Remove the comma
         Integer commaLoc = braceStart - 1;
         while ((tempString[commaLoc] == ',') || (tempString[commaLoc] == ' '))
         {
            --commaLoc;
         }
         tempString = tempString.substr(0, commaLoc+1);
         
         // Add on the trailing chunk
         tempString += setupWithStop.substr(braceEnd+1);
      }
            
      setupStrings.push_back(tempString);
      currentLoc = end+1;

      // Skip trailing comma or white space
      while ((str[currentLoc] == ',') || (str[currentLoc] == ' '))
         ++currentLoc;
      parmstart = generatingString.find("(", currentLoc);
      if (parmstart == (Integer)std::string::npos)
         scanning = false;
   }
   
   // Flag mixed directions in the command
   for (RealArray::iterator d = direction.begin(); d != direction.end(); ++d)
   {
      if ((*d) != direction[0])
         throw CommandException(
            "Mixed propagation directions are not allowed, but are specified "
            "on the Propagate line\n'" + generatingString + "'");
   }
   
   // Now find the stopping conditions
   scanning = true;
   currentLoc = loc;
   
   parmstart = generatingString.find("{", currentLoc);
   if ((UnsignedInt)parmstart == std::string::npos)
      scanning = false;
   
   while (scanning) 
   {
      end = generatingString.find("}", parmstart)+1;
      tempString = generatingString.substr(parmstart+1, end-parmstart-2);
      
      // Split out stops, one at a time
      currentLoc = 0;
      do 
      {
         commaLoc = tempString.find(",", currentLoc);
         oneStop = tempString.substr(currentLoc, commaLoc - currentLoc);
         // Remove leading white space
         while (oneStop[0] == ' ')
            oneStop = oneStop.substr(1);
         // Remove trailing white space
         currentLoc = oneStop.length() - 1;
         while (oneStop[currentLoc] == ' ')
            --currentLoc;
         oneStop = oneStop.substr(0, currentLoc+1);
         stopStrings.push_back(oneStop);

         currentLoc = commaLoc + 1;
      } while (commaLoc != (Integer)std::string::npos);

      currentLoc = end+1;
      
      // Skip trailing comma or white space
      while ((str[currentLoc] == ',') || (str[currentLoc] == ' '))
         ++currentLoc;
      parmstart = generatingString.find("{", currentLoc);
      if (parmstart == (Integer)std::string::npos)
         scanning = false;
   }
}


//------------------------------------------------------------------------------
// void ConfigurePropSetup(std::string &setupDesc)
//------------------------------------------------------------------------------
/**
 * Builds the data needed for the a PropSetup.  Stopping conditions are handled
 * separately.
 *
 * @param <setupDesc>  The string describing the PropSetup.
 */
//------------------------------------------------------------------------------
void Propagate::ConfigurePropSetup(std::string &setupDesc)
{
   #ifdef DEBUG_PROPAGATE_ASSEMBLE
      MessageInterface::ShowMessage("Building PropSetup '%s'\n", 
         setupDesc.c_str());
   #endif

   // First separate the PropSetup from the SpaceObjects
   std::string prop, sats, sat;
   UnsignedInt loc = setupDesc.find("(");
   if (loc == std::string::npos)
      throw CommandException("The propsetup string '" + setupDesc +
         "' does not identify any spacecraft for propagation on "
         + "the command line\n" + generatingString);
   prop = setupDesc.substr(0, loc);
   sats = setupDesc.substr(loc);
   
   CleanString(prop);

   #ifdef DEBUG_PROPAGATE_ASSEMBLE
      MessageInterface::ShowMessage("   PropSetup is '%s'\n", prop.c_str());
   #endif
   SetObject(prop, Gmat::PROP_SETUP);
   
   // Next the SpaceObjects
   StringArray extras;
   extras.push_back("(");
   extras.push_back(")");
   extras.push_back(",");
 
   loc = 0;
   while (loc != std::string::npos)
   {  
      loc = sats.find(',');
      sat = sats.substr(0, loc);
      sats = sats.substr(loc+1);
      CleanString(sat, &extras);
      
      #ifdef DEBUG_PROPAGATE_ASSEMBLE
         MessageInterface::ShowMessage("   Found satellite '%s'\n", sat.c_str());
      #endif
      SetObject(sat, Gmat::SPACECRAFT);
   }
}


//------------------------------------------------------------------------------
// void ConfigureStoppingCondition(std::string &stopDesc)
//------------------------------------------------------------------------------
/**
 * Builds the data needed for a stopping condition.  PropSetups are handled
 * separately.
 *
 * @param <stopDesc>  The string describing the stopping condition.
 */
//------------------------------------------------------------------------------
void Propagate::ConfigureStoppingCondition(std::string &stopDesc)
{
   #ifdef DEBUG_PROPAGATE_ASSEMBLE
      MessageInterface::ShowMessage("Building Stop '%s'\n", 
         stopDesc.c_str());
   #endif

   std::string lhs, rhs = "";
   UnsignedInt loc;
   StringArray extras;
   extras.push_back("{");
   extras.push_back("}");
   extras.push_back("=");
   
   loc = stopDesc.find("=");
   if (loc == std::string::npos)
   {
      lhs = stopDesc;
      CleanString(lhs, &extras);
   }
   else
   {
      lhs = stopDesc.substr(0,loc);
      CleanString(lhs, &extras);
      rhs = stopDesc.substr(loc+1);
      CleanString(rhs, &extras);
   }

   #ifdef DEBUG_PROPAGATE_ASSEMBLE
      MessageInterface::ShowMessage("   Stop = '%s' with value '%s'\n", 
         lhs.c_str(), rhs.c_str());
   #endif
   
   // Now to work!
   std::string paramType, paramObj, paramSystem;
   if (!InterpretParameter(lhs, paramType, paramObj, paramSystem))
      throw CommandException("Cannot decipher the parameter string '" +
         lhs + "' on the command line " + generatingString);

   // Create the stop parameter
   Moderator *theModerator = Moderator::Instance();
   std::string paramName;
   if (paramSystem == "")
      paramName = paramObj + "." + paramType;
   else
      paramName = paramObj + "." + paramSystem + "." + paramType;   
   theModerator->CreateParameter(paramType, paramName, paramObj, paramSystem);
   StopCondition *stopCond = theModerator->CreateStopCondition("StopCondition",
      "StopOn" + paramName);
   
   // Handle some static member initialization if this is the first opportunity
   if (stopCondEpochID == -1)
   {
      stopCondEpochID = stopCond->GetParameterID("Epoch");
      stopCondBaseEpochID = stopCond->GetParameterID("BaseEpoch");
      stopCondStopVarID = stopCond->GetParameterID("StopVar");
   }
   
   // Setup for backwards propagation
   stopCond->SetPropDirection(direction[0]);  // Use direction of assoc'd prop
   stopCond->SetStringParameter(stopCondStopVarID, paramName);
   SetObject(stopCond, Gmat::STOP_CONDITION);
   TakeAction("SetStopSpacecraft", paramObj);
   
   
   if (paramType != "Apoapsis" && paramType != "Periapsis")
   {
      #ifdef DEBUG_PROPAGATE_ASSEMBLE
         MessageInterface::ShowMessage("Propagate::AssemblePropagators()"
            " component = <%s>\n", rhs.c_str());
      #endif
         
      // create goal parameter
      std::string component = CreateParameter(rhs);
      stopCond->SetStringParameter("Goal", component);
   }
   else
   {
      if (rhs.length() != 0)
      {
         throw CommandException("Stopping condition " + paramType + 
            " does not take a value, but it is set using the string '" + 
            stopDesc + "' in the line\n'" + generatingString + "'");
      }
   }
}


//------------------------------------------------------------------------------
// void CleanString(std::string &theString, const StringArray *extras)
//------------------------------------------------------------------------------
/**
 * Strips off leading and trailing whitespace, and additional characters if 
 * specified.
 *
 * @param <theString>  The string that -- might -- need cleaned.
 * @param <extras>     All additional characters (other than a space) that 
 *                     should be stripped off.
 */
//------------------------------------------------------------------------------
void Propagate::CleanString(std::string &theString, const StringArray *extras)
{
   UnsignedInt loc, len = theString.length();
   bool keepGoing = false;
   
   // Clean up the start of the string
   for (loc = 0; loc < len; ++loc)
   {
      if (theString[loc] != ' ')
      {
         if (extras != NULL)
            for (StringArray::const_iterator i = extras->begin(); i != extras->end(); ++i)
               if (theString[loc] == (*i)[0])
                  keepGoing = true;
         if (!keepGoing)
            break;
         else
            keepGoing = false;
      }
   }
   theString = theString.substr(loc);
   
   // Clean up the end of the string
   keepGoing = false;
   for (loc = theString.length() - 1; loc >= 0; --loc)
   {
      if (theString[loc] != ' ')
      {
         if (extras != NULL)
            for (StringArray::const_iterator i = extras->begin(); i != extras->end(); ++i)
               if (theString[loc] == (*i)[0])
                  keepGoing = true;
         if (!keepGoing)
            break;
         else
            keepGoing = false;
      }
   }
   theString = theString.substr(0, loc+1);
}


//------------------------------------------------------------------------------
//  bool InterpretParameter(const std::string text, std::string &paramType,
//                          std::string &paramObj, std::string &parmSystem)
//------------------------------------------------------------------------------
/**
 * Breaks apart a parameter declaration into its component pieces
 *
 * @param <text>       The string that gets decomposed.
 * @param <paramType>  Type of parameter that is needed.
 * @param <paramObj>   The Object used for the parameter calculations.
 * @param <parmSystem> The coordinate system or body used for the parameter
 *                     calculations (or the empty string if this piece is
 *                     unspecified).
 *
 * @return true if the decomposition worked.
 */
//------------------------------------------------------------------------------
bool Propagate::InterpretParameter(const std::string text,
                                   std::string &paramType, 
                                   std::string &paramObj, 
                                   std::string &paramSystem)
{
   //loj: 9/20/06 Used GmatStringUtil to throw invalid parameter syntax
   GmatStringUtil::ParseParameter(text, paramType, paramObj, paramSystem);

   
//    Integer start = 0, dotLoc = text.find(".", 0);
//    if (dotLoc == (Integer)std::string::npos)
//       throw CommandException("Propagate::InterpretParameter: Unable to "
//                "interpret parameter object in the string " +
//                text);
   
//    paramObj = text.substr(start, dotLoc - start);
//    start = dotLoc + 1;
//    dotLoc = text.find(".", start);
//    if (dotLoc != (Integer)std::string::npos) {
//       paramSystem = text.substr(start, dotLoc - start);
//       start = dotLoc + 1;
//    }
//    else {
//       paramSystem = "";
//    }
   
//    paramType = text.substr(start);
   
   #ifdef DEBUG_PROPAGATE_INIT
      MessageInterface::ShowMessage(
         "Built parameter %s for object %s with CS %s\n",
         paramType.c_str(), paramObj.c_str(), paramSystem.c_str());
   #endif

      
   return true;
}


//------------------------------------------------------------------------------
// bool TakeAStep(Real propStep)
//------------------------------------------------------------------------------
/**
 * Advances each of the contained PropSetups by one step.
 *
 * @param <propStep> The requested size of the step.
 *
 * @return true if the step succeeded.
 */
//------------------------------------------------------------------------------
bool Propagate::TakeAStep(Real propStep)
{
   bool retval = false;
   Real stepToTake;
 
   std::vector<Propagator*>::iterator current = p.begin();
   if (propStep == 0.0) 
   {
      switch (currentMode) 
      {
         case INDEPENDENT:
            // Advance each propagator individually, without regard for the
            // epochs of the others
            #ifdef DEBUG_PROPAGATE_EXE
               MessageInterface::ShowMessage
                  ("Propagate::TakeAStep() running in INDEPENDENT mode\n");
            #endif
            while (current != p.end()) 
            { 
               if (!(*current)->Step())
                  throw CommandException(
                     "Propagator failed to take a good step\n");
               ++current;
            }
            retval = true;
            break;
            
         case SYNCHRONIZED:
            // This mode advances the first propagator, and then brings the 
            // others up to the epoch of that first one.
            #ifdef DEBUG_PROPAGATE_EXE
               MessageInterface::ShowMessage
                  ("Propagate::TakeAStep() running in SYNCHRONIZED mode\n");
            #endif
            if (!(*current)->Step())
               throw CommandException("Initial synchronized Propagator failed "
                                      "to take a good step\n");
            stepToTake = (*current)->GetStepTaken();
            ++current;
            while (current != p.end()) 
            {
               if (!(*current)->Step(stepToTake))
                  throw CommandException("Propagator failed to take a good "
                                         "synchronized step\n");
               ++current;
            }
            retval = true;
            break;
            
         default:
            #ifdef DEBUG_PROPAGATE_EXE
               MessageInterface::ShowMessage
                  ("Propagate::TakeAStep() runnning in undefined mode "
                  "(mode = %d)\n", currentMode);
            #endif
            retval = false;
      }
   }
   else 
   {
      #ifdef DEBUG_FIXED_STEP
         std::vector<ForceModel *>::iterator fmod = fm.begin();
      #endif
      
      // Step all of the propagators by the input amount
      while (current != p.end()) 
      {
         #ifdef DEBUG_FIXED_STEP
            MessageInterface::ShowMessage("Stepping '%s' by %le seconds\n", 
               (*current)->GetName().c_str(), propStep);

            Integer fmSize = (*fmod)->GetDimension();
            MessageInterface::ShowMessage("Fmod has dim = %d\n", fmSize);
            MessageInterface::ShowMessage("   Pre Prop:  ");
           
            Real *fmState = (*fmod)->GetState();
            for (Integer q = 0; q < fmSize; ++q)
               MessageInterface::ShowMessage(" %.12lf", fmState[q]);
            MessageInterface::ShowMessage("\n");
            
            ++fmod;
         #endif

         if (!(*current)->Step(propStep))
         {
            char size[32];
            std::sprintf(size, "%.12lf", propStep);
            throw CommandException("Propagator " + (*current)->GetName() + 
               " failed to take a good final step (size = " + size + ")\n");
         }
         

         #ifdef DEBUG_FIXED_STEP
            MessageInterface::ShowMessage("   Post Prop: ");
           
            for (Integer q = 0; q < fmSize; ++q)
               MessageInterface::ShowMessage(" %.12lf", fmState[q]);
            MessageInterface::ShowMessage("\n");
            
            ++fmod;
         #endif
         
         ++current;
      }
      retval = true;
   }
   
   #ifdef DEBUG_PROPAGATE_STEPSIZE
      MessageInterface::ShowMessage("Prop step = %16.13lf\n", 
         p[0]->GetStepTaken());
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// void SetTransientForces(std::vector<PhysicalModel*> *tf)
//------------------------------------------------------------------------------
/**
 * Sets the array of transient forces, so it can be passed to the PropSetups.
 *
 * @param <tf> The array of transient forces.
 */
//------------------------------------------------------------------------------
void Propagate::SetTransientForces(std::vector<PhysicalModel*> *tf)
{
   transientForces = tf;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Performs the initialization needed to run the Propagate command.
 *
 * @return true if the GmatCommand is initialized, false if an error occurs.
 */
//------------------------------------------------------------------------------
bool Propagate::Initialize()
{
   #if DEBUG_PROPAGATE_INIT
      MessageInterface::ShowMessage("Propagate::Initialize() entered.\n%s\n",
                                    generatingString.c_str());
      MessageInterface::ShowMessage("  Size of propName is %d\n",
                                    propName.size());
      MessageInterface::ShowMessage("  Size of direction is %d\n",
                                    direction.size());
   #endif

   GmatCommand::Initialize();
   
   inProgress = false;
   UnsignedInt index = 0;
   prop.clear();
   sats.clear();
   SpaceObject *so;
   std::string pName;
   Real dir;

   // Ensure that we are using fresh objects when buffering stops
   EmptyBuffer();
   
   // Remove old PropSetups
   for (std::vector<PropSetup*>::iterator ps = prop.begin(); ps != prop.end(); 
        ++ps)
   {
      PropSetup *oldPs = *ps;
      *ps = NULL;
      delete oldPs;
   }
   prop.clear();
      
   for (StringArray::iterator i = propName.begin(); i != propName.end(); ++i)
   {
      if (satName.size() <= index)
         throw CommandException("Size mismatch for SpaceObject names\n");
         
      if ((*i)[0] == '-') 
      {
         pName = i->substr(1);
         dir = -1.0;
      }
      else 
      {
        pName = *i;
        dir = 1.0;
      }

      if (objectMap->find(pName) == objectMap->end())
         throw CommandException(
            "Propagate command cannot find Propagator Setup\"" + (pName) +
            "\"\n");
   
      if (satName[index]->empty())
         throw CommandException(
            "Propagate command does not have a SpaceObject for " + (pName) +
            " in \n\"" + generatingString + "\"\n");
   
      if (stopWhen.empty())
         singleStepMode = true;
      else
         singleStepMode = false;

      prop.push_back((PropSetup *)(((*objectMap)[pName])->Clone()));
      // prop.push_back((PropSetup *)((*objectMap)[pName]));
      if (!prop[index])
         return false;
      direction[index] = dir;
      
      Propagator *p = prop[index]->GetPropagator();
      if (!p)
         throw CommandException("Propagator not set in PropSetup\n");
   
      // Toss the spacecraft into the force model
      ForceModel *fm = prop[index]->GetForceModel();
      if (!fm)
         throw CommandException("ForceModel not set in PropSetup\n");
      fm->ClearSpacecraft();
      StringArray::iterator scName;
      StringArray owners, elements;

      /// @todo Check to see if All and All.Epoch belong in place for all modes.
      owners.push_back("All");
      elements.push_back("All.epoch");
      
      bool finiteBurnActive = false;
      
      for (scName = satName[index]->begin(); scName != satName[index]->end(); 
           ++scName) 
      {
         #if DEBUG_PROPAGATE_INIT
            MessageInterface::ShowMessage("   Adding '%s' to propsetup '%s'\n",
               scName->c_str(), i->c_str());
         #endif
         if (objectMap->find(*scName) == objectMap->end()) 
         {
            std::string errmsg = "Unknown SpaceObject \"";
            errmsg += *scName;
            errmsg += "\"";
            throw CommandException(errmsg);
         }
         so = (SpaceObject*)(*objectMap)[*scName];
         if (epochID == -1)
            epochID = so->GetParameterID("A1Epoch");
         if (so->IsManeuvering())
            finiteBurnActive = true;
         sats.push_back(so);
         AddToBuffer(so);
         fm->AddSpaceObject(so);
         if (so->GetType() == Gmat::FORMATION)
            FillFormation(so, owners, elements);
         else 
         {
            SetNames(so->GetName(), owners, elements);
         }
      }
      
      // Check for finite thrusts and update the force model if there are any
      if (finiteBurnActive == true)
         AddTransientForce(satName[index], fm);
   
      streamID = publisher->RegisterPublishedData(owners, elements);
      p->SetPhysicalModel(fm);
      p->SetRealParameter("InitialStepSize", 
         fabs(p->GetRealParameter("InitialStepSize"))*direction[index]);
      p->Initialize();
      ++index;
   } // End of loop through PropSetups
   
   initialized = true;

   stopSats.clear();
   // Setup spacecraft array used for stopping conditions
   for (StringArray::iterator sc = stopSatNames.begin(); 
        sc != stopSatNames.end(); ++sc) {
      if (objectMap->find(*sc) == objectMap->end()) {
         std::string errmsg = "Unknown SpaceObject \"";
         errmsg += *sc;
         errmsg += "\" used in stopping conditions";
         throw CommandException(errmsg);
      }
      so = (SpaceObject*)(*objectMap)[*sc];
      stopSats.push_back(so);
   }

   #if DEBUG_PROPAGATE_INIT
      for (UnsignedInt i=0; i<stopSats.size(); i++)
         MessageInterface::ShowMessage(
            "Propagate::Initialize() stopSats[%d]=%s\n", i, 
            stopSats[i]->GetName().c_str());
   #endif
   
   if ((stopWhen.size() == 0) && !singleStepMode)
      throw CommandException("No stopping conditions specified!");
   
   if (solarSys != NULL)
   {
      StringArray refNames;
      
      for (UnsignedInt i=0; i<stopWhen.size(); i++)
      {
         stopWhen[i]->SetSolarSystem(solarSys);
         
         //Set StopCondition parameters
         refNames = stopWhen[i]->GetRefObjectNameArray(Gmat::PARAMETER);
         
         for (UnsignedInt j=0; j<refNames.size(); j++)
         {
            #if DEBUG_PROPAGATE_INIT
               MessageInterface::ShowMessage("===> refNames=<%s>\n", 
                  refNames[j].c_str());
            #endif
            stopWhen[i]->SetRefObject((*objectMap)[refNames[j]],
                                      Gmat::PARAMETER, refNames[j]);
         }
         
         stopWhen[i]->Initialize();
         stopWhen[i]->SetSpacecraft(sats[0]);
         
         if (!stopWhen[i]->IsInitialized())
         {
            initialized = false;
            MessageInterface::ShowMessage(
               "Propagate::Initialize() StopCondition %s is not initialized.\n",
               stopWhen[i]->GetName().c_str());
            break;
         }
      }
   }
   else
   {
      initialized = false;
      MessageInterface::ShowMessage
         ("Propagate::Initialize() SolarSystem not set in StopCondition");
   }

   #if DEBUG_PROPAGATE_EXE
      MessageInterface::ShowMessage("Propagate::Initialize() complete.\n");
   #endif

   #ifdef DEBUG_PROPAGATE_DIRECTION
      MessageInterface::ShowMessage("Propagate::Initialize():"
                                    " Propagators Identified:\n");
      std::vector<Real>::iterator j = direction.begin();
      for (StringArray::iterator i = propName.begin(); i != propName.end();
           ++i, ++j)
         MessageInterface::ShowMessage("   \"%s\" running %s\n", i->c_str(),
         ((*j) > 0.0 ? "forwards" : "backwards"));
   #endif

   if (singleStepMode)
   {
      commandSummary = "Command Summary: ";
      commandSummary += typeName;
      commandSummary += " Command\nSummary not available in single step mode\n";
   }
      

   return initialized;
}

//------------------------------------------------------------------------------
// void FillFormation(SpaceObject *so)
//------------------------------------------------------------------------------
/**
 * Fill in the components of a formation (recursively).
 *
 * @param <so> The SpaceObject that needs to be filled.
 */
//------------------------------------------------------------------------------
void Propagate::FillFormation(SpaceObject *so, StringArray& owners, 
                              StringArray& elements)
{
   static Integer soEpochId = -1;
   if ((so == NULL) || (so->GetType() != Gmat::FORMATION))
      throw CommandException("Invalid SpaceObject passed to FillFormation");
   
   if (soEpochId == -1)
      soEpochId = so->GetParameterID("A1Epoch");
      
   StringArray comps = so->GetStringArrayParameter(so->GetParameterID("Add"));
   SpaceObject *el;
   Real ep;
   
   for (StringArray::iterator i = comps.begin(); i != comps.end(); ++i) {
      if ((*objectMap).find(*i) == objectMap->end())
         throw CommandException("Formation " + so->GetName() +
            " uses unknown object named '" + (*i) + "'");
            
      el = (SpaceObject*)(*objectMap)[*i];
      if (i == comps.begin())
      {
         ep = el->GetRealParameter(soEpochId);
         so->SetRealParameter(soEpochId, ep);
      }
      
      so->SetRefObject(el, el->GetType(), el->GetName()); 
      if (el->GetType() == Gmat::FORMATION)
         FillFormation(el, owners, elements);
      else     // Setup spacecraft data descriptions
         SetNames(el->GetName(), owners, elements);
   }
   
   ((Formation*)(so))->BuildState();
}


//------------------------------------------------------------------------------
// GmatCommand* GetNext()
//------------------------------------------------------------------------------
/**
 * Returns pointer to next command to be executed.
 * 
 * Propagate::GetNext overrides the base class's GetNext method so that it can
 * poll the moderator for user interrupts periodically.  If the stopping 
 * conditions have not yet been met, GetNext returns this object; otherwise, it 
 * returns the next one in the command list.
 *
 * @return The next pointer, as described above.
 */
//------------------------------------------------------------------------------
GmatCommand* Propagate::GetNext()
{
   if (!inProgress)
      return next;
   return this;
}

//------------------------------------------------------------------------------
// void PrepareToPropagate()
//------------------------------------------------------------------------------
/**
 * Performs initialization needed immediately before propagating.
 */
//------------------------------------------------------------------------------
void Propagate::PrepareToPropagate()
{
   #ifdef DEBUG_PROP_PERFORMANCE
      MessageInterface::ShowMessage(
         "Entered PrepareToPropagate; hasFired = %s\n", 
         (hasFired ? "True" : "False"));
   #endif
   
   if (hasFired == true) 
   {
      for (Integer n = 0; n < (Integer)prop.size(); ++n)
      {
         elapsedTime[n] = 0.0;
         currEpoch[n]   = 0.0;
         fm[n]->SetTime(0.0);
         fm[n]->UpdateInitialData();
      
         p[n]->Initialize();
         p[n]->Update(direction[n] > 0.0);
         state = fm[n]->GetState();
      }   

      baseEpoch.clear();

      for (Integer n = 0; n < (Integer)prop.size(); ++n) {
         #if DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage
               ("Propagate::PrepareToPropagate() SpaceObject names\n");
            
            MessageInterface::ShowMessage
               ("SpaceObject Count = %d\n", satName[n]->size());
            StringArray *sar = satName[n];
            for (Integer i=0; i < (Integer)satName[n]->size(); i++)
            {
               MessageInterface::ShowMessage
                  ("   SpaceObjectName[%d] = %s\n", i, (*sar)[i].c_str());
            }
         #endif
         
         if (satName[n]->empty())
            throw CommandException(
               "Propagator has no associated space objects.");
         GmatBase* sat1 = (*objectMap)[*(satName[n]->begin())];
         baseEpoch.push_back(sat1->GetRealParameter(epochID));
         elapsedTime[n] = fm[n]->GetTime();
         currEpoch[n] = baseEpoch[n] + elapsedTime[n] /
            GmatTimeUtil::SECS_PER_DAY;
         #if DEBUG_PROPAGATE_DIRECTION
            MessageInterface::ShowMessage(
               "Propagate::PrepareToPropagate() running %s %s.\n",
               prop[n]->GetName().c_str(),
               (prop[n]->GetPropagator()->GetRealParameter("InitialStepSize") > 0.0
                  ? "forwards" : "backwards"));
             MessageInterface::ShowMessage("   direction =  %lf.\n",
               direction[n]);
         #endif
      }
   
      // Now setup the stopping condition elements
      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage
            ("Propagate::PrepareToPropagate() Propagate start; epoch = %f\n",
          (baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY));
         MessageInterface::ShowMessage
            ("Propagate::PrepareToPropagate() Propagate start; fm epoch = %f\n",
            (fm[0]->GetRealParameter(fm[0]->GetParameterID("Epoch"))));
         Integer stopCondCount = stopWhen.size();
         MessageInterface::ShowMessage
            ("Propagate::PrepareToPropagate() stopCondCount = %d\n", stopCondCount);
            
         for (Integer i=0; i<stopCondCount; i++)
         {
            MessageInterface::ShowMessage
               ("Propagate::PrepareToPropagate() stopCondName[%d]=%s\n", i,
                      stopWhen[i]->GetName().c_str());
         }
      #endif
       
      stopCondMet = false;
      stopEpoch = 0.0;
      std::string stopVar;
      Real stopEpochBase;
      
      #ifdef DEBUG_STOPPING_CONDITIONS
         if (!singleStepMode)
            MessageInterface::ShowMessage(
               "Stopping condition IDs are [%d, %d, %d]\n",
               stopCondEpochID, stopCondBaseEpochID, stopCondStopVarID);
      #endif
      
      
      try {
         for (UnsignedInt i = 0; i<stopWhen.size(); i++)
         {
            if (i >= stopSats.size())
               throw CommandException("Stopping condition " + 
               stopWhen[i]->GetName() + " has no associated spacecraft.");
      
            #if DEBUG_PROPAGATE_EXE
               MessageInterface::ShowMessage(
                  "Propagate::PrepareToPropagate() stopSat = %s\n",
                  stopSats[i]->GetName().c_str());
            #endif
      
            stopEpochBase = stopSats[i]->GetRealParameter(epochID);
            
            // StopCondition need new base epoch
            stopWhen[i]->SetRealParameter(stopCondBaseEpochID, stopEpochBase);
      
            // ElapsedTime parameters need new initial epoch
            stopVar = stopWhen[i]->GetStringParameter(stopCondStopVarID);
            if (stopVar.find("Elapsed") != stopVar.npos)
            {
               stopWhen[i]->GetStopParameter()->
                  SetRealParameter("InitialEpoch", stopEpochBase);
            }
         }
      }
      catch (BaseException &ex) {
         MessageInterface::ShowMessage(
            "Propagate::PrepareToPropagate() Exception while initializing stopping "
            "conditions\n");
         inProgress = false;
         throw;
      }

      // Publish the initial data
      pubdata[0] = currEpoch[0];
      memcpy(&pubdata[1], state, dim*sizeof(Real));
      publisher->Publish(streamID, pubdata, dim+1);

      inProgress = true;
      return;
   }

   // Reset the initialization data
   Initialize();

   dim = 0;
   p.clear();
   fm.clear();
   
   for (Integer n = 0; n < (Integer)prop.size(); ++n) {
      elapsedTime.push_back(0.0);
      currEpoch.push_back(0.0);
      p.push_back(prop[n]->GetPropagator());
      fm.push_back(prop[n]->GetForceModel());
      fm[n]->SetTime(0.0);
      fm[n]->UpdateInitialData();
   
      p[n]->Initialize();
      p[n]->Update(direction[n] > 0.0);
      state = fm[n]->GetState();
      dim += fm[n]->GetDimension();
   }   

   pubdata = new Real[dim+1];
   baseEpoch.clear();
   
   for (Integer n = 0; n < (Integer)prop.size(); ++n) {
      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage
            ("Propagate::PrepareToPropagate() SpaceObject names\n");
         
         MessageInterface::ShowMessage
            ("SpaceObject Count = %d\n", satName[n]->size());
         StringArray *sar = satName[n];
         for (Integer i=0; i < (Integer)satName[n]->size(); i++)
         {
            MessageInterface::ShowMessage
               ("   SpaceObjectName[%d] = %s\n", i, (*sar)[i].c_str());
         }
      #endif
      
      if (satName[n]->empty())
         throw CommandException(
            "Propagator has no associated space objects.");
      GmatBase* sat1 = (*objectMap)[*(satName[n]->begin())];
//      if (n == 0)
//         epochID = sat1->GetParameterID("A1Epoch");
      baseEpoch.push_back(sat1->GetRealParameter(epochID));
      elapsedTime[n] = fm[n]->GetTime();
      currEpoch[n] = baseEpoch[n] + elapsedTime[n] /
         GmatTimeUtil::SECS_PER_DAY;
      #if DEBUG_PROPAGATE_DIRECTION
         MessageInterface::ShowMessage(
            "Propagate::PrepareToPropagate() running %s %s.\n",
            prop[n]->GetName().c_str(),
            (prop[n]->GetPropagator()->GetRealParameter("InitialStepSize") > 0.0
               ? "forwards" : "backwards"));
          MessageInterface::ShowMessage("   direction =  %lf.\n",
            direction[n]);
      #endif
   }

   // Now setup the stopping condition elements
   #if DEBUG_PROPAGATE_EXE
      MessageInterface::ShowMessage
         ("Propagate::PrepareToPropagate() Propagate start; epoch = %f\n",
       (baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY));
      MessageInterface::ShowMessage
         ("Propagate::PrepareToPropagate() Propagate start; fm epoch = %f\n",
         (fm[0]->GetRealParameter(fm[0]->GetParameterID("Epoch"))));
      Integer stopCondCount = stopWhen.size();
      MessageInterface::ShowMessage
         ("Propagate::PrepareToPropagate() stopCondCount = %d\n", stopCondCount);
      for (Integer i=0; i<stopCondCount; i++)
      {
         MessageInterface::ShowMessage
            ("Propagate::PrepareToPropagate() stopCondName[%d]=%s\n", i,
                   stopWhen[i]->GetName().c_str());
      }
   #endif
    
   stopCondMet = false;
   stopEpoch = 0.0;
   std::string stopVar;
   Real stopEpochBase;
   
   try {
      for (UnsignedInt i = 0; i<stopWhen.size(); i++)
      {
         if (i >= stopSats.size())
            throw CommandException("Stopping condition " + 
            stopWhen[i]->GetName() + " has no associated spacecraft.");
   
         #if DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage(
               "Propagate::PrepareToPropagate() stopSat = %s\n",
               stopSats[i]->GetName().c_str());
         #endif
   
         stopEpochBase = stopSats[i]->GetRealParameter(epochID);
         
         // StopCondition need new base epoch
         stopWhen[i]->SetRealParameter(stopCondBaseEpochID, stopEpochBase);
   
         // ElapsedTime parameters need new initial epoch
         stopVar = stopWhen[i]->GetStringParameter(stopCondStopVarID);
         if (stopVar.find("Elapsed") != stopVar.npos)
         {
            stopWhen[i]->GetStopParameter()->
               SetRealParameter("InitialEpoch", stopEpochBase);
         }
      }
   }
   catch (BaseException &ex) {
      MessageInterface::ShowMessage(
         "Propagate::PrepareToPropagate() Exception while initializing stopping "
         "conditions\n");
      inProgress = false;
      throw;
   }
   
   // Publish the initial data
   pubdata[0] = currEpoch[0];
   memcpy(&pubdata[1], state, dim*sizeof(Real));
   publisher->Publish(streamID, pubdata, dim+1);

   hasFired = true;
   inProgress = true;

   #ifdef DEBUG_FIRST_CALL
   if (state)
   {
      MessageInterface::ShowMessage("Debugging first step\n");
      MessageInterface::ShowMessage(
         "State = [%16.9lf %16.9lf %16.9lf %16.14lf %16.14lf %16.14lf]\n",
         state[0], state[1], state[2], state[3], state[4], state[5]);
      MessageInterface::ShowMessage(
         "Propagator = \n%s\n", 
         prop[0]->GetGeneratingString(Gmat::SCRIPTING, "   ").c_str());
   }
   else
      MessageInterface::ShowMessage("Debugging first step: State not set\n");
   firstStepFired = true;
   #endif
}


//------------------------------------------------------------------------------
// void CheckStopConditions(Integer epochID)
//------------------------------------------------------------------------------
/**
 * Checks the status of the stopping conditions.
 * 
 * @param epochID The parameter ID associated with the epoch field.
 */
//------------------------------------------------------------------------------
void Propagate::CheckStopConditions(Integer epochID)
{
   //------------------------------------------
   // loop through StopCondition list
   //------------------------------------------
   #ifdef DEBUG_STOPPING_CONDITIONS
   try {
   #endif
      for (UnsignedInt i=0; i<stopWhen.size(); i++)
      {
         // StopCondition need epoch for the Interpolator
         stopWhen[i]->SetRealParameter(stopCondEpochID,
            stopSats[i]->GetRealParameter(epochID));
         
         #ifdef DEBUG_STOPPING_CONDITIONS
            MessageInterface::ShowMessage(
               "Evaluating \"%s\" Stopping condition\n",
               stopWhen[i]->GetName().c_str());
         #endif
         
         if (stopWhen[i]->Evaluate())
         {
            stopInterval = stopWhen[i]->GetStopInterval();
            if (stopInterval == 0.0)
            {
               stopEpoch = stopWhen[i]->GetStopEpoch();
            }
            stopCondMet = true;
            stopTrigger = i;
//            stopEpoch = (stopWhen[i]->GetStopEpoch());
            #if DEBUG_PROPAGATE_EXE
               MessageInterface::ShowMessage
                  ("Propagate::CheckStopConditions() %s met\n", 
                   stopWhen[i]->GetName().c_str());
            #endif
            break; // exit if any stop condition met
         }
      }
      
   #ifdef DEBUG_STOPPING_CONDITIONS
   }
   catch (BaseException &ex) {
      MessageInterface::ShowMessage(
         "Propagate::PrepareToPropagate() Exception while evaluating stopping "
         "conditions\n");
      inProgress = false;
      throw;
   }
   #endif
}


//------------------------------------------------------------------------------
// void TakeFinalStep(Integer EpochID, Integer trigger)
//------------------------------------------------------------------------------
/**
 * Takes the final prop step based on data from teh stopping conditions.
 * 
 * @param epochID The parameter ID associated with the epoch field.
 * @param trigger Index indicating which stopping condition was met.
 */
//------------------------------------------------------------------------------
void Propagate::TakeFinalStep(Integer EpochID, Integer trigger)
{
   #if DEBUG_PROPAGATE_EXE
      MessageInterface::ShowMessage(
         "Propagate::TakeFinalStep currEpoch = %f, stopEpoch = %f, "
         "elapsedTime = %f\n", currEpoch[0], stopEpoch, elapsedTime[0]);
   #endif
   
   // Interpolate to get the stop epoch
   if (stopInterval != 0.0)
   {
      if (stopTrigger < 0)
         throw CommandException(
            "Stopping condition was not set for final step on the line \n" +
            GetGeneratingString(Gmat::SCRIPTING));

      // First save the spacecraft for later restoration
      for (UnsignedInt i = 0; i < fm.size(); ++i) 
      {
         #if DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage("   CurrentEpoch[%d] = %.12lf\n", i,
               currEpoch[i]);
         #endif
         fm[i]->UpdateSpaceObject(currEpoch[i]);
      }
      BufferSatelliteStates(true);
      
      // Now fill in the ring buffer
      Real ringStep = stopInterval / 4.0;
      Integer ringStepsTaken = 0;
      bool firstRingStep = true;
      bool stopIsBracketed = false;
      Real elapsedSeconds = 0.0;

      while ((!stopIsBracketed) && (ringStepsTaken < 8))
      {
         // Take a fixed prop step
         if (!TakeAStep(ringStep))
            throw CommandException("Propagator Failed to Step fixed interval "
               "while filling ring buffer\n");
         elapsedSeconds += ringStep;

         // Update spacecraft for that step
         for (UnsignedInt i = 0; i < fm.size(); ++i) 
         {
            fm[i]->UpdateSpaceObject(
               baseEpoch[i] + fm[i]->GetTime() / GmatTimeUtil::SECS_PER_DAY);
         }

         // Update the data in the stop condition
         stopWhen[stopTrigger]->SetRealParameter(stopCondEpochID,
            elapsedSeconds);
         stopIsBracketed = stopWhen[stopTrigger]->AddToBuffer(firstRingStep);
         
         ++ringStepsTaken;
         firstRingStep = false;
      }

      // Now interpolate the epoch...
      stopEpoch = stopWhen[stopTrigger]->GetStopEpoch();

      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage(
            "Propagate::TakeFinalStep set the stopEpoch = %.12lf\n", stopEpoch);
      #endif
      
      // ...and restore the spacecraft and force models
      BufferSatelliteStates(false);
      for (UnsignedInt i = 0; i < fm.size(); ++i) 
      {
         fm[i]->UpdateFromSpaceObject();
         // Back out the steps talen to build the ring buffer
         fm[i]->SetTime(fm[i]->GetTime() - ringStepsTaken * ringStep);

         #if DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage(
               "Force model base Epoch = %.12lf  elapsedTime = %.12lf  "
               "net Epoch = %.12lf\n", baseEpoch[i], fm[i]->GetTime(), 
               baseEpoch[i] + fm[i]->GetTime() / GmatTimeUtil::SECS_PER_DAY);
         #endif
      }
   }
   
   Real secsToStep = stopEpoch;
//      (stopEpoch - currEpoch[trigger]) * GmatTimeUtil::SECS_PER_DAY;

   #if DEBUG_PROPAGATE_EXE
      MessageInterface::ShowMessage(
         "Step = %.12lf sec, calculated off of %.12lf and  %.12lf\n", 
         secsToStep, stopEpoch, currEpoch[trigger]);
   #endif
      
   // Perform stepsize rounding.  Note that the rounding precision can be set
   // by redefining the macro TIME_ROUNDOFF at the top of this file.  Set it to
   // 0.0 to prevent rounding.
   if (TIME_ROUNDOFF != 0.0)
      secsToStep = std::floor(secsToStep / TIME_ROUNDOFF + 0.5) * TIME_ROUNDOFF;

   #if defined DEBUG_PROPAGATE_STEPSIZE or defined DEBUG_PROPAGATE_DIRECTION
      MessageInterface::ShowMessage
         ("Propagate::TakeFinalStep secsToStep at stop = %16.10le\n",
          secsToStep);
   #endif
   #ifdef DEBUG_PROPAGATE_DIRECTION
      MessageInterface::ShowMessage
         ("   stopEpoch = %16.10lf\n   currEpoch = %16.10lf\n",
          stopEpoch, currEpoch[trigger]);
   #endif

   // Toggle propagators into final step mode
   for (std::vector<Propagator*>::iterator current = p.begin(); 
        current != p.end(); ++current)
      (*current)->SetAsFinalStep(true);
   
   if (secsToStep * direction[trigger] > 0.0)
   {
      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage(
            "Propagate::TakeFinalStep: Step(%16.13le) from epoch = %16.10lf\n", 
            secsToStep, 
            (baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY));
      #endif

      if (!TakeAStep(secsToStep))
         throw CommandException("Propagator Failed to Step fixed interval\n");

      for (UnsignedInt i = 0; i < fm.size(); ++i) 
      {
         fm[i]->UpdateSpaceObject(
            baseEpoch[i] + fm[i]->GetTime() / GmatTimeUtil::SECS_PER_DAY);
      }
      // Publish the final data point here
      pubdata[0] = baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY;
      memcpy(&pubdata[1], state, dim*sizeof(Real));
      publisher->Publish(streamID, pubdata, dim+1);
      
      if (!stopWhen[stopTrigger]->IsTimeCondition())
         hasStoppedOnce       = true;     // Only set for interpolated stops
      stepsTaken              = 0;
         
      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage
            ("Propagate::TakeFinalStep: Step(%16.13le) advanced to epoch = %16.10lf\n", 
            secsToStep,
            (baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY));
      #endif
   
      publisher->FlushBuffers();
    
      for (std::vector<StopCondition *>::iterator i = stopWhen.begin(); 
           i != stopWhen.end(); ++i)
      {
         if ((*i)->GetName() == "")
         {
            StopCondition *localSc = *i;
            stopWhen.erase(i);
            delete localSc;
         }
      }
      
      #if DEBUG_PROPAGATE_EXE
         MessageInterface::ShowMessage
            ("Propagate::TakeFinalStep complete; epoch = %16.10lf\n",
             (baseEpoch[0] + fm[0]->GetTime() / GmatTimeUtil::SECS_PER_DAY));
      #endif
   }

   // Toggle propagators out of final step mode
   for (std::vector<Propagator*>::iterator current = p.begin(); 
        current != p.end(); ++current)
      (*current)->SetAsFinalStep(false);
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
/**
 * Propagate the assigned members to the desired stopping condition
 *
 * @return true if the Command runs to completion, false if an error
 *         occurs.
 */
//------------------------------------------------------------------------------
bool Propagate::Execute()
{
   #if DEBUG_PROPAGATE_EXE
      MessageInterface::ShowMessage("Propagate::Execute() entered.\n");
   #endif

   if (initialized == false)
      throw CommandException("Propagate Command was not Initialized\n");

   // Parm used to check for interrupt in the propagation   
   Integer checkCount = 0, trigger = 0;

   try {
      if (!inProgress)
      {
         PrepareToPropagate();

         // Check for initial stop condition before first step in while loop
         // eg) elapsed time of 0 (loj: 4/6/06 added)
         if (publisher->GetRunState() == Gmat::RUNNING)
         {
            for (UnsignedInt i=0; i<stopWhen.size(); i++)
            {
               if (stopWhen[i]->Evaluate())
               {
                  stopInterval = stopWhen[i]->GetStopInterval();
                  stopTrigger = i;
                  stopCondMet = true;
                  stopEpoch = (stopWhen[i]->GetStopEpoch());
                  #if DEBUG_PROPAGATE_EXE
                     MessageInterface::ShowMessage
                        ("Propagate::Execute() %s met\n",
                         stopWhen[i]->GetName().c_str());
                  #endif
                  break; // exit if any stop condition met
               }
            }
         }
      }
      
      while (!stopCondMet)
      {
         // Update the epoch on the force models
         for (UnsignedInt i = 0; i < fm.size(); ++i)
         {
            fm[i]->UpdateInitialData();
         }
         #ifdef DEBUG_EPOCH_UPDATES
            fm[0]->ReportEpochData();
         #endif

         if (!TakeAStep())
            throw CommandException(
               "Propagate::Execute() Propagator Failed to Step\n");
         for (UnsignedInt i = 0; i < fm.size(); ++i) {
            // orbit related parameters use spacecraft for data
            elapsedTime[i] = fm[i]->GetTime();
            currEpoch[i] = baseEpoch[i] + elapsedTime[i] /
               GmatTimeUtil::SECS_PER_DAY;
            
            // Update spacecraft epoch, without argument the spacecraft epoch
            // won't get updated for consecutive Propagate command
            fm[i]->UpdateSpaceObject(currEpoch[i]);
         }

         if (singleStepMode)
            break;

         CheckStopConditions(epochID);
         ++stepsTaken;
         
         /// @todo Make stop triggering more robust when using hasStoppedOnce.
         // Ensure a step across a stopping condition if we already stopped once
         if (hasStoppedOnce && (stepsTaken < 2))
         {
            stopCondMet = false;
         }
         
         if (!stopCondMet)
         {
            // Publish the data here
            pubdata[0] = currEpoch[0];
            memcpy(&pubdata[1], state, dim*sizeof(Real));
            publisher->Publish(streamID, pubdata, dim+1);
         }
         else
         {  
            for (UnsignedInt i = 0; i < fm.size(); ++i) 
            {
               fm[i]->RevertSpaceObject();
               elapsedTime[i] = fm[i]->GetTime();
               currEpoch[i] = baseEpoch[i] +
                  elapsedTime[i] / GmatTimeUtil::SECS_PER_DAY;
            }
         }
         
         #if DEBUG_PROPAGATE_EXE
            MessageInterface::ShowMessage(
               "Propagate::Execute() intermediate; epoch = %f\n", currEpoch[0]);
         #endif
   
         // Periodically see if the user has stopped the run 
         ++checkCount;
         if ((checkCount == interruptCheckFrequency) && !stopCondMet)
         {
            inProgress = true;
            return true;
         }
      }
   }
   catch (BaseException &ex) {
      MessageInterface::ShowMessage
         ("Propagate::Execute() setting inProgress to false\n");
      inProgress = false;
      throw;
   }

   #ifdef DEBUG_EPOCH_UPDATES
      fm[0]->ReportEpochData();
   #endif

   inProgress = false;
   if (!singleStepMode)
   {
      TakeFinalStep(epochID, trigger);
      // reset the stopping conditions so that scanning starts over
      for (UnsignedInt i=0; i<stopWhen.size(); i++)
         stopWhen[i]->Reset(); 
   }

   #ifdef DEBUG_EPOCH_UPDATES
      fm[0]->ReportEpochData();
   #endif
   
   ClearTransientForces();
   // Only build command summary if not in single step mode
   if (!singleStepMode)
      BuildCommandSummary(true);
   return true;
}


//------------------------------------------------------------------------------
// void RunComplete()
//------------------------------------------------------------------------------
/**
 * Resets the Propagate command to an uninitialized state.
 */
//------------------------------------------------------------------------------
void Propagate::RunComplete()
{
   if (inProgress)
      publisher->FlushBuffers();
   
   inProgress = false;
   hasFired = false;
//   
//   for (std::vector<Propagator*>::iterator prop = p.begin(); prop != p.end(); 
//        ++prop)
//      (*prop)->ResetInitialData();
//   

   #ifdef DEBUG_FIRST_CALL
      firstStepFired = false;
   #endif
   
   GmatCommand::RunComplete();
}


//------------------------------------------------------------------------------
// void AddTransientForce(StringArray *sats, ForceModel *p)
//------------------------------------------------------------------------------
/**
 * Passes transient forces into the ForceModel(s).
 *
 * @param <sats> The array of satellites used in the ForceModel.
 * @param <p>    The current ForceModel that is receiving the forces.
 */
//------------------------------------------------------------------------------
void Propagate::AddTransientForce(StringArray *sats, ForceModel *p)
{
   // Find any transient force that is active and add it to the force model
   for (std::vector<PhysicalModel*>::iterator i = transientForces->begin();
        i != transientForces->end(); ++i) {
      StringArray tfSats = (*i)->GetRefObjectNameArray(Gmat::SPACECRAFT);
      // Loop through the spacecraft that go with the force model, ans see if 
      // they are in the spacecraft list for the current transient force
      for (StringArray::iterator current = sats->begin(); 
           current != sats->end(); ++current) {
         if (find(tfSats.begin(), tfSats.end(), *current) != tfSats.end()) {
            p->AddForce(*i);
            break;      // Avoid multiple adds
         }
      }
   }

   #ifdef DEBUG_PROPAGATE_INIT
      ForceModel *fm;
      PhysicalModel *pm;
   
      MessageInterface::ShowMessage(
         "Propagate::AddTransientForces completed; force details:\n");
      for (std::vector<PropSetup*>::iterator p = prop.begin(); 
           p != prop.end(); ++p) {
         fm = (*p)->GetForceModel();
         if (!fm)
            throw CommandException("ForceModel not set in PropSetup \"" + 
                                   (*p)->GetName() + "\"");
         MessageInterface::ShowMessage(
            "   Forces in %s:\n", fm->GetName().c_str());
         for (Integer i = 0; i < fm->GetNumForces(); ++i) {
            pm = fm->GetForce(i);
            MessageInterface::ShowMessage(
               "      %s   %s\n", pm->GetTypeName().c_str(),
               pm->GetName().c_str());
         }
      }
   #endif
}


//------------------------------------------------------------------------------
// void ClearTransientForce()
//------------------------------------------------------------------------------
/**
 * Removes transient forces from the ForceModel(s) after propagation.
 */
//------------------------------------------------------------------------------
void Propagate::ClearTransientForces()
{
   ForceModel *fm;
   PhysicalModel *pm;
   
   // Loop through the forces in each force model, and remove transient ones
   for (std::vector<PropSetup*>::iterator p = prop.begin(); 
        p != prop.end(); ++p) {
      fm = (*p)->GetForceModel();
      if (!fm)
         throw CommandException("ForceModel not set in PropSetup \"" + 
                                (*p)->GetName() + "\"");
      for (Integer i = 0; i < fm->GetNumForces(); ++i) {
         pm = fm->GetForce(i);
         if (pm->IsTransient()) {
            fm->DeleteForce(pm->GetName());
         }
      }
   }
   
   #ifdef DEBUG_PROPAGATE_INIT
      MessageInterface::ShowMessage(
         "Propagate::ClearTransientForces completed; force details:\n");
      for (std::vector<PropSetup*>::iterator p = prop.begin(); 
           p != prop.end(); ++p) {
         fm = (*p)->GetForceModel();
         if (!fm)
            throw CommandException("ForceModel not set in PropSetup \"" + 
                                   (*p)->GetName() + "\"");
         MessageInterface::ShowMessage(
            "   Forces in %s:\n", fm->GetName().c_str());
         for (Integer i = 0; i < fm->GetNumForces(); ++i) {
            pm = fm->GetForce(i);
            MessageInterface::ShowMessage(
               "      %s   %s\n", pm->GetTypeName().c_str(),
               pm->GetName().c_str());
         }
      }
   #endif
}


//------------------------------------------------------------------------------
// void SetNames(const std::string& name, StringArray& owners,
//               StringArray& elements)
//------------------------------------------------------------------------------
/**
 * Sets the parameter names used when publishing Spacecraft data.
 *
 * @param <name>     Name of the Spacecraft that is referenced.
 * @param <owners>   Array of published data identifiers.
 * @param <elements> Individual elements of the published data.
 */
//------------------------------------------------------------------------------
void Propagate::SetNames(const std::string& name, StringArray& owners,
                         StringArray& elements)
{
   // Add satellite labels
   for (Integer i = 0; i < 6; ++i)
      owners.push_back(name);       // X, Y, Z, Vx, Vy, Vz
      
   elements.push_back(name+".X");
   elements.push_back(name+".Y");
   elements.push_back(name+".Z");
   elements.push_back(name+".Vx");
   elements.push_back(name+".Vy");
   elements.push_back(name+".Vz");
}


//------------------------------------------------------------------------------
// std::string CreateParameter(const std::string name)
//------------------------------------------------------------------------------
std::string Propagate::CreateParameter(const std::string &name)
{
   std::string str = name;
   Real rval;

   if (GmatStringUtil::ToDouble(str, &rval))
       return str;

   Moderator *theModerator = Moderator::Instance();
   std::string owner, dep, type;
   Parameter *param;
   
   str = GmatStringUtil::Trim(str, GmatStringUtil::BOTH);
   GmatStringUtil::ParseParameter(str, type, owner, dep);
   
   #ifdef DEBUG_PROPAGATE_ASSEMBLE
   MessageInterface::ShowMessage
      ("Propagate::CreateParameter() name=%s, type=%s, owner=%s, dep=%s\n",
       name.c_str(), type.c_str(), owner.c_str(), dep.c_str());
   #endif

   
   param = theModerator->CreateParameter(type, str, owner, dep);
   
//    if (type != "")
//       param = theModerator->CreateParameter(type, str, owner, dep);
//    else
//       return str;
   
//    Moderator *theModerator = Moderator::Instance();
//    std::string owner, dep, type;

   //loj: this caused crash
   // remove blanks
   //    for (std::string::iterator i = str.begin(); i != str.end(); ++i)
   //       if (*i == ' ')
   //          str.erase(i);
   
//    str = GmatStringUtil::Trim(name, GmatStringUtil::BOTH);
   
//    #if DEBUG_PROPAGATE_OBJ
//       MessageInterface::ShowMessage
//          ("Propagate::CreateParameter() name=<%s>, str=<%s>\n",
//           name.c_str(), str.c_str());
//    #endif

//    // if string is a number
//    if (isdigit(str[0]) || str[0] == '.' || str[0] == '-')
//       return str;

//    // if parameter exist
//    if (theModerator->GetParameter(str))
//       return str;
   
//    std::string::size_type pos1 = str.find('.');
   
//    if (pos1 != str.npos)
//       owner = str.substr(0, pos1);
//    else
//       return str;
   
//    std::string::size_type pos2 = str.find(pos1);
//    if (pos2 != str.npos)
//    {
//       dep = str.substr(pos1, pos2-pos1);
//       type = str.substr(pos2+1);
//    }
//    else
//    {
//       type = str.substr(pos1+1, pos2-pos1);
//    }
   
//    #if DEBUG_PROPAGATE_OBJ
//       MessageInterface::ShowMessage
//          ("Propagate::CreateParameter() str=%s, owner=%s, dep=%s, type=%s\n",
//           str.c_str(), owner.c_str(), dep.c_str(), type.c_str());
//    #endif


//    Parameter *param = theModerator->CreateParameter(type, str);
//    param->SetRefObjectName(Gmat::SPACECRAFT, owner);
   
//    if (param->IsCoordSysDependent())
//    {
//       if (dep == "")
//          dep = "EarthMJ2000Eq";
      
//       param->SetStringParameter("DepObject", dep);
//       param->SetRefObjectName(Gmat::COORDINATE_SYSTEM, dep);
//    }
   
//    if (param->IsOriginDependent())
//    {
//       if (dep == "")
//          dep = "Earth";
      
//       param->SetStringParameter("DepObject", dep);
//       param->SetRefObjectName(Gmat::SPACE_POINT, dep);
      
//       if (param->NeedCoordSystem())
//          /// @todo Update coordinate system to better value for body parms
//          param->SetRefObjectName(Gmat::COORDINATE_SYSTEM, "EarthMJ2000Eq");
//    }
   
   #if DEBUG_PROPAGATE_OBJ
      MessageInterface::ShowMessage
         ("Propagate::CreateParameter() name=%s, owner=%s, dep=%s, type=%s\n",
          param->GetName().c_str(), param->GetStringParameter("Object").c_str(),
          param->GetStringParameter("DepObject").c_str(),
          param->GetTypeName().c_str());
   #endif
   
   return str;
}


//------------------------------------------------------------------------------
// void AddToBuffer(SpaceObject *so)
//------------------------------------------------------------------------------
/**
 * Adds satellites and formations to the state buffer.
 * 
 * @param <so> The SpaceObject that is added.
 */
//------------------------------------------------------------------------------
void Propagate::AddToBuffer(SpaceObject *so)
{
   #ifdef DEBUG_STOPPING_CONDITIONS
      MessageInterface::ShowMessage("Buffering states for '%s'\n", 
         so->GetName().c_str());
   #endif
   
   if (so->IsOfType(Gmat::SPACECRAFT))
   {
      satBuffer.push_back((Spacecraft *)(so->Clone()));
   }
   else if (so->IsOfType(Gmat::FORMATION))
   {
      Formation *form = (Formation*)so;
      formBuffer.push_back((Formation *)(so->Clone()));
      StringArray formSats = form->GetStringArrayParameter("Add");
      
      for (StringArray::iterator i = formSats.begin(); i != formSats.end(); ++i)
         AddToBuffer((SpaceObject *)(*objectMap)[*i]);
   }
   else
      throw CommandException("Object " + so->GetName() + " is not either a "
         "Spacecraft or a Formation; cannot buffer the object for propagator "
         "stopping conditions.");
}


//------------------------------------------------------------------------------
// void EmptyBuffer()
//------------------------------------------------------------------------------
/**
 * Cleans up the satellite state buffer.
 */
//------------------------------------------------------------------------------
void Propagate::EmptyBuffer()
{
   for (std::vector<Spacecraft *>::iterator i = satBuffer.begin(); 
        i != satBuffer.end(); ++i)
   {
      delete (*i);
   }
   satBuffer.clear();
   
   for (std::vector<Formation *>::iterator i = formBuffer.begin(); 
        i != formBuffer.end(); ++i)
   {
      delete (*i);
   }
   formBuffer.clear();
}

//------------------------------------------------------------------------------
// void BufferSatelliteStates(bool fillingBuffer)
//------------------------------------------------------------------------------
/**
 * Preserves satellite state data so it can be restored after interpolating the 
 * stopping condition propagation time.
 * 
 * @param <fillingBuffer> Flag used to indicate the fill direction.
 */
//------------------------------------------------------------------------------
void Propagate::BufferSatelliteStates(bool fillingBuffer)
{
   Spacecraft *fromSat, *toSat;
   Formation *fromForm, *toForm;
   std::string soName;
   
   for (std::vector<Spacecraft *>::iterator i = satBuffer.begin(); 
        i != satBuffer.end(); ++i)
   {
      soName = (*i)->GetName();
      if (fillingBuffer)
      {
         fromSat = (Spacecraft *)((*objectMap)[soName]);
         toSat = *i;
      }
      else
      {
         fromSat = *i;
         toSat = (Spacecraft *)((*objectMap)[soName]);
      }

      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage(
            "   Sat is %s, fill direction is %s; fromSat epoch = %.12lf   "
            "toSat epoch = %.12lf\n",
            fromSat->GetName().c_str(),
            (fillingBuffer ? "from propagator" : "from buffer"),
            fromSat->GetRealParameter("A1Epoch"), 
            toSat->GetRealParameter("A1Epoch"));

         MessageInterface::ShowMessage(
            "   '%s' Satellite state:\n", fromSat->GetName().c_str());
         Real *satrv = fromSat->GetState().GetState();
         MessageInterface::ShowMessage(
            "      %.12lf  %.12lf  %.12lf\n      %.12lf  %.12lf  %.12lf\n",
            satrv[0], satrv[1], satrv[2], satrv[3], satrv[4], satrv[5]);
      #endif
      
      (*toSat) = (*fromSat);
      
      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage(
            "After copy, From epoch %.12lf to epoch %.12lf\n",
            fromSat->GetRealParameter("A1Epoch"), 
            toSat->GetRealParameter("A1Epoch"));
      #endif      
   }

   for (std::vector<Formation *>::iterator i = formBuffer.begin(); 
        i != formBuffer.end(); ++i)
   {
      soName = (*i)->GetName();
      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage("Buffering formation %s, filling = %s\n", 
            soName.c_str(), (fillingBuffer?"true":"false"));
      #endif
      if (fillingBuffer)
      {
         fromForm = (Formation *)((*objectMap)[soName]);
         toForm = *i;
      }
      else
      {
         fromForm = *i;
         toForm = (Formation *)((*objectMap)[soName]);
      }

      #ifdef DEBUG_STOPPING_CONDITIONS
         MessageInterface::ShowMessage(
            "   Formation is %s, fill direction is %s; fromForm epoch = %.12lf"
            "   toForm epoch = %.12lf\n",
            fromForm->GetName().c_str(),
            (fillingBuffer ? "from propagator" : "from buffer"),
            fromForm->GetRealParameter("A1Epoch"), 
            toForm->GetRealParameter("A1Epoch"));
      #endif
      
      (*toForm) = (*fromForm);
      
      toForm->UpdateState();
      
      #ifdef DEBUG_STOPPING_CONDITIONS
         Integer count = fromForm->GetStringArrayParameter("Add").size();

         MessageInterface::ShowMessage(
            "After copy, From epoch %.12lf to epoch %.12lf\n",
            fromForm->GetRealParameter("A1Epoch"), 
            toForm->GetRealParameter("A1Epoch"));

         MessageInterface::ShowMessage(
            "   %s for '%s' Formation state:\n", 
            (fillingBuffer ? "Filling buffer" : "Restoring states"),
            fromForm->GetName().c_str());

         Real *satrv = fromForm->GetState().GetState();
         
         for (Integer i = 0; i < count; ++i)
            MessageInterface::ShowMessage(
               "      %d:  %.12lf  %.12lf  %.12lf  %.12lf  %.12lf  %.12lf\n",
               i, satrv[i*6], satrv[i*6+1], satrv[i*6+2], satrv[i*6+3], 
               satrv[i*6+4], satrv[i*6+5]);
      #endif      
   }
   
   #ifdef DEBUG_STOPPING_CONDITIONS
      for (std::vector<Spacecraft *>::iterator i = satBuffer.begin(); 
           i != satBuffer.end(); ++i)
         MessageInterface::ShowMessage(
            "   Epoch of '%s' is %.12lf\n", (*i)->GetName().c_str(), 
            (*i)->GetRealParameter("A1Epoch"));
   #endif
}
