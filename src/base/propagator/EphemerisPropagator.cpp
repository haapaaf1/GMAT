//$Id$
//------------------------------------------------------------------------------
//                             EphemerisPropagator
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: Mar 26, 2010 by Darrel Conway (Thinking Systems)
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under the FDSS 
// contract, Task 28
//
/**
 * Implementation for the EphemerisPropagator class
 */
//------------------------------------------------------------------------------


#include "EphemerisPropagator.hpp"
#include "MessageInterface.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"


//#define DEBUG_INITIALIZATION
//#define DEBUG_EXECUTION

//------------------------------------------------------------------------------
// Static Data
//------------------------------------------------------------------------------

/// EphemerisPropagator parameter labels
const std::string
EphemerisPropagator::PARAMETER_TEXT[EphemerisPropagatorParamCount - PropagatorParamCount] =
{
      "StepSize",       // EPHEM_STEP_SIZE
      "CentralBody",    // EPHEM_CENTRAL_BODY
      "EpochFormat",    // EPHEM_EPOCH_FORMAT
      "StartEpoch"      // EPHEM_START_EPOCH
};

/// EphemerisPropagator parameter types
const Gmat::ParameterType
EphemerisPropagator::PARAMETER_TYPE[EphemerisPropagatorParamCount - PropagatorParamCount] =
{
      Gmat::REAL_TYPE,        // EPHEM_STEP_SIZE
      Gmat::OBJECT_TYPE,      // EPHEM_CENTRAL_BODY
      Gmat::STRING_TYPE,      // EPHEM_EPOCH_FORMAT
      Gmat::STRING_TYPE       // EPHEM_START_EPOCH
};


//------------------------------------------------------------------------------
// Public Methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// EphemerisPropagator(const std::string & typeStr, const std::string & name)
//------------------------------------------------------------------------------
/**
 * Default constructor
 *
 * @param typeStr Script description for the propagator type
 * @param name Name of the constructed object
 */
//------------------------------------------------------------------------------
EphemerisPropagator::EphemerisPropagator(const std::string & typeStr,
      const std::string & name) :
   Propagator           (typeStr, name),
   ephemStep            (300.0),
   centralBody          ("Earth"),
   epochFormat          ("A1ModJulian"),
   startEpoch           ("21545.0"),
   initialEpoch         (-987654321.0),
   currentEpoch         (-987654321.0),
   timeFromEpoch        (0.0),
   ephemStart           (-987654321.0),
   ephemEnd             (987654321.0),
   psm                  (NULL),
   state                (NULL),
   j2kState             (NULL),
   stepTaken            (0.0),
   startEpochSource     (FROM_SCRIPT)
{
   parameterCount = EphemerisPropagatorParamCount;
}


//------------------------------------------------------------------------------
// ~EphemerisPropagator()
//------------------------------------------------------------------------------
/**
 * Destructor
 */
//------------------------------------------------------------------------------
EphemerisPropagator::~EphemerisPropagator()
{
   if (state != NULL)
      delete [] state;

   if (j2kState != NULL)
      delete [] j2kState;
}


//------------------------------------------------------------------------------
// EphemerisPropagator(const EphemerisPropagator & ep)
//------------------------------------------------------------------------------
/**
 * Copy constructor
 *
 * @param ep The Ephemeris propagator copied to the new one.
 */
//------------------------------------------------------------------------------
EphemerisPropagator::EphemerisPropagator(const EphemerisPropagator & ep) :
   Propagator           (ep),
   ephemStep            (ep.ephemStep),
   centralBody          (ep.centralBody),
   epochFormat          (ep.epochFormat),
   startEpoch           (ep.startEpoch),
   initialEpoch         (ep.initialEpoch),
   currentEpoch         (ep.currentEpoch),
   timeFromEpoch        (ep.timeFromEpoch),
   ephemStart           (ep.ephemStart),
   ephemEnd             (ep.ephemEnd),
   psm                  (NULL),
   state                (NULL),
   j2kState             (NULL),
   stepTaken            (0.0),
   startEpochSource     (ep.startEpochSource)
{
}


//------------------------------------------------------------------------------
// EphemerisPropagator& operator=(const EphemerisPropagator& ep)
//------------------------------------------------------------------------------
/**
 * Assignment operator
 *
 * @param ep The Ephemeris propagator supplying parameters for this one.
 *
 * @return This EphemerisPropagator configured to match ep.
 */
//------------------------------------------------------------------------------
EphemerisPropagator& EphemerisPropagator::operator=(
      const EphemerisPropagator& ep)
{
   if (this != &ep)
   {
      Propagator::operator=(ep);

      ephemStep     = ep.ephemStep;
      centralBody   = ep.centralBody;
      epochFormat   = ep.epochFormat;
      startEpoch    = ep.startEpoch;
      initialEpoch  = ep.initialEpoch;
      currentEpoch  = ep.currentEpoch;
      timeFromEpoch = ep.timeFromEpoch;
      ephemStart    = ep.ephemStart;
      ephemEnd      = ep.ephemEnd;
      psm           = NULL;
      if (state != NULL)
      {
         delete [] state;
         state = NULL;
      }
      if (j2kState != NULL)
      {
         delete [] j2kState;
         j2kState = NULL;
      }
      stepTaken = 0.0;
      startEpochSource = ep.startEpochSource;
   }

   return *this;
}


//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param str string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer EphemerisPropagator::GetParameterID(const std::string &str) const
{
   for (Integer i = PropagatorParamCount;
         i < EphemerisPropagatorParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - PropagatorParamCount])
         return i;
   }

   return Propagator::GetParameterID(str);
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param id Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetParameterText(const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return PARAMETER_TEXT[id - PropagatorParamCount];
   return Propagator::GetParameterText(id);
}


//---------------------------------------------------------------------------
//  std::string GetParameterUnit(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the unit for the parameter.
 *
 * @param id The integer ID for the parameter.
 *
 * @return unit for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetParameterUnit(const Integer id) const
{
   return Propagator::GetParameterUnit(id);
}


//---------------------------------------------------------------------------
//  Gmat::ParameterType GetParameterType(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the enumerated type of the object.
 *
 * @param id The integer ID for the parameter.
 *
 * @return The enumeration for the type of the parameter, or
 *         UNKNOWN_PARAMETER_TYPE.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType EphemerisPropagator::GetParameterType(
      const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return PARAMETER_TYPE[id - PropagatorParamCount];
   return Propagator::GetParameterType(id);
}


//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetParameterTypeString(const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return Propagator::PARAM_TYPE_STRING[GetParameterType(id)];
   return Propagator::GetParameterTypeString(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param id Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool EphemerisPropagator::IsParameterReadOnly(const Integer id) const
{
   if (id == INITIAL_STEP_SIZE)
      return true;
   return Propagator::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param label Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool EphemerisPropagator::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves the Real parameters specific to the EphemerisPropagator
 *
 * @param id The ID for the parameter that is retrieved
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const Integer id) const
{
   if (id == EPHEM_STEP_SIZE)
   {
      return ephemStep;
   }

   return Propagator::GetRealParameter(id);
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the Real parameters specific to the EphemerisPropagator
 *
 * @param id The ID for the parameter that is retrieved
 * @param value The new value for the parameter
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value)
{
   if (id == EPHEM_STEP_SIZE)
   {
      if (value != 0.0)
         ephemStep = value;
      return ephemStep;
   }

   return Propagator::SetRealParameter(id, value);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves parameters specific to the EphemerisPropagator from a RealArray
 *
 * @param id The ID for the parameter that is retrieved
 * @param index The index of the parameter in the RealArray
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const Integer id,
      const Integer index) const
{
   return Propagator::GetRealParameter(id, index);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id, const Integer row,
//       const Integer col) const
//------------------------------------------------------------------------------
/**
 * Retrieves parameters specific to the EphemerisPropagator from a Real matrix
 *
 * @param id The ID for the parameter that is retrieved
 * @param row The row index of the parameter in the matrix
 * @param col The column index of the parameter in the matrix
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const Integer id, const Integer row,
      const Integer col) const
{
   return Propagator::GetRealParameter(id, row, col);
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value,
//       const Integer index)
//------------------------------------------------------------------------------
/**
 * Retrieves parameters specific to the EphemerisPropagator from a RealArray
 *
 * @param id The ID for the parameter that is retrieved
 * @param value The new value for the parameter
 * @param index The index of the parameter in the RealArray
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value,
      const Integer index)
{
   return Propagator::SetRealParameter(id, value, index);
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value, const Integer row,
//       const Integer col)
//------------------------------------------------------------------------------
/**
 * Retrieves parameters specific to the EphemerisPropagator from a RealArray
 *
 * @param id The ID for the parameter that is retrieved
 * @param value The new value for the parameter
 * @param row The row index of the parameter in the matrix
 * @param col The column index of the parameter in the matrix
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value,
      const Integer row, const Integer col)
{
   return Propagator::SetRealParameter(id, value, row, col);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves a Real parameter from then object
 *
 * @param label Script identifier for the parameter
 *
 * @return The parameter's value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string &label, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a Real parameter
 *
 * @param label Script identifier for the parameter
 * @param value The new value
 *
 * @return The parameter's value at the end of the call
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string &label, const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves a Real parameter value from a RealArray
 *
 * @param label Script identifier for the parameter
 * @param index The index of the parameter in the array
 *
 * @return The parameter's value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const std::string &label,
      const Integer index) const
{
   return GetRealParameter(GetParameterID(label), index);
}


//------------------------------------------------------------------------------
// Real EphemerisPropagator::SetRealParameter(const std::string &label,
//       const Real value, const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets a Real parameter value in a RealArray
 *
 * @param label Script identifier for the parameter
 * @param value The new value
 * @param index The index of the parameter in the array
 *
 * @return The parameter's value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value, const Integer index)
{
   return SetRealParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string &label, const Integer row,
//       const Integer col) const
//------------------------------------------------------------------------------
/**
 * Retrieves a Real parameter value from a matrix of Reals
 *
 * @param label Script identifier for the parameter
 * @param row The row index of the parameter in the matrix
 * @param col The column index of the parameter in the matrix
 *
 * @return The parameter's value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::GetRealParameter(const std::string &label,
      const Integer row, const Integer col) const
{
   return GetRealParameter(GetParameterID(label), row, col);
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string &label, const Real value,
//       const Integer row, const Integer col))
//------------------------------------------------------------------------------
/**
 * Sets a Real parameter value in a matrix of Reals
 *
 * @param label Script identifier for the parameter
 * @param value The new value
 * @param row The row index of the parameter in the matrix
 * @param col The column index of the parameter in the matrix
 *
 * @return The parameter's value
 */
//------------------------------------------------------------------------------
Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value, const Integer row, const Integer col)
{
   return SetRealParameter(GetParameterID(label), value, row, col);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string parameters
 *
 * @param id The id for the parameter
 *
 * @return The string
 */
//------------------------------------------------------------------------------
std::string  EphemerisPropagator::GetStringParameter(const Integer id) const
{
   switch (id)
   {
      case EPHEM_CENTRAL_BODY:
         return centralBody;

      case EPHEM_EPOCH_FORMAT:
         return epochFormat;

      case EPHEM_START_EPOCH:
         return startEpoch;

      default:
         break;
   }

   return Propagator::GetStringParameter(id);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a parameter
 *
 * @param id The ID of the parameter
 * @param value The new parameter value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::SetStringParameter(const Integer id,
      const std::string &value)
{
   switch (id)
   {
      case EPHEM_CENTRAL_BODY:
         centralBody = value;
         return true;

      case EPHEM_EPOCH_FORMAT:
         epochFormat = value;
         return true;

      case EPHEM_START_EPOCH:
         startEpoch = value;
         if (startEpoch == "FromSpacecraft")
            startEpochSource = FROM_SPACECRAFT;
         else if (startEpoch == "EphemStart")
            startEpochSource = FROM_EPHEM;
         else
            startEpochSource = FROM_SCRIPT;
         return true;

      default:
         break;
   }

   return Propagator::SetStringParameter(id, value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves string parameters from a StringArray
 *
 * @param id The id for the parameter
 * @param index Index into the array
 *
 * @return The string
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetStringParameter(const Integer id,
      const Integer index) const
{
   return Propagator::GetStringParameter(id, index);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value,
//       const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets string parameters in a StringArray
 *
 * @param id The id for the parameter
 * @param value The new string value
 * @param index Index into the array
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::SetStringParameter(const Integer id,
      const std::string &value, const Integer index)
{
   return Propagator::SetStringParameter(id, value, index);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string parameters
 *
 * @param label The script name for the parameter
 *
 * @return The string
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetStringParameter(
      const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a parameter
 *
 * @param label The script name for the parameter
 * @param value The new parameter value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves string parameters from a StringArray
 *
 * @param label The script name for the parameter
 * @param index Index into the array
 *
 * @return The string
 */
//------------------------------------------------------------------------------
std::string EphemerisPropagator::GetStringParameter(const std::string &label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value,
//       const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets string parameters in a StringArray
 *
 * @param label The script name for the parameter
 * @param value The new string value
 * @param index Index into the array
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


// Reference object code

//------------------------------------------------------------------------------
// std::string  GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
/**
 * Retrieves the name of a reference object of the specified type
 *
 * @param type The type of the object
 *
 * @return The object's name
 */
//------------------------------------------------------------------------------
std::string  EphemerisPropagator::GetRefObjectName(
      const Gmat::ObjectType type) const
{
      return Propagator::GetRefObjectName(type);
}


//------------------------------------------------------------------------------
// const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Retrieves a list of reference objects of the specified type
 *
 * @param type The type of the objects
 *
 * @return The list of names
 */
//------------------------------------------------------------------------------
const StringArray& EphemerisPropagator::GetRefObjectNameArray(
      const Gmat::ObjectType type)
{
   if ((type == Gmat::SPACECRAFT) || (type == Gmat::SPACEOBJECT) ||
       (type == Gmat::FORMATION))
      return propObjectNames;

   return Propagator::GetRefObjectNameArray(type);
}


//------------------------------------------------------------------------------
// bool SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Passes in the name of a reference object for use during initialization
 *
 * @param type The type of the object
 * @param name The object's name
 *
 * @return true if the name was set, false if not
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::SetRefObjectName(const Gmat::ObjectType type,
      const std::string &name)
{
   bool retval = false;

   if ((type == Gmat::SPACECRAFT) || (type == Gmat::SPACEOBJECT) ||
       (type == Gmat::FORMATION))
   {
      if (find(propObjectNames.begin(), propObjectNames.end(), name) ==
            propObjectNames.end())
         propObjectNames.push_back(name);

      retval = true;
   }

   return (Propagator::SetRefObjectName(type, name) || retval);
}


//------------------------------------------------------------------------------
// bool RenameRefObject(const Gmat::ObjectType type, const std::string &oldName,
//       const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Changes the name of a previously set reference object.
 *
 * This method changes the name or list of names of reference objects.  It does
 * not change the actual objects themselves; that side of the renaming is
 * handled separately.
 *
 * @param type The type of object that is being renamed
 * @param oldName The object's name before the change
 * @param newName The name that now identifies the object
 *
 * @return true if a object name was changed, false if not
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::RenameRefObject(const Gmat::ObjectType type,
      const std::string &oldName, const std::string &newName)
{
   bool retval = false;

   if ((type == Gmat::SPACECRAFT) || (type == Gmat::SPACEOBJECT) ||
       (type == Gmat::FORMATION))
   {
      for (UnsignedInt i = 0; i < propObjectNames.size(); ++i)
         if (propObjectNames[i] == oldName)
            propObjectNames[i] = newName;
      retval = true;
   }

   return (Propagator::RenameRefObject(type, oldName, newName) || retval);
}


bool EphemerisPropagator::SetRefObject(GmatBase *obj,
      const Gmat::ObjectType type, const std::string &name)
{
   bool retval = false;

   if (obj->IsOfType(Gmat::SPACEOBJECT))
   {
      for (UnsignedInt i = 0; i < propObjectNames.size(); ++i)
         if (propObjectNames[i] == name)
         {
            propObjects[i] = obj;
            retval = true;
         }
      if (retval == false)
      {
         propObjectNames.push_back(name);
         propObjects.push_back(obj);
      }
      return true;
   }

   return (Propagator::SetRefObject(obj, type, name) || retval);
}


bool EphemerisPropagator::SetRefObject(GmatBase *obj,
      const Gmat::ObjectType type, const std::string &name, const Integer index)
{
   bool retval = false;

   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("Setting object named %s at index %d\n",
            name.c_str(), index);
   #endif

   if (obj->IsOfType(Gmat::SPACEOBJECT))
   {
      if (propObjectNames[index] == name)
      {
         propObjects[index] = obj;
         retval = true;
      }
   }

   return (Propagator::SetRefObject(obj, type, name, index) || retval);
}


//------------------------------------------------------------------------------
// bool UsesODEModel()
//------------------------------------------------------------------------------
/**
 * Used to tell the PropSetup if an ODE model is needed for the propagator
 *
 * @return true if an ODEModel is required, false if not
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::UsesODEModel()
{
   return false;
}


//------------------------------------------------------------------------------
// void SetPropStateManager(PropagationStateManager *sm)
//------------------------------------------------------------------------------
/**
 * Sets the PSM for ephemeris based propagators
 *
 * @param sm The propagation state manager
 */
//------------------------------------------------------------------------------
void EphemerisPropagator::SetPropStateManager(PropagationStateManager *sm)
{
   psm = sm;
}


//------------------------------------------------------------------------------
// bool EphemerisPropagator::Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the EphemeridPropagator for use during a run
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::Initialize()
{
   bool retval = false;

   if (Propagator::Initialize())
   {
      Real oldDim = dimension;
      dimension = 6 * propObjects.size();
      if (dimension > 0)
      {
         if (state != NULL)
            delete [] state;
         state = new Real[dimension];

         if ((j2kState != NULL) && (oldDim != dimension))
         {
            delete [] j2kState;
            j2kState = NULL;
         }

         if (j2kState == NULL)
            j2kState = new Real[dimension];
      }

      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("State epoch %s from spacecraft\n",
               ((startEpochSource == FROM_SPACECRAFT) ? "is" : "is not"));
         MessageInterface::ShowMessage("State epoch %s from ephemeris\n",
               ((startEpochSource == FROM_EPHEM) ? "is" : "is not"));
      #endif
      switch (startEpochSource)
      {
         case FROM_SPACECRAFT:
            if (propObjects.size() > 0)
               initialEpoch = ((SpaceObject*)(propObjects[0]))->GetEpoch();
            break;

         case FROM_EPHEM:
            if (ephemStart > 0)
               initialEpoch = ephemStart;
            break;

         case FROM_SCRIPT:
         default:
            initialEpoch = ConvertToRealEpoch(startEpoch, epochFormat);
            break;


      }

      if (currentEpoch == -987654321.0)
      {
         currentEpoch = initialEpoch;
      }

      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("Initial epoch set to %.12lf\n",
               initialEpoch);
         MessageInterface::ShowMessage("Current epoch set to %.12lf\n",
               currentEpoch);
      #endif

      retval = true;
   }

   return retval;
}


//------------------------------------------------------------------------------
// bool Step(Real dt)
//------------------------------------------------------------------------------
/**
 * Advances the state vector by timestep dt
 *
 * @param dt The time step, in seconds
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::Step(Real dt)
{
   #ifdef DEBUG_EXECUTION
      MessageInterface::ShowMessage("Stepping by %.12lf\n", dt);
   #endif

   bool retval = false;

   if (initialized)
   {
      Real tempStep = ephemStep;
      ephemStep = dt;
      retval = Step();
      ephemStep = tempStep;
   }

   return retval;
}


//------------------------------------------------------------------------------
// Integer GetDimension()
//------------------------------------------------------------------------------
/**
 * Retrieves the size of the state vector that gets propagated
 *
 * @return The vector size
 */
//------------------------------------------------------------------------------
Integer EphemerisPropagator::GetDimension()
{
   if (dimension == 0)
      dimension = 6 * propObjects.size();
   return dimension;
}


//------------------------------------------------------------------------------
// Real* GetState()
//------------------------------------------------------------------------------
/**
 * Retrieves the Real state that gets propagated
 *
 * @return The state
 */
//------------------------------------------------------------------------------
Real* EphemerisPropagator::GetState()
{
   return state;
}


//------------------------------------------------------------------------------
// Real* GetJ2KState()
//------------------------------------------------------------------------------
/**
 * Retrieves the Real state that gets propagated in the J2000 j2kBody reference
 * frame
 *
 * @return The state
 */
//------------------------------------------------------------------------------
Real* EphemerisPropagator::GetJ2KState()
{
   return j2kState;
}


//------------------------------------------------------------------------------
// void UpdateSpaceObject(Real newEpoch)
//------------------------------------------------------------------------------
/**
 * Passes state data from the propagator t the objects that are being propagated
 *
 * @param newEpoch The epoch of the state; use -1.0 to leave the epoch unchanged
 */
//------------------------------------------------------------------------------
void EphemerisPropagator::UpdateSpaceObject(Real newEpoch)
{
   #ifdef DEBUG_EXECUTION
      MessageInterface::ShowMessage(
            "EphemerisPropagator::UpdateSpaceObject(%.12lf) called\n", newEpoch);
   #endif

   if (psm)
   {
      Integer stateSize;
      Integer vectorSize;
      GmatState *newState;
   //   ReturnFromOrigin(newEpoch);

      newState = psm->GetState();
      stateSize = newState->GetSize();
      vectorSize = stateSize * sizeof(Real);

      currentEpoch = initialEpoch + timeFromEpoch / GmatTimeUtil::SECS_PER_DAY;

      // Update the epoch if it was passed in
      if (newEpoch != -1.0)
      {
         currentEpoch = newEpoch;
         timeFromEpoch = (currentEpoch-initialEpoch) * GmatTimeUtil::SECS_PER_DAY;
      }
      UpdateState();

      memcpy(newState->GetState(), j2kState, vectorSize);
      newState->SetEpoch(currentEpoch);
      psm->MapVectorToObjects();

//      // Formation code: Not part of initial release
//      // Update elements for each Formation
//      for (UnsignedInt i = 0; i < stateObjects.size(); ++i)
//         if (stateObjects[i]->IsOfType(Gmat::FORMATION))
//            ((Formation*)stateObjects[i])->UpdateElements();

      #ifdef DEBUG_EXECUTION
         MessageInterface::ShowMessage
               ("EphemerisPropagator::UpdateSpaceObject() on \"%s\" "
                "currentEpoch = %f, passed in epoch = %f\n", GetName().c_str(),
                currentEpoch, newEpoch);
      #endif
   }
}

//------------------------------------------------------------------------------
// void UpdateFromSpaceObject()
//------------------------------------------------------------------------------
/**
 * Fills the state vector with data from the objects that are being propagated
 */
//------------------------------------------------------------------------------
void EphemerisPropagator::UpdateFromSpaceObject()
{
//   // Update elements for each Formation
//   for (UnsignedInt i = 0; i < stateObjects.size(); ++i)
//      if (stateObjects[i]->IsOfType(Gmat::FORMATION))
//         ((Formation*)stateObjects[i])->UpdateState();

   psm->MapObjectsToVector();
   GmatState *newState = psm->GetState();
   memcpy(j2kState, newState->GetState(), newState->GetSize() * sizeof(Real));
/// THIS TOO?
   memcpy(state, newState->GetState(), newState->GetSize() * sizeof(Real));

    // Transform to the force model origin
//    MoveToOrigin();
}


void EphemerisPropagator::RevertSpaceObject()
{
   #ifdef DEBUG_ODEMODEL_EXE
      MessageInterface::ShowMessage
         ("ODEModel::RevertSpacecraft() prevElapsedTime=%f elapsedTime=%f\n",
          prevElapsedTime, elapsedTime);
   #endif

   timeFromEpoch = (previousState.GetEpoch() - initialEpoch) * 86400.0;
   currentEpoch = initialEpoch + timeFromEpoch / 86400.0;
   UpdateState();

//   MoveToOrigin();
}


void EphemerisPropagator::BufferState()
{
   GmatState *stateToBuffer = psm->GetState();
   previousState = (*stateToBuffer);
}


Real EphemerisPropagator::GetTime()
{
   return timeFromEpoch;
}


void EphemerisPropagator::SetTime(Real t)
{
   timeFromEpoch = t;
   currentEpoch = initialEpoch + timeFromEpoch / GmatTimeUtil::SECS_PER_DAY;
   UpdateState();
}


//------------------------------------------------------------------------------
// Protected methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Real ConvertToRealEpoch(const std::string &theEpoch,
//                         const std::string &theFormat)
//------------------------------------------------------------------------------
/**
 * Converts an epoch string is a specified format into
 *
 * @param theEpoch The input epoch
 * @param theFormat The format of the input epoch
 *
 * @return
 */
//------------------------------------------------------------------------------
GmatEpoch EphemerisPropagator::ConvertToRealEpoch(const std::string &theEpoch,
                                   const std::string &theFormat)
{
   Real fromMjd = -999.999;
   Real retval = -999.999;
   std::string outStr;

   TimeConverterUtil::Convert(theFormat, fromMjd, theEpoch, "A1ModJulian",
         retval, outStr);

   if (retval == -999.999)
      throw PropagatorException("Error converting the time string \"" +
            theEpoch + "\"; please check the format for the input string.");
   return retval;
}


void EphemerisPropagator::SetEphemSpan(const GmatEpoch start, const GmatEpoch end)
{
   if (end <= start)
      throw PropagatorException("The ephemeris propagator " + instanceName +
            " was passed an invalid span in the call to "
            "EphemerisPropagator::SetEphemSpan(const GmatEpoch start, "
            "const GmatEpoch end): start >= end");
   ephemStart = start;
   ephemEnd   = end;
}


void EphemerisPropagator::SetEphemSpan(Integer whichOne)
{
   throw PropagatorException("EphemerisPropagator::SetEphemSpan() is not "
         "implemented for the " + instanceName + "ephemeris propagator");
}


bool EphemerisPropagator::IsValidEpoch(GmatEpoch time)
{
   bool retval = false;
   if ((time >= ephemStart) && (time <= ephemEnd))
      retval = true;
   return retval;
}
