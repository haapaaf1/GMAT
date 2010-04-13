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
   psm                  (NULL),
   state                (NULL),
   j2kState             (NULL),
   stepTaken            (0.0)
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
 *
 * @return
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
   psm                  (NULL),
   state                (NULL),
   j2kState             (NULL),
   stepTaken            (0.0)
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
         return true;

      default:
         break;
   }

   return Propagator::SetStringParameter(id, value);
}


std::string EphemerisPropagator::GetStringParameter(const Integer id,
      const Integer index) const
{
   return Propagator::GetStringParameter(id, index);
}


bool EphemerisPropagator::SetStringParameter(const Integer id,
      const std::string &value, const Integer index)
{
   return Propagator::SetStringParameter(id, value, index);
}


std::string EphemerisPropagator::GetStringParameter(
      const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


std::string EphemerisPropagator::GetStringParameter(const std::string &label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


// Reference object code

std::string  EphemerisPropagator::GetRefObjectName(
      const Gmat::ObjectType type) const
{
      return Propagator::GetRefObjectName(type);
}


const StringArray& EphemerisPropagator::GetRefObjectNameArray(
      const Gmat::ObjectType type)
{
   if ((type == Gmat::SPACECRAFT) || (type == Gmat::SPACEOBJECT) ||
       (type == Gmat::FORMATION))
      return propObjectNames;

   return Propagator::GetRefObjectNameArray(type);
}


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

   MessageInterface::ShowMessage("Setting object named %s at index %d\n",
         name.c_str(), index);

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

      initialEpoch = ConvertToRealEpoch(startEpoch, epochFormat);

      if (currentEpoch == -987654321.0)
         currentEpoch = initialEpoch;

      MessageInterface::ShowMessage("Initial epoch set to %.12lf\n",
            initialEpoch);
      MessageInterface::ShowMessage("Current epoch set to %.12lf\n",
            currentEpoch);



      retval = true;
   }

   return retval;
}

Integer EphemerisPropagator::GetDimension()
{
   if (dimension == 0)
      dimension = 6 * propObjects.size();
   return dimension;
}


Real* EphemerisPropagator::GetState()
{
   return state;
}


Real* EphemerisPropagator::GetJ2KState()
{
   return j2kState;
}


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
      GmatState *state;
   //   ReturnFromOrigin(newEpoch);

      state = psm->GetState();
      stateSize = state->GetSize();
      vectorSize = stateSize * sizeof(Real);

      //previousState = (*state);
      memcpy(state->GetState(), j2kState, vectorSize);

      currentEpoch = initialEpoch + timeFromEpoch / GmatTimeUtil::SECS_PER_DAY;

      // Update the epoch if it was passed in
      if (newEpoch != -1.0)
      {
         currentEpoch = newEpoch;
         timeFromEpoch = (currentEpoch-initialEpoch) * GmatTimeUtil::SECS_PER_DAY;
      }

      state->SetEpoch(currentEpoch);
      psm->MapVectorToObjects();

      // Formation code: Not part of initial release
      //// Update elements for each Formation
      //for (UnsignedInt i = 0; i < stateObjects.size(); ++i)
      //   if (stateObjects[i]->IsOfType(Gmat::FORMATION))
      //      ((Formation*)stateObjects[i])->UpdateElements();

      #ifdef DEBUG_EXECUTION
         MessageInterface::ShowMessage
               ("EphemerisPropagator::UpdateSpaceObject() on \"%s\" "
                "currentEpoch = %f, passed in epoch = %f\n", GetName().c_str(),
                currentEpoch, newEpoch);
      #endif
   }
}

void EphemerisPropagator::UpdateFromSpaceObject()
{
   Propagator::UpdateFromSpaceObject();
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
