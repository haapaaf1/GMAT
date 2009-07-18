//$Id$
//------------------------------------------------------------------------------
//                          MeasurementModel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/24
//
/**
 * MeasurementModel implementation used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#include "MeasurementModel.hpp"
#include "MeasurementException.hpp"
#include "MessageInterface.hpp"


//#define TEST_FIRE_MEASUREMENT
//#define TEST_MEASUREMENT_INITIALIZATION


//------------------------------------------------------------------------------
// Static data initialization
//------------------------------------------------------------------------------

const std::string MeasurementModel::PARAMETER_TEXT[] =
{
   "ObservationData",
   "Type",
   "Participants",
   "Bias",
   "NoiseSigma",
   "TimeConstant",
};


const Gmat::ParameterType MeasurementModel::PARAMETER_TYPE[] =
{
   Gmat::OBJECTARRAY_TYPE,
   Gmat::OBJECT_TYPE,
   Gmat::OBJECTARRAY_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
};

//------------------------------------------------------------------------------
// Public methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Object construction, destruction, and replication
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// MeasurementModel(const std::string &nomme)
//------------------------------------------------------------------------------
/**
 * Standard GMAT constructor
 *
 * @param nomme The new object's name
 */
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const std::string &nomme) :
   GmatBase             (Gmat::MEASUREMENT_MODEL, "MeasurementModel", nomme),
   measurementType      ("NoTypeSet"),
   measurement          (NULL),
   theData              (NULL),
   theDataDerivatives   (NULL),
   measurementBias      (0.0),
   noiseSigma           (1.0e-5),
   timeConstant         (6000.0),
   modelID              (-1)
{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");

   parameterCount = MeasurementModelParamCount;
}

//------------------------------------------------------------------------------
// ~MeasurementModel()
//------------------------------------------------------------------------------
/**
 * Measurement Model destructor
 */
//------------------------------------------------------------------------------
MeasurementModel::~MeasurementModel()
{
   // TODO Auto-generated destructor stub
}

//------------------------------------------------------------------------------
// MeasurementModel(const MeasurementModel &mm) :
//------------------------------------------------------------------------------
/**
 * Copy constructor
 */
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase             (mm),
   participantNames     (mm.participantNames),
   measurementType      (mm.measurementType),
   theData              (NULL),
   theDataDerivatives   (NULL),
   measurementBias      (mm.measurementBias),
   noiseSigma           (mm.noiseSigma),
   timeConstant         (mm.timeConstant)
{
   if (mm.measurement != NULL)
      measurement = (CoreMeasurement*)mm.measurement->Clone();
}

//------------------------------------------------------------------------------
// MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
//------------------------------------------------------------------------------
/**
 * Measurement model assignment operator
 *
 * @param mm The model that is copied here
 *
 * @return this measurement model, set to look like mm
 */
//------------------------------------------------------------------------------
MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
{
   if (&mm != this)
   {
      GmatBase::operator=(mm);
      participantNames     = mm.participantNames;
      measurementType      = mm.measurementType;
      theData              = NULL;
      theDataDerivatives   = NULL;
      measurementBias      = mm.measurementBias;
      noiseSigma           = mm.noiseSigma;
      timeConstant         = mm.timeConstant;

      if (mm.measurement != NULL)
         measurement = (CoreMeasurement*)mm.measurement->Clone();
   }

   return *this;
}

//------------------------------------------------------------------------------
// GmatBase *MeasurementModel::Clone() const
//------------------------------------------------------------------------------
/**
 * This method calls the copy constructor to make a duplicate of this object
 *
 * @return The duplicate
 */
//------------------------------------------------------------------------------
GmatBase *MeasurementModel::Clone() const
{
   return new MeasurementModel(*this);
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model and its owned objects
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementModel::Initialize()
{
   bool retval = false;

   if (measurement != NULL)
   {
      // Validate CoreMeasurement member
      if (measurement->Initialize())
      {
         // Set calculated data pointers
         theData              = measurement->GetMeasurementDataPointer();
         theDataDerivatives   = measurement->GetDerivativePointer();
         retval = true;

         #ifdef TEST_MEASUREMENT_INITIALIZATION
            MessageInterface::ShowMessage(
                  "Initialization complete for measurement model %s\n",
                  instanceName.c_str());
         #endif

         #ifdef TEST_FIRE_MEASUREMENT
            MessageInterface::ShowMessage("Test firing measurement model %s\n",
                  instanceName.c_str());

            CalculateMeasurement();

            MessageInterface::ShowMessage("   Calculated %s at epoch %.12lf\n",
                  measurement->GetTypeName().c_str(), theData->epoch);
            MessageInterface::ShowMessage("   FeasibilityValue = %lf\n",
                  theData->feasibilityValue);
            MessageInterface::ShowMessage("   Feasibility:  %s\n",
                  (theData->isFeasible ? "true" : "false"));
            MessageInterface::ShowMessage("   Measurement = [");
            for (RealArray::iterator i = theData->value.begin();
                  i != theData->value.end(); ++i)
               MessageInterface::ShowMessage(" %.12lf ", (*i));
            MessageInterface::ShowMessage("]\n");
         #endif

      }
   }

   return retval;
}


//------------------------------------------------------------------------------
// Parameter handling code
//------------------------------------------------------------------------------


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
Integer MeasurementModel::GetParameterID(const std::string & str) const
{
   for (Integer i = GmatBaseParamCount; i < MeasurementModelParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

//------------------------------------------------------------------------------
// public methods inherited from GmatBase
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < MeasurementModelParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

//---------------------------------------------------------------------------
//  std::string GetParameterUnit(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the unit for the parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return unit for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetParameterUnit(const Integer id) const
{
   return GmatBase::GetParameterUnit(id);
}

//---------------------------------------------------------------------------
//  Gmat::ParameterType GetParameterType(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the enumerated type of the object.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The enumeration for the type of the parameter, or
 *         UNKNOWN_PARAMETER_TYPE.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType MeasurementModel::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < MeasurementModelParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool MeasurementModel::IsParameterReadOnly(const Integer id) const
{
   return GmatBase::IsParameterReadOnly(id);
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool MeasurementModel::IsParameterReadOnly(const std::string & label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves the parameters used in the noise modeling for the Measurement
 *
 * @param id The ID for the parameter that is retrieved
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const Integer id) const
{
   if (id == Bias)
      return measurementBias;

   if (id == NoiseSigma)
      return noiseSigma;

   if (id == TimeConstant)
      return timeConstant;

   return GmatBase::GetRealParameter(id);
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the parameters used in the noise modeling for the Measurement
 *
 * @param id The ID for the parameter that is to be set
 * @param value The new value for the parameter
 *
 * @return The parameter value.  The return value is the new value if it was
 *         changed, or the value prior to the call if the new value wwas not
 *         accepted.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const Integer id, const Real value)
{
   if (id == Bias)
   {
      if (value >= 0)
         measurementBias = value;
      return measurementBias;
   }
   if (id == NoiseSigma)
   {
      if (value >= 0)
         noiseSigma = value;
      return noiseSigma;
   }
   if (id == TimeConstant)
   {
      if (value >= 0)
         timeConstant = value;
      return timeConstant;
   }

   return GmatBase::SetRealParameter(id, value);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string & label) const
//------------------------------------------------------------------------------
/**
 * Retrieves the parameters used in the noise modeling for the Measurement
 *
 * @param label The text label for the parameter that is retrieved
 *
 * @return The parameter value
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const std::string & label) const
{
   return GetRealParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string & label, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the parameters used in the noise modeling for the Measurement
 *
 * @param label The text label for the parameter that is to be set
 * @param value The new value for the parameter
 *
 * @return The parameter value.  The return value is the new value if it was
 *         changed, or the value prior to the call if the new value wwas not
 *         accepted.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const std::string & label,
      const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
// Real MeasurementModel::GetRealParameter(const std::string & label,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const std::string & label,
      const Integer index) const
{
   return GetRealParameter(GetParameterID(label), index);
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string & label, const Real value,
//       const Integer index)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const std::string & label,
      const Real value, const Integer index)
{
   return SetRealParameter(GetParameterID(label), value, index);
}

//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string & label,
//       const Integer row, const Integer col) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const std::string & label,
      const Integer row, const Integer col) const
{
   return GetRealParameter(GetParameterID(label), row, col);
}

//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id, const Integer row,
//       const Integer col) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const Integer id, const Integer row,
      const Integer col) const
{
   return GmatBase::GetRealParameter(id, row, col);
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value,
//       const Integer index)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const Integer id, const Real value,
      const Integer index)
{
   return GmatBase::SetRealParameter(id, value, index);
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value,
//       const Integer row, const Integer col)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const Integer id, const Real value,
      const Integer row, const Integer col)
{
   return GmatBase::SetRealParameter(id, value, row, col);
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string & label,
//       const Real value, const Integer row, const Integer col)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::SetRealParameter(const std::string & label,
      const Real value, const Integer row, const Integer col)
{
   return SetRealParameter(GetParameterID(label), value, row, col);
}

//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
Real MeasurementModel::GetRealParameter(const Integer id, const Integer index) const
{
   return GmatBase::GetRealParameter(id, index);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method retrieves a string parameter
 *
 * @param id The ID for the parameter
 *
 * @return The string parameter
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const Integer id) const
{
   if (id == MeasurementType)
      return measurementType;

   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string & value)
//------------------------------------------------------------------------------
/**
 * This method sets a string parameter
 *
 * @param id The ID for the parameter
 * @param value The new string value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const Integer id,
      const std::string & value)
{
   if (id == ObservationData)
   {
      // Only add the obs data if it is not in the list already
      if (find(observationStreamName.begin(), observationStreamName.end(),
            value) == observationStreamName.end())
      {
         observationStreamName.push_back(value);
         return true;
      }
   }
   if (id == MeasurementType)
   {
      measurementType = value;
      return true;
   }

   if (id == Participants)
   {
      // Only add the participant if it is not in the list already
      if (find(participantNames.begin(), participantNames.end(), value) ==
            participantNames.end())
      {
         participantNames.push_back(value);
      }
      return true;
   }

   return GmatBase::SetStringParameter(id, value);
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string & label) const
//------------------------------------------------------------------------------
/**
 * This method retrieves a string parameter
 *
 * @param label The string label for the parameter
 *
 * @return The parameter
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const std::string & label) const
{
   return GetStringParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string & label, const std::string & value)
//------------------------------------------------------------------------------
/**
 * This method sets a string parameter
 *
 * @param label The string label for the parameter
 * @param value The new string value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const std::string & label,
      const std::string & value)
{
   return SetStringParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method retrieves a string parameter from a StringArray
 *
 * @param id The ID of the parameter
 *
 * @return The parameter
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const Integer id,
      const Integer index) const
{
   if (id == Participants)
      return participantNames[index];
   if (id == ObservationData)
      return observationStreamName[index];

   return GmatBase::GetStringParameter(id, index);
}

//------------------------------------------------------------------------------
// std::string MeasurementModel::GetStringParameter(const std::string & label,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method retrieves a string parameter from a StringArray
 *
 * @param label The string label for the parameter
 *
 * @return The parameter
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const std::string & label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string & label,
//       const std::string & value, const Integer index)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const std::string & label,
      const std::string & value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id,
//       const std::string & value, const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets a specific string in a StringArray
 *
 * This method changes a specific string in a StringArray if a string has been
 * set at the location selected by the index value.  If the index exceeds the
 * size of the name array, the participant name is added to the end of the list.
 *
 * @param id The ID for the StringArray parameter that is being changed
 * @param value The string that needs to be placed in the StringArray
 * @param index The location for the string in the list.  If index exceeds the
 *              size of the StringArray, the string is added to the end of the
 *              array
 *
 * @return true If the string was processed
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const Integer id,
      const std::string & value, const Integer index)
{
   if (index < 0)
   {
      MeasurementException ex;
      ex.SetDetails("The index %d is out-of-range for field \"%s\"", index,
                    GetParameterText(id).c_str());
      throw ex;
   }

   switch (id)
   {
   case Participants:
      {
         if (index < (Integer)participantNames.size())
            participantNames[index] = value;
         else
            if (find(participantNames.begin(), participantNames.end(), value) ==
                  participantNames.end())
               participantNames.push_back(value);

         return true;
      }
   case ObservationData:
      {
         if (index < (Integer)observationStreamName.size())
            observationStreamName[index] = value;
         else
            if (find(observationStreamName.begin(), observationStreamName.end(),
                  value) == observationStreamName.end())
               observationStreamName.push_back(value);

         return true;
      }
   default:
      return GmatBase::SetStringParameter(id, value, index);
   }
}

//------------------------------------------------------------------------------
//  const StringArray& GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param id The ID for the parameter.
 *
 * @return The requested StringArray; throws if the parameter is not a
 *         StringArray.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(
      const Integer id) const
{
   if (id == Participants)
      return participantNames;
   if (id == ObservationData)
      return observationStreamName;

   return GmatBase::GetStringArrayParameter(id);
}

//------------------------------------------------------------------------------
//  const StringArray& GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param label The (string) label for the parameter.
 *
 * @return The requested StringArray; throws if the parameter is not a
 *         StringArray.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(
      const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id,
//       const Integer index) const;
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(const Integer id,
      const Integer index) const
{
   return GmatBase::GetStringArrayParameter(id, index);
}

//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const std::string &label,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(
      const std::string &label, const Integer index) const
{
   return GmatBase::GetStringArrayParameter(label, index);
}



//------------------------------------------------------------------------------
// bool RenameRefObject(const Gmat::ObjectType type,
//------------------------------------------------------------------------------
/**
 * Renames references objects
 *
 * @param type The type of object that is renamed
 * @param oldName The name of the object that is changing
 * @param newName the new object name
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementModel::RenameRefObject(const Gmat::ObjectType type,
      const std::string & oldName, const std::string & newName)
{
   /// @todo MeasurementModel rename code needs to be implemented
   return GmatBase::RenameRefObject(type, oldName, newName);
}

bool MeasurementModel::SetRefObjectName(const Gmat::ObjectType type, const std::string & name)
{
   return GmatBase::SetRefObjectName(type, name);
}

const ObjectTypeArray & MeasurementModel::GetRefObjectTypeArray()
{
   return GmatBase::GetRefObjectTypeArray();
}

//------------------------------------------------------------------------------
// const StringArray& MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Initialization method that identifies the reference objects needed
 *
 * @param type The ObjectType for the references; UNKNOWN_OBJECT retrieves all
 *
 * @return A StringArray with all of the object names.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   #ifdef TEST_MEASUREMENT_INITIALIZATION
      MessageInterface::ShowMessage(
            "MeasurementModel::GetRefObjectNameArray(%d) entered\n", type);
   #endif

   refObjectList.clear();

   if ((type == Gmat::UNKNOWN_OBJECT) || (type == Gmat::SPACE_POINT))
   {
      // Add the participants this model needs
      for (StringArray::iterator i = participantNames.begin();
            i != participantNames.end(); ++i)
      {
         #ifdef TEST_MEASUREMENT_INITIALIZATION
            MessageInterface::ShowMessage(
                  "   Adding: %s\n", i->c_str());
         #endif
         if (find(refObjectList.begin(), refObjectList.end(), *i) ==
               refObjectList.end())
            refObjectList.push_back(*i);
      }
   }
   else
      refObjectList = GmatBase::GetRefObjectNameArray(type);

   return refObjectList;
}

std::string MeasurementModel::GetRefObjectName(const Gmat::ObjectType type) const
{
   return GmatBase::GetRefObjectName(type);
}

GmatBase* MeasurementModel::GetRefObject(const Gmat::ObjectType type, const std::string & name)
{
   return GmatBase::GetRefObject(type, name);
}

GmatBase* MeasurementModel::GetRefObject(const Gmat::ObjectType type, const std::string & name, const Integer index)
{
   return GmatBase::GetRefObject(type, name, index);
}

GmatBase* MeasurementModel::GetOwnedObject(Integer whichOne)
{
   return GmatBase::GetOwnedObject(whichOne);
}

bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, const std::string & name)
{
   #ifdef TEST_MEASUREMENT_INITIALIZATION
      MessageInterface::ShowMessage("Setting ref object %s with type %s\n",
            name.c_str(), obj->GetTypeName().c_str());
   #endif

   if (find(participantNames.begin(), participantNames.end(), name) !=
         participantNames.end())
   {
      if (find(participants.begin(), participants.end(), obj) ==
            participants.end())
      {
         participants.push_back(obj);
         if (measurement != NULL)
            measurement->SetRefObject(obj, type, name);
      }
   }

   return GmatBase::SetRefObject(obj, type, name);
}

// Not needed?
Integer MeasurementModel::GetOwnedObjectCount()
{
   ownedObjectCount = 0;

   return GmatBase::GetOwnedObjectCount();
}

ObjectArray& MeasurementModel::GetRefObjectArray(const std::string & typeString)
{
   return GmatBase::GetRefObjectArray(typeString);
}

bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
      const std::string & name, const Integer index)
{
   #ifdef TEST_MEASUREMENT_INITIALIZATION
      MessageInterface::ShowMessage(""
            "Setting indexed ref object %s with type %s\n", name.c_str(),
            obj->GetTypeName().c_str());
   #endif

   return GmatBase::SetRefObject(obj, type, name, index);
}

ObjectArray & MeasurementModel::GetRefObjectArray(const Gmat::ObjectType type)
{
   return GmatBase::GetRefObjectArray(type);
}


//------------------------------------------------------------------------------
// bool SetMeasurement(CoreMeasurement *meas)
//------------------------------------------------------------------------------
/**
 * Sets the core measurement for the measurement model
 *
 * The core measurement is identified by the "Type" parameter on the measurement
 * model.  The interpreter uses this identifier to pass a CoreMeasurement
 * instance that the model uses when computing the (expected) measurement value.
 *
 * @param meas The CoreMeasurement
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetMeasurement(CoreMeasurement *meas)
{
   bool retval = false;

   if (meas->IsOfType(Gmat::CORE_MEASUREMENT))
   {
      measurement = meas;
      measurementType = measurement->GetTypeName();
      retval = true;

      theData = measurement->GetMeasurementDataPointer();
      theDataDerivatives = measurement->GetDerivativePointer();
   }

   return retval;
}


bool MeasurementModel::IsOwnedObject(Integer id) const
{
   return GmatBase::IsOwnedObject(id);
}


Integer MeasurementModel::GetModelID()
{
   return modelID;
}


void MeasurementModel::SetModelID(Integer newID)
{
   modelID = newID;
}

//------------------------------------------------------------------------------
// const MeasurementData & CalculateMeasurement()
//------------------------------------------------------------------------------
/**
 * Calculates the measurement
 *
 * Calls the core measurement and retrieves the measurement data for the current
 * state of the participants.  If the measurement is not possible given that
 * state, the MeasurementData container is cleared and its isFeasible flag is
 * set to false.
 *
 * @return A reference to the calculated MeasurementData
 */
//------------------------------------------------------------------------------
const MeasurementData & MeasurementModel::CalculateMeasurement()
{
   measurement->CalculateMeasurement(false);
   return *theData;
}

//------------------------------------------------------------------------------
// const MeasurementData & MeasurementModel::GetMeasurement()
//------------------------------------------------------------------------------
/**
 * Retrieves the last calculated measurement
 *
 * @return A reference to the calculated MeasurementData
 */
//------------------------------------------------------------------------------
const MeasurementData & MeasurementModel::GetMeasurement()
{
   return *theData;
}

//------------------------------------------------------------------------------
// const Rmatrix & MeasurementModel::CalculateMeasurementDerivatives()
//------------------------------------------------------------------------------
/**
 * Calculates the measurement and derivatives
 *
 * Calls the core measurement and retrieves the measurement data and derivatives
 * for the current state of the participants.  If the measurement is not
 * possible given that state, the MeasurementData container, theData, is cleared
 * and its isFeasible flag is set to false, and the return pointer is set to
 * NULL.
 *
 * Note that while the measurement is calculated, it is not returned.  Users of
 * this class can retrieve the measurement data using the GetMeassurement()
 * method (to get the last measurement calculated) or the CalculateMeasurement
 * method (to recalculate the measurement).
 *
 * @return A pointer to the calculated derivatives.
 */
//------------------------------------------------------------------------------
const Rmatrix & MeasurementModel::CalculateMeasurementDerivatives()
{
   measurement->CalculateMeasurement(true);
   return *theDataDerivatives;
}


// Currently a place holder for data file writes
bool MeasurementModel::WriteMeasurements()
{
   return false;
}

// Currently a place holder for data file writes
bool MeasurementModel::WriteMeasurement(Integer id)
{
   return false;
}
