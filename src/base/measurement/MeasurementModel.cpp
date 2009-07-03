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

//------------------------------------------------------------------------------
// Static data initialization
//------------------------------------------------------------------------------

const std::string MeasurementModel::PARAMETER_TEXT[] =
{
   "Type",
   "Participants",
   "Bias",
   "NoiseSigma",
   "TimeConstant"
};


const Gmat::ParameterType MeasurementModel::PARAMETER_TYPE[] =
{
   Gmat::STRING_TYPE,
   Gmat::OBJECTARRAY_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE
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
   GmatBase       (Gmat::MEASUREMENT_MODEL, "MeasurementModel", nomme)
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
   GmatBase       (mm)
{

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
 * This method retrieves a string parameter
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
// std::string MeasurementModel::GetStringParameter(const std::string & label,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
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
// std::string GetStringParameter(const Integer id,
//       const Integer index) const
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const Integer id,
      const Integer index) const
{
   return GmatBase::GetStringParameter(id, index);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id,
//       const std::string & value, const Integer index)
//------------------------------------------------------------------------------
/**
 * This method calls the base class method.  It is provided for overload
 * compatibility.  See the base class description for a full description.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const Integer id,
      const std::string & value, const Integer index)
{
   return GmatBase::SetStringParameter(id, value, index);
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



bool MeasurementModel::RenameRefObject(const Gmat::ObjectType type,
      const std::string & oldName, const std::string & newName)
{
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

const StringArray & MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   return GmatBase::GetRefObjectNameArray(type);
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
   return GmatBase::SetRefObject(obj, type, name);
}

// Not needed?
Integer MeasurementModel::GetOwnedObjectCount()
{
   ownedObjectCount = 0;

   return GmatBase::GetOwnedObjectCount();
}

ObjectArray & MeasurementModel::GetRefObjectArray(const std::string & typeString)
{
   return GmatBase::GetRefObjectArray(typeString);
}

bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, const std::string & name, const Integer index)
{
   return GmatBase::SetRefObject(obj, type, name, index);
}

ObjectArray & MeasurementModel::GetRefObjectArray(const Gmat::ObjectType type)
{
   return GmatBase::GetRefObjectArray(type);
}

bool MeasurementModel::IsOwnedObject(Integer id) const
{
   return GmatBase::IsOwnedObject(id);
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
   // todo:  Put in the call to calculate the measurement

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
   // todo:  Put in the call to calculate the measurement and derivatives

   return *theDataDerivatives;
}
