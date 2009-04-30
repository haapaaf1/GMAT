//$Header$
//------------------------------------------------------------------------------
//                         MeasurementModel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/23
//
/**
 * Defines the measurement models used for observer objects.
 */
//------------------------------------------------------------------------------

#include "MeasurementModel.hpp"
#include "lapackpp.h"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "TimeSystemConverter.hpp"
#include "StateConverter.hpp"
#include "Moderator.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string MeasurementModel::MODEL_DESCRIPTIONS[EndModelReps] =
{
    "NotDefined",
    "Range",
    "RangeRate",
    "VariableTransmitterRange",
    "AntennaTracking",
    "SunSensor",
    "StarSensor",
    "GyroPackage",
    "HorizonSensor",
    "Videometers",
    "CoherentDoppler",
    "NonCoherentDoppler",
    "VariableTransmitterDoppler",
    "IntegratedDopplerCount",
    "IMU",
    "Magnetometer",
    "AO_AzEl",
    "RangeAzEl",
    "AO_RaDec",
    "RangeRaDec"
};

const std::string
MeasurementModel::PARAMETER_TEXT[MeasurementModelParamCount - GmatBaseParamCount] =
{
   "DataSource",
   "DataType",
   "DataFileNameß",
   "NumLines",
   "MeasurementTypes",
   "LightTimeCorrection",
   "IonosphericCorrection",
   "TroposphericCorrection",
   "LightTimeModel",
   "IonosphericModel",
   "TroposphericModel"
};


const Gmat::ParameterType
MeasurementModel::PARAMETER_TYPE[MeasurementModelParamCount - GmatBaseParamCount] =
{
   Gmat::OBJECTARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE   
};

//---------------------------------
// methods overridden from GMAT base
//---------------------------------

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the MeasurementModel.
 *
 * @return clone of the MeasurementModel.
 */
//------------------------------------------------------------------------------
GmatBase* MeasurementModel::Clone() const
{
   GmatBase *clone = new MeasurementModel(*this);
   return (clone);
}

//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void MeasurementModel::Copy(const GmatBase* orig)
{
   operator=(*((MeasurementModel *)(orig)));
}

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
   if ((id >= GmatBaseParamCount) && (id < MeasurementModelParamCount))
   {
      //MessageInterface::ShowMessage("'%s':\n",
      //   PARAMETER_TEXT[id - GmatBaseParamCount].c_str());
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   }
   return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < MeasurementModelParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Integer  GetDependentParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the dependent parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested dependent parameter.
 *
 * @return ID for the requested dependent parameter or -1 for no associated ID.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetDependentParameterID(const std::string &str) const
{
   return -1;
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType MeasurementModel::GetParameterType(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < MeasurementModelParamCount))
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the value for a std::string parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetStringParameter(const Integer id) const
{
   if (id == LIGHTTIME_ID)
      return lightTimeModelName;

   if (id == IONOSPHERE_ID)
      return ionoModelName;

   if (id == TROPOSPHERE_ID)
      return tropoModelName;
          
   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == LIGHTTIME_ID)
   {
      lightTimeModelName = value;
      return true;
   }

   if (id == IONOSPHERE_ID)
   {
      ionoModelName = value;
      return true;
   }

   if (id == TROPOSPHERE_ID) 
   {
	tropoModelName = value;
	return true;
   }
 
   return GmatBase::SetStringParameter(id, value);
   
}

//------------------------------------------------------------------------------
//  bool GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the value for a boolean parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::GetBooleanParameter(const Integer id) const
{
   if (id == LIGHTTIMEFLAG_ID)
      return isLightTimeON;

   if (id == IONOSPHEREFLAG_ID)
      return isIonoON;

   if (id == TROPOSPHEREFLAG_ID)
      return isTropoON;
          
   return GmatBase::GetBooleanParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetBooleanParameter(const Integer id, const bool value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::SetBooleanParameter(const Integer id, const bool &value)
{
   if (id == LIGHTTIMEFLAG_ID)
   {
      isLightTimeON = value;
      return true;
   }

   if (id == IONOSPHEREFLAG_ID)
   {
      isIonoON = value;
      return true;
   }

   if (id == TROPOSPHEREFLAG_ID) 
   {
	isTropoON = value;
	return true;
   }
 
   return GmatBase::SetBooleanParameter(id, value);
   
}

//------------------------------------------------------------------------------
//  std::string  GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementModel::GetStringArrayParameter(const Integer id) const
{

   if (id == FILEFORMAT_ID)
      return myDataFileFormats;

   if (id == FILENAME_ID)
      return myDataFileNames;

   if (id == MEASUREMENTTYPES_ID)
      return measurementTypesAllowed;
  
   return GmatBase::GetStringArrayParameter(id);
}
/*
//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const Integer id) const
{
    if (id == NUMLINES_ID)
    {
       //return numLines.front();
        return 0;
    }

    return GmatBase::GetIntegerParameter(id);
}

//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const Integer id,
                                              const Integer index) const
{
    if (id == NUMLINES_ID)
      return numLines[index];

    return GmatBase::GetIntegerParameter(id);
}

//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer MeasurementModel::GetIntegerParameter(const std::string &label,
                                              const Integer index) const
{
   return GetIntegerParameter(GetParameterID(label), index);
}
*/

//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a integer parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter
 *
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const Integer id,
                                              const Integer value)
{

   if (id == NUMLINES_ID)
   {
         numLines.push_back(value);
         return value;
   }
   
   return GmatBase::SetIntegerParameter(id, value);

}

//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const Integer id, const Integer value,
//                             const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets the value for a integer parameter.
 *
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * @param <index> Integer index of the parameter for arrays.
 *
 * @return The value of the parameter
 *
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const Integer id,
                                              const Integer value,
                                              const Integer index)
{

   if (id == NUMLINES_ID)
   {
         numLines[index] = value;
         return value;
   }

   return GmatBase::SetIntegerParameter(id, value, index);

}

//------------------------------------------------------------------------------
// Integer SetIntegerParameter(std::string &label, const Integer& value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a integer parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const std::string &label,
                                              const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
// Integer SetIntegerParameter(std::string &label, const Integer& value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a integer parameter.
 *
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * @param <index> Integer index of the parameter for arrays.
 *
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::SetIntegerParameter(const std::string &label,
                                              const Integer value,
                                              const Integer index)
{
   return SetIntegerParameter(GetParameterID(label), value, index);
}

//------------------------------------------------------------------------------
//  const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * This method returns an array with the names of the referenced objects.
 *
 * @return a vector with the names of objects of the requested type.
 */
//------------------------------------------------------------------------------
const StringArray& 
      MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
{
//   MessageInterface::ShowMessage("Getting the list of data file objects\n");

//   // If we have more than one type, we'll want to do something like this:
//   if (type == Gmat::UNKNOWN_OBJECT)
//      Fill them all in here; otherwise fill in by type
      
   // I'm using a temporary StringArray in case we need additional ref objects
   tempNameArray = myDataFileNames;

   return tempNameArray;
}


//------------------------------------------------------------------------------
// GmatBase* GetRefObject(const Gmat::ObjectType type, const std::string &name)                             
//------------------------------------------------------------------------------
/**
 * This method returns a GmatBase pointer to the desired object.
 *
 * @return a GmatBase pointer.
 */
//------------------------------------------------------------------------------
GmatBase* MeasurementModel::GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name)
{
   GmatBase* retval = NULL;

   if (type == Gmat::DATA_FILE)
   {
      for (ObjectArray::iterator i = myDataSources.begin();
           i != myDataSources.end(); ++i)
      {
         if ((*i)->GetName() == name)
         {
	    retval = *i;
            break;
         }
      }
   }

   if (retval != NULL)
      return retval;
   
   return GmatBase::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
//                   const std::string &name)
//------------------------------------------------------------------------------
bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
                              const std::string &name)
{
   bool retval = false;

   if (obj->IsOfType(Gmat::DATA_FILE))
   {
      if (find(myDataSources.begin(), myDataSources.end(), obj) == myDataSources.end())
      {
         myDataSources.push_back(obj);
         retval = true;
      }
   }
          
    return retval;

}

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
//  MeasurementModel::MeasurementModel()
//------------------------------------------------------------------------------
/**
 * Constructs base MeasurementModel structures
 */
MeasurementModel::MeasurementModel(const std::string typeName,
      const std::string name) :
   GmatBase    (Gmat::MEASUREMENT_MODEL, typeName, name),
   modelID     (0),
   ionoModelName (""),
   tropoModelName (""),
   lightTimeModelName (""),
   isIonoON (false),
   isTropoON (false),
   isLightTimeON (false),
   theStation  (NULL)

{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
   measurementTypesAllowed.push_back("");
   tempNameArray.push_back("");
   //numLines.push_back("");
   myDataFileFormats.push_back("");
   myDataFileNames.push_back("");
   myDataSources.push_back(NULL);

   theModerator  = Moderator::Instance();
}

//------------------------------------------------------------------------------
//   MeasurementModel::MeasurementModel(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase    (mm),
   theModerator (mm.theModerator),
   modelID  (mm.modelID),
   ionoModelName (mm.ionoModelName),
   tropoModelName (mm.tropoModelName),
   lightTimeModelName (mm.lightTimeModelName),
   isIonoON (mm.isIonoON),
   isTropoON (mm.isTropoON),
   isLightTimeON (mm.isLightTimeON),
   numMeasurements (mm.numMeasurements),
   measurementNames (mm.measurementNames),
   measurementUnits (mm.measurementUnits),
   measurementTypesAllowed (mm.measurementTypesAllowed),
   tempNameArray (mm.tempNameArray),
   ccvtr (mm.ccvtr),
   myDataFileFormats (mm.myDataFileFormats),
   myDataFileNames (mm.myDataFileNames),
   myDataSources (mm.myDataSources),
   theStation		   (mm.theStation)

{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
}

//------------------------------------------------------------------------------
//  MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
{
   if (this != &mm)
   {
      SetModelID( mm.GetModelID() );
   }
   return *this;
}

//------------------------------------------------------------------------------
//  MeasurementModel::~MeasurementModel()
//------------------------------------------------------------------------------
MeasurementModel::~MeasurementModel()
{
}

//------------------------------------------------------------------------------
//  std::ostream& operator<<(std::ostream& output, MeasurementModel &mm)
//------------------------------------------------------------------------------
std::ostream& operator<<(std::ostream& output, MeasurementModel &mm)
{

    output << mm.modelID << std::endl;

    return output;
}

//------------------------------------------------------------------------------
//  <friend>
//  std::istream& operator>>(std::istream& input, MeasurementModel &mm)
//------------------------------------------------------------------------------
std::istream& operator>>(std::istream& input, MeasurementModel &mm)
{
    input >> mm.modelID;

    return input;
}

//------------------------------------------------------------------------------
// void SetGroundStation(GroundStation* gs)
//------------------------------------------------------------------------------
/**
 * Set the ground station for this instance of the measurement model.
 *
 * @param mm The ground station that is assigned.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetGroundStation(GroundStation* gs)
{
    theStation = gs;
}

//------------------------------------------------------------------------------
// GroundStation* GetGroundStation()
//------------------------------------------------------------------------------
/**
 * Return the ground station for this instance of the measurement model.
 *
 * @return A pointer to the ground station.
 */
//------------------------------------------------------------------------------
GroundStation* MeasurementModel::GetGroundStation()
{
    return theStation;
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::Initialize() const
{
    // Initialize the coordinate converter
    CoordinateConverter ccvtr;

    // Check to see if a DataSource has already been created elsewhere
    // If not, then create the appropriate kind.
    if (myDataSources.size() == 0 && myDataFileFormats.size() > 0)
    {

        for (unsigned int i = 0; i != myDataFileFormats.size(); i++)
        {

            DataFile *obj = NULL;

            char str[2];
            sprintf(str,"%d",i);
            std::string name = myDataFileFormats[i]+str;
            obj = theModerator->CreateDataFile(myDataFileFormats[i],name);
            if (obj == NULL)
            {
                MessageInterface::ShowMessage("MeasurementModel could not create datafile of type "+myDataFileFormats[i]+"\n");
                throw MeasurementModelException("MeasurementModel could not create datafile of type "+myDataFileFormats[i]+"\n");

            }
            else
            {

                if (numLines[i] > 0)
                {
                    Integer requiredNumLines = obj->GetNumLines();
                    if(requiredNumLines > 0 && numLines[i] != requiredNumLines )
                    {
                        MessageInterface::ShowMessage("WARNING: The "+myDataFileFormats[i]+" format processes a fixed number of lines at a time that can not be overriden.\n");
                    }
                    else
                    {
                        obj->SetNumLines(numLines[i]);
                    }
                }
                obj->SetFileName(myDataFileNames[i].c_str());
                obj->Initialize();
                // TODO: Why can't I push this newly created data file object
                // onto the myDataFileSources stack?!?!?!?!?!
                //SetRefObject(obj,Gmat::DATA_FILE,"");
            }
        }
                
    }

    //for (ObjectArray::iterator i = myDataSources.begin(); i != myDataSources.end(); ++i)
    //{
    //    std::string str = "test";
        //(*i)->CheckDataAvailability(str);
    //}

    return true;

}

// Accessors

//------------------------------------------------------------------------------
// Integer SetModelID() const
//------------------------------------------------------------------------------
/**
 * Sets the name of the measurement model.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetModelID(Integer mId)
{
   modelID = mId;
}

//------------------------------------------------------------------------------
// Integer GetModelID() const
//------------------------------------------------------------------------------
/**
 * Finds the model ID# of the measurement model.
 *
 * @return The model ID#.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetModelID() const
{
   return modelID;
}

//------------------------------------------------------------------------------
// Integer GetNumMeasurments() const
//------------------------------------------------------------------------------
/**
 * Finds the number of measurements returned by the model.
 *
 * @return The numMeasurements.
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetNumMeasurements() const
{
   return numMeasurements;
}

//------------------------------------------------------------------------------
// std::string GetMeasurementNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Finds the name of thee measurements returned by the model.
 *
 * @return The measurementNames.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetMeasurementNameText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementNames[id];

    return "Unknown Measurement ID";
}

//------------------------------------------------------------------------------
// std::string MeasurementModel::GetMeasurementUnitText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Finds the units of the measurements returned by the model.
 *
 * @return The measurementUnits.
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetMeasurementUnitText(const Integer id) const
{
    if (id >= 0 && id < numMeasurements)
      return measurementUnits[id];

    return "Unknown Measurement Unit ID";
}

//------------------------------------------------------------------------------
// Integer MeasurementModel::GetModelID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model ID
 */
//------------------------------------------------------------------------------
Integer MeasurementModel::GetModelID(const std::string &label)
{

   for (Integer i = 0; i < EndModelReps; i++)
   {
      if (label == MODEL_DESCRIPTIONS[i])
         return i;
   }

   return DEFAULT_ID;


}

//------------------------------------------------------------------------------
// const std::string* MeasurementModel::GetModelDescriptions() const
//------------------------------------------------------------------------------
const std::string* MeasurementModel::GetModelDescriptions() const
{
   return MODEL_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string MeasurementModel::GetModelNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model name text corresponding to a model ID
 */
//------------------------------------------------------------------------------
std::string MeasurementModel::GetModelNameText(const Integer &id) const
{
   if ((id >= 0) && (id < EndModelReps))
   {
      return MODEL_DESCRIPTIONS[id];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(Spacecraft *theSat)
{
      return false;
}


//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const ObjectArray targets);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(ObjectArray targets)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative
                                                    (Spacecraft *theSat)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const ObjectArray participants);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative(
                                                const ObjectArray participants)
{
      return false;
}

//------------------------------------------------------------------------------
// Real MeasurementModel::GetDegree(const Real angle, const Real minAngle, 
//                           const Real maxAngle) 
//------------------------------------------------------------------------------
Real MeasurementModel::GetDegree(const Real angle, const Real minAngle, 
                          const Real maxAngle) 
{
   Real angleInRange = GmatMathUtil::Mod(angle,GmatMathUtil::TWO_PI);
   
   if (angleInRange < minAngle)
      angleInRange += GmatMathUtil::TWO_PI;

   else if (angleInRange > maxAngle)
      angleInRange -= GmatMathUtil::TWO_PI;

   return GmatMathUtil::Deg(angleInRange);
}


//------------------------------------------------------------------------------
// LaVectorDouble GetTheMeasurements()
//------------------------------------------------------------------------------
LaVectorDouble MeasurementModel::GetTheMeasurements()
{
    return theMeasurements;
}

//------------------------------------------------------------------------------
// LaVectorDouble GetThePartials(const std::string param)
//------------------------------------------------------------------------------
LaGenMatDouble MeasurementModel::GetThePartials(const std::string param, Integer size, Spacecraft *theSat)
{
    return GetThePartials(GetDependentParameterID(param), Integer size, Spacecraft *theSat);
}

//------------------------------------------------------------------------------
// LaVectorDouble GetThePartials(const Integer paramID)
//------------------------------------------------------------------------------
LaGenMatDouble MeasurementModel::GetThePartials(const Integer paramID, Integer size, Spacecraft *theSat)
{
    return LaGenMatDouble::zeros(size);
}
