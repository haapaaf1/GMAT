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

//---------------------------------
//  static data
//---------------------------------
const std::string MeasurementModel::MODEL_DESCRIPTIONS[NUM_MODELS] =
{
    "NotDefined",
    "Range",
    "RangeRate",
    "LightTime",
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
/*   static StringArray fullList;  // Maintain scope if the full list is requested
   fullList.clear();
   
   if (attitude)
   {
      try
      {
         fullList.push_back(attitude->GetRefObjectName(type));
      }
      catch (GmatBaseException& be)
      {
         // ignore exceptions here
      }
   }
   
   if (type == Gmat::UNKNOWN_OBJECT)
   {
      fullList.push_back(coordSysName);
      return fullList;      
   }
   else
   {
      if (type == Gmat::ATTITUDE)
         return fullList;
      
      if (type == Gmat::FUEL_TANK)
         return tankNames;
      if (type == Gmat::THRUSTER)
         return thrusterNames;
      
      if (type == Gmat::HARDWARE) 
      {
         fullList.clear();
         fullList = tankNames;
         for (StringArray::iterator i = thrusterNames.begin();
              i < thrusterNames.end(); ++i)
            fullList.push_back(*i);
         return fullList;
      }
      
      if (type == Gmat::COORDINATE_SYSTEM)
      {
         fullList.push_back(coordSysName);
         return fullList;
      }
   }
  */  
   return GmatBase::GetRefObjectNameArray(type);

}

//------------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
//                   const std::string &name)
//------------------------------------------------------------------------------
bool MeasurementModel::SetRefObject(GmatBase *obj, const Gmat::ObjectType type, 
                              const std::string &name)
{
    /*
   #ifdef DEBUG_SC_REF_OBJECT
   MessageInterface::ShowMessage("Entering SC::SetRefObject\n");
   #endif
   
   if (obj == NULL)
      return false;
   
   // first, try setting it on the attitude (owned object)
   if (attitude)
   {
      try
      {
         attitude->SetRefObject(obj, type, name);
      }
      catch (BaseException &be)
      {
         #ifdef DEBUG_SC_ATTITUDE
         MessageInterface::ShowMessage(
         "------ error setting ref object %s on attitude\n",
         name.c_str());
         #endif
      }
   }
   if (type == Gmat::HARDWARE) {
      std::string typeStr = obj->GetTypeName();
    
      if (typeStr == "FuelTank") {
         if (find(tanks.begin(), tanks.end(), obj) == tanks.end()) {
            tanks.push_back(obj);
            return true;
         }
         return false;
      }
      
      if (typeStr == "Thruster") {
         if (find(thrusters.begin(), thrusters.end(), obj) == thrusters.end()) {
            thrusters.push_back(obj);
            return true;
         }
         return false;
      }
      
      return false;
   }
   else if (type == Gmat::COORDINATE_SYSTEM)
   {
      CoordinateSystem *cs = (CoordinateSystem*)obj;
      
      #if DEBUG_MeasurementModel_CS
      MessageInterface::ShowMessage
         ("MeasurementModel::SetRefObject() coordinateSystem=%s<%p>, cs=%s<%p> on %s\n",
          coordinateSystem->GetName().c_str(), coordinateSystem,
          cs->GetName().c_str(), cs, instanceName.c_str());
      #endif
      
      if (coordinateSystem != cs)
      {
         coordinateSystem = cs;         
         TakeAction("ApplyCoordinateSystem");
         
         #if DEBUG_MeasurementModel_CS
         MessageInterface::ShowMessage
            ("MeasurementModel::SetRefObject() coordinateSystem applied ----------\n");
         #endif
      }
      
      return true;
   }
   else if (type == Gmat::ATTITUDE)
   {
      #ifdef DEBUG_SC_ATTITUDE
         MessageInterface::ShowMessage("Setting attitude object on MeasurementModel %s\n",
         instanceName.c_str());
      #endif
      if ((attitude != NULL) && (attitude != (Attitude*) obj)) delete attitude;
      attitude = (Attitude*) obj;
      // set epoch ...
      #ifdef DEBUG_SC_ATTITUDE
         MessageInterface::ShowMessage("Setting attitude object on MeasurementModel %s\n",
         instanceName.c_str());
         MessageInterface::ShowMessage(
         "Setting epoch on attitude object for MeasurementModel %s\n",
         instanceName.c_str());
      #endif
      attitude->SetEpoch(state.GetEpoch());
      ownedObjectCount++;
      return true;
   }
   
   #ifdef DEBUG_SC_REF_OBJECT
   MessageInterface::ShowMessage
      ("Exiting SC::SetRefObject, Calling SpaceObject::SetRefObject()\n");
   #endif
   
     */
    return GmatBase::SetRefObject(obj, type, name);

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
   modelID     (0)
{
   objectTypes.push_back(Gmat::MEASUREMENT_MODEL);
   objectTypeNames.push_back("MeasurementModel");
}

//------------------------------------------------------------------------------
//   MeasurementModel::MeasurementModel(const MeasurementModel &mm)
//------------------------------------------------------------------------------
MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase    (mm),
   modelID  (mm.modelID)
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

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
void MeasurementModel::Initialize() const
{
    CoordinateConverter ccvtr;
/*    
    if(dataFormat == "B3")
    {
	myData = new ProcessB3Data();
    }
    else if (dataFormat == "SLR")
    {
	myData = new ProcessSLRData();
    }
    else if (dataFormat == "TLE")
    {
	myData = new ProcessTLEData();
    }
    else
    {
      throw MeasurementModelException("Unable to process data with the specified data format: " + dataFormat);
      MessageInterface::ShowMessage(
            "Unable to process data with the\n specified data format: " + dataFormat);
    }
 
 */
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
// const StringArray& MeasurementModel::GetMeasurements()
//------------------------------------------------------------------------------
/**
 * Returns the measurements generated by the model.
 *
 * @return The measurements.
 */
//------------------------------------------------------------------------------
const Real* MeasurementModel::GetMeasurements() const
{
   return measurements;
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
   if (!strcmp(label.c_str(),"Range")) {
       return RANGE_ID;
   } else if (!strcmp(label.c_str(),"RangeRate")) {
       return RANGERATE_ID;
   } else if (!strcmp(label.c_str(),"LightTime")) {
       return LIGHTTIME_ID;
   } else if (!strcmp(label.c_str(),"VariableTransmitterRange")) {
       return VARIABLETRANSMITTERRANGE_ID;
   } else if (!strcmp(label.c_str(),"AntennaTracking")) {
       return ANTENNATRACKING_ID;
   } else if (!strcmp(label.c_str(),"SunSensor")) {
       return SUNSENSOR_ID;
   } else if (!strcmp(label.c_str(),"StarSensor")) {
       return STARSENSOR_ID;
   } else if (!strcmp(label.c_str(),"GyroPackage")) {
       return GYROPACKAGE_ID;
   } else if (!strcmp(label.c_str(),"HorizonSensor")) {
       return HORIZONSENSOR_ID;
   } else if (!strcmp(label.c_str(),"Videometers")) {
       return VIDEOMETERS_ID;
   } else if (!strcmp(label.c_str(),"CoherentDoppler")) {
       return COHERENTDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"NonCoherentDoppler")) {
       return NONCOHERENTDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"VariableTransmitterDoppler")) {
       return VARIABLETRANSMITTERDOPPLER_ID;
   } else if (!strcmp(label.c_str(),"IntegratedDopplerCount")) {
       return INTEGRATEDDOPPLERCOUNT_ID;
   } else if (!strcmp(label.c_str(),"IMU")) {
       return IMU_ID;
   } else if (!strcmp(label.c_str(),"Magnetometer")) {
       return MAGNETOMETER_ID;
   } else if (!strcmp(label.c_str(),"AO_AzEl")) {
       return AO_AZEL_ID;
   } else if (!strcmp(label.c_str(),"RangeAzEl")) {
       return RANGEAZEL_ID;
   } else if (!strcmp(label.c_str(),"AO_RaDec")) {
       return AO_RADEC_ID;
   } else if (!strcmp(label.c_str(),"RangeRaDec")) {
       return RANGERADEC_ID;
   } else
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
// Integer ComputeMeasurement(ObjectArray participants,
//			      const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(ObjectArray participants,
					  LaVectorDouble &myMeasurements)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeMeasurement(GroundStation *theStation,
      Spacecraft *theSat, LaVectorDouble &myMeasurements)
{
      return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(ObjectArray participants, 
//		const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative(
	    ObjectArray participants, LaGenMatDouble &myCartDerivatives)
{
    return false;
}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool MeasurementModel::ComputeCartesianPartialDerivative(
      GroundStation *theStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
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
// void SetDataFormat(std::string df)
//------------------------------------------------------------------------------
/**
 * Set the data format associated with this instance of the measurement model.
 *
 * @param mm The data format that is assigned.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetDataFormat(std::string &df)
{
    dataFormat = df;
}

//------------------------------------------------------------------------------
// std::string GetDataFormat()
//------------------------------------------------------------------------------
/**
 * Return the data format associated with this instance of the measurement model.
 *
 * @return The data format.
 */
//------------------------------------------------------------------------------
std::string& MeasurementModel::GetDataFormat()
{
    return dataFormat;
}

//------------------------------------------------------------------------------
// void SetDataTypes(StringArray dt)
//------------------------------------------------------------------------------
/**
 * Set the data types associated with this instance of the measurement model.
 *
 * @param mm The data types that are assigned.
 */
//------------------------------------------------------------------------------
void MeasurementModel::SetDataTypes(StringArray &dt)
{
    dataTypes = dt;
}

//------------------------------------------------------------------------------
// StringArray GetDataTypes()
//------------------------------------------------------------------------------
/**
 * Return the data types associated with this instance of the measurement model.
 *
 * @return The data types.
 */
//------------------------------------------------------------------------------
StringArray& MeasurementModel::GetDataTypes()
{
    return dataTypes;
}









