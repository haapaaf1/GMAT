#include "CCSDSOEMObType.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOEMObType::CCSDS_OEM_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMDataReps] =
{
    "CCSDSOEMType",
    "SecurityClassification",
    "SatelliteID",
    "SensorID",
    "Year",
    "DayOfYear",
    "Hour",
    "Minute",
    "Seconds",
    "Elevation",
    "Declination",
    "RightAscension",
    "Azimuth",
    "Range",
    "RangeRate",
    "Ecf_X",
    "Ecf_Y",
    "Ecf_Z"
};

const std::string CCSDSOEMObType::CCSDS_OEM_UNIT_DESCRIPTIONS[EndCCSDSOEMDataReps] =
{
    "",
    "",
    "",
    "",
    "year",
    "DayOfYear",
    "hrs",
    "min",
    "sec",
    "deg",
    "deg",
    "deg",
    "deg",
    "km",
    "km/sec",
    "km",
    "km",
    "km"
};

const Gmat::ParameterType CCSDSOEMObType::CCSDS_OEM_PARAMETER_TYPE[EndCCSDSOEMDataReps] =
{
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE
};

const bool CCSDSOEMObType::CCSDS_OEM_IS_REQUIRED[EndCCSDSOEMDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false
};

const std::string CCSDSOEMObType::CCSDS_OEM_DATATYPE_DESCRIPTIONS[EndCCSDSOEMTypeReps] =
{
    "Range",
    "RangeRate",
    "Azimuth",
    "Elevation",
    "RightAscension",
    "Declination"
    
};
    
const std::string CCSDSOEMObType::CCSDS_OEM_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps] =
{
    "UTC"
};


//------------------------------------------------------------------------------
//  CCSDSOEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::CCSDSOEMObType() : CCSDSObType("CCSDSOEMObType", ""),
	ccsdsOEMMetaData(NULL),
	ccsdsOEMData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSOEMObType(const CCSDSOEMObType &oem)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::CCSDSOEMObType(const CCSDSOEMObType &oem) : CCSDSObType(oem),
	ccsdsOEMMetaData(oem.ccsdsOEMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSOEMObType& operator=(const CCSDSOEMObType &oem)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <oem> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOEMObType& CCSDSOEMObType::operator=(const CCSDSOEMObType &oem)
{
   if (&oem == this)
      return *this;

    ccsdsOEMMetaData = oem.ccsdsOEMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOEMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::~CCSDSOEMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOEMDataFile.
 *
 * @return clone of the ProcessCCSDSOEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOEMObType::Clone() const
{
   GmatBase *clone = new CCSDSOEMObType(*this);
   return (clone);
}

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSOEMObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndOEMDataReps)
	return CCSDS_OEM_IS_REQUIRED[id];
    else
	return false;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOEMMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOEMMetaData *myMetadata)
{

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   output << "META_START" << std::endl;

   for (Integer i = 0; i < myMetadata->metadataComments.size(); i++ )
   {
       output << "COMMENT " << myMetadata->metadataComments[i] << std::endl;
   }
   output << "OBJECT_NAME = " << myMetadata->objectName << std::endl;
   output << "OBJECT_ID = " << myMetadata->internationalDesignator << std::endl;
   output << "CENTER_NAME = " << myMetadata->refFrameOrigin << std::endl;
   output << "REF_FRAME = " << myMetadata->refFrame << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;
   output << "START_TIME = " << myMetadata->startTime << std::endl;
   output << "USEABLE_START_TIME = " << myMetadata->useableStartTime << std::endl;
   output << "USEABLE_STOP_TIME = " << myMetadata->useableStopTime << std::endl;
   output << "STOP_TIME = " << myMetadata->stopTime << std::endl;
   output << "INTERPOLATION = " << myMetadata->interpolationMethod << std::endl;
   output << "INTERPOLATION_DEGREE = " << myMetadata->interpolationDegree << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}
