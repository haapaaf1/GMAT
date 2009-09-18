#include "CCSDSOEMObtype.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOEMObtype::CCSDS_OEM_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMDataReps] =
{
    "B3Type",
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

const std::string CCSDSOEMObtype::CCSDS_OEM_UNIT_DESCRIPTIONS[EndCCSDSOEMDataReps] =
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

const Gmat::ParameterType CCSDSOEMObtype::CCSDS_OEM_PARAMETER_TYPE[EndCCSDSOEMDataReps] =
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

const bool CCSDSOEMObtype::CCSDS_OEM_IS_REQUIRED[EndCCSDSOEMDataReps] =
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

const std::string CCSDSOEMObtype::CCSDS_OEM_DATATYPE_DESCRIPTIONS[EndCCSDSOEMTypeReps] =
{
    "Range",
    "RangeRate",
    "Azimuth",
    "Elevation",
    "RightAscension",
    "Declination"
    
};
    
const std::string CCSDSOEMObtype::CCSDS_OEM_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps] =
{
    "UTC"
};


//------------------------------------------------------------------------------
//  CCSDSOEMObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObtype::CCSDSOEMObtype() : CCSDSObtype(),
	ccsdsOEMMetaData(NULL),
	ccsdsOEMData(NULL)
{
}

//------------------------------------------------------------------------------
//  ~CCSDSOEMObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObtype::~CCSDSOEMObtype()
{
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
bool CCSDSOEMObtype::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndOEMDataReps)
	return CCSDS_OEM_IS_REQUIRED[id];
    else
	return false;
}