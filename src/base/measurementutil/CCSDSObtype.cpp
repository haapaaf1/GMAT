#include "CCSDSObType.hpp";
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSObType::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps] =
{
    "Quaternion",
    "Euler Angle",
    "Spin Stabilized",
    "State Vector",
    "Keplerian Elements",
    "Spacecraft Parameters",
    "Maneuver",
    "Generic Data Type"
};

const std::string CCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

const std::string CCSDSObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "HeaderComments"
};

const std::string CCSDSObType::CCSDS_KEYWORDS[EndCCSDSDataReps] =
{
    "CCSDS_VERSION",
    "CREATION_DATE",
    "ORIGINATOR",
    "COMMENTS"
};

const bool CCSDSObType::CCSDS_IS_REQUIRED[EndCCSDSDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSObType::CCSDS_PARAMETER_TYPE[EndCCSDSDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "",
    "",
    "",
    ""
};

//------------------------------------------------------------------------------
//  CCSDSObType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const std::string &type, const std::string &name) :
   ObType(type, name),
    ccsdsHeader(NULL),
    ccsdsData(NULL),
    ccsdsQuaternion(NULL),
    ccsdsEulerAngle(NULL),
    ccsdsSpinStabilized(NULL),
    ccsdsStateVector(NULL),
    ccsdsKeplerianElements(NULL),
    ccsdsSpacecraftParameters(NULL),
    ccsdsManeuver(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSObType(const CCSDSObType &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const CCSDSObType &ob) : ObType(ob),
    ccsdsHeader(ob.ccsdsHeader),
    ccsdsData(ob.ccsdsData),
    ccsdsQuaternion(ob.ccsdsQuaternion),
    ccsdsEulerAngle(ob.ccsdsEulerAngle),
    ccsdsSpinStabilized(ob.ccsdsSpinStabilized),
    ccsdsStateVector(ob.ccsdsStateVector),
    ccsdsKeplerianElements(ob.ccsdsKeplerianElements),
    ccsdsSpacecraftParameters(ob.ccsdsSpacecraftParameters),
    ccsdsManeuver(ob.ccsdsManeuver)
{
}

//---------------------------------------------------------------------------
//  CCSDSObType& operator=(const CCSDSObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSObType& CCSDSObType::operator=(const CCSDSObType &ob)
{
   if (&ob == this)
      return *this;

    ccsdsHeader = ob.ccsdsHeader;
    ccsdsData = ob.ccsdsData;
    ccsdsQuaternion = ob.ccsdsQuaternion;
    ccsdsEulerAngle = ob.ccsdsEulerAngle;
    ccsdsSpinStabilized = ob.ccsdsSpinStabilized;
    ccsdsStateVector = ob.ccsdsStateVector;
    ccsdsKeplerianElements = ob.ccsdsKeplerianElements;
    ccsdsSpacecraftParameters = ob.ccsdsSpacecraftParameters;
    ccsdsManeuver = ob.ccsdsManeuver;
    
   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::~CCSDSObType()
{
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }
      
   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSObType::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObType::GetBoolDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObType::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_HEADERCOMMENTS_ID:

	    return ccsdsHeader->headerComments;

        default:

            return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSObType::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_CREATIONDATE_ID:

	    return ccsdsHeader->creationDate;

	case CCSDS_ORIGINATOR_ID:

	    return ccsdsHeader->originator;
	    
        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

       case CCSDS_VERSION_ID:

            return ccsdsHeader->ccsdsVersion;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable data types.
 *
 * @return String array of all data types.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSObType::GetDataTypes() const
{
   return CCSDS_DATATYPE_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetDataTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type text corresponding to a ID
 *
 * @param <id> Integer ID associated with the data type
 * @return The string description of the data type
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTypeReps))
   {
      return CCSDS_DATATYPE_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetDataTypeID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type ID
 *
 * @param <label> The string label associated with the data type
 * @return The integer data type ID
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetDataTypeID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// const StringArray GetTimeSystems() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable time systems.
 *
 * @return String array of all time systems
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSObType::GetTimeSystems() const
{
   return CCSDS_TIMESYSTEM_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetTimeSystemText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system text corresponding to a ID
 *
 * @param <id> Integer ID associated with the time system
 * @return The string description of the time system
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetTimeSystemID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system ID
 *
 * @param <label> The string label associated with the time system
 * @return The integer time system ID
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = 0; i < EndCCSDSTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
 
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
bool CCSDSObType::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSDataReps)
    return CCSDS_IS_REQUIRED[id];
else
    return false;
}

//------------------------------------------------------------------------------
//  bool CheckDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool CCSDSObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS TDM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSObType::GetKeywords() const
{
   return CCSDS_KEYWORDS;
}

//------------------------------------------------------------------------------
//  const Integer GetKeywordID(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
const Integer CCSDSObType::GetKeywordID(const std::string str) const
{
   return -1;
}

//------------------------------------------------------------------------------
//  std::string GetUnits(const Integer &id) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetUnits(const Integer &id) const
{
   return std::string("");
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSQuaternion *myQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSQuaternion *myQuaternion)
{
   using namespace std;

   output << "Quaternion Type = " << myQuaternion->quarternionType << endl;
   output << "Q_FRAME_A = " << myQuaternion->frameA << endl;
   output << "Q_FRAME_B = " << myQuaternion->frameB << endl;
   output << "Q_DIR = " << myQuaternion->direction << endl;
   output << "Q1 = " << myQuaternion->q1 << endl;
   output << "Q2 = " << myQuaternion->q2 << endl;
   output << "Q3 = " << myQuaternion->q3 << endl;
   output << "QC = " << myQuaternion->qC << endl;
   output << "Q1_DOT = " << myQuaternion->q1Dot << endl;
   output << "Q2_DOT = " << myQuaternion->q2Dot << endl;
   output << "Q3_DOT = " << myQuaternion->q3Dot << endl;
   output << "QC_DOT = " << myQuaternion->qcDot << endl;
   output << "X_RATE = " << myQuaternion->xRate << endl;
   output << "Y_RATE = " << myQuaternion->yRate << endl;
   output << "Z_RATE = " << myQuaternion->zRate << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSEulerAngle *myEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSEulerAngle *myEulerAngle)
{
   using namespace std;

   output << "Euler Angle Type = " << myEulerAngle->eulerAngleType << endl;
   output << "EULER_FRAME_A = " << myEulerAngle->frameA << endl;
   output << "EULER_FRAME_B = " << myEulerAngle->frameB << endl;
   output << "EULER_DIR = " << myEulerAngle->direction << endl;
   output << "EULER_ROT_SEQ = " << myEulerAngle->rotationSequence << endl;
   output << "RATE_FRAME = " << myEulerAngle->rateFrame << endl;
   output << "X_ANGLE = " << myEulerAngle->xAngle << endl;
   output << "Y_ANGLE = " << myEulerAngle->yAngle << endl;
   output << "Z_ANGLE = " << myEulerAngle->zAngle << endl;
   output << "X_RATE = " << myEulerAngle->xRate << endl;
   output << "Y_RATE = " << myEulerAngle->yRate << endl;
   output << "Z_RATE = " << myEulerAngle->zRate << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSSpinStabilized *mySpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSSpinStabilized *mySpinStabilized)
{
   using namespace std;

   output << "Attitude type = " << mySpinStabilized->attitudeType << endl;
   for (Integer i = 0; i < mySpinStabilized->comments.size(); i++)
   {
       output << "COMMENT " << mySpinStabilized->comments[i] << endl;
   }
   output << "SPIN_FRAME_A = " << mySpinStabilized->frameA << endl;
   output << "SPIN_FRAME_B = " << mySpinStabilized->frameB << endl;
   output << "SPIN_DIR = " << mySpinStabilized->direction << endl;
   output << "SPIN_ALPHA = " << mySpinStabilized->spinAlpha << endl;
   output << "SPIN_DELTA = " << mySpinStabilized->spinDelta << endl;
   output << "SPIN_ANGLE = " << mySpinStabilized->spinAngle << endl;
   output << "SPIN_ANGLE_VEL = " << mySpinStabilized->spinAngleVelocity << endl;
   output << "NUTATION = " << mySpinStabilized->nutation << endl;
   output << "NUTATION_PER = " << mySpinStabilized->nutationPeriod << endl;
   output << "NUTATION_PHASE = " << mySpinStabilized->nutationPhase << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSStateVector *myStateVector)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myStateVector>    CCSDS state vector data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSStateVector *myStateVector)
{
   using namespace std;

   output << "EPOCH = " << myStateVector->epoch << endl;
   output << "X = " << myStateVector->X << endl;
   output << "Y = " << myStateVector->Y << endl;
   output << "Z = " << myStateVector->Z << endl;
   output << "X_DOT = " << myStateVector->xDot << endl;
   output << "Y_DOT = " << myStateVector->yDot << endl;
   output << "Z_DOT = " << myStateVector->zDot << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myOb>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSKeplerianElements *myKeplerianElements)
{
   using namespace std;

   output << "SEMI_MAJOR_AXIS = " << myKeplerianElements->semiMajorAxis << endl;
   output << "ECCENTRICITY = " << myKeplerianElements->eccentricity << endl;
   output << "INCLINATION = " << myKeplerianElements->inclination << endl;
   output << "RA_OF_ASC_NODE = " << myKeplerianElements->raan << endl;
   output << "ARG_OF_PERICENTER = " << myKeplerianElements->argumentOfPericenter << endl;
   output << "TRUE_ANOMALY = " << myKeplerianElements->trueAnomaly << endl;
   output << "MEAN_ANOMALY = " << myKeplerianElements->meanAnomaly << endl;
   output << "GM = " << myKeplerianElements->gravitationalCoefficient << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSSpacecraftParameters *mySpacecraftParameters)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpacecraftParameters>    CCSDS spacecraft parameter data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSSpacecraftParameters *mySpacecraftParameters)
{
   using namespace std;

   output << "MASS = " << mySpacecraftParameters->mass << endl;
   output << "SOLAR_RAD_AREA = " << mySpacecraftParameters->solarRadiationArea << endl;
   output << "SOLAR_RAD_COEFF = " << mySpacecraftParameters->solarRadiationCoefficient << endl;
   output << "DRAG_AREA = " << mySpacecraftParameters->dragArea << endl;
   output << "DRAG_COEFF = " << mySpacecraftParameters->dragCoefficient << endl;
   output << "INERTIA_REF_FRAME = " << mySpacecraftParameters->intertiaRefFrame << endl;
   output << "I11 = " << mySpacecraftParameters->i11 << endl;
   output << "I22 = " << mySpacecraftParameters->i22 << endl;
   output << "I33 = " << mySpacecraftParameters->i33 << endl;
   output << "I12 = " << mySpacecraftParameters->i12 << endl;
   output << "I13 = " << mySpacecraftParameters->i13 << endl;
   output << "I23 = " << mySpacecraftParameters->i23 << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSData *myData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myData>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSData *myData)
{
    using namespace std;

   output << myData->keywordID << " = " << myData->timeTag << " " << myData->measurement << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myHeader>    CCSDS header data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
{
    using namespace std;

    output << "CCSDS_" << myHeader->fileType << "_VERS = " << myHeader->ccsdsVersion << endl;
    for (Integer i = 0; i < myHeader->headerComments.size(); i++)
    {
        output << "COMMENT " << myHeader->headerComments[i] << endl;
    }
    output << "CREATION_DATE = " << myHeader->creationDate << endl;
    output << "ORIGINATOR = " << myHeader->originator << endl;

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_IGNITION = " << myManeuver->ignitionEpoch << endl;
    output << "MAN_DURATION = " << myManeuver->duration << endl;
    output << "MAN_DELTA_MASS = " << myManeuver->deltaMass << endl;
    output << "MAN_REF_FRAME = " << myManeuver->refFrame << endl;
    output << "MAN_DV_1 = " << myManeuver->deltaV1 << endl;
    output << "MAN_DV_2 = " << myManeuver->deltaV2 << endl;
    output << "MAN_DV_3 = " << myManeuver->deltaV3 << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, 
//                           const CCSDSAttitudeManeuver *myAttitudeManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAttitudeManeuver *myAttitudeManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_START = " << myAttitudeManeuver->epochStart << endl;
    output << "MAN_DURATION = " << myAttitudeManeuver->duration << endl;
    output << "MAN_REF_FRAME = " << myAttitudeManeuver->refFrame << endl;
    output << "MAN_TOR_1 = " << myAttitudeManeuver->tor1 << endl;
    output << "MAN_TOR_2 = " << myAttitudeManeuver->tor2 << endl;
    output << "MAN_TOR_3 = " << myAttitudeManeuver->tor3 << endl;

    return output;
}