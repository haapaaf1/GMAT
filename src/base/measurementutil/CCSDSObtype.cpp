#include "CCSDSObtype.hpp";
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSObtype::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps] =
{
"Quaternion",
"Euler Angle",
"Spin Stabilized",
"State Vector",
"Keplerian Elements",
"Spacecraft Parameters",
"Generic Data Type"
};

const std::string CCSDSObtype::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

const std::string CCSDSObtype::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "HeaderComments"
};

const std::string CCSDSObtype::CCSDS_KEYWORDS[EndCCSDSDataReps] =
{
    "CCSDS_VERSION",
    "CREATION_DATE",
    "ORIGINATOR",
    "COMMENTS"
};

const bool CCSDSObtype::CCSDS_IS_REQUIRED[EndCCSDSDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSObtype::CCSDS_PARAMETER_TYPE[EndCCSDSDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSObtype::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "",
    "",
    "",
    ""
};

//------------------------------------------------------------------------------
//  CCSDSObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObtype::CCSDSObtype() : Obtype(),
    ccsdsHeader(NULL),
    ccsdsData(NULL),
    ccsdsQuaternion(NULL),
    ccsdsEulerAngle(NULL),
    ccsdsSpinStabilized(NULL),
    ccsdsStateVector(NULL),
    ccsdsKeplerianElements(NULL),
    ccsdsSpacecraftParameters(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSObtype(const CCSDSObtype &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObtype::CCSDSObtype(const CCSDSObtype &ob) : Obtype(ob),
    ccsdsHeader(ob.ccsdsHeader),
    ccsdsData(ob.ccsdsData),
    ccsdsQuaternion(ob.ccsdsQuaternion),
    ccsdsEulerAngle(ob.ccsdsEulerAngle),
    ccsdsSpinStabilized(ob.ccsdsSpinStabilized),
    ccsdsStateVector(ob.ccsdsStateVector),
    ccsdsKeplerianElements(ob.ccsdsKeplerianElements),
    ccsdsSpacecraftParameters(ob.ccsdsSpacecraftParameters)
{
}

//---------------------------------------------------------------------------
//  CCSDSObtype& operator=(const CCSDSObtype &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Obtype structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSObtype& CCSDSObtype::operator=(const CCSDSObtype &ob)
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
    
   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObtype::~CCSDSObtype()
{
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetDataParameterText(const Integer id) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetDataUnits(const Integer id) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSObtype::GetDataParameterID(const std::string &str) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSObtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
std::string CCSDSObtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
Integer CCSDSObtype::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSObtype::GetIntegerDataParameter(const std::string &label) const
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
bool CCSDSObtype::GetBoolDataParameter(const Integer id) const
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
bool CCSDSObtype::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
StringArray CCSDSObtype::GetStringArrayDataParameter(const Integer id) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
StringArray CCSDSObtype::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetStringDataParameter(const Integer id) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSObtype::GetRealDataParameter(const Integer id) const
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSObtype::GetRealDataParameter(const std::string &label) const
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
const std::string* CCSDSObtype::GetDataTypes() const
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
std::string CCSDSObtype::GetDataTypeText(const Integer &id) const
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
Integer CCSDSObtype::GetDataTypeID(const std::string &label)
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
const std::string* CCSDSObtype::GetTimeSystems() const
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
std::string CCSDSObtype::GetTimeSystemText(const Integer &id) const
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
Integer CCSDSObtype::GetTimeSystemID(const std::string &label)
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
bool CCSDSObtype::IsParameterRequired(const Integer id) const
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
bool CCSDSObtype::CheckDataAvailability(const std::string str) const
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
const std::string* CCSDSObtype::GetKeywords() const
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
const Integer CCSDSObtype::GetKeywordID(const std::string str) const
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
std::string CCSDSObtype::GetUnits(const Integer &id) const
{
   return std::string("");
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSObtype *myOb)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObtype data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myOb>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSObtype *myOb)
{
   using namespace std;

   output.setf(std::ios::showpoint);
   output.setf(std::ios::scientific);

   if (myOb->ccsdsHeader != NULL)
   {
        output << "CCSDS Version = " << myOb->ccsdsHeader->ccsdsVersion << std::endl;
        output << "Creation Date = " << myOb->ccsdsHeader->creationDate << std::endl;
        output << "Originator = " << myOb->ccsdsHeader->originator << std::endl;
        for (Integer i = 0; i < myOb->ccsdsHeader->headerComments.size(); i++)
        {
            output << "Comments = " << myOb->ccsdsHeader->headerComments[i] << std::endl;
        }
        output << "******************************************************" << std::endl;
   }

   if (myOb->ccsdsData != NULL)
   {
        output << "Keyword = " << myOb->ccsdsData->keywordID << std::endl;
        output << "Time Tag = " << myOb->ccsdsData->timeTag << std::endl;
        output << "Measurement = " << myOb->ccsdsData->measurement << std::endl;
        output << "******************************************************" << std::endl;
   }

   if (myOb->ccsdsSpacecraftParameters != NULL)
   {
        output << "Mass = " << myOb->ccsdsSpacecraftParameters->mass << std::endl;
        output << "Solar Radiation Area = " << myOb->ccsdsSpacecraftParameters->solarRadiationArea << std::endl;
        output << "Solar Radiation Coefficient = " << myOb->ccsdsSpacecraftParameters->solarRadiationCoefficient << std::endl;
        output << "Drag Area = " << myOb->ccsdsSpacecraftParameters->dragArea << std::endl;
        output << "Drag Coefficient = " << myOb->ccsdsSpacecraftParameters->dragCoefficient << std::endl;
        output << "Intertial Reference Frame = " << myOb->ccsdsSpacecraftParameters->intertiaRefFrame << std::endl;
        output << "I(1,1) = " << myOb->ccsdsSpacecraftParameters->i11 << std::endl;
        output << "I(2,2) = " << myOb->ccsdsSpacecraftParameters->i22 << std::endl;
        output << "I(3,3) = " << myOb->ccsdsSpacecraftParameters->i33 << std::endl;
        output << "I(1,2) = " << myOb->ccsdsSpacecraftParameters->i12 << std::endl;
        output << "I(1,3) = " << myOb->ccsdsSpacecraftParameters->i13 << std::endl;
        output << "I(2,3) = " << myOb->ccsdsSpacecraftParameters->i23 << std::endl;
        output << "******************************************************" << std::endl;
   }

   if (myOb->ccsdsKeplerianElements != NULL)
   {
        output << "Semimajor Axis = " << myOb->ccsdsKeplerianElements->semiMajorAxis << std::endl;
        output << "Eccentricity = " << myOb->ccsdsKeplerianElements->eccentricity << std::endl;
        output << "Inclination = " << myOb->ccsdsKeplerianElements->inclination << std::endl;
        output << "RAAN = " << myOb->ccsdsKeplerianElements->raan << std::endl;
        output << "Argument of Pericenter = " << myOb->ccsdsKeplerianElements->argumentOfPericenter << std::endl;
        output << "True Anomaly = " << myOb->ccsdsKeplerianElements->trueAnomaly << std::endl;
        output << "Mean Anomaly = " << myOb->ccsdsKeplerianElements->meanAnomaly << std::endl;
        output << "Gravitational Coefficient = " << myOb->ccsdsKeplerianElements->gravitationalCoefficient << std::endl;
        output << "******************************************************" << std::endl;
   }
 
   if (myOb->ccsdsStateVector != NULL)
   {
        output << "Epoch = " << myOb->ccsdsStateVector->epoch << std::endl;
        output << "X = " << myOb->ccsdsStateVector->X << std::endl;
        output << "Y = " << myOb->ccsdsStateVector->Y << std::endl;
        output << "Z = " << myOb->ccsdsStateVector->Z << std::endl;
        output << "X dot = " << myOb->ccsdsStateVector->xDot << std::endl;
        output << "Y dot = " << myOb->ccsdsStateVector->yDot << std::endl;
        output << "Z dot = " << myOb->ccsdsStateVector->zDot << std::endl;
        output << "******************************************************" << std::endl;
   }

   if (myOb->ccsdsSpinStabilized != NULL)
   {
       output << "Attitude type = " << myOb->ccsdsSpinStabilized->attitudeType << std::endl;
        for (Integer i = 0; i < myOb->ccsdsSpinStabilized->comments.size(); i++)
        {
           output << "Comments = " << myOb->ccsdsSpinStabilized->comments[i] << std::endl;
        }
       output << "Ref Frame A = " << myOb->ccsdsSpinStabilized->frameA << std::endl;
       output << "Ref Frame B = " << myOb->ccsdsSpinStabilized->frameB << std::endl;
       output << "Direction = " << myOb->ccsdsSpinStabilized->direction << std::endl;
       output << "Spin Alpha = " << myOb->ccsdsSpinStabilized->spinAlpha << std::endl;
       output << "Spin Delta = " << myOb->ccsdsSpinStabilized->spinDelta << std::endl;
       output << "Spin Angle = " << myOb->ccsdsSpinStabilized->spinAngle << std::endl;
       output << "Spin Angle Velocity = " << myOb->ccsdsSpinStabilized->spinAngleVelocity << std::endl;
       output << "Nutation = " << myOb->ccsdsSpinStabilized->nutation << std::endl;
       output << "Nutation Period = " << myOb->ccsdsSpinStabilized->nutationPeriod << std::endl;
       output << "Nutation Phase = " << myOb->ccsdsSpinStabilized->nutationPhase << std::endl;
       output << "******************************************************" << std::endl;
   }

   if (myOb->ccsdsEulerAngle != NULL)
   {
       output << "Euler Angle Type = " << myOb->ccsdsEulerAngle->eulerAngleType << std::endl;
       output << "Ref Frame A = " << myOb->ccsdsEulerAngle->frameA << std::endl;
       output << "Ref Frame B = " << myOb->ccsdsEulerAngle->frameB << std::endl;
       output << "Direction = " << myOb->ccsdsEulerAngle->direction << std::endl;
       output << "Rotation Sequence = " << myOb->ccsdsEulerAngle->rotationSequence << std::endl;
       output << "Rate Frame = " << myOb->ccsdsEulerAngle->rateFrame << std::endl;
       output << "X Angle = " << myOb->ccsdsEulerAngle->xAngle << std::endl;
       output << "Y Angle = " << myOb->ccsdsEulerAngle->yAngle << std::endl;
       output << "Z Angle = " << myOb->ccsdsEulerAngle->zAngle << std::endl;
       output << "X Rate = " << myOb->ccsdsEulerAngle->xRate << std::endl;
       output << "Y Rate = " << myOb->ccsdsEulerAngle->yRate << std::endl;
       output << "Z Rate = " << myOb->ccsdsEulerAngle->zRate << std::endl;
       output << "******************************************************" << std::endl;
   }
    
   if (myOb->ccsdsQuaternion != NULL)
   {
        output << "Quaternion Type = " << myOb->ccsdsQuaternion->quarternionType << std::endl;
        output << "Ref Frame A = " << myOb->ccsdsQuaternion->frameA << std::endl;
        output << "Ref Frame B = " << myOb->ccsdsQuaternion->frameB << std::endl;
        output << "Direction = " << myOb->ccsdsQuaternion->direction << std::endl;
        output << "Q1 = " << myOb->ccsdsQuaternion->q1 << std::endl;
        output << "Q2 = " << myOb->ccsdsQuaternion->q2 << std::endl;
        output << "Q3 = " << myOb->ccsdsQuaternion->q3 << std::endl;
        output << "QC = " << myOb->ccsdsQuaternion->qC << std::endl;
        output << "Q1 dot = " << myOb->ccsdsQuaternion->q1Dot << std::endl;
        output << "Q2 dot = " << myOb->ccsdsQuaternion->q2Dot << std::endl;
        output << "Q3 dot = " << myOb->ccsdsQuaternion->q3Dot << std::endl;
        output << "QC dot = " << myOb->ccsdsQuaternion->qcDot << std::endl;
        output << "X Rate = " << myOb->ccsdsQuaternion->xRate << std::endl;
        output << "Y Rate = " << myOb->ccsdsQuaternion->yRate << std::endl;
        output << "Z Rate = " << myOb->ccsdsQuaternion->zRate << std::endl;
        output << "******************************************************" << std::endl;
   }

   return output;
}



