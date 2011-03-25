//$Header$
//------------------------------------------------------------------------------
//                             KeplerianElementsCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the Kepelerian Element data construct that is used
 * by the CCSDS Orbit Parameter Message format.
 *
 */
//------------------------------------------------------------------------------

#include "KeplerianElementsCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_KEYWORDS[EndKeplerianElementsCCSDSDataDataReps] =
{
    "SEMI_MAJOR_AXIS",
    "ECCENTRICITY",
    "INCLINATION",
    "RA_OF_ASC_NODE",
    "ARG_OF_PERICENTER",
    "TRUE_ANOMALY",
    "MEAN_ANOMALY",
    "GM",
    "COMMENT"
};

const std::string KeplerianElementsCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndKeplerianElementsCCSDSDataDataReps] =
{
    "km",
    "",
    "deg",
    "deg",
    "deg",
    "deg",
    "deg",
    "km^3/s^2",
    ""
};

const std::string KeplerianElementsCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndKeplerianElementsCCSDSDataDataReps] =
{
    "Keplerian Elements Semimajor Axis",
    "Keplerian Elements Eccentricity",
    "Keplerian Elements Inclination",
    "Keplerian Elements Right Ascension of the Ascending Node",
    "Keplerian Elements Argument of Pericenter",
    "Keplerian Elements True Anomaly",
    "Keplerian Elements Mean Anomaly",
    "Keplerian Elements Gravitational Coefficient",
    "Keplerian Elements Comments"
};

const bool KeplerianElementsCCSDSData::CCSDS_IS_REQUIRED[EndKeplerianElementsCCSDSDataDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    true,
    false
};

const Gmat::ParameterType KeplerianElementsCCSDSData::CCSDS_PARAMETER_TYPE[EndKeplerianElementsCCSDSDataDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  KeplerianElementsCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the KeplerianElementsCCSDSData class
 */
//------------------------------------------------------------------------------
KeplerianElementsCCSDSData::KeplerianElementsCCSDSData() : CCSDSData(),
    semiMajorAxis(GmatBase::REAL_PARAMETER_UNDEFINED),
    eccentricity(GmatBase::REAL_PARAMETER_UNDEFINED),
    inclination(GmatBase::REAL_PARAMETER_UNDEFINED),
    raan(GmatBase::REAL_PARAMETER_UNDEFINED),
    argumentOfPericenter(GmatBase::REAL_PARAMETER_UNDEFINED),
    theAnomaly(0,0,0,Anomaly::TA,false),
    gravitationalCoefficient(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  KeplerianElementsCCSDSData(const KeplerianElementsCCSDSData &ke)
//------------------------------------------------------------------------------
/**
 * Constructor for the KeplerianElementsCCSDSData class
 */
//------------------------------------------------------------------------------
KeplerianElementsCCSDSData::KeplerianElementsCCSDSData
               (const KeplerianElementsCCSDSData &ke) :
    CCSDSData(ke),
    semiMajorAxis(ke.semiMajorAxis),
    eccentricity(ke.eccentricity),
    inclination(ke.inclination),
    raan(ke.raan),
    argumentOfPericenter(ke.argumentOfPericenter),
    theAnomaly(ke.theAnomaly),
    gravitationalCoefficient(ke.gravitationalCoefficient),
    comments(ke.comments)
{
}

//---------------------------------------------------------------------------
//  KeplerianElementsCCSDSData& operator=
//                                   (const KeplerianElementsCCSDSData &ke)
//---------------------------------------------------------------------------
/**
 * Assignment operator for KeplerianElementsCCSDSData structures.
 *
 * @param <ke> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const KeplerianElementsCCSDSData& KeplerianElementsCCSDSData::operator=
                                     (const KeplerianElementsCCSDSData &ke)
{
    if (&ke == this)
        return *this;

    semiMajorAxis = ke.semiMajorAxis;
    eccentricity = ke.eccentricity;
    inclination = ke.inclination;
    raan = ke.raan;
    argumentOfPericenter = ke.argumentOfPericenter;
    theAnomaly = ke.theAnomaly;
    gravitationalCoefficient = ke.gravitationalCoefficient;
    comments = ke.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~KeplerianElementsCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the KeplerianElementsCCSDSData class
 */
//------------------------------------------------------------------------------
KeplerianElementsCCSDSData::~KeplerianElementsCCSDSData()
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
std::string KeplerianElementsCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndKeplerianElementsCCSDSDataDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
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
Integer KeplerianElementsCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndKeplerianElementsCCSDSDataDataReps; i++)
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
Gmat::ParameterType KeplerianElementsCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndKeplerianElementsCCSDSDataDataReps))
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
std::string KeplerianElementsCCSDSData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real KeplerianElementsCCSDSData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

            return semiMajorAxis;

	case CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID:

            return eccentricity;

	case CCSDS_KEPLERIANELEMENTS_INCLINATION_ID:

            return inclination;

	case CCSDS_KEPLERIANELEMENTS_RAAN_ID:

            return raan;

	case CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

            return argumentOfPericenter;

	case CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID:

            return theAnomaly.GetTrueAnomaly();

	case CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID:

            return theAnomaly.GetMeanAnomaly();

	case CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

            return gravitationalCoefficient;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real KeplerianElementsCCSDSData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray KeplerianElementsCCSDSData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_KEPLERIANELEMENTS_COMMENTS_ID:

	    return comments;

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
StringArray KeplerianElementsCCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Real &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool KeplerianElementsCCSDSData::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {
	case CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

            semiMajorAxis = value;
            return true;

	case CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID:

            eccentricity = value;
            return true;

	case CCSDS_KEPLERIANELEMENTS_INCLINATION_ID:

            inclination = value;
            return true;

	case CCSDS_KEPLERIANELEMENTS_RAAN_ID:

            raan = value;
            return true;

	case CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

            argumentOfPericenter = value;
            return true;

	case CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID:

            // Note this assumes that the value is provide in degrees
            theAnomaly.SetValue(value,false);
            return true;

	case CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID:

            // Note this assumes that the value is provide in degrees
            theAnomaly.SetValue(value,false);
            return true;

	case CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

            gravitationalCoefficient = value;
            return true;

        default:

            return false;

    }

}


//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Real &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <label> String label identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool KeplerianElementsCCSDSData::SetDataParameter(const std::string &label, const Real &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const StringArray &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool KeplerianElementsCCSDSData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {
        case CCSDS_KEPLERIANELEMENTS_COMMENTS_ID:

	    comments = value;
            return true;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const StringArray &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool KeplerianElementsCCSDSData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS OPM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* KeplerianElementsCCSDSData::GetKeywords() const
{
   return CCSDS_KEPLERIANELEMENTS_KEYWORDS;
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
const Integer KeplerianElementsCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndKeplerianElementsCCSDSDataDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_KEPLERIANELEMENTS_KEYWORDS[i]))
            return i;
    }

   return -1;

}

//------------------------------------------------------------------------------
//  std::string  GetUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string KeplerianElementsCCSDSData::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndKeplerianElementsCCSDSDataDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
bool KeplerianElementsCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndKeplerianElementsCCSDSDataDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  Integer CountRequiredNumberKeplerianElementsParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberKeplerianElementsParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < KeplerianElementsCCSDSData::EndKeplerianElementsCCSDSDataDataReps; id++)
        if (KeplerianElementsCCSDSData::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
}

//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool KeplerianElementsCCSDSData::Validate() const
{

    for (unsigned int i = 0; i < EndKeplerianElementsCCSDSDataDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    if (!IsParameterDefined(GetRealDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Real parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    if (!IsParameterDefined(GetStringArrayDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                default:
                    return false;
                    break;
            }
        }
    }

    return true;
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
std::ostream& operator<< (std::ostream &output,
                                             const KeplerianElementsCCSDSData *myKE)
{
    using namespace std;

   if (!myKE->Validate()) return output;

    unsigned int i;
    for (i = 0; i < myKE->comments.size(); i++ )
    {
        output << "COMMENT " << myKE->comments[i] << endl;
    }
    if (i > 0) output << endl;

   output << "SEMI_MAJOR_AXIS = " << myKE->semiMajorAxis << endl;
   output << "ECCENTRICITY = " << myKE->eccentricity << endl;
   output << "INCLINATION = " << myKE->inclination << endl;
   output << "RA_OF_ASC_NODE = " << myKE->raan << endl;
   output << "ARG_OF_PERICENTER = " << myKE->argumentOfPericenter << endl;
   output << "TRUE_ANOMALY = " << myKE->theAnomaly.GetTrueAnomaly() << endl;
   output << "MEAN_ANOMALY = " << myKE->theAnomaly.GetMeanAnomaly() << endl;
   output << "GM = " << myKE->gravitationalCoefficient << endl;
   output << endl;

   return output;
}