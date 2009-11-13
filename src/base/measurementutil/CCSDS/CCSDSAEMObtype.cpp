#include "CCSDSAEMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAEMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType() : CCSDSObType("CCSDSAEMObType", ""),
	ccsdsMetaData(NULL),
        ccsdsAEMQuaternion(NULL),
        ccsdsAEMEulerAngle(NULL),
        ccsdsAEMSpinStabilized(NULL),
        commentsCurrentlyAllowed(false)
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMObType(const CCSDSAEMObType &aem)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType(const CCSDSAEMObType &aem) : CCSDSObType(aem),
	ccsdsMetaData(aem.ccsdsMetaData),
        ccsdsAEMQuaternion(aem.ccsdsAEMQuaternion),
        ccsdsAEMEulerAngle(aem.ccsdsAEMEulerAngle),
        ccsdsAEMSpinStabilized(aem.ccsdsAEMSpinStabilized),
        commentsCurrentlyAllowed(aem.commentsCurrentlyAllowed)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMObType& operator=(const CCSDSAEMObType &aem)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <AEM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &aem)
{
   if (&aem == this)
      return *this;

   CCSDSObType::operator=(aem);

   ccsdsMetaData = aem.ccsdsMetaData;
   commentsCurrentlyAllowed = aem.commentsCurrentlyAllowed;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::~CCSDSAEMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAEMDataFile.
 *
 * @return clone of the ProcessCCSDSAEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAEMObType::Clone() const
{
   GmatBase *clone = new CCSDSAEMObType(*this);
   return (clone);
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
const std::string* CCSDSAEMObType::GetTimeSystems() const
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
std::string CCSDSAEMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAEMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return CCSDSObType::GetTimeSystemText(id);
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
Integer CCSDSAEMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAEMTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}

    }

    return CCSDSObType::GetTimeSystemID(label);

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS AEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
{

    if (myAEM->commentsCurrentlyAllowed)
    {
        StringArray comments = myAEM->ccsdsAEMQuaternion->GetStringArrayDataParameter(CCSDSQuaternion::CCSDS_QUATERNION_COMMENTS_ID);
        unsigned int i;
        for (i = 0; i < comments.size(); i++ )
        {
            output << "COMMENT " << comments[i];
            output << std::endl;
        }
        if (i > 0) output << std::endl;
        output << "DATA_START" << std::endl;

    }

    if (myAEM->ccsdsAEMQuaternion != NULL);
        output << myAEM->ccsdsAEMQuaternion;

    if (myAEM->ccsdsAEMEulerAngle != NULL)
        output << myAEM->ccsdsAEMEulerAngle;

    if (myAEM->ccsdsAEMSpinStabilized != NULL)
        output << myAEM->ccsdsAEMSpinStabilized;

    return output;
}