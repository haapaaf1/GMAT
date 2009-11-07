#include "CCSDSOEMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOEMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB",
    "GMST",
    "MET",
    "MRT",
    "SCLK",
    "UT1"
};

//------------------------------------------------------------------------------
//  CCSDSOEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::CCSDSOEMObType() : CCSDSObType("CCSDSOEMObType", ""),
	ccsdsMetaData(NULL),
        ccsdsOEMStateVector(NULL),
        commentsCurrentlyAllowed(false)
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
	ccsdsMetaData(oem.ccsdsMetaData),
        ccsdsOEMStateVector(oem.ccsdsOEMStateVector),
        commentsCurrentlyAllowed(oem.commentsCurrentlyAllowed)
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

   CCSDSObType::operator=(oem);
   ccsdsMetaData = oem.ccsdsMetaData;
   ccsdsOEMStateVector = oem.ccsdsOEMStateVector;
   commentsCurrentlyAllowed = oem.commentsCurrentlyAllowed;

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
const std::string* CCSDSOEMObType::GetTimeSystems() const
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
std::string CCSDSOEMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSOEMTimeReps))
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
Integer CCSDSOEMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSOEMTimeReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSOEMObType *myOEM)
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
std::ostream& operator<< (std::ostream &output, const CCSDSOEMObType *myOEM)
{

    if (myOEM->commentsCurrentlyAllowed)
    {
        StringArray comments = myOEM->ccsdsOEMStateVector->GetStringArrayDataParameter(CCSDSStateVector::CCSDS_STATEVECTOR_COMMENTS_ID);
        unsigned int i;
        for (i = 0; i < comments.size(); i++ )
        {
            output << "COMMENT " << comments[i];
            output << std::endl;
        }
        if (i > 0) output << std::endl;
    }

    if(myOEM->ccsdsOEMStateVector != NULL)
        output << myOEM->ccsdsOEMStateVector;

    return output;
}

