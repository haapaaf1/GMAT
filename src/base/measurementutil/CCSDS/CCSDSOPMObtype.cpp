#include "CCSDSOPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSOPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType() : CCSDSObType("CCSDSOPMObType", ""),
	ccsdsMetaData(NULL),
        ccsdsOPMStateVector(NULL),
        ccsdsOPMKeplerianElements(NULL),
        ccsdsOPMSpacecraftParameters(NULL),
        ccsdsOPMManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMObType(const CCSDSOPMObType &opm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType(const CCSDSOPMObType &opm) : CCSDSObType(opm),
	ccsdsMetaData(opm.ccsdsMetaData),
        ccsdsOPMStateVector(opm.ccsdsOPMStateVector),
        ccsdsOPMKeplerianElements(opm.ccsdsOPMKeplerianElements),
        ccsdsOPMSpacecraftParameters(opm.ccsdsOPMSpacecraftParameters),
        ccsdsOPMManeuvers(opm.ccsdsOPMManeuvers)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMObType& operator=(const CCSDSOPMObType &opm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <OPM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMObType& CCSDSOPMObType::operator=(const CCSDSOPMObType &opm)
{
    if (&opm == this)
        return *this;

    CCSDSObType::operator=(opm);

    ccsdsMetaData = opm.ccsdsMetaData;
    ccsdsOPMStateVector = opm.ccsdsOPMStateVector;
    ccsdsOPMKeplerianElements = opm.ccsdsOPMKeplerianElements;
    ccsdsOPMSpacecraftParameters = opm.ccsdsOPMSpacecraftParameters;
    ccsdsOPMManeuvers = opm.ccsdsOPMManeuvers;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::~CCSDSOPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOPMDataFile.
 *
 * @return clone of the ProcessCCSDSOPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOPMObType::Clone() const
{
   GmatBase *clone = new CCSDSOPMObType(*this);
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
const std::string* CCSDSOPMObType::GetTimeSystems() const
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
std::string CCSDSOPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSOPMTimeReps))
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
Integer CCSDSOPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSOPMTimeReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
{
    if(myOPM->ccsdsOPMStateVector != NULL)
        output << myOPM->ccsdsOPMStateVector;
    else
        return output;

    if(myOPM->ccsdsOPMKeplerianElements != NULL)
        output << myOPM->ccsdsOPMKeplerianElements;

    if(myOPM->ccsdsOPMSpacecraftParameters != NULL)
        output << myOPM->ccsdsOPMSpacecraftParameters;

    for (std::vector<CCSDSManeuver*>::const_iterator
         j=myOPM->ccsdsOPMManeuvers.begin();
         j!=myOPM->ccsdsOPMManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j);
    }

    return output;
}

