//$Header$
//------------------------------------------------------------------------------
//                             OEMCCSDSObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/04
//
/**
 *
 * This class specifies the CCSDS Orbit Ephemeris observation data type.
 *
 */
//------------------------------------------------------------------------------

#include "OEMCCSDSObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string OEMCCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps-EndCCSDSTimeReps] =
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
//  OEMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
OEMCCSDSObType::OEMCCSDSObType() : CCSDSObType("OEMCCSDSObType", ""),
	ccsdsMetaData(NULL),
        ccsdsOEMStateVector(NULL),
        commentsCurrentlyAllowed(false)
{
}

//------------------------------------------------------------------------------
//  OEMCCSDSObType(const OEMCCSDSObType &oem)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
OEMCCSDSObType::OEMCCSDSObType(const OEMCCSDSObType &oem) : CCSDSObType(oem),
	ccsdsMetaData(oem.ccsdsMetaData),
        ccsdsOEMStateVector(oem.ccsdsOEMStateVector),
        commentsCurrentlyAllowed(oem.commentsCurrentlyAllowed)
{
}

//---------------------------------------------------------------------------
//  OEMCCSDSObType& operator=(const OEMCCSDSObType &oem)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <oem> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const OEMCCSDSObType& OEMCCSDSObType::operator=(const OEMCCSDSObType &oem)
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
//  ~OEMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
OEMCCSDSObType::~OEMCCSDSObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the OEMCCSDSDataFile.
 *
 * @return clone of the OEMCCSDSDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* OEMCCSDSObType::Clone() const
{
   GmatBase *clone = new OEMCCSDSObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(OEMCCSDSMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void OEMCCSDSObType::SetMetaData(OEMCCSDSMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// OEMCCSDSMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
OEMCCSDSMetaData* OEMCCSDSObType::GetMetaData()
{
   return ccsdsMetaData;
}

//------------------------------------------------------------------------------
// void SetStateVector(OEMStateVectorCCSDSData *myOEMStateVector)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS state vector variable construct.
 *
 */
//------------------------------------------------------------------------------
void OEMCCSDSObType::SetStateVector(OEMStateVectorCCSDSData *myOEMStateVector)
{
   ccsdsOEMStateVector = myOEMStateVector;
}

//------------------------------------------------------------------------------
// OEMStateVectorCCSDSData* GetStateVector()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS state vector variable construct
 *
 * @return The pointer to the CCSDS state vector
 *
 */
//------------------------------------------------------------------------------
OEMStateVectorCCSDSData* OEMCCSDSObType::GetStateVector()
{
   return ccsdsOEMStateVector;
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
const std::string* OEMCCSDSObType::GetTimeSystems() const
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
std::string OEMCCSDSObType::GetTimeSystemText(const Integer &id) const
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
Integer OEMCCSDSObType::GetTimeSystemID(const std::string &label)
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

//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool OEMCCSDSObType::Validate() const
{

    if (ccsdsHeader != NULL)
    {
        if (!ccsdsHeader->Validate())
            return false;
    }
    else
        return false;

    if (ccsdsMetaData != NULL)
    {
        if (!ccsdsMetaData->Validate())
            return false;
    }
    else
        return false;

    if (ccsdsOEMStateVector != NULL)
        if (!ccsdsOEMStateVector->Validate())
            return false;

    return true;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const OEMCCSDSObType *myOEM)
//------------------------------------------------------------------------------
/**
 * Formats COEMCCSDSObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const OEMCCSDSObType *myOEM)
{

    if (myOEM->commentsCurrentlyAllowed)
    {
        StringArray comments = myOEM->ccsdsOEMStateVector->GetStringArrayDataParameter(StateVectorCCSDSData::CCSDS_STATEVECTOR_COMMENTS_ID);
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

