//$Header$
//------------------------------------------------------------------------------
//                             OPMCCSDSObType
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
 * This class specifies the CCSDS Orbit Parameter observation data type.
 *
 */
//------------------------------------------------------------------------------

#include "OPMCCSDSObType.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string OPMCCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  OPMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
OPMCCSDSObType::OPMCCSDSObType() : CCSDSObType("OPMCCSDSObType", ""),
	ccsdsMetaData(NULL),
        ccsdsOPMStateVector(NULL),
        ccsdsOPMKeplerianElements(NULL),
        ccsdsOPMSpacecraftParameters(NULL),
        ccsdsOPMManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  OPMCCSDSObType(const OPMCCSDSObType &opm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
OPMCCSDSObType::OPMCCSDSObType(const OPMCCSDSObType &opm) : CCSDSObType(opm),
	ccsdsMetaData(opm.ccsdsMetaData),
        ccsdsOPMStateVector(opm.ccsdsOPMStateVector),
        ccsdsOPMKeplerianElements(opm.ccsdsOPMKeplerianElements),
        ccsdsOPMSpacecraftParameters(opm.ccsdsOPMSpacecraftParameters),
        ccsdsOPMManeuvers(opm.ccsdsOPMManeuvers)
{
}

//---------------------------------------------------------------------------
//  OPMCCSDSObType& operator=(const OPMCCSDSObType &opm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <OPM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const OPMCCSDSObType& OPMCCSDSObType::operator=(const OPMCCSDSObType &opm)
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
//  ~OPMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
OPMCCSDSObType::~OPMCCSDSObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSOPMDataFile.
 *
 * @return clone of the CCSDSOPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* OPMCCSDSObType::Clone() const
{
   GmatBase *clone = new OPMCCSDSObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(OPMCCSDSMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void OPMCCSDSObType::SetMetaData(OPMCCSDSMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// OPMCCSDSMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
OPMCCSDSMetaData* OPMCCSDSObType::GetMetaData()
{
   return ccsdsMetaData;
}
    
//------------------------------------------------------------------------------
// void SetStateVector(OPMStateVectorCCSDSData *myOPMStateVector)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS state vector variable construct.
 *
 */
//------------------------------------------------------------------------------
void OPMCCSDSObType::SetStateVector(OPMStateVectorCCSDSData *myOPMStateVector)
{
   ccsdsOPMStateVector = myOPMStateVector;
}

//------------------------------------------------------------------------------
// OPMStateVectorCCSDSData* GetStateVector()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS state vector variable construct
 *
 * @return The pointer to the CCSDS state vector
 *
 */
//------------------------------------------------------------------------------
OPMStateVectorCCSDSData* OPMCCSDSObType::GetStateVector()
{
   return ccsdsOPMStateVector;
}

//------------------------------------------------------------------------------
// void SetKeplerianElements(KeplerianElementsCCSDSData *myOPMKeplerianElements)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Keplerian Element variable construct.
 *
 */
//------------------------------------------------------------------------------
void OPMCCSDSObType::SetKeplerianElements(KeplerianElementsCCSDSData *myOPMKeplerianElements)
{
   ccsdsOPMKeplerianElements = myOPMKeplerianElements;
}

//------------------------------------------------------------------------------
// KeplerianElementsCCSDSData* GetKeplerianElements()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Keplerian Element variable construct
 *
 * @return The pointer to the CCSDS Keplerian Element
 *
 */
//------------------------------------------------------------------------------
KeplerianElementsCCSDSData* OPMCCSDSObType::GetKeplerianElements()
{
   return ccsdsOPMKeplerianElements;
}

//------------------------------------------------------------------------------
// void SetSpacecraftParameters(SpacecraftParametersCCSDSData *mySpacecraftParameters)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void OPMCCSDSObType::SetSpacecraftParameters(SpacecraftParametersCCSDSData *mySpacecraftParameters)
{
   ccsdsOPMSpacecraftParameters = mySpacecraftParameters;
}

//------------------------------------------------------------------------------
// SpacecraftParametersCCSDSData* GetSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
SpacecraftParametersCCSDSData* OPMCCSDSObType::GetSpacecraftParameters()
{
   return ccsdsOPMSpacecraftParameters;
}

//------------------------------------------------------------------------------
// void AddManeuver(ManeuverCCSDSData *myManeuver)
//------------------------------------------------------------------------------
/**
 * Add a maneuver to the vector container of maneuvers
 *
 */
//------------------------------------------------------------------------------
void OPMCCSDSObType::AddManeuver(ManeuverCCSDSData *myManeuver)
{
   ccsdsOPMManeuvers.push_back(myManeuver);
}

//------------------------------------------------------------------------------
// ManeuverCCSDSData* GetCurrentManeuver()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the current CCSDS maneuver variable construct
 *
 * @return The pointer to the CCSDS Maneuver
 *
 */
//------------------------------------------------------------------------------
ManeuverCCSDSData* OPMCCSDSObType::GetCurrentManeuver()
{
   return (*i_ccsdsOPMManeuvers);
}

//------------------------------------------------------------------------------
// bool AdvanceToNextManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the next CCSDS maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
bool OPMCCSDSObType::AdvanceToNextManeuver()
{
   i_ccsdsOPMManeuvers++;

   if (i_ccsdsOPMManeuvers == ccsdsOPMManeuvers.end())
       return false;
   else
       return true;
}

//------------------------------------------------------------------------------
// bool BackupToPreviousManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the previous CCSDS maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
bool OPMCCSDSObType::BackupToPreviousManeuver()
{
   i_ccsdsOPMManeuvers--;

   if (i_ccsdsOPMManeuvers == ccsdsOPMManeuvers.begin())
       return false;
   else
       return true;

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
const std::string* OPMCCSDSObType::GetTimeSystems() const
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
std::string OPMCCSDSObType::GetTimeSystemText(const Integer &id) const
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
Integer OPMCCSDSObType::GetTimeSystemID(const std::string &label)
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


//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool OPMCCSDSObType::Validate() const
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

    if (ccsdsOPMStateVector != NULL)
        if (!ccsdsOPMStateVector->Validate())
            return false;

    if (ccsdsOPMKeplerianElements != NULL)
        if (!ccsdsOPMKeplerianElements->Validate())
            return false;

    if (ccsdsOPMSpacecraftParameters != NULL)
        if (!ccsdsOPMSpacecraftParameters->Validate())
            return false;

    for (std::vector<ManeuverCCSDSData*>::const_iterator
         j = ccsdsOPMManeuvers.begin();
         j != ccsdsOPMManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            if (!(*j)->Validate())
                return false;
    }

    return true;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const OPMCCSDSObType *myOPM)
//------------------------------------------------------------------------------
/**
 * Formats COPMCCSDSObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const OPMCCSDSObType *myOPM)
{
    if(myOPM->ccsdsOPMStateVector != NULL)
        output << myOPM->ccsdsOPMStateVector;
    else
        return output;

    if(myOPM->ccsdsOPMKeplerianElements != NULL)
        output << myOPM->ccsdsOPMKeplerianElements;

    if(myOPM->ccsdsOPMSpacecraftParameters != NULL)
        output << myOPM->ccsdsOPMSpacecraftParameters;

    for (std::vector<ManeuverCCSDSData*>::const_iterator
         j = myOPM->ccsdsOPMManeuvers.begin();
         j != myOPM->ccsdsOPMManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j);
    }

    return output;
}

