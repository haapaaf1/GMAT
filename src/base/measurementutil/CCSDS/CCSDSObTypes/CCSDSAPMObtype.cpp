//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAPMObType
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
 * This class specifies the CCSDS Attitude Parameter observation data type.
 *
 */
//------------------------------------------------------------------------------

#include "CCSDSAPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSAPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType() : CCSDSObType("CCSDSAPMObType", ""),
	ccsdsMetaData(NULL),
        ccsdsAPMQuaternion(NULL),
        ccsdsAPMEulerAngle(NULL),
        ccsdsAPMSpinStabilized(NULL),
        ccsdsAPMSpacecraftInertia(NULL),
        ccsdsAPMAttitudeManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMObType(const CCSDSAPMObType &apm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType(const CCSDSAPMObType &apm) : CCSDSObType(apm),
	ccsdsMetaData(apm.ccsdsMetaData),
        ccsdsAPMQuaternion(apm.ccsdsAPMQuaternion),
        ccsdsAPMEulerAngle(apm.ccsdsAPMEulerAngle),
        ccsdsAPMSpinStabilized(apm.ccsdsAPMSpinStabilized),
        ccsdsAPMSpacecraftInertia(apm.ccsdsAPMSpacecraftInertia),
        ccsdsAPMAttitudeManeuvers(apm.ccsdsAPMAttitudeManeuvers)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMObType& operator=(const CCSDSAPMObType &apm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMObType& CCSDSAPMObType::operator=(const CCSDSAPMObType &apm)
{
   if (&apm == this)
      return *this;

    CCSDSObType::operator=(apm);

    ccsdsMetaData = apm.ccsdsMetaData;
    ccsdsAPMQuaternion = apm.ccsdsAPMQuaternion;
    ccsdsAPMEulerAngle = apm.ccsdsAPMEulerAngle;
    ccsdsAPMSpinStabilized = apm.ccsdsAPMSpinStabilized;
    ccsdsAPMSpacecraftInertia = apm.ccsdsAPMSpacecraftInertia;
    ccsdsAPMAttitudeManeuvers = apm.ccsdsAPMAttitudeManeuvers;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::~CCSDSAPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAPMDataFile.
 *
 * @return clone of the ProcessCCSDSAPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAPMObType::Clone() const
{
   GmatBase *clone = new CCSDSAPMObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(CCSDSAPMMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::SetMetaData(CCSDSAPMMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// CCSDSAPMMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
CCSDSAPMMetaData* CCSDSAPMObType::GetMetaData()
{
   return ccsdsMetaData;
}

//------------------------------------------------------------------------------
// void SetQuaternion(CCSDSAPMQuaternion *myQuaternion)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Quaternion variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::SetQuaternion(CCSDSAPMQuaternion *myQuaternion)
{
   ccsdsAPMQuaternion = myQuaternion;
}

//------------------------------------------------------------------------------
// CCSDSAPMQuaternion* GetQuaternion()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Quaternion variable construct
 *
 * @return The pointer to the CCSDS Quaternion
 *
 */
//------------------------------------------------------------------------------
CCSDSAPMQuaternion* CCSDSAPMObType::GetQuaternion()
{
   return ccsdsAPMQuaternion;
}

//------------------------------------------------------------------------------
// void SetEulerAngle(CCSDSAPMEulerAngle *myEulerAngle)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Euler Angle variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::SetEulerAngle(CCSDSAPMEulerAngle *myEulerAngle)
{
   ccsdsAPMEulerAngle = myEulerAngle;
}

//------------------------------------------------------------------------------
// CCSDSAPMEulerAngle* GetEulerAngle()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Euler Angle variable construct
 *
 * @return The pointer to the CCSDS Euler Angle
 *
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle* CCSDSAPMObType::GetEulerAngle()
{
   return ccsdsAPMEulerAngle;
}

//------------------------------------------------------------------------------
// void SetSpinStabilized(CCSDSAPMSpinStabilized *mySpinStabilized)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Spin Stabilized variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::SetSpinStabilized(CCSDSAPMSpinStabilized *mySpinStabilized)
{
   ccsdsAPMSpinStabilized = mySpinStabilized;
}

//------------------------------------------------------------------------------
// CCSDSAPMSpinStabilized* GetSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Spin Stabilized variable construct
 *
 * @return The pointer to the CCSDS Spin Stabilized
 *
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized* CCSDSAPMObType::GetSpinStabilized()
{
   return ccsdsAPMSpinStabilized;
}

//------------------------------------------------------------------------------
// void SetSpacecraftInertia(CCSDSSpacecraftInertia *mySpacecraftInertia)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Spacecraft Inertia variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::SetSpacecraftInertia(CCSDSSpacecraftInertia *mySpacecraftInertia)
{
   ccsdsAPMSpacecraftInertia =mySpacecraftInertia;
}

//------------------------------------------------------------------------------
// CCSDSSpacecraftInertia* GetSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Spacecraft Inertia variable construct
 *
 * @return The pointer to the CCSDS Spacecraft Inertia
 *
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia* CCSDSAPMObType::GetSpacecraftInertia()
{
   return ccsdsAPMSpacecraftInertia;
}

//------------------------------------------------------------------------------
// void AddAttitudeManeuver(CCSDSManeuver *myAttitudeManeuver)
//------------------------------------------------------------------------------
/**
 * Add an attitude maneuver to the vector container of attitude maneuvers
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::AddAttitudeManeuver(CCSDSAttitudeManeuver *myAttitudeManeuver)
{
   ccsdsAPMAttitudeManeuvers.push_back(myAttitudeManeuver);
}

//------------------------------------------------------------------------------
// CCSDSAttitudeManeuver* GetCurrentAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the current CCSDS attitude maneuver variable construct
 *
 * @return The pointer to the CCSDS attitude maneuver
 *
 */
//------------------------------------------------------------------------------
CCSDSAttitudeManeuver* CCSDSAPMObType::GetCurrentAttitudeManeuver()
{
   return (*i_ccsdsAPMAttitudeManeuvers);
}

//------------------------------------------------------------------------------
// void AdvanceToNextAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the next CCSDS attitude
 * maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::AdvanceToNextAttitudeManeuver()
{
   i_ccsdsAPMAttitudeManeuvers++;
}

//------------------------------------------------------------------------------
// void BackupToPreviousAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the previous CCSDS attitude
 * maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
void CCSDSAPMObType::BackupToPreviousAttitudeManeuver()
{
   i_ccsdsAPMAttitudeManeuvers--;
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
const std::string* CCSDSAPMObType::GetTimeSystems() const
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
std::string CCSDSAPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAPMTimeReps))
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
Integer CCSDSAPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAPMTimeReps; i++)
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
bool CCSDSAPMObType::Validate() const
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

    if (ccsdsAPMQuaternion != NULL)
        if (!ccsdsAPMQuaternion->Validate())
            return false;

    if (ccsdsAPMEulerAngle != NULL)
        if (!ccsdsAPMEulerAngle->Validate())
            return false;

    if (ccsdsAPMSpinStabilized != NULL)
        if (!ccsdsAPMSpinStabilized->Validate())
            return false;

    if (ccsdsAPMSpacecraftInertia != NULL)
        if (!ccsdsAPMSpacecraftInertia->Validate())
            return false;

    for (std::vector<CCSDSAttitudeManeuver*>::const_iterator
        j = ccsdsAPMAttitudeManeuvers.begin();
        j != ccsdsAPMAttitudeManeuvers.end(); ++j)
    {
        if ((*j) != NULL)
            if (!(*j)->Validate())
                return false;
    }


    return true;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
{
    if (myAPM->ccsdsAPMQuaternion != NULL);
        output << myAPM->ccsdsAPMQuaternion << std::endl;

    if (myAPM->ccsdsAPMEulerAngle != NULL)
        output << myAPM->ccsdsAPMEulerAngle << std::endl;

    if (myAPM->ccsdsAPMSpinStabilized != NULL)
        output << myAPM->ccsdsAPMSpinStabilized << std::endl;

    if (myAPM->ccsdsAPMSpacecraftInertia != NULL)
        output << myAPM->ccsdsAPMSpacecraftInertia << std::endl;

    for (std::vector<CCSDSAttitudeManeuver*>::const_iterator 
         j = myAPM->ccsdsAPMAttitudeManeuvers.begin();
         j != myAPM->ccsdsAPMAttitudeManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j) << std::endl;
    }
    return output;
}
