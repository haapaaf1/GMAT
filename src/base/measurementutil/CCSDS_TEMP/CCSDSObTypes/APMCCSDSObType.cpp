//$Header$
//------------------------------------------------------------------------------
//                             APMCCSDSObType
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

#include "APMCCSDSObType.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string APMCCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  APMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMCCSDSObType::APMCCSDSObType() : CCSDSObType("APMCCSDSObType", ""),
	ccsdsMetaData(NULL),
        ccsdsAPMQuaternion(NULL),
        ccsdsAPMEulerAngle(NULL),
        ccsdsAPMSpinStabilized(NULL),
        ccsdsAPMSpacecraftInertia(NULL),
        ccsdsAPMAttitudeManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  APMCCSDSObType(const APMCCSDSObType &apm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMCCSDSObType::APMCCSDSObType(const APMCCSDSObType &apm) : CCSDSObType(apm),
	ccsdsMetaData(apm.ccsdsMetaData),
        ccsdsAPMQuaternion(apm.ccsdsAPMQuaternion),
        ccsdsAPMEulerAngle(apm.ccsdsAPMEulerAngle),
        ccsdsAPMSpinStabilized(apm.ccsdsAPMSpinStabilized),
        ccsdsAPMSpacecraftInertia(apm.ccsdsAPMSpacecraftInertia),
        ccsdsAPMAttitudeManeuvers(apm.ccsdsAPMAttitudeManeuvers)
{
}

//---------------------------------------------------------------------------
//  APMCCSDSObType& operator=(const APMCCSDSObType &apm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const APMCCSDSObType& APMCCSDSObType::operator=(const APMCCSDSObType &apm)
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
//  ~APMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
APMCCSDSObType::~APMCCSDSObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSAPMDataFile.
 *
 * @return clone of the CCSDSAPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* APMCCSDSObType::Clone() const
{
   GmatBase *clone = new APMCCSDSObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(APMCCSDSMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void APMCCSDSObType::SetMetaData(APMCCSDSMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// APMCCSDSMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
APMCCSDSMetaData* APMCCSDSObType::GetMetaData()
{
   return ccsdsMetaData;
}

//------------------------------------------------------------------------------
// void SetQuaternion(APMQuaternionCCSDSData *myQuaternion)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Quaternion variable construct.
 *
 */
//------------------------------------------------------------------------------
void APMCCSDSObType::SetQuaternion(APMQuaternionCCSDSData *myQuaternion)
{
   ccsdsAPMQuaternion = myQuaternion;
}

//------------------------------------------------------------------------------
// APMQuaternionCCSDSData* GetQuaternion()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Quaternion variable construct
 *
 * @return The pointer to the CCSDS Quaternion
 *
 */
//------------------------------------------------------------------------------
APMQuaternionCCSDSData* APMCCSDSObType::GetQuaternion()
{
   return ccsdsAPMQuaternion;
}

//------------------------------------------------------------------------------
// void SetEulerAngle(APMEulerAngleCCSDSData *myEulerAngle)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Euler Angle variable construct.
 *
 */
//------------------------------------------------------------------------------
void APMCCSDSObType::SetEulerAngle(APMEulerAngleCCSDSData *myEulerAngle)
{
   ccsdsAPMEulerAngle = myEulerAngle;
}

//------------------------------------------------------------------------------
// APMEulerAngleCCSDSData* GetEulerAngle()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Euler Angle variable construct
 *
 * @return The pointer to the CCSDS Euler Angle
 *
 */
//------------------------------------------------------------------------------
APMEulerAngleCCSDSData* APMCCSDSObType::GetEulerAngle()
{
   return ccsdsAPMEulerAngle;
}

//------------------------------------------------------------------------------
// void SetSpinStabilized(APMSpinStabilizedCCSDSData *mySpinStabilized)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Spin Stabilized variable construct.
 *
 */
//------------------------------------------------------------------------------
void APMCCSDSObType::SetSpinStabilized(APMSpinStabilizedCCSDSData *mySpinStabilized)
{
   ccsdsAPMSpinStabilized = mySpinStabilized;
}

//------------------------------------------------------------------------------
// APMSpinStabilizedCCSDSData* GetSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Spin Stabilized variable construct
 *
 * @return The pointer to the CCSDS Spin Stabilized
 *
 */
//------------------------------------------------------------------------------
APMSpinStabilizedCCSDSData* APMCCSDSObType::GetSpinStabilized()
{
   return ccsdsAPMSpinStabilized;
}

//------------------------------------------------------------------------------
// void SetSpacecraftInertia(SpacecraftInertiaCCSDSData *mySpacecraftInertia)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS Spacecraft Inertia variable construct.
 *
 */
//------------------------------------------------------------------------------
void APMCCSDSObType::SetSpacecraftInertia(SpacecraftInertiaCCSDSData *mySpacecraftInertia)
{
   ccsdsAPMSpacecraftInertia =mySpacecraftInertia;
}

//------------------------------------------------------------------------------
// SpacecraftInertiaCCSDSData* GetSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS Spacecraft Inertia variable construct
 *
 * @return The pointer to the CCSDS Spacecraft Inertia
 *
 */
//------------------------------------------------------------------------------
SpacecraftInertiaCCSDSData* APMCCSDSObType::GetSpacecraftInertia()
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
void APMCCSDSObType::AddAttitudeManeuver(AttitudeManeuverCCSDSData *myAttitudeManeuver)
{
   ccsdsAPMAttitudeManeuvers.push_back(myAttitudeManeuver);
}

//------------------------------------------------------------------------------
// AttitudeManeuverCCSDSData* GetCurrentAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the current CCSDS attitude maneuver variable construct
 *
 * @return The pointer to the CCSDS attitude maneuver
 *
 */
//------------------------------------------------------------------------------
AttitudeManeuverCCSDSData* APMCCSDSObType::GetCurrentAttitudeManeuver()
{
   return (*i_ccsdsAPMAttitudeManeuvers);
}

//------------------------------------------------------------------------------
// bool AdvanceToNextAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the next CCSDS attitude
 * maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
bool APMCCSDSObType::AdvanceToNextAttitudeManeuver()
{
   i_ccsdsAPMAttitudeManeuvers++;

   if (i_ccsdsAPMAttitudeManeuvers == ccsdsAPMAttitudeManeuvers.end())
       return false;
   else
       return true;

}

//------------------------------------------------------------------------------
// bool BackupToPreviousAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Moves the iterator pointer to the previous CCSDS attitude
 * maneuver variable construct
 *
 */
//------------------------------------------------------------------------------
bool APMCCSDSObType::BackupToPreviousAttitudeManeuver()
{
   i_ccsdsAPMAttitudeManeuvers--;

   if (i_ccsdsAPMAttitudeManeuvers == ccsdsAPMAttitudeManeuvers.begin())
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
const std::string* APMCCSDSObType::GetTimeSystems() const
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
std::string APMCCSDSObType::GetTimeSystemText(const Integer &id) const
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
Integer APMCCSDSObType::GetTimeSystemID(const std::string &label)
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
bool APMCCSDSObType::Validate() const
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

    for (std::vector<AttitudeManeuverCCSDSData*>::const_iterator
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
// std::ostream& operator<< (std::ostream &output, const APMCCSDSObType *myAPM)
//------------------------------------------------------------------------------
/**
 * Formats CAPMCCSDSObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const APMCCSDSObType *myAPM)
{
    if (myAPM->ccsdsAPMQuaternion != NULL);
        output << myAPM->ccsdsAPMQuaternion << std::endl;

    if (myAPM->ccsdsAPMEulerAngle != NULL)
        output << myAPM->ccsdsAPMEulerAngle << std::endl;

    if (myAPM->ccsdsAPMSpinStabilized != NULL)
        output << myAPM->ccsdsAPMSpinStabilized << std::endl;

    if (myAPM->ccsdsAPMSpacecraftInertia != NULL)
        output << myAPM->ccsdsAPMSpacecraftInertia << std::endl;

    for (std::vector<AttitudeManeuverCCSDSData*>::const_iterator
         j = myAPM->ccsdsAPMAttitudeManeuvers.begin();
         j != myAPM->ccsdsAPMAttitudeManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j) << std::endl;
    }
    return output;
}
