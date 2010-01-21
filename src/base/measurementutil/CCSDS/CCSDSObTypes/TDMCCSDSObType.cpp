//$Header$
//------------------------------------------------------------------------------
//                             TDMCCSDSObType
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
 * This class specifies the CCSDS Tracking Data observation data type.
 *
 */
//------------------------------------------------------------------------------

#include "TDMCCSDSObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string TDMCCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "GPS",
    "SCLK"
};

//------------------------------------------------------------------------------
//  TDMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSObType::TDMCCSDSObType() : CCSDSObType("TDMCCSDSObType", ""),
	ccsdsMetaData(NULL),
        ccsdsTrackingData(NULL),
        commentsCurrentlyAllowed(false)
{
}

//------------------------------------------------------------------------------
//  TDMCCSDSObType(const TDMCCSDSObType &tdm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSObType::TDMCCSDSObType(const TDMCCSDSObType &tdm) : CCSDSObType(tdm),
	ccsdsMetaData(tdm.ccsdsMetaData),
        ccsdsTrackingData(tdm.ccsdsTrackingData),
        commentsCurrentlyAllowed(tdm.commentsCurrentlyAllowed)
{
}

//---------------------------------------------------------------------------
//  TDMCCSDSObType& operator=(const TDMCCSDSObType &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tdm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const TDMCCSDSObType& TDMCCSDSObType::operator=(const TDMCCSDSObType &tdm)
{
   if (&tdm == this)
      return *this;

   TDMCCSDSObType::operator=(tdm);

   ccsdsMetaData = tdm.ccsdsMetaData;
   ccsdsTrackingData = tdm.ccsdsTrackingData;
   commentsCurrentlyAllowed = tdm.commentsCurrentlyAllowed;

   return *this;
}

//------------------------------------------------------------------------------
//  ~TDMCCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSObType::~TDMCCSDSObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSTDMDataFile.
 *
 * @return clone of the CCSDSTDMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* TDMCCSDSObType::Clone() const
{
   GmatBase *clone = new TDMCCSDSObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(TDMCCSDSMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void TDMCCSDSObType::SetMetaData(TDMCCSDSMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// TDMCCSDSMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
TDMCCSDSMetaData* TDMCCSDSObType::GetMetaData()
{
   return ccsdsMetaData;
}

//------------------------------------------------------------------------------
// void SetTrackingData(TrackingCCSDSData *myTrackingCCSDSData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS TrackingData variable construct.
 *
 */
//------------------------------------------------------------------------------
void TDMCCSDSObType::SetTrackingData(TrackingCCSDSData *myTrackingCCSDSData)
{
   ccsdsTrackingData = myTrackingCCSDSData;
}

//------------------------------------------------------------------------------
// TrackingCCSDSData* GetTrackingData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS TrackingData variable construct
 *
 * @return The pointer to the CCSDS TrackingData
 *
 */
//------------------------------------------------------------------------------
TrackingCCSDSData* TDMCCSDSObType::GetTrackingData()
{
   return ccsdsTrackingData;
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
const std::string* TDMCCSDSObType::GetTimeSystems() const
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
std::string TDMCCSDSObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSTDMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return TDMCCSDSObType::GetTimeSystemText(id);
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
Integer TDMCCSDSObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSTDMTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return TDMCCSDSObType::GetTimeSystemID(label);
 
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
bool TDMCCSDSObType::Validate() const
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

    if (ccsdsTrackingData != NULL)
        if (!ccsdsTrackingData->Validate())
            return false;

    return true;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const TDMCCSDSObType *myTDM)
//------------------------------------------------------------------------------
/**
 * Formats CTDMCCSDSObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS TDM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const TDMCCSDSObType *myTDM)
{
    if (myTDM->ccsdsTrackingData != NULL)
    {
        if (myTDM->commentsCurrentlyAllowed)
        {
            output << "DATA_START" << std::endl;
            StringArray comments = myTDM->ccsdsTrackingData->GetStringArrayDataParameter(TrackingCCSDSData::CCSDS_TRACKINGDATA_COMMENTS_ID);

            for (unsigned int i = 0; i < comments.size(); i++ )
            {
                output << "COMMENT " << comments[i] << std::endl;
            }
        }

        output << myTDM->ccsdsTrackingData;
    }
    return output;
}
