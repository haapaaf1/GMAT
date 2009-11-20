//$Header$
//------------------------------------------------------------------------------
//                             CCSDSTDMObType
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

#include "CCSDSTDMObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTDMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "GPS",
    "SCLK"
};

//------------------------------------------------------------------------------
//  CCSDSTDMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::CCSDSTDMObType() : CCSDSObType("CCSDSTDMObType", ""),
	ccsdsMetaData(NULL),
        ccsdsTrackingData(NULL),
        commentsCurrentlyAllowed(false)
{
}

//------------------------------------------------------------------------------
//  CCSDSTDMObType(const CCSDSTDMObType &tdm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::CCSDSTDMObType(const CCSDSTDMObType &tdm) : CCSDSObType(tdm),
	ccsdsMetaData(tdm.ccsdsMetaData),
        ccsdsTrackingData(tdm.ccsdsTrackingData),
        commentsCurrentlyAllowed(tdm.commentsCurrentlyAllowed)
{
}

//---------------------------------------------------------------------------
//  CCSDSTDMObType& operator=(const CCSDSTDMObType &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tdm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSTDMObType& CCSDSTDMObType::operator=(const CCSDSTDMObType &tdm)
{
   if (&tdm == this)
      return *this;

   CCSDSTDMObType::operator=(tdm);

   ccsdsMetaData = tdm.ccsdsMetaData;
   ccsdsTrackingData = tdm.ccsdsTrackingData;
   commentsCurrentlyAllowed = tdm.commentsCurrentlyAllowed;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSTDMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::~CCSDSTDMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSTDMDataFile.
 *
 * @return clone of the ProcessCCSDSTDMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSTDMObType::Clone() const
{
   GmatBase *clone = new CCSDSTDMObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// void SetMetaData(CCSDSTDMMetaData *myCCSDSMetaData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS MetaData variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSTDMObType::SetMetaData(CCSDSTDMMetaData *myCCSDSMetaData)
{
   ccsdsMetaData = myCCSDSMetaData;
}

//------------------------------------------------------------------------------
// CCSDSTDMMetaData* GetMetaData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS MetaData variable construct
 *
 * @return The pointer to the CCSDS MetaData
 *
 */
//------------------------------------------------------------------------------
CCSDSTDMMetaData* CCSDSTDMObType::GetMetaData()
{
   return ccsdsMetaData;
}

//------------------------------------------------------------------------------
// void SetTrackingData(CCSDSTrackingData *myCCSDSTrackingData)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the CCSDS TrackingData variable construct.
 *
 */
//------------------------------------------------------------------------------
void CCSDSTDMObType::SetTrackingData(CCSDSTrackingData *myCCSDSTrackingData)
{
   ccsdsTrackingData = myCCSDSTrackingData;
}

//------------------------------------------------------------------------------
// CCSDSTrackingData* GetTrackingData()
//------------------------------------------------------------------------------
/**
 * Gets the pointer to the CCSDS TrackingData variable construct
 *
 * @return The pointer to the CCSDS TrackingData
 *
 */
//------------------------------------------------------------------------------
CCSDSTrackingData* CCSDSTDMObType::GetTrackingData()
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
const std::string* CCSDSTDMObType::GetTimeSystems() const
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
std::string CCSDSTDMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSTDMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return CCSDSTDMObType::GetTimeSystemText(id);
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
Integer CCSDSTDMObType::GetTimeSystemID(const std::string &label)
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
      
    return CCSDSTDMObType::GetTimeSystemID(label);
 
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
bool CCSDSTDMObType::Validate() const
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
// std::ostream& operator<< (std::ostream &output, const CCSDSTDMObType *myTDM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSTDMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS TDM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSTDMObType *myTDM)
{
    if (myTDM->ccsdsTrackingData != NULL)
    {
        if (myTDM->commentsCurrentlyAllowed)
        {
            output << "DATA_START" << std::endl;
            StringArray comments = myTDM->ccsdsTrackingData->GetStringArrayDataParameter(CCSDSTrackingData::CCSDS_TRACKINGDATA_COMMENTS_ID);

            for (unsigned int i = 0; i < comments.size(); i++ )
            {
                output << "COMMENT " << comments[i] << std::endl;
            }
        }

        output << myTDM->ccsdsTrackingData;
    }
    return output;
}
