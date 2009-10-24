#include "CCSDSHeader.hpp"

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSHeader::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps] =
{
    "Quaternion",
    "Euler Angle",
    "Spin Stabilized",
    "State Vector",
    "Keplerian Elements",
    "Spacecraft Parameters",
    "Spacecraft Inertia",
    "Maneuver",
    "Attitude Maneuver",
    "Generic Data Type"
};

const std::string CCSDSHeader::CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[EndCCSDSHeaderDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "Header Comments"
};

const std::string CCSDSHeader::CCSDS_HEADER_KEYWORDS[EndCCSDSHeaderDataReps] =
{
    "CCSDS_VERSION",
    "CREATION_DATE",
    "ORIGINATOR",
    "COMMENT"
};

const bool CCSDSHeader::CCSDS_HEADER_IS_REQUIRED[EndCCSDSHeaderDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSHeader::CCSDS_HEADER_PARAMETER_TYPE[EndCCSDSHeaderDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSHeader::CCSDS_HEADER_UNIT_DESCRIPTIONS[EndCCSDSHeaderDataReps] =
{
    "",
    "",
    "",
    ""
};

//------------------------------------------------------------------------------
//  CCSDSHeader()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSHeader class
 */
//------------------------------------------------------------------------------
CCSDSHeader::CCSDSHeader() : CCSDSObType(),
    fileType(std::string("")),
    ccsdsVersion(std::string("")),
    creationDate(std::string("")),
    originator(std::string("")),
    comments(),
    dataType(0)

{
}

//------------------------------------------------------------------------------
//  CCSDSHeader(const CCSDSHeader &header)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSHeader::CCSDSHeader(const CCSDSHeader &header) : CCSDSObType(header).
    fileType(header.fileType),
    ccsdsVersion(header.ccsdsVersion),
    creationDate(header.creationDate),
    originator(header.originator),
    comments(header.comments),
    dataType(header.dataType)
{
}

//---------------------------------------------------------------------------
//  CCSDSHeader& operator=(const CCSDSHeader &header)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <header> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header)

{
    if (&header == this)
        return *this;

    CCSDSObType::operator=(header);

    fileType = header.fileType;
    ccsdsVersion = header.ccsdsVersion;
    creationDate = header.creationDate;
    originator = header.originator;
    comments = header.comments;
    dataType = header.dataType;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSHeader()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSHeader class
 */
//------------------------------------------------------------------------------
CCSDSHeader::~CCSDSHeader()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSHeader.
 *
 * @return clone of the CCSDSHeader.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSHeader::Clone() const
{
   GmatBase *clone = new CCSDSHeader(*this);
   return (clone);
}

