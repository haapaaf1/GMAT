#include "CCSDSObType.hpp";
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

//------------------------------------------------------------------------------
//  CCSDSObType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const std::string &type, const std::string &name) :
   ObType(type, name),
    ccsdsHeader(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSObType(const CCSDSObType &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const CCSDSObType &ob) : ObType(ob),
    ccsdsHeader(ob.ccsdsHeader)
{
}

//---------------------------------------------------------------------------
//  CCSDSObType& operator=(const CCSDSObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSObType& CCSDSObType::operator=(const CCSDSObType &ob)
{
   if (&ob == this)
      return *this;

   ObType::operator=(ob);

   ccsdsHeader = ob.ccsdsHeader;
   
   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::~CCSDSObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSObType.
 *
 * @return clone of the CCSDSObType.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSObType::Clone() const
{
   GmatBase *clone = new CCSDSObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

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
const std::string* CCSDSObType::GetTimeSystems() const
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
std::string CCSDSObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer CCSDSObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = 0; i < EndCCSDSTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
 
}


//------------------------------------------------------------------------------
// std::string GetCCSDSObType(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Obtains the CCSDS ObType
 *
 * @param <myOb> the CCSDS data
 *
 * @return The CCSDS ObType (eg. CCSDSTDM,CCSDSOPM,CCSDSOEM,CCSDSAPM,CCSDSAEM)
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetCCSDSObType()
{
    std::string regex = "^(CCSDS[A-Z]{3})ObType.*";
    std::string theObType;

    if (pcrecpp::RE(regex).FullMatch(GetTypeName(),&theObType))
        return theObType;
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
}