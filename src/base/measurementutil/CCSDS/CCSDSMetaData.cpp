#include "CCSDSMetaData.hpp"

//------------------------------------------------------------------------------
//  CCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::CCSDSMetaData()
{
}

//------------------------------------------------------------------------------
//  CCSDSMetaData(const CCSDSMetaData &md)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::CCSDSMetaData(const CCSDSMetaData &md)
{
}

//---------------------------------------------------------------------------
//  CCSDSMetaData& operator=(const CCSDSMetaData &md)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSMetaData structures.
 *
 * @param <aemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSMetaData& CCSDSMetaData::operator=(const CCSDSMetaData &md)
{
    if (&md == this)
        return *this;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::~CCSDSMetaData()
{
}

//------------------------------------------------------------------------------
//  bool GetBooleanDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
bool CCSDSMetaData::GetBooleanDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
//  bool GetBooleanDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
bool CCSDSMetaData::GetBooleanDataParameter(const std::string &label) const
{
   return GetBooleanDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Real CCSDSMetaData::GetRealDataParameter(const Integer id) const
{
    return GmatBase::REAL_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSMetaData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSMetaData::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSMetaData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSMetaData::GetStringDataParameter(const Integer id) const
{
    return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSMetaData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
{
    return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

