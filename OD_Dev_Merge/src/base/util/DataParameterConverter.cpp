#include "DataParameterConverter.hpp"

//---------------------------------------------------------------------------
//  bool TimeConverterUtil::Convert(const &Real origValue,
//                                  const std::string &fromType,
//                                  const std::string &toType
//				    Real &finalValue)
//---------------------------------------------------------------------------
/**
 * Assignment operator for TimeConverter structures.
 *
 * @param <origValue> Given data parameter
 * @param <fromType>  Original data parameter type keyword
 * @param <toType>    Desired data parameter type keyword
 * @param <finalValue> Converted data parameter
 *
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool DataParameterConverter::Convert(Real &origValue,
                                const Integer fromType,
                                const Integer toType,
				Real &finalValue)
{   

    if (fromType == toType)
    {
	finalValue = origValue;
	return true;
    }

    if (fromType == ObType::TWOWAYTIMEOFFLIGHT_ID && toType == ObType::RANGE_ID)
    {
	// @TODO: Light time iteration here???
	// @TODO: Check for consistent units!!!
	// Speed of light is in meters/second so convert to kilometers
	finalValue = 0.5*GmatPhysicalConst::c*origValue*0.001;
	return true;
    }
    
    if (fromType == ObType::RANGE_ID && toType == ObType::TWOWAYTIMEOFFLIGHT_ID)
    {
	// @TODO: Light time iteration here???
	// @TODO: Check for consistent units!!!
	// Speed of light is in meters/second so convert to kilometers
	finalValue = 2.0*origValue*1000/GmatPhysicalConst::c;
	return true;
    }
    
    return false;
}

//---------------------------------------------------------------------------
//  bool TimeConverterUtil::Convert(const &Real origValue,
//                                  const std::string &fromType,
//                                  const std::string &toType
//				    Real &finalValue)
//---------------------------------------------------------------------------
/**
 * Assignment operator for TimeConverter structures.
 *
 * @param <origValue> Given data array
 * @param <fromType>  Original data parameter type keyword
 * @param <toType>    Desired data parameter type keyword
 * @param <finalValue> Converted data array
 *
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool DataParameterConverter::Convert(Real *origValue,
                                const Integer fromType,
                                const Integer toType,
				Real *finalValue)
{   

    if (fromType == toType)
    {
	finalValue = origValue;
	return true;
    }
    
    return false;
}