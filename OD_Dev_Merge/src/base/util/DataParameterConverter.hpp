/* 
 * File:   DataParameterConverter.hpp
 * Author: matthewwilkins
 *
 * Created on September 2, 2009, 7:32 AM
 */

#ifndef _DATAPARAMETERCONVERTER_HPP
#define	_DATAPARAMETERCONVERTER_HPP

#include "gmatdefs.hpp"
#include "Obtype.hpp"
#include "RealUtilities.hpp"
#include "PhysicalConstants.hpp"

class DataParameterConverter
{

public:
    
    bool Convert(Real &origValue, const Integer fromType,
                 const Integer toType, Real &finalValue);

    bool Convert(Real *origValue, const Integer fromType,
                 const Integer toType, Real *finalValue);

};

#endif	/* _DATAPARAMETERCONVERTER_HPP */

