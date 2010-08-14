//$Id$
//------------------------------------------------------------------------------
//                               InterpolatorException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number #####
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2009/09/28
//
//------------------------------------------------------------------------------

#ifndef InterpolatorException_hpp
#define InterpolatorException_hpp

#include "BaseException.hpp"

/**
 * Exceptions thrown by the Interpolators
 */
class GMAT_API InterpolatorException : public BaseException
{
public:
   // class constructor
   InterpolatorException(std::string details = "");
   // class destructor
   ~InterpolatorException();
};

#endif // InterpolatorException_hpp

