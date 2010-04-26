//$Id$
//------------------------------------------------------------------------------
//                              AttitudeException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy Shoan/GSFC
// Created: 2006.02.16
//
/**
 * This class provides an exception class for the Attitude classes
 */
//------------------------------------------------------------------------------
#ifndef AttitudeException_hpp
#define AttitudeException_hpp

#include "gmatdefs.hpp"
#include "BaseException.hpp" // inheriting class's header file

class AttitudeException : public BaseException
{
public:

   AttitudeException(std::string details = "");
   ~AttitudeException();
};
#endif /*AttitudeException_hpp*/