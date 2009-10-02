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

#include "InterpolatorException.hpp"

// class constructor
InterpolatorException::InterpolatorException(std::string details) :
   BaseException("Interpolator Exception: ", details)
{
}

// class destructor
InterpolatorException::~InterpolatorException()
{
}

