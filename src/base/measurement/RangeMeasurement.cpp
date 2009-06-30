//$Id$
//------------------------------------------------------------------------------
//                         RangeMeasurement
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/29
//
/**
 * Implementation of the geometric range measurement.
 */
//------------------------------------------------------------------------------


#include "RangeMeasurement.hpp"



RangeMeasurement::RangeMeasurement(const std::string &name) :
   GeometricMeasurement          (name)
{

}

RangeMeasurement::~RangeMeasurement()
{
   // TODO Auto-generated destructor stub
}

RangeMeasurement::RangeMeasurement(const RangeMeasurement &rm) :
   GeometricMeasurement          (rm)
{

}

RangeMeasurement& RangeMeasurement::operator=(const RangeMeasurement &rm)
{
   if (&rm != this)
   {

   }

   return *this;
}

bool RangeMeasurement::Evaluate(bool withDerivatives)
{
   return false;
}


