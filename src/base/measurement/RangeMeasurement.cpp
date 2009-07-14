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
#include "MessageInterface.hpp"
#include "MeasurementException.hpp"


RangeMeasurement::RangeMeasurement(const std::string &name) :
   GeometricMeasurement          ("Range", name)
{
   objectTypeNames.push_back("RangeMeasurement");

//   parameterCount = GeometricRangeMeasurementParamCount;
}


RangeMeasurement::~RangeMeasurement()
{
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


GmatBase* RangeMeasurement::Clone() const
{
   return new RangeMeasurement(*this);
}


bool RangeMeasurement::Evaluate(bool withDerivatives)
{
   MessageInterface::ShowMessage("Range?  I'll give you range!\n");

   if (this->participants.size() != 2)
      throw MeasurementException("Range measurements require exactly 2 "
            "participants");

   return true;
}
