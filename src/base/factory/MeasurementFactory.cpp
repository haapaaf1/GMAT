//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#include "MeasurementFactory.hpp"
#include "MessageInterface.hpp"

/// Specific types supported
#include "RangeMeasurement.hpp"
//#include "RangeRateMeasurement.hpp"
//#include "AzElMeasurement.hpp"
//#include "RADecMeasurement.hpp"

GeometricMeasurement *MeasurementFactory::CreateMeasurement(const std::string & ofType, const std::string & withName)
{
   if (ofType == "Range")
      return new RangeMeasurement(withName);
//   if (ofType == "RangeRate")
//      return new RangeRateMeasurement(withName);
//   if (ofType == "AzEl")
//      return new AzElMeasurement(withName);
//   if (ofType == "RADec")
//      return new RADecMeasurement(withName);

   return NULL;
}


MeasurementFactory::MeasurementFactory() :
   Factory     (Gmat::CORE_MEASUREMENT)
{
   if (creatables.empty())
   {
      creatables.push_back("Range");
//      creatables.push_back("RangeRate");
//      creatables.push_back("AzEl");
//      creatables.push_back("RADec");
   }
}


MeasurementFactory::~MeasurementFactory()
{
}


MeasurementFactory & MeasurementFactory::operator=(const MeasurementFactory & fact)
{
   Factory::operator=(fact);
   if (creatables.empty())

   {
      creatables.push_back("Range");
//      creatables.push_back("RangeRate");
//      creatables.push_back("AzEl");
//      creatables.push_back("RADec");
   }

   return *this;
}


MeasurementFactory::MeasurementFactory(StringArray createList) :
   Factory        (createList, Gmat::CORE_MEASUREMENT)
{
   if (creatables.empty())
   {
      creatables.push_back("Range");
//      creatables.push_back("RangeRate");
//      creatables.push_back("AzEl");
//      creatables.push_back("RADec");
   }
}



MeasurementFactory::MeasurementFactory(const MeasurementFactory & fact) :
   Factory        (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("Range");
//      creatables.push_back("RangeRate");
//      creatables.push_back("AzEl");
//      creatables.push_back("RADec");
   }
}
