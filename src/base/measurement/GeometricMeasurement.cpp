//------------------------------------------------------------------------------
//                         GeometricMeasurement
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
 * Implementation of the geometric measurement base class.
 */
//------------------------------------------------------------------------------


#include "GeometricMeasurement.hpp"

GeometricMeasurement::GeometricMeasurement(const std::string &nomme) :
   GmatBase          (Gmat::MEASUREMENT_MODEL, "MeasurementPrimitive", nomme)
{

}

GeometricMeasurement::~GeometricMeasurement()
{
}

GeometricMeasurement::GeometricMeasurement(const GeometricMeasurement& gm) :
   GmatBase          (gm)
{
}

GeometricMeasurement& GeometricMeasurement::operator=(const GeometricMeasurement& gm)
{
   if (&gm != this)
   {

   }

   return *this;
}
const MeasurementData & GeometricMeasurement::CalculateMeasurement()
{
   if (Evaluate(true) == false)
      // throw here
      ;

   return currentMeasurement;
}



const Rmatrix & GeometricMeasurement::CalculateMeasurementDerivatives()
{
   // Add a check to see if current data has been evaluated

   return currentDerivatives;
}



