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


const std::string
GeometricMeasurement::PARAMETER_TEXT[GeometricMeasurementParamCount -
                                     GmatBaseParamCount] =
{
   "Participants",
};


const Gmat::ParameterType
GeometricMeasurement::PARAMETER_TYPE[GeometricMeasurementParamCount -
                                     GmatBaseParamCount] =
{
   Gmat::OBJECTARRAY_TYPE,
};


GeometricMeasurement::GeometricMeasurement(const std::string &nomme) :
   GmatBase          (Gmat::CORE_MEASUREMENT, "CoreMeasurement", nomme)
{
   objectTypes.push_back(Gmat::CORE_MEASUREMENT);
   objectTypeNames.push_back("CoreMeasurement");      // Move to CoreMeas when built
   objectTypeNames.push_back("GeometricMeasurement");

   parameterCount = GeometricMeasurementParamCount;
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

std::string GeometricMeasurement::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < GeometricMeasurementParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
}

Integer GeometricMeasurement::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < GeometricMeasurementParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

Gmat::ParameterType GeometricMeasurement::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < GeometricMeasurementParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

std::string GeometricMeasurement::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

MeasurementData* GeometricMeasurement::GetMeasurementDataPointer()
{
   return &currentMeasurement;
}

Rmatrix* GeometricMeasurement::GetDerivativePointer()
{
   return &currentDerivatives;
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



