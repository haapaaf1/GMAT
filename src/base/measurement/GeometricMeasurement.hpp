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
 * Definition of the geometric measurement base class.
 */
//------------------------------------------------------------------------------


#ifndef GEOMETRICMEASUREMENT_HPP_
#define GEOMETRICMEASUREMENT_HPP_

#include "EstimationDefs.hpp"

#include "Rmatrix.hpp"
#include "SpacePoint.hpp"


class GeometricMeasurement : public /*CoreMeasurement*/ GmatBase
{
public:
   GeometricMeasurement(const std::string &type, const std::string &nomme = "");
   virtual ~GeometricMeasurement();
   GeometricMeasurement(const GeometricMeasurement& gm);
   GeometricMeasurement&      operator=(const GeometricMeasurement& gm);

   virtual std::string        GetParameterText(const Integer id) const;
   virtual Integer            GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                              GetParameterType(const Integer id) const;
   virtual std::string        GetParameterTypeString(const Integer id) const;

   // Move to CoreMeasurement when it is created
   MeasurementData*           GetMeasurementDataPointer();
   Rmatrix*                   GetDerivativePointer();

   virtual const MeasurementData&
                              CalculateMeasurement(bool withDerivatives = true);
   virtual const Rmatrix&     CalculateMeasurementDerivatives();

protected:
   MeasurementData            currentMeasurement;
   Rmatrix                    currentDerivatives;

   StringArray                participantNames;
   SpacePoint                 *anchorPoint;
   std::vector<SpacePoint*>   participants;

   virtual bool               Evaluate(bool withDerivatives = false) = 0;


   /// Enumerated parameter IDs
   enum
   {
       PARTICIPANT = GmatBaseParamCount,
       GeometricMeasurementParamCount
   };
   /// Array of supported parameters
   static const std::string PARAMETER_TEXT[GeometricMeasurementParamCount -
                                           GmatBaseParamCount];
   /// Array of parameter types
   static const Gmat::ParameterType PARAMETER_TYPE[GeometricMeasurementParamCount -
                                                   GmatBaseParamCount];
};

#endif /* GEOMETRICMEASUREMENT_HPP_ */
