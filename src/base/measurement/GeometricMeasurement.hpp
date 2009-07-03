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
   GeometricMeasurement(const std::string &nomme = "");
   virtual ~GeometricMeasurement();
   GeometricMeasurement(const GeometricMeasurement& gm);
   GeometricMeasurement& operator=(const GeometricMeasurement& gm);

   virtual const MeasurementData &CalculateMeasurement();
   virtual const Rmatrix &CalculateMeasurementDerivatives();

protected:
   MeasurementData            currentMeasurement;
   Rmatrix                    currentDerivatives;

   StringArray                participantNames;
   SpacePoint                 *anchorPoint;
   std::vector<SpacePoint*>   participants;

   virtual bool               Evaluate(bool withDerivatives = false) = 0;
};

#endif /* GEOMETRICMEASUREMENT_HPP_ */
