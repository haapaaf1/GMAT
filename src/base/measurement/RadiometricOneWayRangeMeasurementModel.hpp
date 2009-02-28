//$Header$
//------------------------------------------------------------------------------
//                              RadiometricOneWayRangeMeasurementModel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/08/27
//
/**
 *
 * Implements the Radiometric range measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _RadiometricOneWayRangeMeasurementModel_HPP
#define	_RadiometricOneWayRangeMeasurementModel_HPP

#include "MeasurementModel.hpp"
#include "CoordinateConverter.hpp"

class GMAT_API RadiometricOneWayRangeMeasurementModel : public MeasurementModel
{
public:
    RadiometricOneWayRangeMeasurementModel(const std::string name = "");
    RadiometricOneWayRangeMeasurementModel(const RadiometricOneWayRangeMeasurementModel &RMM);
    RadiometricOneWayRangeMeasurementModel& operator=(const RadiometricOneWayRangeMeasurementModel &RMM);
    virtual ~RadiometricOneWayRangeMeasurementModel();

    void Initialize() const;
    
    virtual bool ComputeMeasurement(GroundStation *theStation,
          Spacecraft *theSat, LaVectorDouble &myMeasurements);
    virtual bool ComputeCartesianPartialDerivative(
          GroundStation *theStation, Spacecraft *theSat,
          LaGenMatDouble &myCartDerivatives);


    virtual GmatBase *Clone() const;

private:

protected:

};

#endif	/* _RadiometricOneWayRangeMeasurementModel_HPP */

