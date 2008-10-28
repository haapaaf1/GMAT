//$Header$
//------------------------------------------------------------------------------
//                              RangeMeasurementModel
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
 * Implements the geometric range measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _RANGEMEASUREMENTMODEL_HPP
#define	_RANGEMEASUREMENTMODEL_HPP

#include "MeasurementModel.hpp"

class GMAT_API RangeMeasurementModel : public MeasurementModel
{
public:
    RangeMeasurementModel(const std::string name = "");
    RangeMeasurementModel(const RangeMeasurementModel &RMM);
    RangeMeasurementModel& operator=(const RangeMeasurementModel &RMM);
    virtual ~RangeMeasurementModel();

    virtual bool ComputeMeasurement(GroundStation &theStation,
          Spacecraft &theSat, Rvector &myMeasurements);
    virtual bool ComputeCartesianPartialDerivative(
          GroundStation &theStation, Spacecraft &theSat,
          Rvector &myCartDerivatives);


    virtual GmatBase *Clone() const;

private:

protected:

};

#endif	/* _RANGEMEASUREMENTMODEL_HPP */

