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

#include "RangeMeasurementModel.hpp"
#include "Rvector6.hpp"

RangeMeasurementModel::RangeMeasurementModel(const std::string name) :
   MeasurementModel        ("Range", name)
{

}

RangeMeasurementModel::RangeMeasurementModel(const RangeMeasurementModel &RMM):
   MeasurementModel        (RMM)
{

}

RangeMeasurementModel& RangeMeasurementModel::operator=(const RangeMeasurementModel &RMM)
{
   if (&RMM != this)
   {
      MeasurementModel::operator=(RMM);
   }
   return *this;
}

RangeMeasurementModel::~RangeMeasurementModel()
{
}

GmatBase *RangeMeasurementModel::Clone() const
{
   return new RangeMeasurementModel(*this);
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
void RangeMeasurementModel::Initialize() const
{

}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool RangeMeasurementModel::ComputeMeasurement(
      GroundStation *theStation, Spacecraft *theSat, LaVectorDouble &myMeasurements)
{

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(epoch);

    Rvector6 range = satState-gsState;

    myMeasurements(0) = range.GetMagnitude();

    return true;

}

//------------------------------------------------------------------------------
// bool ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool RangeMeasurementModel::ComputeCartesianPartialDerivative(
      GroundStation *theStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
{
    
    if (myCartDerivatives.size(0) < 6)
	return false;
    
    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(epoch);

    Rvector6 range = satState-gsState;
    
    Real rangeMag = range.GetMagnitude();

    if (rangeMag > 0) {
        myCartDerivatives(0,0) = range(0)/rangeMag;
	myCartDerivatives(0,1) = range(1)/rangeMag;
	myCartDerivatives(0,2) = range(2)/rangeMag;
	myCartDerivatives(0,3) = 0;
	myCartDerivatives(0,4) = 0;
	myCartDerivatives(0,5) = 0;
	return true;
    }
    else
    {
	return false;
    }
}

