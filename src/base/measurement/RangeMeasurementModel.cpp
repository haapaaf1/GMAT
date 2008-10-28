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

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &theStation,
//		const Spacecraft &theSat, const Rvector &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool RangeMeasurementModel::ComputeMeasurement(
      GroundStation &theStation, Spacecraft &theSat, Rvector &myMeasurements)
{

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat.GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat.GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6  gsState = theStation.GetMJ2000State(epoch);

    Rvector6 range = satState-gsState;

    myMeasurements.Set(1,range.GetMagnitude());

    return true;

}

//------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const Rvector &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool RangeMeasurementModel::ComputeCartesianPartialDerivative(
      GroundStation &theStation, Spacecraft &theSat, Rvector &myCartDerivatives)
{
    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat.GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat.GetMJ2000State(epoch);

    // Get position (X, Y, Z)
    Real posX = satState.Get(0);
    Real posY = satState.Get(1);
    Real posZ = satState.Get(2);



    return false;
}

