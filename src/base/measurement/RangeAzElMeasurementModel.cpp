//$Header$
//------------------------------------------------------------------------------
//                              RaDecMeasurementModel
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
 * Implements the geometric right ascension and declination measurement model.
 *
 */
//------------------------------------------------------------------------------

#include "RangeAzElMeasurementModel.hpp"

RangeAzElMeasurementModel::RangeAzElMeasurementModel(const std::string name) :
   MeasurementModel("RangeAzEl", name)
{
  numMeasurements = 3;

  // Name of each measurement returned
  measurementNames = new std::string[numMeasurements];
  // Units of each measurement returned
  measurementUnits = new std::string[numMeasurements];
  // Measurement returned by the model
  measurements = new Real[numMeasurements];
}

RangeAzElMeasurementModel::RangeAzElMeasurementModel(const RangeAzElMeasurementModel &raeModel) :
   MeasurementModel        (raeModel)
{
   numMeasurements = 3;

   // Name of each measurement returned
   measurementNames = new std::string[numMeasurements];
   // Units of each measurement returned
   measurementUnits = new std::string[numMeasurements];
   // Measurement returned by the model
   measurements = new Real[numMeasurements];
}

RangeAzElMeasurementModel& RangeAzElMeasurementModel::operator=(const RangeAzElMeasurementModel &raeModel)
{
   if (&raeModel != this)
   {

   }
   return *this;
}

RangeAzElMeasurementModel::~RangeAzElMeasurementModel()
{
    delete[] measurementNames;
    delete[] measurementUnits;
    delete[] measurements;
}

GmatBase *RangeAzElMeasurementModel::Clone() const
{
   return new RangeAzElMeasurementModel(*this);
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
  bool RangeAzElMeasurementModel::ComputeMeasurement(GroundStation &theStation,
        Spacecraft &theSat, Rvector &myMeasurements)
  {

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat.GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat.GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6  gsState = theStation.GetMJ2000State(epoch);

    Rvector3 range = satState.GetR()-gsState.GetR();
    Rvector3 toporange;

    // where mj2k is a J2000 system and topo is Topocentric
// TODO: Following line needs to be fixed
//    ccvtr.Convert(epoch, range, mj2k, toporange, topo);

    Rvector rangeUnitVector = toporange.Normalize();

    // Get position (X, Y, Z)
    Real posX = rangeUnitVector[0];
    Real posY = rangeUnitVector[1];
    Real posZ = rangeUnitVector[2];

    // Get right ascension measured positive to the East
    Real alpha = GmatMathUtil::ATan2(posY,posX);
// TODO: Following line needs to be fixed
//    alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

    // Get declination measured positive to the north
    Real rDeltaSat = GmatMathUtil::Sqrt(posX*posX+posY*posY);
    Real delta = GmatMathUtil::ATan2(posZ,rDeltaSat);
// TODO: Following line needs to be fixed
//    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI);

    myMeasurements.Set(2,alpha,delta);

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

  bool RangeAzElMeasurementModel::ComputeCartesianPartialDerivative(
        GroundStation &theStation, Spacecraft &theSat, Rvector &myCartDerivatives)
  {
      return false;
  }

