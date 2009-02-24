//$Header$
//------------------------------------------------------------------------------
//                              GeometricRaDecMeasurementModel
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
  Bool GeometricRaDecMeasurementModel::ComputeMeasurement(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myMeasurements)
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
    ccvtr.Convert(epoch, range, mj2k, toporange, topo);    

    Rvector rangeUnitVector = toporange.Normalize();
    
    // Get position (X, Y, Z)
    Real posX = rangeUnitVector.Get(0); 
    Real posY = rangeUnitVector.Get(1);
    Real posZ = rangeUnitVector.Get(2);

    // Get right ascension measured positive to the East
    Real alpha = GmatMathUtil::ATan2(posY,posX); 
    alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI); 
  
    // Get declination measured positive to the north
    Real rDeltaSat = GmatMathUtil::Sqrt(posX*posX+posY*posY);
    Real delta = GmatMathUtil::ATan2(posZ,rDeltaSat); 
    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI); 

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

  Bool GeometricRaDecMeasurementModel::ComputeCartesianPartialDerivative(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myCartDerivatives);
  {
      return false;
  }
  