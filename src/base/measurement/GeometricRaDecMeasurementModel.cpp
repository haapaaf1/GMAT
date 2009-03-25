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
// Integer ComputeMeasurement(Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
Bool GeometricRaDecMeasurementModel::ComputeMeasurement(Spacecraft *theSat)
{
      
    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();
    
    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);
    
    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6  gsState = theStation.GetMJ2000State(epoch);
    
    Rvector3 range = satState.GetR()-gsState.GetR();
    Rvector3 rangeRate = satState.GetV()-gsState.GetV();
    
    // Get position (X, Y, Z)
    Real posX = range.Get(0); 
    Real posY = range.Get(1);
    Real posZ = range.Get(2);

    Real temp = GmatMathUtil::Sqrt(posX*posX+posY*posY);

    // Get right ascension measured positive to the East
    Real alpha = 0.0;

    if (temp != 0.0)
    {
        alpha = GmatMathUtil::ATan2(posY,posX);
        alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);
    }
    else
    {
        alpha = GmatMathUtil::ATan2(rangeRate.Get(1),rangeRate.Get(0));
        alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);
    }
  

    // Get declination measured positive to the north
    Real delta = GmatMathUtil::ASin(posZ,range.GetMagnitude());
    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI); 

    theMeasurements(0) = alpha;
    theMeasurements(1) = delta;

    return true;

}
  
  //------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
Bool GeometricRaDecMeasurementModel::ComputeCartesianPartialDerivative(
                                                      Spacecraft *theSat);
{

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6  gsState = theStation.GetMJ2000State(epoch);

    Rvector3 range = satState.GetR()-gsState.GetR();
    Real rangeMagnitude = range.GetMagnitude();
    Real rInverse = 1.0/rangeMagnitude;

    // Get position (X, Y, Z)
    Real posX = range.Get(0);
    Real posY = range.Get(1);
    Real posZ = range.Get(2);

    Real temp = GmatMathUtil::Sqrt(posX*posX+posY*posY);

    // Get right ascension measured positive to the East
    Real alpha = 0.0;

    if (temp != 0.0)
    {
        alpha = GmatMathUtil::ATan2(posY,posX);
        alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

        Real sec2alpha = GmatMathUtil::Pow(GmatMathUtil::Sec(alpha),2);

        theCartDerivatives(0,0) = -posY*sec2alpha/(posX*posX);
        theCartDerivatives(0,1) = sec2alpha/posX;
        theCartDerivatives(0,2) = 0.0;
        theCartDerivatives(0,3) = 0.0;
        theCartDerivatives(0,4) = 0.0;
        theCartDerivatives(0,5) = 0.0;
    }
    else
    {
        // Get velocity (VX, VY, VZ)
        Rvector3 rangeRate = satState.GetV()-gsState.GetV();
        Real velX = rangeRate.Get(0);
        Real velY = rangeRate.Get(1);
        Real velZ = rangeRate.Get(2);

        alpha = GmatMathUtil::ATan2(velY,velX);
        alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

        Real sec2alpha = GmatMathUtil::Pow(GmatMathUtil::Sec(alpha),-2);

        theCartDerivatives(0,0) = 0.0;
        theCartDerivatives(0,1) = 0.0;
        theCartDerivatives(0,2) = 0.0;
        theCartDerivatives(0,3) = -velY*sec2alpha/(velX*velX);
        theCartDerivatives(0,4) = sec2alpha/velX;
        theCartDerivatives(0,5) = 0.0;
    }

    alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

    // Get declination measured positive to the north
    Real delta = GmatMathUtil::ASin(posZ,rangeMagnitude);
    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI);

    Real invCosDelta = 1.0/GmatMathUtil::Cos(delta);

    theCartDerivatives(1,0) = -posX*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(1,1) = -posY*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(1,2) = -posZ*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(1,3) = 0.0;
    theCartDerivatives(1,4) = 0.0;
    theCartDerivatives(1,5) = 0.0;

    return true;

}
  