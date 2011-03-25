//$Header$
//------------------------------------------------------------------------------
//                              GeometricAzElMeasurementModel
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
 * Implements the geometric azimuth and elevation measurement model.
 *
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(Spacecraft &theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
  Bool GeometricAzElMeasurementModel::ComputeMeasurement(Spacecraft *theSat)
  {
      
    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();
    
    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);
    
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

    theMeasurements(0) = alpha;
    theMeasurements(1) = delta;
    
    return true;
    
  }
  
  //------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const GroundStation &theStation, 
//		Spacecraft &theSat, const Rvector &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------

  Bool GeometricAzElMeasurementModel::ComputeCartesianPartialDerivative(
                                                        Spacecraft *theSat);
  {
    // Make sure derivative matrix is properly dimensioned
    if (theCartDerivatives.rows() < numMeasurements &&
	  theCartDerivatives.cols() < 6)
	  return false;

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(epoch);
    Rvector3 gsPos = theStation->GetBodyFixedLocation(epoch);

    // The range vector in MJ2000 coordinates
    Rvector6 rangeMJ2000 = satState-gsState;

    Real rangeMag = rangeMJ2000.GetMagnitude();

    // Compute relative velocity
    Rvector3 vsens;
    vsens(0) = -bodySpinRate*gsPos(1);
    vsens(1) = bodySpinRate*gsPos(0);
    vsens(2) = 0.0;

    Rvector3 vrel;
    vrel(0) = satState(3) - vsens(1);
    vrel(1) = satState(4) - vsens(2);
    vrel(2) = satState(5);

    // Compute Range Rate
    // Real rangeRate = vrel(0)*theCartDerivatives(0,0) +
    //	    vrel(1)*theCartDerivatives(0,1) +
    //	    vrel(2)*theCartDerivatives(0,2);

    // We need the rotation matrix from MJ2000 to SEZ
    // So we need the transpose of rotMat provided by the BFCS
    // TODO: Darrel, Is this correct?
    Rmatrix33 rotMat = theStation->GetBodyFixedCoordinateSystem()->GetLastRotationMatrix().Transpose();

    // Compute the range vector in SEZ coordinates
    Rvector3 rangeSEZ;
    rangeSEZ(1) = rangeMJ2000(0)*rotMat(0,0) + rangeMJ2000(1)*rotMat(0,1) +
		rangeMJ2000(2)*rotMat(0,2);
    rangeSEZ(2) = rangeMJ2000(0)*rotMat(1,0) + rangeMJ2000(1)*rotMat(1,1) +
		rangeMJ2000(2)*rotMat(1,2);
    rangeSEZ(3) = rangeMJ2000(0)*rotMat(2,0) + rangeMJ2000(1)*rotMat(2,1) +
		rangeMJ2000(2)*rotMat(2,2);

    Real mag_rangeSEZ = rangeSEZ.GetMagnitude();

    // Compute elevation angle
    Real el = GmatMathUtil::ASin( rangeSEZ(2), mag_rangeSEZ );
    el = GetDegree(el,0.0,GmatMathUtil::TWO_PI);

    // Initialize azimuth
    Real az = 0.0;

    // Compute azimuth and the partials of azimuth w.r.t. cartesian state
    if(el == GmatMathUtil::PI_OVER_TWO)
    {

	// Compute range rate for the case where elevation is 90 degrees
	// exactly and computing azimuth goes singular
	Rvector3 rangeRateSEZ;
	rangeRateSEZ(1) = vrel(0)*rotMat(0,0) + vrel(1)*rotMat(0,1) +
		   vrel(2)*rotMat(0,2);
        rangeRateSEZ(2) = vrel(0)*rotMat(1,0) + vrel(1)*rotMat(1,1) +
		   vrel(2)*rotMat(1,2);
	rangeRateSEZ(3) = vrel(0)*rotMat(2,0) + vrel(1)*rotMat(2,1) +
		   vrel(2)*rotMat(2,2);

	Real squared2 = rangeRateSEZ(0)*rangeRateSEZ(0) + rangeRateSEZ(1)*rangeRateSEZ(1);

	az = GmatMathUtil::ASin(rangeRateSEZ(2),GmatMathUtil::Sqrt(rangeRateSEZ(1)*
     		   rangeRateSEZ(1)+rangeRateSEZ(2)*rangeRateSEZ(2)));


	theCartDerivatives(0,0) = 0.0;
	theCartDerivatives(0,1) = 0.0;
	theCartDerivatives(0,2) = 0.0;
	theCartDerivatives(0,3) = rotMat(1,0)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,0) + rangeRateSEZ(1)*rotMat(1,0))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(0,4) = rotMat(1,1)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,1) + rangeRateSEZ(1)*rotMat(1,1))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(0,5) = -rangeRateSEZ(0)*rotMat(0,2)*rangeRateSEZ(1)/
		(GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)*
		GmatMathUtil::Cos(az));
    }
    else
    {

	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));

	Real squared = rangeSEZ(0)*rangeSEZ(0) + rangeSEZ(1)*rangeSEZ(1);

	theCartDerivatives(0,0) = rotMat(1,0)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,0)+rangeSEZ(1)*rotMat(1,0))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
        theCartDerivatives(0,1) = rotMat(1,1)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,1)+rangeSEZ(2)*rotMat(1,1))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(0,2) = -rangeSEZ(0)*rotMat(0,2)*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)*
		GmatMathUtil::Cos(az);
	theCartDerivatives(0,3) = 0.0;
	theCartDerivatives(0,4) = 0.0;
	theCartDerivatives(0,5) = 0.0;
    }

    // Partials of elevation w.r.t. cartesian state
    theCartDerivatives(1,0) = rotMat(3,1)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(1)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(1,1) = rotMat(3,2)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(2)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(1,2) = rotMat(3,3)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(3)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(1,3) = 0.0;
    theCartDerivatives(1,4) = 0.0;
    theCartDerivatives(1,5) = 0.0;

    return true;

  }
  