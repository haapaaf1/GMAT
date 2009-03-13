//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeAzElMeasurementModel
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
 * Implements the geometric range, azimuth and elevation measurement model.
 *
 */
//------------------------------------------------------------------------------

#include "GeometricRangeAzElMeasurementModel.hpp"
#include "BodyFixedPoint.hpp"


//#define DEBUG_PARAMETER_CALCULATIONS


GeometricRangeAzElMeasurementModel::GeometricRangeAzElMeasurementModel(const std::string name) :
   MeasurementModel  ("GeometricRangeAzEl", name),
   bodySpinRate          (7.29211585530e-5)
{
  numMeasurements = 3;

  // TODO: FIX the "new" statements
  // Name of each measurement returned
  //measurementNames = new StringArray(numMeasurements);
  // Units of each measurement returned
  //measurementUnits = new StringArray(numMeasurements);
}

GeometricRangeAzElMeasurementModel::GeometricRangeAzElMeasurementModel(const GeometricRangeAzElMeasurementModel &raeModel) :
   MeasurementModel        (raeModel),
   bodySpinRate            (raeModel.bodySpinRate)
{
}

GeometricRangeAzElMeasurementModel& GeometricRangeAzElMeasurementModel::operator=(const GeometricRangeAzElMeasurementModel &raeModel)
{
    // TODO: Is this correct?
   if (&raeModel != this)
   {
      //theBody = NULL;
   }
   return *this;
}

GeometricRangeAzElMeasurementModel::~GeometricRangeAzElMeasurementModel()
{
    delete[] &measurementNames;
    delete[] &measurementUnits;
}

GmatBase *GeometricRangeAzElMeasurementModel::Clone() const
{
   return new GeometricRangeAzElMeasurementModel(*this);
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
bool GeometricRangeAzElMeasurementModel::Initialize()
{

    bodySpinRate = theStation->GetSpinRate();     
    
    return true;
}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement( const Spacecraft &theSat );
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was surotMatessfully computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeAzElMeasurementModel::ComputeMeasurement(Spacecraft *theSat)
{

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

    // TODO: Move this to initialization if possible
    // Find the body spin rate to compute relative velocity
    //std::string cBodyName = theStation->GetStringParameter("CentralBody");
    //theBody = (CelestialBody*)(theStation->GetRefObject(Gmat::SPACE_POINT,cBodyName));
    //bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();

   #ifdef DEBUG_PARAMETER_CALCULATIONS
       MessageInterface::ShowMessage(
             "Retrieved spin rate is %.12lf; should be about 7.29211585530e-5\n",
             bodySpinRate);
   #endif

    // TODO - If moved to init, update the spin rate here if it varies dynamically
    //bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();

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
    // Real rangeRate = vrel(0)*myCartDerivatives(0,0) +
    //	    vrel(1)*myCartDerivatives(0,1) +
    //	    vrel(2)*myCartDerivatives(0,2);

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

    // Compute azimuth
    // The equations for computing azimuth are undefined when
    // the elevation is exactly 90 degrees. In this case,
    // we use the range rate to compute azimuth.
    Real az = 0.0;
    if (el == GmatMathUtil::PI_OVER_TWO)
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

	az = GmatMathUtil::ASin(rangeRateSEZ(2),GmatMathUtil::Sqrt(rangeRateSEZ(1)*
     		   rangeRateSEZ(1)+rangeRateSEZ(2)*rangeRateSEZ(2)));
    }
    else
    {
	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));
    }

    theMeasurements(0) = rangeMag;
    theMeasurements(1) = az;
    theMeasurements(2) = el;

    return true;

  }

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &myStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was surotMatessfully computed.
 */
//------------------------------------------------------------------------------
  bool GeometricRangeAzElMeasurementModel::ComputeMeasurement(
        GroundStation *myStation,Spacecraft *theSat,
        LaVectorDouble &myMeasurements)
  {

   // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = myStation->GetMJ2000State(epoch);
    Rvector3 gsPos = myStation->GetBodyFixedLocation(epoch);
   
    // The range vector in MJ2000 coordinates
    Rvector6 rangeMJ2000 = satState-gsState;
    
    Real rangeMag = rangeMJ2000.GetMagnitude();

    // TODO: Move this to initialization if possible
    // Find the body spin rate to compute relative velocity
    //std::string cBodyName = theStation->GetStringParameter("CentralBody");
    //theBody = (CelestialBody*)(theStation->GetRefObject(Gmat::SPACE_POINT,cBodyName));
    //bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();

   #ifdef DEBUG_PARAMETER_CALCULATIONS
       MessageInterface::ShowMessage(
             "Retrieved spin rate is %.12lf; should be about 7.29211585530e-5\n", 
             bodySpinRate);
   #endif
    
    // TODO - If moved to init, update the spin rate here if it varies dynamically
    //bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();
    
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
    // Real rangeRate = vrel(0)*myCartDerivatives(0,0) + 
    //	    vrel(1)*myCartDerivatives(0,1) + 
    //	    vrel(2)*myCartDerivatives(0,2);

    // We need the rotation matrix from MJ2000 to SEZ
    // So we need the transpose of rotMat provided by the BFCS
    // TODO: Darrel, Is this correct?
    Rmatrix33 rotMat = myStation->GetBodyFixedCoordinateSystem()->GetLastRotationMatrix().Transpose();
    
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
    
    // Compute azimuth
    // The equations for computing azimuth are undefined when
    // the elevation is exactly 90 degrees. In this case,
    // we use the range rate to compute azimuth.
    Real az = 0.0;
    if (el == GmatMathUtil::PI_OVER_TWO)
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

	az = GmatMathUtil::ASin(rangeRateSEZ(2),GmatMathUtil::Sqrt(rangeRateSEZ(1)*
     		   rangeRateSEZ(1)+rangeRateSEZ(2)*rangeRateSEZ(2)));
    }
    else
    {
	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));
    }

    myMeasurements(0) = rangeMag;
    myMeasurements(1) = az;
    myMeasurements(2) = el;

    return true;

}

//------------------------------------------------------------------------------
// bool ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were surotMatessfully
 * computed.
 */
//------------------------------------------------------------------------------
  bool GeometricRangeAzElMeasurementModel::ComputeCartesianPartialDerivative(
                                                            Spacecraft *theSat)
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

    // Partials of range w.r.t. cartesian state
    if (rangeMag > 0) {
        theCartDerivatives(0,0) = rangeMJ2000(0) / rangeMag;
        theCartDerivatives(0,1) = rangeMJ2000(1) / rangeMag;
	theCartDerivatives(0,2) = rangeMJ2000(2) / rangeMag;
	theCartDerivatives(0,3) = 0;
	theCartDerivatives(0,4) = 0;
	theCartDerivatives(0,5) = 0;
	return true;
    }
    else
    {
	return false;
    }

    // TODO: Move this to initialization if possible
    // Find the body spin rate to compute relative velocity
    // std::string cBodyName = theStation->GetStringParameter("CentralBody");
    // theBody = (CelestialBody*)(theStation->GetRefObject(Gmat::SPACE_POINT,cBodyName));
    // bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();
    // TODO: If above moved to init, update body spin rate here if it is dynamic

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


	theCartDerivatives(1,0) = 0.0;
	theCartDerivatives(1,1) = 0.0;
	theCartDerivatives(1,2) = 0.0;
	theCartDerivatives(1,3) = rotMat(1,0)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,0) + rangeRateSEZ(1)*rotMat(1,0))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(1,4) = rotMat(1,1)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,1) + rangeRateSEZ(1)*rotMat(1,1))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(1,5) = -rangeRateSEZ(0)*rotMat(0,2)*rangeRateSEZ(1)/
		(GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)*
		GmatMathUtil::Cos(az));
    }
    else
    {

	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));

	Real squared = rangeSEZ(0)*rangeSEZ(0) + rangeSEZ(1)*rangeSEZ(1);

	theCartDerivatives(1,0) = rotMat(1,0)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,0)+rangeSEZ(1)*rotMat(1,0))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
        theCartDerivatives(1,1) = rotMat(1,1)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,1)+rangeSEZ(2)*rotMat(1,1))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
	theCartDerivatives(1,2) = -rangeSEZ(0)*rotMat(0,2)*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)*
		GmatMathUtil::Cos(az);
	theCartDerivatives(1,3) = 0.0;
	theCartDerivatives(1,4) = 0.0;
	theCartDerivatives(1,5) = 0.0;
    }

    // Partials of elevation w.r.t. cartesian state
    theCartDerivatives(2,0) = rotMat(3,1)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(1)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(2,1) = rotMat(3,2)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(2)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(2,2) = rotMat(3,3)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(3)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theCartDerivatives(2,3) = 0.0;
    theCartDerivatives(2,4) = 0.0;
    theCartDerivatives(2,5) = 0.0;

 }

//------------------------------------------------------------------------------
// bool ComputeCartesianPartialDerivative(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaGenMatDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were surotMatessfully
 * computed.
 */
//------------------------------------------------------------------------------
  bool GeometricRangeAzElMeasurementModel::ComputeCartesianPartialDerivative(
        GroundStation *myStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
  {
    
    // Make sure derivative matrix is properly dimensioned
    if (myCartDerivatives.rows() < numMeasurements && 
	  myCartDerivatives.cols() < 6)
	  return false;
      
    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = myStation->GetMJ2000State(epoch);
    Rvector3 gsPos = myStation->GetBodyFixedLocation(epoch);
   
    // The range vector in MJ2000 coordinates
    Rvector6 rangeMJ2000 = satState-gsState;
    
    Real rangeMag = rangeMJ2000.GetMagnitude();

    // Partials of range w.r.t. cartesian state
    if (rangeMag > 0) {
        myCartDerivatives(0,0) = rangeMJ2000(0) / rangeMag;
	myCartDerivatives(0,1) = rangeMJ2000(1) / rangeMag;
	myCartDerivatives(0,2) = rangeMJ2000(2) / rangeMag;
	myCartDerivatives(0,3) = 0;
	myCartDerivatives(0,4) = 0;
	myCartDerivatives(0,5) = 0;
	return true;
    }
    else
    {
	return false;
    }

    // TODO: Move this to initialization if possible
    // Find the body spin rate to compute relative velocity
    // std::string cBodyName = myStation->GetStringParameter("CentralBody");
    // theBody = (CelestialBody*)(myStation->GetRefObject(Gmat::SPACE_POINT,cBodyName));
    // bodySpinRate = theBody->GetAngularVelocity().GetMagnitude();
    // TODO: If above moved to init, update body spin rate here if it is dynamic
    
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
    // Real rangeRate = vrel(0)*myCartDerivatives(0,0) + 
    //	    vrel(1)*myCartDerivatives(0,1) + 
    //	    vrel(2)*myCartDerivatives(0,2);

    // We need the rotation matrix from MJ2000 to SEZ
    // So we need the transpose of rotMat provided by the BFCS
    // TODO: Darrel, Is this correct?
    Rmatrix33 rotMat = myStation->GetBodyFixedCoordinateSystem()->GetLastRotationMatrix().Transpose();
     
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

	
	myCartDerivatives(1,0) = 0.0;
	myCartDerivatives(1,1) = 0.0;
	myCartDerivatives(1,2) = 0.0;
	myCartDerivatives(1,3) = rotMat(1,0)/GmatMathUtil::Sqrt(squared2) - 
		(rangeRateSEZ(0)*rotMat(0,0) + rangeRateSEZ(1)*rotMat(1,0))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	myCartDerivatives(1,4) = rotMat(1,1)/GmatMathUtil::Sqrt(squared2) - 
		(rangeRateSEZ(0)*rotMat(0,1) + rangeRateSEZ(1)*rotMat(1,1))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	myCartDerivatives(1,5) = -rangeRateSEZ(0)*rotMat(0,2)*rangeRateSEZ(1)/
		(GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)*
		GmatMathUtil::Cos(az));
    }
    else
    {

	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));

	Real squared = rangeSEZ(0)*rangeSEZ(0) + rangeSEZ(1)*rangeSEZ(1);

	myCartDerivatives(1,0) = rotMat(1,0)/GmatMathUtil::Sqrt(squared) - 
		(rangeSEZ(0)*rotMat(0,0)+rangeSEZ(1)*rotMat(1,0))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
        myCartDerivatives(1,1) = rotMat(1,1)/GmatMathUtil::Sqrt(squared) - 
		(rangeSEZ(0)*rotMat(0,1)+rangeSEZ(2)*rotMat(1,1))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
	myCartDerivatives(1,2) = -rangeSEZ(0)*rotMat(0,2)*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)*
		GmatMathUtil::Cos(az);
	myCartDerivatives(1,3) = 0.0;
	myCartDerivatives(1,4) = 0.0;
	myCartDerivatives(1,5) = 0.0;
    }
    
    // Partials of elevation w.r.t. cartesian state
    myCartDerivatives(2,0) = rotMat(3,1)/rangeMag - 
	    rangeSEZ(3)*rangeMJ2000(1)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    myCartDerivatives(2,1) = rotMat(3,2)/rangeMag - 
	    rangeSEZ(3)*rangeMJ2000(2)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    myCartDerivatives(2,2) = rotMat(3,3)/rangeMag - 
	    rangeSEZ(3)*rangeMJ2000(3)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    myCartDerivatives(2,3) = 0.0;
    myCartDerivatives(2,4) = 0.0;
    myCartDerivatives(2,5) = 0.0;

 }

  





