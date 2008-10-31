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
#include "BodyFixedPoint.hpp"

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

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
void RangeAzElMeasurementModel::Initialize() const
{

}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const GroundStation &theStation,
//		const Spacecraft &theSat, const LaVectorDouble &myMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was surotMatessfully computed.
 */
//------------------------------------------------------------------------------
  bool RangeAzElMeasurementModel::ComputeMeasurement(GroundStation *theStation,
        Spacecraft *theSat, LaVectorDouble &myMeasurements)
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

    // Find the body spin rate to compute relative velocity
    // TODO - Darrel, can you fix this????
    //std::string cBodyName = theStation->GetStringParameter(GroundStation::CENTRAL_BODY);
    //SpacePoint* theBody = theStation->GetRefObject(Gmat::SPACE_POINT,cBodyName);
    //Real spinRate = theBody->GetAngularVelocity().GetMagnitude();
    
    Real spinRate = 7.29211585530e-5;

    // Compute relative velocity
    Rvector3 vsens;
    vsens(0) = -spinRate*gsPos(1);
    vsens(1) = spinRate*gsPos(0);
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
    
    // Compute range rate for the case where elevation is 90 degrees
    // exactly and computing azimuth goes singular
    Rvector3 rangeRateSEZ;   
    rangeRateSEZ(1) = vrel(0)*rotMat(0,0) + vrel(1)*rotMat(0,1) + 
		      vrel(2)*rotMat(0,2);
    rangeRateSEZ(2) = vrel(0)*rotMat(1,0) + vrel(1)*rotMat(1,1) + 
		      vrel(2)*rotMat(1,2);
    rangeRateSEZ(3) = vrel(0)*rotMat(2,0) + vrel(1)*rotMat(2,1) + 
		      vrel(2)*rotMat(2,2);

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

  bool RangeAzElMeasurementModel::ComputeCartesianPartialDerivative(
        GroundStation *theStation, Spacecraft *theSat, LaGenMatDouble &myCartDerivatives)
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
    Rvector6 gsState = theStation->GetMJ2000State(epoch);
    Rvector3 gsPos = theStation->GetBodyFixedLocation(epoch);
   
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

    // Get body spin rate
    // TODO - Darrel, can you fix this????
    //std::string cBodyName = theStation->GetStringParameter(GroundStation::CENTRAL_BODY);
    //SpacePoint* theBody = theStation->GetRefObject(Gmat::SPACE_POINT,cBodyName);
    //Real spinRate = theBody->GetAngularVelocity().GetMagnitude();

    Real spinRate = 7.29211585530e-5;
    
    // Compute relative velocity
    Rvector3 vsens;
    vsens(0) = -spinRate*gsPos(1); 
    vsens(1) = spinRate*gsPos(0);
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
    
    // Compute range rate for the case where elevation is 90 degrees
    // exactly and computing azimuth goes singular
    Rvector3 rangeRateSEZ;   
    rangeRateSEZ(1) = vrel(0)*rotMat(0,0) + vrel(1)*rotMat(0,1) + 
		   vrel(2)*rotMat(0,2);
    rangeRateSEZ(2) = vrel(0)*rotMat(1,0) + vrel(1)*rotMat(1,1) + 
		   vrel(2)*rotMat(1,2);
    rangeRateSEZ(3) = vrel(0)*rotMat(2,0) + vrel(1)*rotMat(2,1) + 
		   vrel(2)*rotMat(2,2);

    // Compute elevation angle
    Real el = GmatMathUtil::ASin( rangeSEZ(2), mag_rangeSEZ );
    el = GetDegree(el,0.0,GmatMathUtil::TWO_PI); 
    
    // Compute azimuth
    Real az = 0.0;
    if (el == GmatMathUtil::PI_OVER_TWO)
    {
	az = GmatMathUtil::ASin(rangeRateSEZ(2),GmatMathUtil::Sqrt(rangeRateSEZ(1)*
     		   rangeRateSEZ(1)+rangeRateSEZ(2)*rangeRateSEZ(2)));
    }
    else
    {
	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));
    }
  
    Real squared = rangeSEZ(0)*rangeSEZ(0) + rangeSEZ(1)*rangeSEZ(1);
    Real squared2 = rangeRateSEZ(0)*rangeRateSEZ(0) + rangeRateSEZ(1)*rangeRateSEZ(1);

    // Partials of azimuth w.r.t. cartesian state
    if(el == GmatMathUtil::PI_OVER_TWO)
    {
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

