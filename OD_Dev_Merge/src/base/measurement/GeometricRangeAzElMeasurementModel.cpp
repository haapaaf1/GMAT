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

//---------------------------------
//  static data
//---------------------------------
const std::string GeometricRangeAzElMeasurementModel::DEPENDENT_PARAMETER_TEXT[EndDependentParams - MMDependentParamCount] =
{
    "Cartesian",
};

//------------------------------------------------------------------------------
//  Integer  GetDependentParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the dependent parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested dependent parameter.
 *
 * @return ID for the requested dependent parameter or -1 for no associated ID.
 */
//------------------------------------------------------------------------------
Integer GeometricRangeAzElMeasurementModel::GetDependentParamID(const std::string &param) const
{

   for (Integer i = 0; i < EndDependentParams; i++)
   {
      if (param == DEPENDENT_PARAMETER_TEXT[i])
         return i;
   }

   // Return -1 indicating not found
   return -1;

}


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
//    bool GetTheMeasurements(SpacePoint* theSpacePoint,
//                                  const A1Mjd &atTime,
//                                  LaGenMatDouble &theMeasurements);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was surotMatessfully computed.
 */
//------------------------------------------------------------------------------
   bool GeometricRangeAzElMeasurementModel::GetTheMeasurements(SpacePoint* theSpacePoint,
                                  const A1Mjd &atTime,
                                  LaGenMatDouble &theMeasurements)
   {

    // The target object state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);
    Rvector3 gsPos = theStation->GetBodyFixedLocation(atTime);

    // The range vector in MJ2000 coordinates
    Rvector6 rangeMJ2000 = targetState-gsState;

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
    vrel(0) = targetState(3) - vsens(1);
    vrel(1) = targetState(4) - vsens(2);
    vrel(2) = targetState(5);

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

    theMeasurements(0,0) = rangeMag;
    theMeasurements(1,0) = az;
    theMeasurements(2,0) = el;

    return true;

  }

//------------------------------------------------------------------------------
// bool GetThePartials(const Integer &paramID,
//                     const SpacePoint* theSpacePoint,
//                     const A1Mjd &atTime,
//                     LaGenMatDouble &theDerivatives)
//------------------------------------------------------------------------------
bool GeometricRangeAzElMeasurementModel::GetThePartials(const Integer &paramID,
                                      SpacePoint* theSpacePoint,
                                      const A1Mjd &atTime,
                                      LaGenMatDouble &theDerivatives)
{
    switch (paramID)
    {
        case CARTESIAN_ID:
            return ComputeCartesianPartialDerivative(theSpacePoint,
                                                     atTime,theDerivatives);
            break;
        default:
            return false;
    }

}

//------------------------------------------------------------------------------
// bool ComputeCartesianPartialDerivative(
//                              SpacePoint* theSpacePoint,
//                              const A1Mjd &atTime,
//                              LaGenMatDouble &theDerivatives);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were surotMatessfully
 * computed.
 */
//------------------------------------------------------------------------------
  bool GeometricRangeAzElMeasurementModel::ComputeCartesianPartialDerivative(
                              SpacePoint* theSpacePoint,
                              const A1Mjd &atTime,
                              LaGenMatDouble &theDerivatives)
  {

    // Make sure derivative matrix is properly dimensioned
    if (theDerivatives.rows() < numMeasurements &&
	  theDerivatives.cols() < 6)
	  return false;

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);
    Rvector3 gsPos = theStation->GetBodyFixedLocation(atTime);

    // The range vector in MJ2000 coordinates
    Rvector6 rangeMJ2000 = targetState-gsState;

    Real rangeMag = rangeMJ2000.GetMagnitude();

    // Partials of range w.r.t. cartesian state
    if (rangeMag > 0) {
        theDerivatives(0,0) = rangeMJ2000(0) / rangeMag;
        theDerivatives(0,1) = rangeMJ2000(1) / rangeMag;
	theDerivatives(0,2) = rangeMJ2000(2) / rangeMag;
	theDerivatives(0,3) = 0;
	theDerivatives(0,4) = 0;
	theDerivatives(0,5) = 0;
    }
    else
    {
	return false;
    }
    
    // Compute relative velocity
    Rvector3 vsens;
    vsens(0) = -bodySpinRate*gsPos(1);
    vsens(1) = bodySpinRate*gsPos(0);
    vsens(2) = 0.0;

    Rvector3 vrel;
    vrel(0) = targetState(3) - vsens(1);
    vrel(1) = targetState(4) - vsens(2);
    vrel(2) = targetState(5);

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


	theDerivatives(1,0) = 0.0;
	theDerivatives(1,1) = 0.0;
	theDerivatives(1,2) = 0.0;
	theDerivatives(1,3) = rotMat(1,0)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,0) + rangeRateSEZ(1)*rotMat(1,0))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theDerivatives(1,4) = rotMat(1,1)/GmatMathUtil::Sqrt(squared2) -
		(rangeRateSEZ(0)*rotMat(0,1) + rangeRateSEZ(1)*rotMat(1,1))*
		rangeRateSEZ(1)/GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)/
		GmatMathUtil::Cos(az);
	theDerivatives(1,5) = -rangeRateSEZ(0)*rotMat(0,2)*rangeRateSEZ(1)/
		(GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared2),3)*
		GmatMathUtil::Cos(az));
    }
    else
    {

	az = GmatMathUtil::ASin(rangeSEZ(2),GmatMathUtil::Sqrt(rangeSEZ(1)*
		   rangeSEZ(1)+rangeSEZ(2)*rangeSEZ(2)));

	Real squared = rangeSEZ(0)*rangeSEZ(0) + rangeSEZ(1)*rangeSEZ(1);

	theDerivatives(1,0) = rotMat(1,0)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,0)+rangeSEZ(1)*rotMat(1,0))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
        theDerivatives(1,1) = rotMat(1,1)/GmatMathUtil::Sqrt(squared) -
		(rangeSEZ(0)*rotMat(0,1)+rangeSEZ(2)*rotMat(1,1))*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)/
		GmatMathUtil::Cos(az);
	theDerivatives(1,2) = -rangeSEZ(0)*rotMat(0,2)*rangeSEZ(1)/
		GmatMathUtil::Pow(GmatMathUtil::Sqrt(squared),3)*
		GmatMathUtil::Cos(az);
	theDerivatives(1,3) = 0.0;
	theDerivatives(1,4) = 0.0;
	theDerivatives(1,5) = 0.0;
    }

    // Partials of elevation w.r.t. cartesian state
    theDerivatives(2,0) = rotMat(3,1)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(1)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theDerivatives(2,1) = rotMat(3,2)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(2)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theDerivatives(2,2) = rotMat(3,3)/rangeMag -
	    rangeSEZ(3)*rangeMJ2000(3)/GmatMathUtil::Pow(rangeMag,3)/
	    GmatMathUtil::Cos(el);
    theDerivatives(2,3) = 0.0;
    theDerivatives(2,4) = 0.0;
    theDerivatives(2,5) = 0.0;

    return true;

 }

