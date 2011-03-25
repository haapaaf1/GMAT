//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeRaDecMeasurementModel
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
 * Implements the geometric range, right ascension and declination measurement model.
 *
 */
//------------------------------------------------------------------------------

GeometricRangeRaDecMeasurementModel::GeometricRangeRaDecMeasurementModel(const std::string name) :
   MeasurementModel  ("GeometricRangeRaDec", name),
   bodySpinRate          (7.29211585530e-5)
{
  numMeasurements = 3;

  // TODO: FIX the "new" statements
  // Name of each measurement returned
  //measurementNames = new StringArray(numMeasurements);
  // Units of each measurement returned
  //measurementUnits = new StringArray(numMeasurements);
  // Measurement returned by the model
  measurements = new Real[numMeasurements];
}

GeometricRangeRaDecMeasurementModel::GeometricRangeRaDecMeasurementModel(const GeometricRangeRaDecMeasurementModel &rrdModel) :
   MeasurementModel        (rrdModel),
   bodySpinRate            (rrdModel.bodySpinRate),
   theStation		   (rrdModel.theStation)
{
}

GeometricRangeRaDecMeasurementModel& GeometricRangeRaDecMeasurementModel::operator=(const GeometricRangeRaDecMeasurementModel &rrdModel)
{
    // TODO: Is this correct?
   if (&rrdModel != this)
   {
      //theBody = NULL;
   }
   return *this;
}

GeometricRangeRaDecMeasurementModel::~GeometricRangeRaDecMeasurementModel()
{
    delete[] &measurementNames;
    delete[] &measurementUnits;
    delete[] measurements;
}

GmatBase *GeometricRangeRaDecMeasurementModel::Clone() const
{
   return new GeometricRangeRaDecMeasurementModel(*this);
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
bool GeometricRangeRaDecMeasurementModel::Initialize()
{

    bodySpinRate = theStation->GetSpinRate();

    return true;
}

//------------------------------------------------------------------------------
// Integer ComputeMeasurement(const Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
Bool GeometricRangeRaDecMeasurementModel::ComputeMeasurement(
                                                       const Spacecraft *theSat)
{

    // GMAT's A.1 modified Julian epoch
    Real epoch = theSat->GetEpoch();

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 satState = theSat->GetMJ2000State(epoch);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6  gsState = theStation.GetMJ2000State(epoch);

    Rvector3 range = satState.GetR()-gsState.GetR();
    Real rangeMagnitude = range.GetMagnitude();

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
    }
    else
    {
        alpha = GmatMathUtil::ATan2(rangeRate.Get(1),rangeRate.Get(0));
    }

    alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

    // Get declination measured positive to the north
    Real delta = GmatMathUtil::ASin(posZ,rangeMagnitude);
    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI);

    theMeasurements(0) = rangeMagnitude;
    theMeasurements(1) = alpha;
    theMeasurements(2) = delta;
    
    return true;
    
}
  
  //------------------------------------------------------------------------------
// Integer ComputeCartesianPartialDerivative(const Spacecraft *theSat);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------

Bool GeometricRangeRaDecMeasurementModel::ComputeCartesianPartialDerivative(
                                                      const Spacecraft *theSat);
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

    theCartDerivatives(0,0) = posX*rInverse;
    theCartDerivatives(0,1) = posY*rInverse;
    theCartDerivatives(0,2) = posZ*rInverse;
    theCartDerivatives(0,3) = 0.0;
    theCartDerivatives(0,4) = 0.0;
    theCartDerivatives(0,5) = 0.0;

    Real temp = GmatMathUtil::Sqrt(posX*posX+posY*posY);

    // Get right ascension measured positive to the East
    Real alpha = 0.0;

    if (temp != 0.0)
    {
        alpha = GmatMathUtil::ATan2(posY,posX);
        alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

        Real sec2alpha = GmatMathUtil::Pow(GmatMathUtil::Sec(alpha),2);

        theCartDerivatives(1,0) = -posY*sec2alpha/(posX*posX);
        theCartDerivatives(1,1) = sec2alpha/posX;
        theCartDerivatives(1,2) = 0.0;
        theCartDerivatives(1,3) = 0.0;
        theCartDerivatives(1,4) = 0.0;
        theCartDerivatives(1,5) = 0.0;
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

        theCartDerivatives(1,0) = 0.0;
        theCartDerivatives(1,1) = 0.0;
        theCartDerivatives(1,2) = 0.0;
        theCartDerivatives(1,3) = -velY*sec2alpha/(velX*velX);
        theCartDerivatives(1,4) = sec2alpha/velX;
        theCartDerivatives(1,5) = 0.0;
    }

    alpha = GetDegree(alpha,0.0,GmatMathUtil::TWO_PI);

    // Get declination measured positive to the north
    Real delta = GmatMathUtil::ASin(posZ,rangeMagnitude);
    delta = GetDegree(delta,0.0,GmatMathUtil::TWO_PI);

    Real invCosDelta = 1.0/GmatMathUtil::Cos(delta);

    theCartDerivatives(2,0) = -posX*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(2,1) = -posY*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(2,2) = -posZ*posZ*rInverse*rInverse*rInverse*invCosDelta;
    theCartDerivatives(2,3) = 0.0;
    theCartDerivatives(2,4) = 0.0;
    theCartDerivatives(2,5) = 0.0;

    return true;
}
  