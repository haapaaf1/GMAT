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
// void SetGroundStation(GroundStation* gs)
//------------------------------------------------------------------------------
/**
 * Set the ground station for this instance of the measurement model.
 *
 * @param mm The ground station that is assigned.
 */
//------------------------------------------------------------------------------
void GeometricRangeAzElMeasurementModel::SetGroundStation(GroundStation* gs)
{
    theStation = gs;
}

//------------------------------------------------------------------------------
// GroundStation* GetGroundStation()
//------------------------------------------------------------------------------
/**
 * Return the ground station for this instance of the measurement model.
 *
 * @return A pointer to the ground station.
 */
//------------------------------------------------------------------------------
GroundStation* GeometricRangeAzElMeasurementModel::GetGroundStation()
{
    return theStation;
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
  Bool GeometricRangeRaDecMeasurementModel::ComputeMeasurement(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myMeasurements)
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

    myMeasurements.Set(3,range.GetMagnitude(),alpha,delta);
    
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

  Bool GeometricRangeRaDecMeasurementModel::ComputeCartesianPartialDerivative(const GroundStation &theStation, const Spacecraft &theSat, const Rvector &myCartDerivatives);
  {
      return false;
  }
  