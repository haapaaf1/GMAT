//$Header$
//------------------------------------------------------------------------------
//                              GeometricRangeMeasurementModel
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

#include "GeometricRangeMeasurementModel.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string GeometricRangeMeasurementModel::DEPENDENT_PARAMETER_TEXT[EndDependentParams - MMDependentParamCount] =
{
    "Cartesian",
    "X",
    "Y",
    "Z",
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
Integer GeometricRangeMeasurementModel::GetDependentParamID(const std::string &param) const
{

   for (Integer i = 0; i < EndDependentParams; i++)
   {
      if (param == DEPENDENT_PARAMETER_TEXT[i])
         return i;
   }

   // Return -1 indicating not found
   return -1;

}


GeometricRangeMeasurementModel::GeometricRangeMeasurementModel(const std::string name) :
   MeasurementModel        ("GeometricRange", name)
{
  numMeasurements = 1;

  // TODO: FIX the "new" statements
  // Name of each measurement returned
  //StringArray *myMeasurementNames = new StringArray[numMeasurements];
  //measurementNames = &myMeasurementNames;
  // Units of each measurement returned
  //measurementUnits = new String("Kilometers");

}

GeometricRangeMeasurementModel::GeometricRangeMeasurementModel(const GeometricRangeMeasurementModel &RMM):
   MeasurementModel        (RMM)
{

}

GeometricRangeMeasurementModel& GeometricRangeMeasurementModel::operator=(const GeometricRangeMeasurementModel &RMM)
{
   if (&RMM != this)
   {
      MeasurementModel::operator=(RMM);
   }
   return *this;
}

GeometricRangeMeasurementModel::~GeometricRangeMeasurementModel()
{
}

GmatBase *GeometricRangeMeasurementModel::Clone() const
{
   return new GeometricRangeMeasurementModel(*this);
}

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the measurement model.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::Initialize() const
{
    return true;
}

//------------------------------------------------------------------------------
// bool GetTheMeasurements(const SpacePoint *theSpacePoint,
//                            const A1Mjd &atTime,
//                            LaGenMatDouble &theMeasurements)
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurements between a ground station and a
 * spacecraft. The model returns true if a vector of measurments at
 * the time of the spacecraft state was successfully computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::GetTheMeasurements(
                                      SpacePoint *theSpacePoint,
                                      const A1Mjd &atTime,
                                      LaGenMatDouble &theMeasurements)
{

    if (theMeasurements.size(1) < 1) return false;

    // The target state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);

    Rvector6 range = targetState-gsState;

    theMeasurements(0,0) = range.GetMagnitude();

    return true;

}

//------------------------------------------------------------------------------
// bool GetThePartials(const Integer &paramID,
//                     const SpacePoint* theSpacePoint,
//                     const A1Mjd &atTime,
//                     LaGenMatDouble &theDerivatives)
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::GetThePartials(const Integer &paramID,
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
        case X_ID:
            return ComputeXPartialDerivative(theSpacePoint,atTime,
                                             theDerivatives);
            break;
        case Y_ID:
            return ComputeYPartialDerivative(theSpacePoint,atTime,
                                             theDerivatives);
            break;
        case Z_ID:
            return ComputeZPartialDerivative(theSpacePoint,atTime,
                                             theDerivatives);
            break;            
        default:
            return false;
    }

}


//------------------------------------------------------------------------------
// bool ComputeCartesianPartialDerivative(const SpacePoint* theSpacePoint,
//                                        const A1Mjd &atTime,
//                                        LaGenMatDouble &theDerivatives)
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the estimator
 * state. The code returns true if the partial derivatives were successfully
 * computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::ComputeCartesianPartialDerivative(
                                           SpacePoint* theSpacePoint,
                                           const A1Mjd &atTime,
                                           LaGenMatDouble &theDerivatives)
{
    // Test to make sure derivative matrix is properly sized
    if (theDerivatives.size(1) < 6) return false;

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);

    Rvector6 range = targetState-gsState;
    
    Real rangeMag = range.GetMagnitude();

    if (rangeMag > 0) {
        theDerivatives(0,0) = range(0)/rangeMag;
	theDerivatives(0,1) = range(1)/rangeMag;
	theDerivatives(0,2) = range(2)/rangeMag;
	theDerivatives(0,3) = 0;
	theDerivatives(0,4) = 0;
	theDerivatives(0,5) = 0;
	return true;
    }
    else
    {
	return false;
    }
}

//------------------------------------------------------------------------------
// bool ComputeXPartialDerivative(const SpacePoint* theSpacePoint,
//                                const A1Mjd &atTime,
//                                LaGenMatDouble &theDerivatives);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the cartesian
 * coordinate x. The code returns true if the partial derivative was
 * successfully computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::ComputeXPartialDerivative(
                                           SpacePoint* theSpacePoint,
                                           const A1Mjd &atTime,
                                           LaGenMatDouble &theDerivatives)
{
    // Test to make sure derivative matrix is properly sized
    if (theDerivatives.size(1) < 1) return false;

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);

    Rvector6 range = targetState-gsState;

    Real rangeMag = range.GetMagnitude();

    if (rangeMag > 0) {
        theDerivatives(0,0) = range(0)/rangeMag;
	return true;
    }
    else
    {
	return false;
    }
}

//------------------------------------------------------------------------------
// bool ComputeYPartialDerivative(const SpacePoint* theSpacePoint,
//                                const A1Mjd &atTime,
//                                LaGenMatDouble &theDerivatives);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the cartesian
 * coordinate y. The code returns true if the partial derivative was
 * successfully computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::ComputeYPartialDerivative(
                                           SpacePoint* theSpacePoint,
                                           const A1Mjd &atTime,
                                           LaGenMatDouble &theDerivatives)
{
    // Test to make sure derivative matrix is properly sized
    if (theDerivatives.size(1) < 1) return false;

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);

    Rvector6 range = targetState-gsState;

    Real rangeMag = range.GetMagnitude();

    if (rangeMag > 0) {
        theDerivatives(0,0) = range(1)/rangeMag;
	return true;
    }
    else
    {
	return false;
    }
}

//------------------------------------------------------------------------------
// bool ComputeZPartialDerivative(const SpacePoint* theSpacePoint,
//                                const A1Mjd &atTime,
//                                LaGenMatDouble &theDerivatives);
//------------------------------------------------------------------------------
/**
 * Code used to simulate measurement derivatives with respect to the cartesian
 * coordinate z. The code returns true if the partial derivative was
 * successfully computed.
 */
//------------------------------------------------------------------------------
bool GeometricRangeMeasurementModel::ComputeZPartialDerivative(
                                           SpacePoint* theSpacePoint,
                                           const A1Mjd &atTime,
                                           LaGenMatDouble &theDerivatives)
{
    // Test to make sure derivative matrix is properly sized
    if (theDerivatives.size(1) < 1) return false;

    // The satellite state in MJ2000 Cartesian coordinates
    Rvector6 targetState = theSpacePoint->GetMJ2000State(atTime);

    // The groundstation position and velocity in MJ2000 Cartesian coordinates
    Rvector6 gsState = theStation->GetMJ2000State(atTime);

    Rvector6 range = targetState-gsState;

    Real rangeMag = range.GetMagnitude();

    if (rangeMag > 0) {
        theDerivatives(0,0) = range(2)/rangeMag;
	return true;
    }
    else
    {
	return false;
    }
}

