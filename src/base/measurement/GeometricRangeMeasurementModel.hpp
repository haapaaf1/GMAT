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

#ifndef _GeometricRangeMeasurementModel_HPP
#define	_GeometricRangeMeasurementModel_HPP

#include "MeasurementModel.hpp"
#include "CoordinateConverter.hpp"

class GMAT_API GeometricRangeMeasurementModel : public MeasurementModel
{
public:
    GeometricRangeMeasurementModel(const std::string name = "");
    GeometricRangeMeasurementModel(const GeometricRangeMeasurementModel &RMM);
    GeometricRangeMeasurementModel& operator=(const GeometricRangeMeasurementModel &RMM);
    virtual ~GeometricRangeMeasurementModel();

    virtual GmatBase *Clone() const;

    bool Initialize() const;

    Integer     GetDependentParamID(const std::string &str) const;

    bool GetTheMeasurements(SpacePoint* theSpacePoint,
                            const A1Mjd &atTime,
                            LaGenMatDouble &theMeasurements);

    // Obtain the partials
    //bool GetThePartials(const std::string &param,
    //                    SpacePoint* theSpacePoint,
    //                    const A1Mjd &atTime,
    //                    LaGenMatDouble &theDerivatives);
    bool GetThePartials(const Integer &paramID,
                        SpacePoint* theSpacePoint,
                        const A1Mjd &atTime,
                        LaGenMatDouble &theDerivatives);

    bool ComputeCartesianPartialDerivative(SpacePoint* theSpacePoint,
                                           const A1Mjd &atTime,
                                           LaGenMatDouble &theDerivatives);
    bool ComputeXPartialDerivative(SpacePoint* theSpacePoint,
                                   const A1Mjd &atTime,
                                   LaGenMatDouble &theDerivatives);
    bool ComputeYPartialDerivative(SpacePoint* theSpacePoint,
                                   const A1Mjd &atTime,
                                   LaGenMatDouble &theDerivatives);
    bool ComputeZPartialDerivative(SpacePoint* theSpacePoint,
                                   const A1Mjd &atTime,
                                   LaGenMatDouble &theDerivatives);


protected:

   enum DEPENDENT_PARAMS
   {
      CARTESIAN_ID = MMDependentParamCount,
      X_ID,
      Y_ID,
      Z_ID,
      EndDependentParams
   };

   static const std::string  DEPENDENT_PARAMETER_TEXT[EndDependentParams-MMDependentParamCount];

};

#endif	/* _GeometricRangeMeasurementModel_HPP */

