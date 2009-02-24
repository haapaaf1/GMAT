//$Header$
//------------------------------------------------------------------------------
//                              RangeMeasurementModel
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

#ifndef _AzElMEASUREMENTMODEL_HPP
#define	_AzElMEASUREMENTMODEL_HPP

class GMAT_API AzElMeasurementModel : public MeasurmentModel
{
public:
    AzElMeasurementModel();
    AzElMeasurementModel(const AzElMeasurementModel &AzElMeasurementModel);
    AzElMeasurementModel& operator=(const AzElMeasurementModel &AzElMeasurementModel);
    virtual ~AzElMeasurementModel();  
private:
    
protected:
    
}

#endif	/* _AzElMEASUREMENTMODEL_HPP */

