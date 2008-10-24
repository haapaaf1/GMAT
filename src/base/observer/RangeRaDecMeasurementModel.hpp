//$Header$
//------------------------------------------------------------------------------
//                              RangeRaDecMeasurementModel
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

#ifndef _RangeRaDecMEASUREMENTMODEL_HPP
#define	_RangeRaDecMEASUREMENTMODEL_HPP

class GMAT_API RangeRaDecMeasurementModel : public MeasurmentModel
{
public:
    RangeRaDecMeasurementModel();
    RangeRaDecMeasurementModel(const RangeRaDecMeasurementModel &RangeRaDecMeasurementModel);
    RangeRaDecMeasurementModel& operator=(const RangeRaDecMeasurementModel &RangeRaDecMeasurementModel);
    virtual ~RangeRaDecMeasurementModel();  
private:
    
protected:
    
}

#endif	/* _RangeRaDecMEASUREMENTMODEL_HPP */

