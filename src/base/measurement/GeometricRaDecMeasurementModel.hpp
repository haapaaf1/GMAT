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
 * Implements the geometric right ascension and declination measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _RADECMEASUREMENTMODEL_HPP
#define	_RADECMEASUREMENTMODEL_HPP

class GMAT_API RaDecMeasurementModel : public MeasurmentModel
{
public:
    RaDecMeasurementModel();
    RaDecMeasurementModel(const RaDecMeasurementModel &RaDecMeasurementModel);
    RaDecMeasurementModel& operator=(const RaDecMeasurementModel &RaDecMeasurementModel);
    virtual ~RaDecMeasurementModel();  
private:
    
protected:
    
}

#endif	/* _RADECMEASUREMENTMODEL_HPP */

