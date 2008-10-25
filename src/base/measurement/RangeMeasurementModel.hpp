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
 * Implements the geometric range measurement model.
 *
 */
//------------------------------------------------------------------------------

#ifndef _RANGEMEASUREMENTMODEL_HPP
#define	_RANGEMEASUREMENTMODEL_HPP

class GMAT_API RangeMeasurementModel : public MeasurmentModel
{
public:
    RangeMeasurementModel();
    RangeMeasurementModel(const MeasurementModel &RMM);
    RangeMeasurementModel& operator=(const RangeMeasurementModel &RMM);
    virtual ~RangeMeasurementModel();  
private:
    
protected:
    
}

#endif	/* _RANGEMEASUREMENTMODEL_HPP */

