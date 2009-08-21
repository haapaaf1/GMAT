//------------------------------------------------------------------------------
//                         RangeMeasurement
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/29
//
/**
 * Definition of the geometric range measurement.
 */
//------------------------------------------------------------------------------


#ifndef RANGEMEASUREMENT_HPP_
#define RANGEMEASUREMENT_HPP_

#include "GeometricMeasurement.hpp"

class RangeMeasurement: public GeometricMeasurement
{
public:
   RangeMeasurement(const std::string &name = "");
   virtual ~RangeMeasurement();
   RangeMeasurement(const RangeMeasurement &rm);
   RangeMeasurement& operator=(const RangeMeasurement &rm);

   virtual GmatBase*       Clone() const;
   virtual bool            Initialize();
   virtual const std::vector<RealArray>&
                           CalculateMeasurementDerivatives(GmatBase *obj,
                                 Integer id);

protected:
   bool                    Evaluate();
};

#endif /* RANGEMEASUREMENT_HPP_ */
