//$Id$
//------------------------------------------------------------------------------
//                            LagrangeInterpolator
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number #####
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2009/09/28
//
/**
 * Declares LagrangeInterpolator class as specified in the GMAT Math Spec.
 */
//------------------------------------------------------------------------------
#ifndef LagrangeInterpolator_hpp
#define LagrangeInterpolator_hpp


#include "Interpolator.hpp"

class GMAT_API LagrangeInterpolator : public Interpolator
{
public:
   LagrangeInterpolator(const std::string &name = "", Integer dim = 1,
                        Integer ord = 7);
   virtual ~LagrangeInterpolator();
   LagrangeInterpolator(const LagrangeInterpolator &li);
   LagrangeInterpolator& operator=(const LagrangeInterpolator &li);
   
   // inherited from Interpolator
   virtual Integer      IsInterpolationFeasible(Real ind);
   virtual void         Clear();
   virtual bool         AddPoint(const Real ind, const Real *data);
   virtual bool         Interpolate(const Real ind, Real *results);
   
   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   
protected:
   static const Integer MAX_BUFFER_SIZE = 80;
   
   /// Order of interpolation
   Integer order;
   /// Actual size to be used
   Integer actualSize;
   /// Starting index used in finding center point
   Integer beginIndex;
   /// Starting index of interpolation range
   Integer startPoint;
   /// Value of the last point, to determine if the data buffer need updating
   Real  lastX;
   /// Array of ordered independent variables used
   Real  x[MAX_BUFFER_SIZE];
   /// Array of ordered dependent variables used
   Real  *y[MAX_BUFFER_SIZE];
   
   // Inherited methods that need some revision for LagrangeInterpolator
   virtual void AllocateArrays();
   virtual void CleanupArrays();
   virtual void CopyArrays(const LagrangeInterpolator &i);
   
   void    BuildDataPoints(Real ind);
   bool    IsDataNearCenter(Real ind);
   Integer FindStartingPoint(Real ind);
};


#endif // LagrangeInterpolator_hpp