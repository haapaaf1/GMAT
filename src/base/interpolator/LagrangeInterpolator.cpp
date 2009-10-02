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
 * Implements LagrangeInterpolator class as specified in the GMAT Math Spec.
 */
//------------------------------------------------------------------------------

#include "LagrangeInterpolator.hpp"
#include "InterpolatorException.hpp"
#include "RealUtilities.hpp"         // for GmatMathUtil::Abs()
#include "MessageInterface.hpp"

//#define DEBUG_LAGRANGE_BUILD
//#define DEBUG_LAGRANGE_INTERPOLATE

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  LagrangeInterpolator(const std::string &name, Integer dim, Integer ord)
//------------------------------------------------------------------------------
/**
 * Constructs lagrange interpolator (default constructor).
 * 
 * @param <name>  Name for this interpolator ("")
 * @param <dim>   Dimension of data that gets interpolated (1).
 * @param <order> The order of interpolation (7)
 */
//------------------------------------------------------------------------------
LagrangeInterpolator::LagrangeInterpolator(const std::string &name, Integer dim,
                                           Integer ord) :
   Interpolator  (name, "LagrangeInterpolator", dim),
   order         (ord),
   startPoint    (0),
   lastX         (-9.9999e75)
{
   // Made bufferSize twice bigger than dimension, so that we can collect more
   // data to place requested ind parameter in the near to the center of the
   // interpolation range.
   requiredPoints = order + 1;
   bufferSize = requiredPoints * 2;
   if (bufferSize > MAX_BUFFER_SIZE)
      bufferSize = MAX_BUFFER_SIZE;
   
   #ifdef DEBUG_LAGRANGE
   MessageInterface::ShowMessage
      ("LagrangeInterpolator() order=%d, requiredPoints=%d, bufferSize=%d\n",
       order, requiredPoints, bufferSize);
   #endif
   
   for (Integer i = 0; i < bufferSize; ++i)
   {
      x[i]  = -9.9999e75;
      y[i]  = NULL;
   }
}


//------------------------------------------------------------------------------
//  ~LagrangeInterpolator()
//------------------------------------------------------------------------------
/**
 * Destroys lagrange interpolator (destructor).
 */
//------------------------------------------------------------------------------
LagrangeInterpolator::~LagrangeInterpolator()
{
   CleanupArrays();
}


//------------------------------------------------------------------------------
//  LagrangeInterpolator(const LagrangeInterpolator &li)
//------------------------------------------------------------------------------
/**
 * Constructs lagrange interpolator, based on another (copy constructor).
 * 
 * @param li The original that is being copied.
 */
//------------------------------------------------------------------------------
LagrangeInterpolator::LagrangeInterpolator(const LagrangeInterpolator &li) :
   Interpolator   (li),
   order          (li.order),
   startPoint     (li.startPoint),
   lastX          (li.lastX)
{
   Integer i;
   for (i = 0; i < bufferSize; ++i)
   {
      x[i]  = -9.9999e75;
      y[i]  = NULL;
   }
}


//------------------------------------------------------------------------------
//  LagrangeInterpolator& operator=(const LagrangeInterpolator &li)
//------------------------------------------------------------------------------
/**
 * Sets this lagrange interpolator to match another (assignment operator).
 * 
 * @param li The original that is being copied.
 * 
 * @return A reference to the copy (aka *this).
 */
//------------------------------------------------------------------------------
LagrangeInterpolator&
LagrangeInterpolator::operator=(const LagrangeInterpolator &li)
{
   if (&li == this)
      return *this;
   
   CleanupArrays();
   CopyArrays(li);
   
   order      = li.order;
   startPoint = li.startPoint;
   lastX      = li.lastX;
   
   return *this;
}


//------------------------------------------------------------------------------
//  void Clear()
//------------------------------------------------------------------------------
/**
 * @see Interpolator
 */
//------------------------------------------------------------------------------
void LagrangeInterpolator::Clear()
{
   Interpolator::Clear();
   
   startPoint = 0;
   for (Integer i = 0; i < bufferSize; ++i)
      x[i] = -9.9999e75;
}


//------------------------------------------------------------------------------
//  bool AddPoint(const Real ind, const Real *data)
//------------------------------------------------------------------------------
/**
 * See Interpolator
 *
 * @exception thrown when independent data direction changes
 */
//------------------------------------------------------------------------------
bool LagrangeInterpolator::AddPoint(const Real ind, const Real *data)
{
   if (ind < previousX)
   {
      InterpolatorException ie;
      ie.SetDetails("The independent data provided is not monotonic, current dada is %f, "
                    "previous data is %f", ind, previousX);
      throw ie;
   }
   
   return Interpolator::AddPoint(ind, data);
}


//------------------------------------------------------------------------------
//  bool Interpolate(const Real ind, Real *results)
//------------------------------------------------------------------------------
/**
 * Perform the interpolation.
 * 
 * This method is the core interface for the lagrange interpolation.
 * See the GMAT math spec for the algorithm.
 * 
 * @param ind       The value of the independent parameter.
 * @param results   Data structure for the estimates.
 * 
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool LagrangeInterpolator::Interpolate(const Real ind, Real *results)
{
   #ifdef DEBUG_LAGRANGE_INTERPOLATE
   MessageInterface::ShowMessage("Lagrange::Interpolate() entered, ind=%f\n", ind);
   #endif
   
   // Check for interpolation feasibility
   if (!IsInterpolationFeasible(ind))
      return false;
   
   // Build data points before and after independent value
   BuildDataPoints(ind);
   
   // Now interpolate using the alorithm in the Math Spec.
   Real *products = new Real[dimension];
   Real *estimates = new Real[dimension];
   
   for (Integer dim = 0; dim < dimension; dim++)
      estimates[dim] = 0.0;
   
   for (Integer i = startPoint; i < startPoint + order; i++)
   {
      for (Integer dim = 0; dim < dimension; dim++)
         products[dim] = y[i][dim];
      
      for (Integer j = startPoint; j < startPoint + order; j++)
      {
         if (i != j)
         {
            for (Integer dim = 0; dim < dimension; dim++)
               products[dim] = products[dim] * (ind - x[j]) / (x[i]- x[j]);
         }
      }
      
      for (Integer dim = 0; dim < dimension; dim++)
         estimates[dim] = estimates[dim] + products[dim];
   }
   
   // copy results
   for (Integer dim = 0; dim < dimension; dim++)
   {
      results[dim] = estimates[dim];
      #ifdef DEBUG_LAGRANGE_INTERPOLATE
      MessageInterface::ShowMessage("   results[%d] = %f\n", dim, results[dim]);
      #endif
   }
   
   delete [] products;
   delete [] estimates;
   
   #ifdef DEBUG_LAGRANGE_INTERPOLATE
   MessageInterface::ShowMessage("Lagrange::Interpolate() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the LagrangeInterpolator.
 *
 * @return clone of the LagrangeInterpolator.
 */
//------------------------------------------------------------------------------
GmatBase* LagrangeInterpolator::Clone() const
{
   return (new LagrangeInterpolator(*this));
}


//---------------------------------
//  protected methods
//---------------------------------

//------------------------------------------------------------------------------
//  void AllocateArrays()
//------------------------------------------------------------------------------
/**
 * Allocates lagrange buffers and calls the base method to build the ring buffer.  
 */
//------------------------------------------------------------------------------
void LagrangeInterpolator::AllocateArrays()
{
   Interpolator::AllocateArrays();
   
   Integer i;
   for (i = 0; i < bufferSize; ++i)
   {
      x[i] = -9.9999e75;
      y[i]  = new Real[dimension];
   }
   
   latestPoint = -1;
}


//------------------------------------------------------------------------------
//  void CleanupArrays()
//------------------------------------------------------------------------------
/**
 * Frees the memory used by the lagrange buffer and calls the base method to 
 * manage the ring buffer.
 */
//------------------------------------------------------------------------------
void LagrangeInterpolator::CleanupArrays()
{
   Integer i = 0;
   if (y[i])
   {
      for (i = 0; i < bufferSize; ++i)
      {
         x[i] = -9.9999e75;
         delete [] y[i];
         y[i] = NULL;
      }
   }
   
   Interpolator::CleanupArrays();
}


//------------------------------------------------------------------------------
//  void CopyArrays(const Interpolator &i)
//------------------------------------------------------------------------------
/**
 * Copies the ring buffer from one Interpolator to this one.  
 * 
 * @param i The Interpolator that supplies the data copied to this one.
 */
//------------------------------------------------------------------------------
void LagrangeInterpolator::CopyArrays(const LagrangeInterpolator &i)
{
   Interpolator::CopyArrays(i);
   Integer j;
   for (j = 0; j < bufferSize; ++j)
   {
      x[j] = i.x[j];
      memcpy( y[j],  i.y[j], dimension*sizeof(Real));
   }
}


//------------------------------------------------------------------------------
//  bool IsInterpolationFeasible(Real ind)
//------------------------------------------------------------------------------
/**
 * Checks if interpolation is feasible.
 *
 * @param ind The value of the independent parameter.
 * @return true if feasible, false otherwise.
 */
//------------------------------------------------------------------------------
bool LagrangeInterpolator::IsInterpolationFeasible(Real ind)
{
   #ifdef DEBUG_LAGRANGE_FEASIBLE
   MessageInterface::ShowMessage
      ("LagrangeInterpolator::IsInterpolationFeasible() ind=%f, pointCount = %d, "
       "requiredPoints = %d\n", ind, pointCount, requiredPoints);
   #endif
   
   // If not enough data points, throw an exception
   if (pointCount < requiredPoints)
   {
      InterpolatorException ie;
      ie.SetDetails("There is not enough data to interpolate %f, number of "
                    "required points is %d, received %d points", ind, requiredPoints,
                    pointCount);
      throw ie;
   }
   
   SetRange();
   
   #ifdef DEBUG_LAGRANGE_FEASIBLE
   MessageInterface::ShowMessage
      ("   range1 = %f, range2 = %f\n", range[0], range[1]);
   #endif
   
   // If independent data not within the range, throw an exception
   if (ind < range[0] || ind > range[1])
   {
      InterpolatorException ie;
      ie.SetDetails("The requested data %f is not within the data range of [%f : %f]",
                    ind, range[0],  range[1]);
      throw ie;
   }
   
   #ifdef DEBUG_LAGRANGE_FEASIBLE
   MessageInterface::ShowMessage
      ("LagrangeInterpolator::IsInterpolationFeasible() returning true\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
//  void BuildDataPoints(Real ind)
//------------------------------------------------------------------------------
/**
 * Use the ring buffer to load the arrays used to build the lagrange buffer.
 * Lagrange buffer should be constructed so that requested data sits in the
 * middle of the buffer as possible.
 */
//------------------------------------------------------------------------------
void LagrangeInterpolator::BuildDataPoints(Real ind)
{
   #ifdef DEBUG_LAGRANGE_BUILD
   MessageInterface::ShowMessage("Lagrange::BuildDataPoints() entered, ind=%f\n", ind);
   #endif
   
   Integer i, j, start = 0;
   Real sign = (dataIncreases ? 1.0 : -1.0);
   Real temp = sign * independent[0];
   
   // Compute actual size to use since bufferSize is twice of order
   Integer actualSize = bufferSize;
   if (actualSize > pointCount)
      actualSize = pointCount;
   
   #ifdef DEBUG_LAGRANGE_BUILD
   MessageInterface::ShowMessage("   temp = %f, actualSize=%d\n", temp, actualSize);
   #endif
   
   for (i = 1; i < actualSize; ++i)
   {
      if (sign*independent[i] < temp)
      {
         start = i;
         temp = sign*independent[i];
      }
   }
   
   for (i = 0; i < actualSize; ++i, ++start)
   {
      if (start == bufferSize)
         start = 0;
      x[i] = independent[start];
      
      #ifdef DEBUG_LAGRANGE_BUILD
      MessageInterface::ShowMessage
         ("   start = %2d, x[%2d] = %f\n", start, i, x[i]);
      #endif
      for (j = 0; j < dimension; j++)
         y[i][j] = dependent[start][j];
   }
   
   // Find the data point near the requested value
   Real minDiff = 1.0e30, diff, meanX;
   Integer qMin = 0;
   for (Integer q = 0; q < actualSize; ++q)
   {
      meanX = ( x[q + dimension] + x[q] ) / 2;
      diff = GmatMathUtil::Abs( meanX - ind );
      if (diff < minDiff)
      {
         qMin = q;
         minDiff = diff;
      }
   }
   
   // Assign to starting point
   startPoint = qMin;
   
   // We don't want to pass the actual data size, so adjust
   if (qMin + order > actualSize)
      startPoint = startPoint - abs(order - qMin);
   
   #ifdef DEBUG_LAGRANGE_BUILD
   MessageInterface::ShowMessage
      ("   startPoint after adjust = %d\n", startPoint);
   #endif
   
   if (startPoint < 0)
      startPoint = 0;
   
   #ifdef DEBUG_LAGRANGE_BUILD
   MessageInterface::ShowMessage
      ("LagrangeInterpolator::BuildDataPoints() leaving, qMin = %d, startPoint = %d\n",
       qMin, startPoint);
   #endif
}


