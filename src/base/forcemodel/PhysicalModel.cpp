//$Header$
//------------------------------------------------------------------------------
//                              PhysicalModel
//------------------------------------------------------------------------------
// *** File Name : PhysicalModel.cpp
// *** Created   : October 1, 2002
// **************************************************************************
// ***  Developed By  :  Thinking Systems, Inc. (www.thinksysinc.com)     ***
// ***  For:  Flight Dynamics Analysis Branch (Code 572)                  ***
// ***  Under Contract:  P.O.  GSFC S-66617-G                             ***
// ***                                                                    ***
// ***  Copyright U.S. Government 2002                                    ***
// ***  Copyright United States Government as represented by the          ***
// ***  Administrator of the National Aeronautics and Space               ***
// ***  Administration                                                    ***
// ***                                                                    ***
// ***  This software is subject to the Sofware Usage Agreement described ***
// ***  by NASA Case Number GSC-14735-1.  The Softare Usage Agreement     ***
// ***  must be included in any distribution.  Removal of this header is  ***
// ***  strictly prohibited.                                              ***
// ***                                                                    ***
// ***                                                                    ***
// ***  Header Version: July 12, 2002                                     ***
// **************************************************************************
// Module Type               : ANSI C++ Source
// Development Environment   : Visual C++ 7.0
// Modification History      : 11/26/2002 - D. Conway, Thinking Systems, Inc.
//                             Original delivery
//
//                           : 1/8/2003 - D. Conway, Thinking Systems, Inc.
//                             Updated interfaces based on GSFC feedback
//
//                           : 2/5/2003 - D. Conway, Thinking Systems, Inc.
//                             Incorporated the Derivative class into this
//                             class and removed Derivative from the class
//                             heirarchy
//
//                           : 3/3/2003 - D. Conway, Thinking Systems, Inc.
//                             Updated parameter strings to include units;
//                             Added code to switch between relative and
//                             absolute error calculations
//
//                           : 09/24/2003 - W. Waktola, Missions Applications Branch
//                              Changes:
//                                - Updated style using GMAT cpp style guide//
//
//                           : 10/15/2003 - W. Waktola, Missions Applications Branch
//                              Changes:
//                                - All double types to Real types
//                                - All primitive int types to Integer types
//                                - virtual char* GetParameterName(const int parm) const to
//                                  virtual std::string GetParameterName(const int parm) const
//                                - Changed GetParameterName() from if statements to switch statements
//                              Removals:
//                                - static Real parameterUndefined
//                                - SetUndefinedValue()
//                                - ParameterCount()
//                                - GetParameter()
//                                - SetParameter()
//                              Additions:
//                                - PARAMTER_TEXT[]
//                                - PARAMETER_TYPE[]
//                                - GetParameterText()
//                                - GetParameterID()
//                                - GetParameterType()
//                                - GetParameterTypeString()
//                                - GetRealParameter()
//                                - SetRealParameter()
//
//                           : 10/20/2003 - W. Waktola, Missions Applications Branch
//                              Changes:
//                                - Fixed format.
//                                - parameterCount to PhysicalModelParamCount.
//                              Removals:
//                                - GetParameterName()
//
//                           : 10/23/2003 - D. Conway, Thinking Systems, Inc. &
//                                          W. Waktola, Missions Applications Branch
//                              Changes:
//                                - Changed constructor from PhysicalModel::PhysicalModel(void) to
//                                  PhysicalModel(Gmat::ObjectType typeId, const std::string &typeStr,
//                                  const std::string &nomme = "")
//                                - Added parameterCount = 1 in constructors
//                                - In SetErrorThreshold(), changed statement from relativeErrorThreshold = fabs(thold);
//                                  to relativeErrorThreshold = (thold >= 0.0 ? thold : -thold);
//
// **************************************************************************

#include "PhysicalModel.hpp"
#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "CelestialBody.hpp"
#include "MessageInterface.hpp"

//---------------------------------
// static data
//---------------------------------
const std::string
PhysicalModel::PARAMETER_TEXT[PhysicalModelParamCount - GmatBaseParamCount] =
{
   "Epoch",
   "ElapsedSeconds",
};

const Gmat::ParameterType
PhysicalModel::PARAMETER_TYPE[PhysicalModelParamCount - GmatBaseParamCount] =
{
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
};

//---------------------------------
// public
//---------------------------------

//------------------------------------------------------------------------------
// PhysicalModel(Gmat::ObjectType typeId, const std::string &typeStr,
//    const std::string &nomme = "")
//------------------------------------------------------------------------------
/**
 * Constructor for the Physical Model base class
 *
 * This constructor sets the size of the physical model to one variable, and
 * NULLs the state pointer.  Derived classes should set the dimension parameter
 * to a more appropriate value; the Initialize() method is used to allocate 
 * state data array.
 */
//------------------------------------------------------------------------------
PhysicalModel::PhysicalModel(Gmat::ObjectType id, const std::string &typeStr,
                             const std::string &nomme) :
   GmatBase                    (id, typeStr, nomme),
   dimension                   (1),
   initialized                 (false),
   stateChanged                (false),
   modelState                  (NULL),
   epoch                       (21545.0),
   elapsedTime                 (0.0),
   deriv                       (NULL),
   relativeErrorThreshold      (0.10),
   solarSystem                 (NULL)
{
   parameterCount = PhysicalModelParamCount;
}
             
//------------------------------------------------------------------------------
// PhysicalModel::~PhysicalModel(void)
//------------------------------------------------------------------------------
/**
 * Destructor for the model
 * If the state array has been allocated, this destructor destroys it.
 */
//------------------------------------------------------------------------------
PhysicalModel::~PhysicalModel(void)
{
   if (modelState)
      delete [] modelState;

   if (deriv)
      delete [] deriv;
}

//------------------------------------------------------------------------------
// PhysicalModel::PhysicalModel(const PhysicalModel& pm)
//------------------------------------------------------------------------------
/**
 * The copy constructor for the physical model
 */
//------------------------------------------------------------------------------
PhysicalModel::PhysicalModel(const PhysicalModel& pm) :
   GmatBase                    (pm),
   dimension                   (pm.dimension),
   initialized                 (pm.initialized),
   stateChanged                (pm.stateChanged),
   epoch                       (pm.epoch),
   elapsedTime                 (pm.elapsedTime),
   relativeErrorThreshold      (pm.relativeErrorThreshold),
   solarSystem                 (pm.solarSystem)
{
   if (pm.modelState != NULL) 
   {
      modelState = new Real[dimension];
      if (modelState != NULL) 
         memcpy(modelState, pm.modelState, dimension * sizeof(Real));
      else
         initialized = false;
   }
   else
      modelState = NULL;

   if (pm.deriv != NULL) 
   {
      deriv = new Real[dimension];
      if (deriv != NULL) 
         memcpy(deriv, pm.deriv, dimension * sizeof(Real));
      else
         initialized = false;
   }
   else
      deriv = NULL;

   parameterCount = PhysicalModelParamCount;
}

//------------------------------------------------------------------------------
// PhysicalModel& PhysicalModel::operator=(const PhysicalModel& pm)
//------------------------------------------------------------------------------
/**
 * The assignment operator for the physical model
 */
//------------------------------------------------------------------------------
PhysicalModel& PhysicalModel::operator=(const PhysicalModel& pm)
{
   if (&pm == this)
      return *this;

   GmatBase::operator=(pm);
   dimension   = pm.dimension;
   initialized = pm.initialized;
   epoch       = pm.epoch;
   elapsedTime = pm.elapsedTime;
   
   if (pm.modelState) 
   {
      if (modelState) 
      {
         delete [] modelState;
         modelState = NULL;
      }
      modelState = new Real[dimension];
      if (modelState != NULL) 
         memcpy(modelState, pm.modelState, dimension * sizeof(Real));
      else
         initialized = false;
   
      stateChanged = pm.stateChanged;
   }
   
   if (pm.deriv) 
   {
      if (deriv)
      {
         delete [] deriv;
         deriv = NULL;
      }
      deriv = new Real[dimension];
      if (deriv != NULL) 
         memcpy(deriv, pm.deriv, dimension * sizeof(Real));
      else
         initialized = false;
   }
   
   return *this;
}

//  //------------------------------------------------------------------------------
//  // CelestialBody* GetBody()
//  //------------------------------------------------------------------------------
//  /**
//   * 
//   */
//  //------------------------------------------------------------------------------
//  CelestialBody* PhysicalModel::GetBody()
//  {
//     return theBody;
//  }

//  //------------------------------------------------------------------------------
//  // void SetBody(CelestialBody *body)
//  //------------------------------------------------------------------------------
//  /**
//   *
//   */
//  //------------------------------------------------------------------------------
//  void PhysicalModel::SetBody(CelestialBody *body)
//  {  
//      if (body != NULL)
//      {
//          if (theBody != NULL)
//          {
//              delete theBody;
//          }
//      }
  
//      theBody = body;
//  //    mu = theBody->GetGravitationalConstant();
//  }

//  //------------------------------------------------------------------------------
//  // bool SetBody(const std::string &name)
//  //------------------------------------------------------------------------------
//  /**
//   *
//   */
//  //------------------------------------------------------------------------------
//  bool PhysicalModel::SetBody(const std::string &name)
//  {
//      CelestialBody *body = solarSystem->GetBody(name);
//      if (body != NULL)
//      {
//          SetBody(body);
//          return true;
//      }
//      return false;
//  }

//------------------------------------------------------------------------------
// bool PhysicalModel::Initialize(void)
//------------------------------------------------------------------------------
/**
 * Prepare the physical model for use
 *
 * This method allocates the state and deriv arrays, and can be overridden to 
 * perform other actions for the system setup.
 *
 * Note that deriv is allocated even if it is not used.  This feature may be 
 * PhysicalModelremoved in a later release.
 */
//------------------------------------------------------------------------------
bool PhysicalModel::Initialize(void)
{ 
   //MessageInterface::ShowMessage("PhysicalModel::Initialize() entered\n");
   
   if (modelState) {
      delete [] modelState;
      modelState = NULL;

      initialized = false;
   }

   if (deriv) {
      delete [] deriv;
      deriv = NULL;
   }

   modelState = new Real[dimension];
   if (modelState != NULL) {
      deriv = new Real[dimension];
      if (deriv)
         initialized = true;
      else
         initialized = false;
   }
  
   return initialized;
}

//------------------------------------------------------------------------------
// Real PhysicalModel::GetErrorThreshold(void) const
//------------------------------------------------------------------------------
/**
 * Returns the threshold for switching between relative and absolute error
 */
//------------------------------------------------------------------------------
Real PhysicalModel::GetErrorThreshold(void) const
{
   return relativeErrorThreshold;
   // DJC: Should relativeErrorThreshold be added to the list of parameters? :
}

//------------------------------------------------------------------------------
// bool PhysicalModel::SetErrorThreshold(const Real thold)
//------------------------------------------------------------------------------
/**
 * Sets the threshold for switching between relative and absolute error
 *
 * @param thold         The new threshold value
 */
//------------------------------------------------------------------------------
bool PhysicalModel::SetErrorThreshold(const Real thold)
{
   relativeErrorThreshold = (thold >= 0.0 ? thold : -thold);
   return true;
}

//------------------------------------------------------------------------------
// Integer PhysicalModel::GetDimension(void)
//------------------------------------------------------------------------------
/**
 * Accessor method used by Propagator class to determine # of vars
 * The Propagator class evolves the system being modeled by advancing some
 * number of variables.  The count of the variables must be coordinated 
 * between the propagator and the physical model of the system; 
 * GetDimension() is called by the Propagator class to obtain this 
 * information from the PhysicalModel class.
*/
//------------------------------------------------------------------------------
Integer PhysicalModel::GetDimension(void)
{
   return dimension;
}

//------------------------------------------------------------------------------
// void PhysicalModel::SetDimension(Integer n)
//------------------------------------------------------------------------------
/**
 * Accessor method used to set # of vars
 * Use this method to reset the count of the variables modeled by the physical 
 * model of the system for models that allow for changes in this value.
*/
//------------------------------------------------------------------------------
void PhysicalModel::SetDimension(Integer n)
{
   dimension = n;
   initialized = false;
}

//------------------------------------------------------------------------------
// Real * PhysicalModel::GetState(void)
//------------------------------------------------------------------------------
/**
 * Accessor method used to access the state array
 * Use this method with care -- it exposes the internal array of state data to
 * external users.  The Propagator and Integrator classes can use this access to
 * make system evelotion more efficient, but at the cost of loss of 
 * encapsulation of the state data.
 */
//------------------------------------------------------------------------------
Real * PhysicalModel::GetState(void)
{
   return modelState;
}

//------------------------------------------------------------------------------
// void PhysicalModel::SetState(const Real * st)
//------------------------------------------------------------------------------
/**
 * Used to set the elements of the state array
 *
 * @param st    Array of data containing the desired values for the state elements
 */
//------------------------------------------------------------------------------
void PhysicalModel::SetState(const Real * st)
{
   for (Integer i = 0; i < dimension; i++)
      modelState[i] = st[i];
   stateChanged = true;
}

//------------------------------------------------------------------------------
// const Real* PhysicalModel::GetDerivativeArray(void)
//------------------------------------------------------------------------------
/**
 * Accessor for the derivative array
 * This method returns a pointer to the derivative array.  The Predictor-
 * Correctors need this access in order to extrapolate the next state.
 */
//------------------------------------------------------------------------------
const Real* PhysicalModel::GetDerivativeArray(void)
{
   return deriv;
}

//------------------------------------------------------------------------------
// void PhysicalModel::IncrementTime(Real dt)
//------------------------------------------------------------------------------
/**
 * Used to increment the internal time counter
 *
 * @param dt    Amount of time to increment by (usually in seconds)
 */
//------------------------------------------------------------------------------
void PhysicalModel::IncrementTime(Real dt)
{
   elapsedTime += dt;
   stateChanged = true;
}

//------------------------------------------------------------------------------
// Real PhysicalModel::GetTime(void)
//------------------------------------------------------------------------------
/**
 * Read accessor for the time elapsed 
 * Use this method to track the elapsed time for the model.  You can set the 
 * system to start from a non-zero time by setting the value for the elapsedTime
 * parameter to the desired start value.  See the SetTime parameter for the  
 * write accessor.
 */
//------------------------------------------------------------------------------
Real PhysicalModel::GetTime(void)
{
   return elapsedTime;
}

//------------------------------------------------------------------------------
// void PhysicalModel::SetTime(Real t)
//------------------------------------------------------------------------------
/**
 * Write accessor for the total time elapsed 
 * Use this method to set time for the model
 */
//------------------------------------------------------------------------------
void PhysicalModel::SetTime(Real t)
{
   elapsedTime = t;
}


//------------------------------------------------------------------------------
// bool PhysicalModel::GetDerivatives(Real * state, Real dt, Integer order)
//------------------------------------------------------------------------------
/**
 * Method invoked to calculate derivatives
 * This method is invoked to fill the deriv array with derivative information 
 * for the system that is being integrated.  It uses the state and elapsedTime 
 * parameters, along with the time interval dt passed in as a parameter, to 
 * calculate the derivative information at time \f$t=t_0+t_{elapsed}+dt\f$.
 *
 * @param dt            Additional time increment for the derivatitive 
 *                      calculation; defaults to 0.
 * @param state         Pointer to the current state data.  This can differ
 *                      from the PhysicalModel state if the subscribing
 *                              integrator samples other state values during 
 *                              propagation.  (For example, the Runge-Kutta integrators 
 *                      do this during the stage calculations.)
 * @param order         The order of the derivative to be taken (first 
 *                              derivative, second derivative, etc)
 *
 * @return                      true if the call succeeds, false on failure.  This default 
 *                      implementation always returns false.
 */
//------------------------------------------------------------------------------
bool PhysicalModel::GetDerivatives(Real * state, Real dt, Integer order)
{
   return false;
}

//------------------------------------------------------------------------------
// Real PhysicalModel::EstimateError(Real * diffs, Real * answer) const
//------------------------------------------------------------------------------
/**
 * Interface used to estimate the error in the current step
 *
 * The method calculates the largest local estimate of the error from the 
 * integration given the components of the differences calculated by the 
 * integrator.  It returns the largest error estimate found.  
 *  
 * The default implementation returns the largest single relative component 
 * found based on the input arrays.  In other words, the implementation provided 
 * here returns the largest component of the following vector:
 *
 * \f[\vec \epsilon = |{{{EE}_n}\over{x_n^f - x_n^i}}|\f]
 *
 * subject to the discussion of the relativeErrorThreshold parameter, below.
 *  
 * There are several alternatives that users of this class can implement: the 
 * error could be calculated based on the largest error in the individual 
 * components of the state vector, as the magnitude of the state vector (that 
 * is, the L2 (rss) norm of the error estimate vector).  The estimated error 
 * should never be negative, so a return value less than 0.0 can be used to 
 * indicate an error condition.
 *
 * One item to note in this implementation is the relativeErrorThreshold local 
 * variable.  This parameter looks at the difference between the initial state 
 * of the variables and the state after the integration.  If that difference is 
 * smaller in magnitude than the value of relativeErrorThreshold, then the error 
 * value calculated is the absolute error; if it is larger, the calculated value 
 * is scaled by the difference.  In other words, given
 *
 * \f[\Delta^i = |r^i(t + \delta t) - r^i(t)|\f]
 *
 * this method will return the largest error in the final states if each 
 * component of \f$\Delta^i\f$ is smaller than relativeErrorThreshold, and will 
 * return the largest value of the error divided by the corresponding 
 * \f$\Delta^i\f$ if each component of \f$\Delta^i\f$ is larger than 
 * relativeErrorThreshold.  This property lets the integrators step over small
 * discontinuities (for example, shadow crossings for spacecraft orbital models) 
 * without hanging.
 *    
 * @param diffs         Array of differences calculated by the integrator.  This array 
 *                      must be the same size as the state vector
 * @param answer        Candidate new state from the integrator
 *
 * @returns             The maximum calculated error
 */
//------------------------------------------------------------------------------
Real PhysicalModel::EstimateError(Real * diffs, Real * answer) const
{
   Real retval = 0.0, err, delta;

   for (Integer i = 0; i < dimension; ++i)
   {
      delta = answer[i] - modelState[i];
      if (delta > relativeErrorThreshold)
         err = fabs(diffs[i] / delta);
      else
         err = fabs(diffs[i]);
      if (err > retval)
         retval = err;
   }

   return retval;
}

//------------------------------------------------------------------------------
// bool PhysicalModel::GetComponentMap(Integer * map, Integer order) const
//------------------------------------------------------------------------------
/**
 * Used to get the mapping in the state variable between components 
 * This method is used to obtain a mapping between the elements of the state
 * vector.  It is used, for instance, to map the position components to the
 * velocity components for a spacecraft state vector so that the 
 * Runge-Kutta-Nystrom integrators can obtain the velocity information they 
 * need.  The default implementation simply returns false.
 *
 * When the model for the class can provide a map for the data elements, it will
 * fill in the array of elements with either a "no map" indicator of -1, or the 
 * mapping between the selected element and its corresponding derivative.  These
 * data are placed into the input "map" array, which must be an integer array
 * sized to match the dimension of the model.  The user also specifies the order
 * of the mapping; for instance, to obtain the mapping for first derivative 
 * information, the order is set to 1.
 *
 * An example of the return data is in order for this method.  Suppose that the 
 * state vector consists of six elements, (X, Y, Z, Vx, Vy, Vz).  A user can 
 * request the mapping for the first derivative components by calling 
 * GetComponentMap(map, 1).  The array, map, that is returned will contain these
 * data: (3, 4, 5, -1, -1, -1).
 *
 *  @param map          Array that will contain the mapping of the elements
 *  @param order        The order for the mapping (1 maps 1st derivatives to their base 
 *                      components, 2 maps 2nd derivatives, and so on)
 *
 *  @return             Returns true if a mapping was made, false otherwise.  A false return 
 *                      value can be used to indicate that the requested map is not 
 *                      available, and verefore that the model may not be appropriate for the
 *                      requested operations.
 * 
 * @todo This method needs serious rework for the formation pieces in build 3.
 */
//------------------------------------------------------------------------------
bool PhysicalModel::GetComponentMap(Integer * map, Integer order) const
{
   //    return false;
   int i6;

   if (order == 1) {
      // Calculate how many spacecraft are in the model
      int satCount = (int)(dimension / 6);
      for (int i = 0; i < satCount; i++) {
         i6 = i * 6;
    
         map[ i6 ] = i6 + 3;
         map[i6+1] = i6 + 4;
         map[i6+2] = i6 + 5;
         map[i6+3] = -1;
         map[i6+4] = -1;
         map[i6+5] = -1;
      }
   }
    
   return true;

}

//------------------------------------------------------------------------------
// void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 * Sets the solar system pointer
 * 
 * @param ss Pointer to the solar system used in the modeling.
 */
//------------------------------------------------------------------------------
void PhysicalModel::SetSolarSystem(SolarSystem *ss)
{
   solarSystem = ss;
}

//------------------------------------------------------------------------------
// void PhysicalModel::SetSatelliteParameter(const Integer i, 
//                                           const std::string parmName, 
//                                           const Real parm)
//------------------------------------------------------------------------------
/**
 * Passes spacecraft parameters to the force model.
 * 
 * This default implementation just returns.
 * 
 * @param i ID for the spacecraft
 * @param parmName name of the Spacecraft parameter 
 * @param parm Parameter value
 */
//------------------------------------------------------------------------------
void PhysicalModel::SetSatelliteParameter(const Integer i, 
                                          const std::string parmName, 
                                          const Real parm)
{
}

//------------------------------------------------------------------------------
// void PhysicalModel::SetSatelliteParameter(const Integer i, 
//                                           const std::string parmName, 
//                                           const std::string parm)
//------------------------------------------------------------------------------
/**
 * Passes spacecraft parameters to the force model.
 * 
 * This default implementation just returns.
 * 
 * @param i ID for the spacecraft
 * @param parmName name of the Spacecraft parameter 
 * @param parm Parameter value
 */
//------------------------------------------------------------------------------
void PhysicalModel::SetSatelliteParameter(const Integer i, 
                                          const std::string parmName, 
                                          const std::string parm)
{
}

//------------------------------------------------------------------------------
// bool PhysicalModel::StateChanged(bool reset)
//------------------------------------------------------------------------------
// DJC: Probably should document this a bit better -- want me to do it? :
bool PhysicalModel::StateChanged(bool reset)
{
   bool retval = stateChanged;
   if (reset)
      stateChanged = false;
   return retval;
}


//---------------------------------
// inherited methods from GmatBase
//---------------------------------

//------------------------------------------------------------------------------
// std::string PhysicalModel::GetParameterText(const Integer id)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PhysicalModel::GetParameterText(const Integer id) const
{
   if (id >= EPOCH && id < PhysicalModelParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterText(id);
}

//------------------------------------------------------------------------------
// Integer PhysicalModel::GetParameterID(const std::string str)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer PhysicalModel::GetParameterID(const std::string &str) const
{
   for (int i = EPOCH; i < PhysicalModelParamCount; i++)
   {
      if (str == PhysicalModel::PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }
   return GmatBase::GetParameterID(str);
}

//------------------------------------------------------------------------------
// Gmat::ParameterType PhysicalModel::GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Gmat::ParameterType PhysicalModel::GetParameterType(const Integer id) const
{
   if (id >= EPOCH && id < PhysicalModelParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
// std::string PhysicalModel::GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string PhysicalModel::GetParameterTypeString(const Integer id) const
{
   if (id >= EPOCH && id < PhysicalModelParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
   else
      return GmatBase::GetParameterTypeString(id);
}

//------------------------------------------------------------------------------
// Real PhysicalModel::GetRealParameter(const Integer id)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real PhysicalModel::GetRealParameter(const Integer id) const
{
   if (id == EPOCH)
      return epoch + elapsedTime / 86400.0;
   if (id == ELAPSED_SECS)
      return elapsedTime;

   return GmatBase::GetRealParameter(id);
}

//------------------------------------------------------------------------------
// Real PhysicalModel::SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real PhysicalModel::SetRealParameter(const Integer id, const Real value)
{
   if (id == ELAPSED_SECS) 
   {
      elapsedTime = value;
      return elapsedTime;
   }
   if (id == EPOCH) 
   {
      epoch = value;
      elapsedTime = 0.0;
      return epoch;
   }
   return GmatBase::SetRealParameter(id, value);
}
