//$Id$
//------------------------------------------------------------------------------
//                                  StopCondition
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/10/09
//
/**
 * Implements StopCondition class.
 */
//------------------------------------------------------------------------------

#include "StopCondition.hpp"
#include "StopConditionException.hpp"
#include "NotAKnotInterpolator.hpp"
#include "RealUtilities.hpp"           // for Abs()
#include "AngleUtil.hpp"               // for PutAngleInDegRange()
#include "KeplerianParameters.hpp"     // for KepEcc()
#include "SphericalParameters.hpp"     // for SphRMag()
#include "StringUtil.hpp"              // for ReplaceName()
#include "MessageInterface.hpp"


//#define DEBUG_STOPCOND_INIT
//#define DEBUG_STOPCOND_GET
//#define DEBUG_STOPCOND_SET
//#define DEBUG_STOPCOND_EVAL
//#define DEBUG_RENAME
//#define DEBUG_STOPCOND_PERIAPSIS
//#define DEBUG_BUFFER_FILLING
//#define DEBUG_CYCLIC_PARAMETERS
//#define DEBUG_STOPCOND_EPOCH
//#define DEBUG_WRAPPERS

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif


using namespace GmatMathUtil;

//---------------------------------
// static data
//---------------------------------
const Real StopCondition::STOP_COND_TOL = 1.0e-11;

const std::string
StopCondition::PARAMETER_TEXT[StopConditionParamCount - GmatBaseParamCount] =
{
   "BaseEpoch",
   "Epoch",
   "EpochVar",
   "StopVar",
   "Goal",
   "Repeat",
};

const Gmat::ParameterType
StopCondition::PARAMETER_TYPE[StopConditionParamCount - GmatBaseParamCount] =
{
   Gmat::REAL_TYPE,    //"BaseEpoch",
   Gmat::REAL_TYPE,    //"Epoch",
   Gmat::STRING_TYPE,  //"EpochVar",
   Gmat::STRING_TYPE,  //"StopVar",
   Gmat::STRING_TYPE,  //"Goal",
   Gmat::INTEGER_TYPE, //"Repeat",
};

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// StopCondition(const std::string &name, const std::string &desc,
//               Parameter *epochParam, Parameter *stopParam, const Real &goal
//               const Real &tol, const Integer repeatCount,
//               Interpolator *interp)
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
StopCondition::StopCondition(const std::string &name, const std::string &desc,
                             Parameter *epochParam, Parameter *stopParam,
                             const Real &goal, const Real &tol,
                             const Integer repeatCount,
                             Interpolator *interp)
   : GmatBase(Gmat::STOP_CONDITION, "StopCondition", name),
     mBaseEpoch         (0.0),
     mEpoch             (0.0),
     mGoal              (goal),
     mRepeatCount       (repeatCount),
     mSolarSystem       (NULL),
     mInterpolator      (interp),
     mDescription       (desc),
     mStopParamType     (""),
     mStopParamName     (""),
     mEpochParamName    (""),
     lhsString          (""),
     rhsString          ("0.0"),
     mStopParam         (stopParam),
     mGoalParam         (NULL),
     mEpochParam        (epochParam),
     mEccParam          (NULL),
     mRmagParam         (NULL),
     lhsWrapper         (NULL),
     rhsWrapper         (NULL),
     mNumValidPoints    (0),
     mBufferSize        (0),
     mStopEpoch         (REAL_PARAMETER_UNDEFINED),
     mStopInterval      (0.0),
     previousEpoch      (-999999.999999),
     previousValue      (-999999.999999),
     previousGoalValue  (-999999.999999),
     mUseInternalEpoch  (false),
     mInitialized       (false),
     mNeedInterpolator  (false),
     mAllowGoalParam    (false),
     mBackwardsProp     (false),
     activated          (true),
     isAngleParameter   (false),
     isCyclicCondition(false),
     isPeriapse         (false),
     isApoapse          (false),
     scCycleType        (NOT_CYCLIC)
{
   objectTypeNames.push_back("StopCondition");

   objectTypes.push_back(Gmat::STOP_CONDITION);
   objectTypeNames.push_back("StopCondition");
   
   if (mStopParam != NULL)
   {
      mStopParamType = mStopParam->GetTypeName();
      mStopParamName = mStopParam->GetName();
   }
   
   if (epochParam == NULL)
      mUseInternalEpoch = true;
   
   // Create default Interpolator
   if (mInterpolator == NULL)
   {
      mInterpolator = new NotAKnotInterpolator("InternalInterpolator");
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (mInterpolator, mInterpolator->GetName(), "StopCondition::StopCondition()",
          "mInterpolator = (new NotAKnotInterpolator()");
      #endif
   }
}


//------------------------------------------------------------------------------
// StopCondition(const StopCondition &copy)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 */
//------------------------------------------------------------------------------
StopCondition::StopCondition(const StopCondition &copy)
   : GmatBase(copy),
     mBaseEpoch         (copy.mBaseEpoch),
     mEpoch             (copy.mEpoch),
     mGoal              (copy.mGoal),
     mRepeatCount       (copy.mRepeatCount),
     mSolarSystem       (copy.mSolarSystem),
     mDescription       (copy.mDescription),
     mStopParamType     (copy.mStopParamType),
     mStopParamName     (copy.mStopParamName),
     mEpochParamName    (""),
     lhsString          (copy.lhsString),
     rhsString          (copy.rhsString),
     mStopParam         (copy.mStopParam),
     mGoalParam         (copy.mGoalParam),
     mEpochParam        (copy.mEpochParam),
     mEccParam          (NULL),
     mRmagParam         (NULL),
     lhsWrapper         (copy.lhsWrapper),
     rhsWrapper         (copy.rhsWrapper),
     mNumValidPoints    (0),
     mBufferSize        (0),
     mStopEpoch         (copy.mStopEpoch),
     mStopInterval      (0.0),
     previousEpoch      (-999999.999999),
     previousValue      (-999999.999999),
     previousGoalValue  (-999999.999999),
     mUseInternalEpoch  (copy.mUseInternalEpoch),
     mInitialized       (copy.mInitialized),
     mNeedInterpolator  (copy.mNeedInterpolator),
     mAllowGoalParam    (copy.mAllowGoalParam),
     mBackwardsProp     (copy.mBackwardsProp),
     activated          (copy.activated),
     
     isAngleParameter   (copy.isAngleParameter),
     isCyclicCondition(copy.isCyclicCondition),
     isPeriapse         (copy.isPeriapse),
     isApoapse          (copy.isApoapse)
{
   mAllRefObjectNames = copy.mAllRefObjectNames;  // Is this correct?

   if (copy.mInterpolator != NULL)
      if (copy.mInterpolator->GetName() == "InternalInterpolator")
      {
         mInterpolator = (Interpolator*)copy.mInterpolator->Clone();
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (mInterpolator, mInterpolator->GetName(), "StopCondition::StopCondition(copy)",
             "mInterpolator = (Interpolator*)copy.mInterpolator->Clone()");
         #endif
      }
   
   if (copy.mEccParam != NULL)
   {
      mEccParam = (Parameter*)copy.mEccParam->Clone();
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (mEccParam, mEccParam->GetName(), "StopCondition::StopCondition(copy)",
          "mEccParam = (Parameter*)copy.mEccParam->Clone()");
      #endif
   }
   
   if (copy.mRmagParam != NULL)
   {
      mRmagParam = (Parameter*)copy.mRmagParam->Clone();
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (mRmagParam, mRmagParam->GetName(), "StopCondition::StopCondition(copy)",
          "mRmagParam = (Parameter*)copy.mRmagParam->Clone()");
      #endif
   }
   
   CopyDynamicData(copy);
}


//------------------------------------------------------------------------------
// StopCondition& operator= (const StopCondition &right)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 */
//------------------------------------------------------------------------------
StopCondition& StopCondition::operator= (const StopCondition &right)
{
   if (this != &right)
   {
      GmatBase::operator=(right);
      
      mAllRefObjectNames = right.mAllRefObjectNames; // Is this correct?
      
      mBaseEpoch = right.mBaseEpoch;
      mEpoch = right.mEpoch;
      mGoal = right.mGoal;
      mRepeatCount = right.mRepeatCount;
      
      if (mInterpolator != NULL)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (mInterpolator, mInterpolator->GetName(), "StopCondition::operator=",
             "deleting mInterpolator");
         #endif
         delete mInterpolator;
      }
      
      if (right.mInterpolator != NULL)
         if (right.mInterpolator->GetName() == "InternalInterpolator")
         {
            mInterpolator = (Interpolator*)right.mInterpolator->Clone();
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (mInterpolator, mInterpolator->GetName(), "StopCondition::operator=",
                "mInterpolator = (Interpolator*)right.mInterpolator->Clone()");
            #endif
         }
      
      mSolarSystem = right.mSolarSystem;
      mDescription = right.mDescription;
      mStopParamType = right.mStopParamType;
      mStopParamName = right.mStopParamName;
      lhsString = right.lhsString;
      rhsString = right.rhsString;
      
      mStopEpoch = right.mStopEpoch;
      mStopInterval = right.mStopInterval;
      mStopParam = right.mStopParam;
      mEpochParam = right.mEpochParam;
      mGoalParam = right.mGoalParam;
      lhsWrapper = right.lhsWrapper;
      rhsWrapper = right.rhsWrapper;
      
      if (mEccParam != NULL)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (mEccParam, mEccParam->GetName(), "StopCondition::operator=",
             "deleting mEccParam");
         #endif
         delete mEccParam;
      }
      
      if (right.mEccParam != NULL)
      {
         mEccParam = (Parameter*)right.mEccParam->Clone();
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (mEccParam, mEccParam->GetName(), "StopCondition::operator=",
             "mEccParam = (Parameter*)right.mEccParam->Clone()");
         #endif
      }
      else
         mEccParam = NULL;
      
      if (mRmagParam != NULL)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (mRmagParam, mRmagParam->GetName(), "StopCondition::operator=",
             "deleting mRmagParam");
         #endif
         delete mRmagParam;
      }
      
      if (right.mRmagParam != NULL)
      {
         mRmagParam = (Parameter*)right.mRmagParam->Clone();
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (mRmagParam, mRmagParam->GetName(), "StopCondition::operator=",
             "mRmagParam = (Parameter*)right.mRmagParam->Clone()");
         #endif
      }
      else
         mRmagParam = NULL;
      
      mInitialized = right.mInitialized;
      mUseInternalEpoch = right.mUseInternalEpoch;
      mNeedInterpolator = right.mNeedInterpolator;
      mAllowGoalParam = right.mAllowGoalParam;
      mBackwardsProp = right.mBackwardsProp;
      activated = right.activated;
      
      previousEpoch = -999999.999999;
      previousValue = -999999.999999;
      previousGoalValue = -999999.999999;
      
      isAngleParameter = right.isAngleParameter;
      isCyclicCondition = right.isCyclicCondition;
      isPeriapse       = right.isPeriapse;
      isApoapse        = right.isApoapse;
      
      CopyDynamicData(right);
   }
   
   return *this;
}


//------------------------------------------------------------------------------
// virtual ~StopCondition()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
StopCondition::~StopCondition()
{
   if (mEccParam != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (mEccParam, mEccParam->GetName(), "StopCondition::~StopCondition()",
          "deleting mEccParam");
      #endif
      delete mEccParam;
   }
   
   if (mRmagParam != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (mRmagParam, mRmagParam->GetName(), "StopCondition::~StopCondition()",
          "deleting mRmagParam");
      #endif
      delete mRmagParam;
   }
   
   if (mInterpolator != NULL)
   {
      if (mInterpolator->GetName() == "InternalInterpolator")
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (mInterpolator, mInterpolator->GetName(), "StopCondition::~StopCondition()",
             "deleting mInterpolator");
         #endif
         delete mInterpolator;
      }
   }
}

//------------------------------------------------------------------------------
// virtual bool Evaluate()
//------------------------------------------------------------------------------
/**
 * @return true if single parameter value stopping condition has been met;
 *   false otherwise
 *
 * @exception <StopConditionException> thrown if unable to retrieve parameter
 *   value
 *
 * @note count for repeat stop count will be implemented in the next build.
 */
//------------------------------------------------------------------------------
bool StopCondition::Evaluate()
{
   bool goalMet = false, readyToTest = true;
   Real epoch;
   Real currentParmValue;
   
   if (mStopParam == NULL || (mAllowGoalParam && mGoalParam == NULL))
      Initialize();
   
   #ifdef DEBUG_BUFFER_FILLING
      MessageInterface::ShowMessage(
         "StopCondition::Evaluate called, mNumValidPoints=%d, mAllowGoalParam=%d\n",
         mNumValidPoints, mAllowGoalParam);
   #endif
   
   // evaluate goal
   if (mAllowGoalParam)
   {
      try
      {
         mGoal = mGoalParam->EvaluateReal();
      }
      catch (BaseException &be)
      {
         // try with rhsWrapper
         if (rhsWrapper != NULL)
            mGoal = rhsWrapper->EvaluateReal();
         else
            throw;
      }
   }
   
   
   // set current epoch
   if (mUseInternalEpoch)
      epoch = mEpoch;
   else
      epoch = mEpochParam->EvaluateReal();
   
   // set current value
   currentParmValue = mStopParam->EvaluateReal();
   
   #ifdef DEBUG_BUFFER_FILLING
   MessageInterface::ShowMessage
      ("   currentParmValue=%.15f, mGoal=%.15f\n", currentParmValue, mGoal);
   #endif
   
   if (isAngleParameter)
   {
      mGoal = AngleUtil::PutAngleInDegRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
      
      readyToTest = CheckOnAnomaly(currentParmValue);
      if (!readyToTest)
      {
         previousValue = currentParmValue;
         previousEpoch = epoch;
         previousGoalValue = mGoal;
      }
   }
   else if (isCyclicCondition)
   {
      readyToTest = CheckCyclicCondition(currentParmValue);
      
      if (!readyToTest)
      {
         previousValue = currentParmValue;
         previousEpoch = epoch;
         previousGoalValue = mGoal;
      }
   }
   
   if (isApoapse)
   {
      readyToTest = CheckOnApoapsis();
      if (!readyToTest)
      {
         previousValue = currentParmValue;
         previousEpoch = epoch;
         previousGoalValue = mGoal;
      }
   }
   
   if (isPeriapse)
   {
      readyToTest = CheckOnPeriapsis();
      if (!readyToTest)
      {
         previousValue = currentParmValue;
         previousEpoch = epoch;
         previousGoalValue = mGoal;
      }
   }
   
   if (mNumValidPoints == 0)
   {
      previousValue = currentParmValue;
      previousEpoch = epoch;
      previousGoalValue = mGoal;
      ++mNumValidPoints;
      
      #ifdef DEBUG_STOPCOND_EVAL
      MessageInterface::ShowMessage
         ("StopCondition::Evaluate()() returning false, mNumValidPoints is zero\n");
      #endif
      return false;
   }
   
   if (!mStopParam->IsTimeParameter())
   {
      Real min, max;
      min = (currentParmValue<previousValue ? currentParmValue : previousValue);
      max = (currentParmValue>previousValue ? currentParmValue : previousValue);
      
      #ifdef DEBUG_STOPCOND_EVAL
         MessageInterface::ShowMessage(
            "Evaluating: min = %lf, max = %lf, goal = %lf\n", min, max, mGoal);
      #endif
      
      if ((min != max) && readyToTest)
      {
         if ((mGoal >= min) && (mGoal <= max) && activated)
         {
            goalMet = true;
            mStopInterval = (epoch - previousEpoch) * 86400.0;
      
            #ifdef DEBUG_STOPCOND_EVAL
               MessageInterface::ShowMessage(
                  "Previous Epoch = %.12lf, Epoch  %.12lf, Values = [%.12lf  %.12lf], "
                  "StopInterval = %.12lf\n", previousEpoch, epoch,
                  previousValue, currentParmValue, mStopInterval);
            #endif
         }
         else if (activated)
         {
            // Save the found values in for next time through
            previousEpoch = epoch;
            previousValue = currentParmValue;
            previousGoalValue = mGoal;
         }
      }
   }
   else
   // for time data we don't need to interpolate
   {
      Real prevGoalDiff = previousValue - mGoal,
           currGoalDiff = currentParmValue - mGoal;
      
      Real direction = 
           (currGoalDiff - prevGoalDiff > 0.0 ? 1.0 : -1.0);

      #ifdef DEBUG_STOPCOND_EVAL
         MessageInterface::ShowMessage(
            "prev = %15.9lf, curr = %15.9lf, lhs = %15.9lf, rhs = %15.9lf, "
            "direction = %15.9lf, \n", previousValue, currentParmValue, 
            prevGoalDiff, currGoalDiff, direction);
      #endif

      // todo: Fix the direction test code here!!!
//      if (mNumValidPoints == 1)
//         if (((2.0*mGoal - currentParmValue - previousValue) * direction) < 0.0)
//            MessageInterface::ShowMessage(
//               "Warning!  Time based stopping condition \"%s\" = %.10lf will "
//               "never be satisfied\n",
//               instanceName.c_str(), mGoal);
      
      // Goal met if it falls between previous and current values
      if ((currGoalDiff*direction >= 0.0) && 
          (prevGoalDiff*direction <= 0.0) && activated)
      {
         goalMet = true;
         
         #ifdef DEBUG_STOPCOND_EVAL
            MessageInterface::ShowMessage
               ("StopCondition::Evaluate() mUseInternalEpoch = %d, "
               "epoch = %15.9lf, mGoal = %15.9lf, "
               "currentParmValue = %15.9lf, "
               "previousValue = %15.9lf, "
               "GoalDiffs = [%15.9lf  %15.9lf]\n",  
                mUseInternalEpoch, epoch, 
                mGoal, currentParmValue, previousValue, 
                prevGoalDiff, currGoalDiff);
         #endif
      }
      else if (activated)
      {
         previousValue = currentParmValue;
         previousEpoch = epoch;
         previousGoalValue = mGoal;
      }
   }
   
   #ifdef DEBUG_BUFFER_FILLING
      MessageInterface::ShowMessage(
         "Value = %.12lf, Previous = %.12lf; Goal (%.12lf) %s, epoch = %15.12lf\n", 
         currentParmValue, previousValue, mGoal, (goalMet ? "met" : "not met"), epoch);
   #endif
   
   ++mNumValidPoints;
   
   #ifdef DEBUG_STOPCOND_EVAL
   MessageInterface::ShowMessage
      ("StopCondition::Evaluate() returning %s\n", goalMet ? "goal met" : "goal not met");
   #endif
   return goalMet;
}


//------------------------------------------------------------------------------
// bool IsTimeCondition()
//------------------------------------------------------------------------------
/**
 * Checks to see if teh stopping condition is a time based condition.
 * 
 * @return true if this is a time based stopping condition, false otherwise.
 */
//------------------------------------------------------------------------------
bool StopCondition::IsTimeCondition()
{
   return mStopParam->IsTimeParameter();
}


//------------------------------------------------------------------------------
// bool AddToBuffer(bool isInitialPoint)
//------------------------------------------------------------------------------
/**
 * Adds a data point to the ring buffer used for interpolation.
 * 
 * @param <isInitialPoint> Flag indicating if this is the first point for this 
 * stop evaluation.
 * 
 * @return true if a stop epoch can be evaluated; false if more points are 
 * required.
 */
//------------------------------------------------------------------------------
bool StopCondition::AddToBuffer(bool isInitialPoint)
{
   #ifdef DEBUG_BUFFER_FILLING
      MessageInterface::ShowMessage(
         "StopCondition::AddToBuffer(%s) called, internalEpoch is %s\n",
         (isInitialPoint ? "true" : "false"),
         (mUseInternalEpoch ? "true" : "false"));
   #endif
   
   // For now, skip time cases because they are already built
   if (IsTimeCondition())
      return true;
   
   // Flag indicating if both the ring buffer is full, and the stop condition 
   // is bracketed
   bool retval = false;

   Real epoch;
   Real currentParmValue, currentGoalValue;
   Real stopEpoch; //in A1Mjd
   
   // evaluate goal
   if (mAllowGoalParam)
   {
      try
      {
         mGoal = mGoalParam->EvaluateReal();
      }
      catch (BaseException &be)
      {
         // try with rhsWrapper
         if (rhsWrapper != NULL)
            mGoal = rhsWrapper->EvaluateReal();
         else
            throw;
      }
   }
   currentGoalValue = mGoal;

   // set current epoch
   if (mUseInternalEpoch)
      epoch = mEpoch;
   else
      epoch = mEpochParam->EvaluateReal();
   
   // set current LHS value
   currentParmValue = mStopParam->EvaluateReal();
   try
   {
      currentParmValue = mStopParam->EvaluateReal();
   }
   catch (BaseException &be)
   {
      // try with rhsWrapper
      if (lhsWrapper != NULL)
         currentParmValue = lhsWrapper->EvaluateReal();
      else
         throw;
   }
   
   // Force anomalies to handle wrapping
   if (isAngleParameter)
   {
      mGoal = AngleUtil::PutAngleInDegRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
      if (!CheckOnAnomaly(currentParmValue))
         return false;
   }
   else if(isCyclicCondition)
   {
      mGoal = PutInRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
      if (!CheckCyclicCondition(currentParmValue))
         return false;
   }
   
   #ifdef DEBUG_BUFFER_FILLING
      MessageInterface::ShowMessage(
         "  New point: %.12lf, %.12lf\n",
         epoch, currentParmValue);
   #endif
      
   // Actions taken to initialize the ring buffer
   if (isInitialPoint)
   {
      // Reset the internal buffer and the point count
      mNumValidPoints = 1;  // We always have the data for the initial point
      
      for (int i = 0; i < mBufferSize; ++i)
      {
         mEpochBuffer[i] = 0.0;
         lhsValueBuffer[i] = 0.0;
         rhsValueBuffer[i] = 0.0;
      }
      
      // Fill in the data for the first point
      lhsValueBuffer[mBufferSize-1] = previousValue;
      rhsValueBuffer[mBufferSize-1] = previousGoalValue;
      
      if (mUseInternalEpoch)
         mEpochBuffer[mBufferSize-1] = 0.0; 
      else
         mEpochBuffer[mBufferSize-1] = previousEpoch;
   }

   // Roll values in the ring buffer to make room for newest value
   for (int i = 0; i < mBufferSize-1; ++i)
   {
      mEpochBuffer[i]   = mEpochBuffer[i+1];
      lhsValueBuffer[i] = lhsValueBuffer[i+1];
      rhsValueBuffer[i] = rhsValueBuffer[i+1];
   }
   
   // Fill in the next data point
   mEpochBuffer[mBufferSize-1]   = epoch;
   rhsValueBuffer[mBufferSize-1] = mGoal;
   lhsValueBuffer[mBufferSize-1] = currentParmValue;
   ++mNumValidPoints;
   
   // Only start looking for a solution when the ring buffer is full   
   if (mNumValidPoints >= mBufferSize)
   {
      Real minVal = lhsValueBuffer[0], maxVal = lhsValueBuffer[mBufferSize - 1];
      for (int i = 0; i < mBufferSize; ++i)
      {
         if (minVal > lhsValueBuffer[i])
            minVal = lhsValueBuffer[i];
         if (maxVal < lhsValueBuffer[i])
            maxVal = lhsValueBuffer[i];
      }
      
      #ifdef DEBUG_BUFFER_FILLING
         MessageInterface::ShowMessage("Min value = %.12lf, Max = %.12lf\n",
            minVal, maxVal);
      #endif

      // If -- and only if -- the goal is bracketed, then interpolate the epoch
      if ((mGoal >= minVal) && (mGoal <= maxVal))
      {
         // Prep the interpolator
         mInterpolator->Clear();
         for (int i=0; i<mBufferSize; i++)
         {
            #ifdef DEBUG_STOPCOND_EVAL
            MessageInterface::ShowMessage
               ("StopCondition::Evaluate() i=%d, lhsValueBuffer=%f, "
                "mEpochBuffer=%f\n", i, lhsValueBuffer[i], mEpochBuffer[i]);
            #endif
            mInterpolator->AddPoint(lhsValueBuffer[i], &mEpochBuffer[i]);
         }
         
         // Finally, if we can interpolate an epoch, we have success!
         if (mInterpolator->Interpolate(mGoal, &stopEpoch))
         {
            mStopEpoch = stopEpoch;
            retval = true;
         }
         
         #ifdef DEBUG_STOPCOND_EVAL
         MessageInterface::ShowMessage
            ("StopCondition::Evaluate() mStopEpoch=%f\n", mStopEpoch);
         #endif
      }
   }

   #ifdef DEBUG_BUFFER_FILLING
      MessageInterface::ShowMessage("Valid points: %d, Buffer contents:\n",
         mNumValidPoints);
      for (int i=0; i<mBufferSize; i++)
      {
         MessageInterface::ShowMessage
            ("   [%d]   %.12lf  %.12lf\n", i, mEpochBuffer[i], lhsValueBuffer[i]);
      }
   #endif


   return retval;
}


//------------------------------------------------------------------------------
// Real GetStopEpoch()
//------------------------------------------------------------------------------
/**
 * Calculates the time to step (in seconds) needed to reach the stop epoch.
 * 
 * @return the time to step (in seconds)
 */
//------------------------------------------------------------------------------
Real StopCondition::GetStopEpoch()
{
   if (IsTimeCondition())
   {
      
      #ifdef DEBUG_STOPCOND_EPOCH
         MessageInterface::ShowMessage
            ("StopCondition::GetStopEpoch()\n   Previous = %15.9lf\n   "
            "Current = %15.9lf\n   Goal = %15.9lf", previousEpoch, previousValue, 
            mGoal);
      #endif

      Real dt = (mGoal - previousValue) * GetTimeMultiplier();
      return dt;
   }
      
   Real stopEpoch = 0.0;
   
   #ifdef DEBUG_STOPCOND_EPOCH
      MessageInterface::ShowMessage
         ("StopCondition::Evaluate()\n   Ring buffer:\n");
   #endif

   mInterpolator->Clear();
   for (int i=0; i<mBufferSize; i++)
   {
      #ifdef DEBUG_STOPCOND_EPOCH
         MessageInterface::ShowMessage
            ("      i=%d, lhsValueBuffer=%.12lf, "
             "mEpochBuffer=%.12lf\n", i, lhsValueBuffer[i], mEpochBuffer[i]);
      #endif
      
      mInterpolator->AddPoint(lhsValueBuffer[i], &mEpochBuffer[i]);
   }
   
   if (mInterpolator->Interpolate(mGoal, &stopEpoch))
      mStopEpoch = stopEpoch;
   else
      throw StopConditionException("Unable to interpolate a stop epoch");
  
   #ifdef DEBUG_STOPCOND_EPOCH
      MessageInterface::ShowMessage
         ("   Interpolated epoch = %.12lf\n", mStopEpoch);
   #endif
      
   return mStopEpoch;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the StopCondition.
 *
 * @return clone of the StopCondition.
 *
 */
//------------------------------------------------------------------------------
GmatBase* StopCondition::Clone() const
{
   return (new StopCondition(*this));
}


//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// bool CheckOnPeriapsis()
//------------------------------------------------------------------------------
bool StopCondition::CheckOnPeriapsis()
{   
   #ifdef DEBUG_STOPCOND_PERIAPSIS
   MessageInterface::ShowMessage
      ("StopCondition::CheckOnPeriapsis() entered, previousValue=%.15f, mGoal=%.15f\n",
       previousValue, mGoal);
   #endif
   
   bool goalMet = false;
   
   // Eccentricity must be large enough to keep osculations from masking the
   // stop point
   Real ecc = mEccParam->EvaluateReal();
//   Real rmag = mRmagParam->EvaluateReal();  // ???
   
   #ifdef DEBUG_STOPCOND_PERIAPSIS
   MessageInterface::ShowMessage("   ecc=%.15f\n", ecc);
   #endif
   
   //----------------------------------------------------------------------
   // A necessary condition for periapse stop: when moving forward in time,
   // R dot V must cross from negative to positive, so previous value must be 
   // less than the goal value. 
   //----------------------------------------------------------------------
//   if ((rmag <= mRange) && (ecc >= mEccTol))
   if (ecc >= 1.0e-6)
   {
      if (( mBackwardsProp && (previousValue >= mGoal))  ||  
          (!mBackwardsProp && (previousValue <= mGoal)))
         goalMet = true;
      //return true;
   }
   
   #ifdef DEBUG_STOPCOND_PERIAPSIS
   MessageInterface::ShowMessage
      ("StopCondition::CheckOnPeriapsis() returning %s\n", goalMet ?
       "goal met" : "goal not met");
   #endif
   
   return goalMet;
   //return false;
}


//------------------------------------------------------------------------------
// bool CheckOnApoapsis()
//------------------------------------------------------------------------------
bool StopCondition::CheckOnApoapsis()
{
   if (mEccParam == NULL)
      throw StopConditionException
         ("StopCondition::CheckOnApoapsis() ECC parameter has NULL pointer.\n");
   
   // Eccentricity must be large enough to keep asculations from masking the
   // stop point
   
   Real ecc = mEccParam->EvaluateReal();
   
   //----------------------------------------------------------------------
   // A necessary condition for apoapse stop: when moving forward in time,
   // R dot V must cross from positive to negative, so previous value must be 
   // greater than the goal value. 
   //----------------------------------------------------------------------
   
//   if (ecc >= mEccTol)
   if (ecc >= 1.0e-6)
   {
      if ((mBackwardsProp && (previousValue <= mGoal))  ||  
          (!mBackwardsProp && (previousValue >= mGoal)))
         return true;
   }
   
   return false;
}


//------------------------------------------------------------------------------
// bool CheckOnAnomaly(Real anomaly)
//------------------------------------------------------------------------------
bool StopCondition::CheckOnAnomaly(Real &anomaly)
{
   Real tempGoal = AngleUtil::PutAngleInDegRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
   
   #ifdef DEBUG_STOPCOND_ANOMALY
      MessageInterface::ShowMessage(
         "CheckOnAnomaly(%.12lf), tempGoal = %.12lf\n", anomaly, tempGoal);
   #endif
      
   // set current current history to be between goal - pi and goal + pi
   anomaly =
      AngleUtil::PutAngleInDegRange(anomaly, tempGoal - GmatMathUtil::PI_DEG, 
                                    tempGoal + GmatMathUtil::PI_DEG);
   Real diff = AngleUtil::PutAngleInDegRange
      (GmatMathUtil::Abs(tempGoal - anomaly),
       0.0, GmatMathUtil::TWO_PI_DEG);
   
   if (diff <= GmatMathUtil::PI_DEG/2.0)
      return true;
   
   return false;
}


//------------------------------------------------------------------------------
// bool CheckCyclicCondition(Real &value)
//------------------------------------------------------------------------------
/**
 * Performs range setting for cyclic stopping conditions, and checks to see that
 * the condition falls near the goal.
 * 
 * @param <value> Reference to the value of the parameter. On return from this 
 *                call, the input variable is changed to fall in the valid range
 *                for the variable, mapped around the current goal.
 * 
 * @return true if the stopping condition is near the goal, false otherwise.
 */ 
//------------------------------------------------------------------------------
bool StopCondition::CheckCyclicCondition(Real &value)
{
   Real min, max;
   bool retval = false;
   
   if (GetRange(min, max))
   {
      Real range2 = (max - min) / 2.0;
      mGoal = PutInRange(mGoal, min, max);
      value = PutInRange(value, mGoal - range2, 
                   mGoal + range2);
      previousValue = PutInRange(previousValue, mGoal - range2, 
                   mGoal + range2);

      #ifdef DEBUG_CYCLIC_PARAMETERS
         MessageInterface::ShowMessage(
            "Cyclic condition: Goal = %lf, currValue = %lf, prevValue = %lf\n",
            mGoal, value, previousValue);
      #endif
             
      if (fabs(mGoal - value) < (range2 / 2.0))
         retval = true;
   }
   
   return retval;
}

//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool StopCondition::Initialize()
{
   #ifdef DEBUG_STOPCOND_INIT
   MessageInterface::ShowMessage("StopCondition::Initialize() entered\n");
   #endif
   
   mInitialized = false;
   isApoapse = false;
   isPeriapse = false;
   mStopInterval = 0.0;
   
   // clear local parameters
   if (mEccParam != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (mEccParam, mEccParam->GetName(), "StopCondition::Initialize()",
          "deleting mEccParam");
      #endif
      delete mEccParam;
   }
   
   if (mRmagParam != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (mRmagParam, mRmagParam->GetName(), "StopCondition::Initialize()",
          "deleting mRmagParam");
      #endif
      delete mRmagParam;
   }
   
   mEccParam = NULL;
   mRmagParam = NULL;
   
   if (Validate())
   {
      if (mStopParamType == "Apoapsis" ||
          mStopParamType == "Periapsis")
      {
         mGoal = 0.0;
         mAllowGoalParam = false;
         if (mStopParamType == "Apoapsis")
            isApoapse = true;
         if (mStopParamType == "Periapsis")
            isPeriapse = true;
      }
      
      std::string paramTypeName = mStopParam->GetTypeName();
      
      if (paramTypeName == "TA" || paramTypeName == "MA" ||
          paramTypeName == "EA" || paramTypeName == "Longitude")
      {
         isCyclicCondition = true;
         
         if (mStopParamType == "TA" || mStopParamType == "MA" ||
             mStopParamType == "EA")
            scCycleType = ZERO_360;
         if (paramTypeName == "Longitude")
            scCycleType = PLUS_MINUS_180;
      }
      
      if (mNeedInterpolator)
      {
         mBufferSize = mInterpolator->GetBufferSize();
         mEpochBuffer.reserve(mBufferSize);
         lhsValueBuffer.reserve(mBufferSize);
         rhsValueBuffer.reserve(mBufferSize);
         
         for (int i=0; i<mBufferSize; i++)
         {
            mEpochBuffer.push_back(0.0);
            lhsValueBuffer.push_back(0.0);
            rhsValueBuffer.push_back(0.0);
         }
         
         mNumValidPoints = 0;
         mInitialized = true;
      }
      else
      {
         mInitialized = true;
      }
   }
   
   #ifdef DEBUG_STOPCOND_INIT
   MessageInterface::ShowMessage
      ("StopCondition::Initialize() returning mInitialized=%d\n", mInitialized);
   #endif
   
   return mInitialized;
}


//------------------------------------------------------------------------------
// virtual bool Validate()
//------------------------------------------------------------------------------
/**
 * @return true if all necessary objects have been set; false otherwise
 *
 * @exception thrown if necessary object pointer is NULL
 */
//------------------------------------------------------------------------------
bool StopCondition::Validate()
{   
   #ifdef DEBUG_STOPCOND_INIT   
   MessageInterface::ShowMessage
      ("StopCondition::Validate() entered, mUseInternalEpoch=%d, mEpochParam=<%p>, "
       "mStopParam=<%p>, mAllowGoalParam=%d, mGoalParam=<%p>\n", mUseInternalEpoch,
       mEpochParam, mStopParam, mAllowGoalParam, mGoalParam);
   #endif
   
   // check on epoch parameter
   if (!mUseInternalEpoch && mEpochParam == NULL)
   {
      throw StopConditionException
         ("StopCondition::Validate() epoch parameter: " + mEpochParamName +
          " has NULL pointer.\n");
   }
   
   // check on stop parameter
   if (mStopParam == NULL)
   {
      #ifdef DEBUG_STOPCOND_INIT
      MessageInterface::ShowMessage
         ("StopCondition::Validate() stop parameter '%s' is NULL\n",
          mStopParamName.c_str());
      #endif
      //throw StopConditionException
      //   ("StopCondition::Validate() stop parameter: " + mStopParamName +
      //    " has NULL pointer.\n");
      throw StopConditionException
         ("Currently GMAT expects a Spacecraft Parameter to be on the LHS of "
          "stopping condition");
   }
   
   // check if stop parameter is a system Parameter such as Sat.X
   if (mStopParam->GetKey() != GmatParam::SYSTEM_PARAM)
   {
      throw StopConditionException
         ("Currently GMAT expects a Spacecraft Parameter to be on the LHS of "
          "stopping condition");
   }
   
   // check on interpolator
   if (mStopParam->IsTimeParameter())
   {
      mNeedInterpolator = false;
      // Set time parameter type
      std::string timeTypeName = mStopParam->GetTypeName();
      if (timeTypeName == "ElapsedSecs")
         stopParamTimeType = SECOND_PARAM;
      else if (timeTypeName == "ElapsedDays")
         stopParamTimeType = DAY_PARAM;
      else if (timeTypeName.find("ModJulian") != std::string::npos)
         stopParamTimeType = EPOCH_PARAM;
      else
         stopParamTimeType = UNKNOWN_PARAM_TIME_TYPE;
      
      #ifdef DEBUG_STOPCOND_INIT   
         MessageInterface::ShowMessage(
            "Stop parameter \"%s\" has time type %d\n", timeTypeName.c_str(), 
            stopParamTimeType);
      #endif
   }
   else
   {
      if (mInterpolator == NULL)
      {
         throw StopConditionException
            ("StopCondition::Validate() Interpolator has NULL pointer.\n");
//            ("StopCondition::Validate() Interpolator: " + mInterpolatorName +
//             " has NULL pointer.\n");
      }
      
      mNeedInterpolator = true;
   }
   
   // check on goal parameter
   if (mAllowGoalParam && mGoalParam == NULL)
      throw StopConditionException
         ("StopCondition::Validate() goal parameter: " + rhsString +
          " has NULL pointer.\n");
   
   // Apoapsis and Periapsis need additional parameters
   if (mStopParamType == "Apoapsis" ||
       mStopParamType == "Periapsis")
   {
      // check on Ecc parameter
      if (mEccParam  == NULL)
      {
         #ifdef DEBUG_STOPCOND_INIT
         MessageInterface::ShowMessage
            ("StopCondition::Validate(): Creating KepEcc...\n");
         #endif
         
         mEccParam = new KepEcc("");
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (mEccParam, mEccParam->GetName(), "StopCondition::Validate()",
             "mEccParam = new KepEcc("")");
         #endif
         
         mEccParam->AddRefObject
            (mStopParam->GetRefObject(Gmat::SPACECRAFT, 
                                      mStopParam->GetRefObjectName(Gmat::SPACECRAFT)));
         mEccParam->AddRefObject
            (mStopParam->GetRefObject(Gmat::COORDINATE_SYSTEM, 
                                      mStopParam->GetRefObjectName(Gmat::COORDINATE_SYSTEM)));
         
         mEccParam->AddRefObject
            (mStopParam->GetRefObject(Gmat::SPACE_POINT, 
                                      mStopParam->GetRefObjectName(Gmat::SPACE_POINT)),
             true);
         
         mEccParam->SetInternalCoordSystem(mStopParam->GetInternalCoordSystem());            
         mEccParam->AddRefObject(mSolarSystem);
         mEccParam->Initialize();
      }
      
      // check on SphRMag parameter if "Periapsis"
      if (mStopParamType == "Periapsis")
      {
         if (mRmagParam == NULL)
         {
            #ifdef DEBUG_STOPCOND_INIT
            MessageInterface::ShowMessage
               ("StopCondition::Validate(): Creating SphRMag...\n");
            #endif
            
            std::string depObjName = mStopParam->GetStringParameter("DepObject");
            
            #ifdef DEBUG_STOPCOND_INIT
            MessageInterface::ShowMessage
               ("StopCondition::Validate() depObjName of mStopParam=%s\n",
                depObjName.c_str());
            #endif
            
            mRmagParam = new SphRMag("");
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (mRmagParam, mRmagParam->GetName(), "StopCondition::Validate()",
                "mRmagParam = new SphRMag("")");
            #endif
            
            mRmagParam->SetStringParameter("DepObject", depObjName);
            
            mRmagParam->AddRefObject
               (mStopParam->GetRefObject(Gmat::SPACECRAFT,
                                         mStopParam->GetRefObjectName(Gmat::SPACECRAFT)));
            mRmagParam->AddRefObject
               (mStopParam->GetRefObject(Gmat::COORDINATE_SYSTEM,
                                         mStopParam->GetRefObjectName(Gmat::COORDINATE_SYSTEM)));
            mRmagParam->AddRefObject
               (mStopParam->GetRefObject(Gmat::SPACE_POINT,
                                         mStopParam->GetRefObjectName(Gmat::SPACE_POINT)),
                true);
            
            mRmagParam->SetInternalCoordSystem(mStopParam->GetInternalCoordSystem());
            mRmagParam->AddRefObject(mSolarSystem);
            mRmagParam->Initialize();
            
//            // set mRange
//            if (depObjName == "Earth")
//               mRange = 5.0e5;
//            else if (depObjName == "Luna")
//               //mRange = 2.0e5; // Swingby has this.
//               mRange = 5.0e5;
//            else
//               mRange = 1.0e10;
         }
      }
   }
   
   #ifdef DEBUG_STOPCOND_INIT
   MessageInterface::ShowMessage
      ("StopCondition::Validate() returning true, mUseInternalEpoch=%d, mEpochParam=<%p>, "
       "mInterpolator=<%p>\n   mStopParamType=%s, mStopParamName=%s, mStopParam=<%p>, "
       "mGoalParam=<%p>\n", mUseInternalEpoch, mEpochParam, mInterpolator, mStopParamType.c_str(),
       mStopParamName.c_str(), mStopParam, mGoalParam);
   #endif
   
   return true;
}

void StopCondition::Reset()
{
   mNumValidPoints = 0;
}

Real StopCondition::GetStopInterval()
{
   return mStopInterval;
}


//------------------------------------------------------------------------------
// bool IsInitialized()
//------------------------------------------------------------------------------
bool StopCondition::IsInitialized()
{
   return mInitialized;
}


//------------------------------------------------------------------------------
// Integer GetBufferSize()
//------------------------------------------------------------------------------
Integer StopCondition::GetBufferSize()
{
   return mBufferSize;
}


//------------------------------------------------------------------------------
// std::string& GetDescription()
//------------------------------------------------------------------------------
std::string& StopCondition::GetDescription()
{
   return mDescription;
}


//------------------------------------------------------------------------------
// Parameter* GetEpochParameter()
//------------------------------------------------------------------------------
Parameter* StopCondition::GetEpochParameter()
{
   return mEpochParam;
}


//------------------------------------------------------------------------------
// Parameter* GetStopParameter()
//------------------------------------------------------------------------------
Parameter* StopCondition::GetStopParameter()
{
   return mStopParam;
}


//------------------------------------------------------------------------------
// Parameter* GetStopParameter()
//------------------------------------------------------------------------------
Parameter* StopCondition::GetGoalParameter()
{
   return mGoalParam;
}


//------------------------------------------------------------------------------
// Interpolator* GetInterpolator()
//------------------------------------------------------------------------------
Interpolator* StopCondition::GetInterpolator()
{
   return mInterpolator;
}


//------------------------------------------------------------------------------
// void SetDescription(const std::string &desc)
//------------------------------------------------------------------------------
void StopCondition::SetDescription(const std::string &desc)
{
   mDescription = desc;
}


//------------------------------------------------------------------------------
// void SetPropDirection(Real dir)
//------------------------------------------------------------------------------
void StopCondition::SetPropDirection(Real dir)
{   
   if (dir >= 1.0)
      mBackwardsProp = false;
   else
      mBackwardsProp = true;
   
   #ifdef DEBUG_STOPCOND
   MessageInterface::ShowMessage
      ("StopCondition::SetPropDirection() dir=%f, mBackwardsProp=%d\n",
       dir, mBackwardsProp);
   #endif
}


//------------------------------------------------------------------------------
// void SetSolarSystem(SolarSystem *solarSystem)
//------------------------------------------------------------------------------
/**
 * Sets SolarSystem
 */
//------------------------------------------------------------------------------
void StopCondition::SetSolarSystem(SolarSystem *solarSystem)
{
   if (solarSystem != NULL)
   {
      mSolarSystem = solarSystem;
   }
   else
   {
      throw StopConditionException(
         "Attempting to set solar system with NULL pointer.");
   }
}


//------------------------------------------------------------------------------
// bool SetInterpolator(Interpolator *interp)
//------------------------------------------------------------------------------
/**
 * Sets Interpolator to interpolate stop condition epoch.
 *
 * @return true if Interplator is successfully set; false otherwise
 */
//------------------------------------------------------------------------------
bool StopCondition::SetInterpolator(Interpolator *interp)
{
   if (interp != NULL)
   {
      if (mInterpolator->GetName() == "InternalInterpolator")
         delete mInterpolator;
      
      mInterpolator = interp;
      return true;
   }
   else
   {
      return false;
   }
}


//------------------------------------------------------------------------------
// bool SetEpochParameter(Parameter *param)
//------------------------------------------------------------------------------
/**
 * Sets epoch parameter which will be used to interpolate stop epoch
 *
 * @return true if EpochParam is successfully set; false otherwise
 */
//------------------------------------------------------------------------------
bool StopCondition::SetEpochParameter(Parameter *param)
{
   if (param != NULL)
   {
      mEpochParam = param;
      mUseInternalEpoch = false;
      return true;
   }
   else
   {
      return false;
   }
}


//------------------------------------------------------------------------------
// virtual bool SetStopParameter(Parameter *param)
//------------------------------------------------------------------------------
/**
 * Add parameter to stop condition.
 *
 * @return true if parameter has added to array.
 */
//------------------------------------------------------------------------------
bool StopCondition::SetStopParameter(Parameter *param)
{
   #ifdef DEBUG_STOP_PARAM
   MessageInterface::ShowMessage
      ("StopCondition::SetStopParameter() param=<%p>\n", param);
   #endif
   
   if (param != NULL)
   {
      mStopParam = param;
      mStopParamType = mStopParam->GetTypeName();
      
      if (param->IsTimeParameter())
         mInitialized = true;
      
      return true;
   }
   
   return false;
}


//------------------------------------------------------------------------------
// virtual bool SetGoalParameter(Parameter *param)
//------------------------------------------------------------------------------
/**
 * Sets goal parameter to stop condition.
 *
 * @return true if parameter has added to array.
 */
//------------------------------------------------------------------------------
bool StopCondition::SetGoalParameter(Parameter *param)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetGoalParameter() param=<%p>\n", param);
   #endif
   
   mGoalParam = param;
   return true;
}


//------------------------------------------------------------------------------
// void SetLhsString(const std::string &str)
//------------------------------------------------------------------------------
void StopCondition::SetLhsString(const std::string &str)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetLhsString() this=<%p>'%s' entered, str='%s', "
       "lhsString='%s'\n", this, GetName().c_str(), str.c_str(), lhsString.c_str());
   #endif
   
   lhsString = str;
   
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetLhsString() this=<%p>'%s' leaving, str='%s', "
       "lhsString='%s'\n", this, GetName().c_str(), str.c_str(), lhsString.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void SetRhsString(const std::string &str)
//------------------------------------------------------------------------------
void StopCondition::SetRhsString(const std::string &str)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetLhsString() this=<%p>'%s' entered, str='%s', "
       "rhsString='%s'\n", this, GetName().c_str(), str.c_str(), rhsString.c_str());
   #endif
   
   rhsString = str;
   
   // remove leading blanks
   std::string::size_type pos = rhsString.find_first_not_of(' ');
   rhsString.erase(0, pos);
   
   // if str is just a number
   if (isdigit(rhsString[0]) || rhsString[0] == '.' || rhsString[0] == '-')
   {
      mGoal = atof(rhsString.c_str());
      mAllowGoalParam = false;
   }
   else
   {
      mAllowGoalParam = true;
   }
   
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetRhsString() mAllowGoalParam=%d, rhsString=<%s>, "
       "mGoal=%le\n", mAllowGoalParam, rhsString.c_str(), mGoal);
   #endif
   
}


//------------------------------------------------------------------------------
// std::string GetLhsString()
//------------------------------------------------------------------------------
std::string StopCondition::GetLhsString()
{
   return lhsString;
}


//------------------------------------------------------------------------------
// std::string GetRhsString()
//------------------------------------------------------------------------------
std::string StopCondition::GetRhsString()
{
   return rhsString;
}


//------------------------------------------------------------------------------
// bool SetLhsWrapper(ElementWrapper *toWrapper)
//------------------------------------------------------------------------------
bool StopCondition::SetLhsWrapper(ElementWrapper *toWrapper)
{
   #ifdef DEBUG_WRAPPERS   
   MessageInterface::ShowMessage
      ("StopCondition::SetLhsWrapper() this=<%p>'%s' entered with toWrapper=<%p>\n",
       this, GetName().c_str(), toWrapper);
   #endif
   
   if (toWrapper == NULL)
      return false;
   
   lhsWrapper = toWrapper;
   return true;
}


//------------------------------------------------------------------------------
// bool SetRhsWrapper(ElementWrapper *toWrapper)
//------------------------------------------------------------------------------
bool StopCondition::SetRhsWrapper(ElementWrapper *toWrapper)
{
   #ifdef DEBUG_WRAPPERS   
   MessageInterface::ShowMessage
      ("StopCondition::SetRhsWrapper() this=<%p>'%s' entered with toWrapper=<%p>\n",
       this, GetName().c_str(), toWrapper);
   #endif
   
   if (toWrapper == NULL)
      return false;
   
   rhsWrapper = toWrapper;
   return true;
}


//------------------------------------------------------------------------------
// virtual bool SetSpacecraft(Spacecraft *sc)
//------------------------------------------------------------------------------
/**
 * Sets spacecraft pointer to internal parameter used in stop condition.
 *
 * @return true if spacecraft has been set.
 */
//------------------------------------------------------------------------------
bool StopCondition::SetSpacecraft(SpaceObject *sc)
{
   if (mEccParam != NULL)
      mEccParam->SetRefObject(sc, Gmat::SPACECRAFT, sc->GetName());
   
   if (mRmagParam != NULL)
      mRmagParam->SetRefObject(sc, Gmat::SPACECRAFT, sc->GetName());
   
   return true;
}


//---------------------------------
// methods inherited from GmatBase
//---------------------------------

//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool StopCondition::RenameRefObject(const Gmat::ObjectType type,
                                    const std::string &oldName,
                                    const std::string &newName)
{
   #ifdef DEBUG_RENAME
   MessageInterface::ShowMessage
      ("StopCondition::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   if (type != Gmat::SPACECRAFT && type != Gmat::PARAMETER)
      return true;
   
   //set new StopCondition name
   std::string name = GetName();
   std::string::size_type pos = name.find(oldName);
   if (pos != name.npos)
   {
      name = GmatStringUtil::ReplaceName(name, oldName, newName);
      SetName(name);
   }
   
   //set new epoch parameter name
   pos = mEpochParamName.find(oldName);
   if (pos != mEpochParamName.npos)
      mEpochParamName = GmatStringUtil::ReplaceName(mEpochParamName, oldName, newName);
   
   //set new stop parameter name
   pos = mStopParamName.find(oldName);
   if (pos != mStopParamName.npos)
      mStopParamName = GmatStringUtil::ReplaceName(mStopParamName, oldName, newName);
   
   //set new stop goal string
   pos = rhsString.find(oldName);
   if (pos != mStopParamName.npos)
      rhsString = GmatStringUtil::ReplaceName(rhsString, oldName, newName);
   
   return true;
}


//---------------------------------------------------------------------------
// const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//---------------------------------------------------------------------------
/**
 * Retrieves reference object name array for given type. It will return all
 * object names if type is Gmat::UNKNOWN_NAME.
 *
 * @param <type> object type
 * @return reference object name.
 */
//------------------------------------------------------------------------------
const StringArray&
StopCondition::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   mAllRefObjectNames.clear();
   
   if (type == Gmat::UNKNOWN_OBJECT || type == Gmat::PARAMETER)
   {
      mAllRefObjectNames.push_back(mStopParamName);
      if (mAllowGoalParam)
         mAllRefObjectNames.push_back(rhsString);
   }
   
   return mAllRefObjectNames;
}


//---------------------------------------------------------------------------
// bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                   const std::string &name)
//---------------------------------------------------------------------------
bool StopCondition::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   #ifdef DEBUG_STOPCOND_GET
   MessageInterface::ShowMessage
      ("StopCondition::SetRefObject() Name=%s, mStopParamName=%s, rhsString=%s\n",
       this->GetName().c_str(), mStopParamName.c_str(), rhsString.c_str());
   #endif
   
   if (type == Gmat::PARAMETER)
   {
      if (name == mStopParamName)
         SetStopParameter((Parameter*)obj);
      if (name == rhsString)
         SetGoalParameter((Parameter*)obj);
      if (name == mEpochParamName)
         SetEpochParameter((Parameter*)obj);
      
      return true;
   }
   else if (type == Gmat::INTERPOLATOR)
   {
      SetInterpolator((Interpolator*)obj);
      return true;
   }
   
   return GmatBase::SetRefObject(obj, type, name);
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string StopCondition::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < StopConditionParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterText(id);
    
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer StopCondition::GetParameterID(const std::string &str) const
{
   #ifdef DEBUG_STOPCOND_GET
   MessageInterface::ShowMessage
      ("StopCondition::GetParameterID() str = %s\n", str.c_str());
   #endif
   
   for (int i=GmatBaseParamCount; i<StopConditionParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }
   
   return GmatBase::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType StopCondition::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < StopConditionParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
   else
      return GmatBase::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string StopCondition::GetParameterTypeString(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < StopConditionParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
   else
      return GmatBase::GetParameterTypeString(id);
    
}


//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer StopCondition::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
   case REPEAT_COUNT:
      return mRepeatCount;
   default:
      return GmatBase::GetIntegerParameter(id);
   }
}


//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
Integer StopCondition::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
Integer StopCondition::SetIntegerParameter(const Integer id,
                                               const Integer value)
{
   switch (id)
   {
   case REPEAT_COUNT:
      mRepeatCount = value;
      return mRepeatCount;
   default:
      return GmatBase::SetIntegerParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const std::string &label, const Integer value)
//------------------------------------------------------------------------------
Integer StopCondition::SetIntegerParameter(const std::string &label,
                                               const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
Real StopCondition::GetRealParameter(const Integer id) const
{
   switch (id)
   {
//   case TOLERANCE:
//      return mTolerance;
//   case ECC_TOL:
//      return mEccTol;
//   case RANGE:
//      return mRange;
   case BASE_EPOCH:
      return mBaseEpoch;
   case EPOCH:
      return mEpoch;
   default:
      return GmatBase::GetRealParameter(id);
   }
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
Real StopCondition::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
Real StopCondition::SetRealParameter(const Integer id, const Real value)
{
   switch (id)
   {
//   case TOLERANCE:
//      mTolerance = value;
//      return mTolerance;
//   case ECC_TOL:
//      mEccTol = value;
//      return mEccTol;
//   case RANGE:
//      mRange = value;
//      return mRange;
   case BASE_EPOCH:
      mBaseEpoch = value;
      return mBaseEpoch;
   case EPOCH:
      mEpoch = value;
      return mEpoch;
   default:
      return GmatBase::GetRealParameter(id);
   }
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string &label, const Real value)
//------------------------------------------------------------------------------
Real StopCondition::SetRealParameter(const std::string &label, const Real value)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetRealParameter() label=%s, "
       "value=%le\n", label.c_str(), value);
   #endif
   
   return SetRealParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string StopCondition::GetStringParameter(const Integer id) const
{
   switch (id)
   {
   case EPOCH_VAR:
      return mEpochParamName;
   case STOP_VAR:
      return mStopParamName;
   case GOAL:
      return rhsString;
//   case INTERPOLATOR:
//      return mInterpolatorName;
   default:
      return GmatBase::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string StopCondition::GetStringParameter(const std::string &label) const
{
   #ifdef DEBUG_STOPCOND_GET
   MessageInterface::ShowMessage
      ("StopCondition::GetStringParameter() label = %s\n", label.c_str());
   #endif
   
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool StopCondition::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetStringParameter() this=<%p>'%s' entered, id = %d, "
       "value = %s \n", this, GetName().c_str(), id, value.c_str());
   #endif
   
   switch (id)
   {
   case EPOCH_VAR:
      mEpochParamName = value;
      return true;
   case STOP_VAR:
      mStopParamName = value;
      return true;
   case GOAL:
      SetRhsString(value);
      return true;
//   case INTERPOLATOR:
//      mInterpolatorName = value;
//      return true;
   default:
      return GmatBase::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool StopCondition::SetStringParameter(const std::string &label,
                                           const std::string &value)
{
   #ifdef DEBUG_STOPCOND_SET
   MessageInterface::ShowMessage
      ("StopCondition::SetStringParameter() label = %s, "
       "value = %s \n", label.c_str(), value.c_str());
   #endif
   
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// Real GetStopValue()
//------------------------------------------------------------------------------
/**
 * Used to access the last calculated stopping condition value.
 * 
 * @return The last calculated value obtained from the mStopParam member.
 */ 
//------------------------------------------------------------------------------
Real StopCondition::GetStopValue()
{
   return previousValue;
}


//------------------------------------------------------------------------------
// Real GetStopDifference()
//------------------------------------------------------------------------------
/**
 * Used to access the difference between the last calculated stopping condition 
 * value and the desired value.
 * 
 * @return The difference between the goal and achieved value (goal - achieved).
 */ 
//------------------------------------------------------------------------------
Real StopCondition::GetStopDifference()
{
   Real goalValue, achievedValue;
   if (mGoalParam)
   {
      try
      {
         goalValue = mGoalParam->EvaluateReal();
      }
      catch (BaseException &be)
      {
         // try with rhsWrapper
         if (rhsWrapper != NULL)
            goalValue = rhsWrapper->EvaluateReal();
         else
            throw;
      }      
   }
   else
      goalValue = mGoal;
   
   #ifdef DEBUG_STOPCOND_EVAL
      MessageInterface::ShowMessage("%s goal (%s) = %16.9lf, value = %16.9lf\n", 
         instanceName.c_str(), 
         (mGoalParam ? mGoalParam->GetName().c_str() : "Fixed goal"), goalValue,
         mStopParam->EvaluateReal());
   #endif
      
   achievedValue = mStopParam->EvaluateReal();
   Real min, max, delta;
   GetRange(min, max);
   delta = (max - min) * 0.5;
   if (IsCyclicParameter())
      achievedValue = PutInRange(achievedValue, goalValue-delta, 
            goalValue + delta);
   
   return goalValue - achievedValue;
}


//------------------------------------------------------------------------------
// Real GetStopGoal()
//------------------------------------------------------------------------------
/**
 * Used to access the desired stopping value.
 * 
 * @return The goal value.
 */ 
//------------------------------------------------------------------------------
Real StopCondition::GetStopGoal()
{
   Real goalValue;
   if (mGoalParam)
   {
      try
      {
         goalValue = mGoalParam->EvaluateReal();
      }
      catch (BaseException &be)
      {
         // try with rhsWrapper
         if (rhsWrapper != NULL)
            goalValue = rhsWrapper->EvaluateReal();
         else
            throw;
      }      
   }
   else
      goalValue = mGoal;
   
   return goalValue;
}


//------------------------------------------------------------------------------
// Real GetStopTime()
//------------------------------------------------------------------------------
/**
 * Used to access the last calculated stopping condition epoch.
 * 
 * @return The last calculated value obtained from the mEpochParam member.
 */ 
//------------------------------------------------------------------------------
Real StopCondition::GetStopTime()
{
   return previousEpoch;
}


//------------------------------------------------------------------------------
// Real GetTimeMultiplier()
//------------------------------------------------------------------------------
/**
 * Used to find the conversion factor for changing time parameters to seconds.
 * 
 * @return The factor.
 */ 
//------------------------------------------------------------------------------
Real StopCondition::GetTimeMultiplier()
{
   switch (stopParamTimeType)
   {
      case MINUTE_PARAM:
         return 60.0;

      case HOUR_PARAM:
         return 3600.0;
      
      case DAY_PARAM:
      case EPOCH_PARAM:
         return 86400.0;
      
      // All intentional fall-throughs
      case SECOND_PARAM:
      case NOT_TIME_PARAM:
      case UNKNOWN_PARAM_TIME_TYPE:
      default:
         return 1.0;
   };
   
   return 1.0;
}


//------------------------------------------------------------------------------
// bool IsCyclicParameter()
//------------------------------------------------------------------------------
/**
 * Access method used to determine if the stop parameter is cyclic.
 * 
 * @return true for cyclic parameters, false if the parameter is not cyclic.
 */ 
//------------------------------------------------------------------------------
bool StopCondition::IsCyclicParameter()
{
   return isCyclicCondition;
}


//------------------------------------------------------------------------------
// bool GetRange(Real &min, Real &max)
//------------------------------------------------------------------------------
/**
 * Retrieves the minimum and maximum values for the range variable.
 * 
 * @param <min> Reference to the variable that, upon return, contains the range 
 *              minimum
 * @param <max> Reference to the variable that, upon return, contains the range 
 *              maximum
 * 
 * @return true if the references were filled in, false if there has a problem 
 *         setting the range values.
 */ 
//------------------------------------------------------------------------------
bool StopCondition::GetRange(Real &min, Real &max)
{
   bool retval = true;
   if (!isCyclicCondition)
      retval = false;
   else
   {
      switch (scCycleType)
      {
         case ZERO_90:
            min = 0.0;
            max = 90.0;
            break;
            
         case ZERO_180:
            min = 0.0;
            max = 180.0;
            break;
            
         case ZERO_360:
            min = 0.0;
            max = 360.0;
            break;
            
         case PLUS_MINUS_90:
            min = -90.0;
            max = 90.0;
            break;
            
         case PLUS_MINUS_180:
            min = -180.0;
            max = 180.0;
            break;
            
         case OTHER_CYCLIC:   // Intentional drop-through
         case NOT_CYCLIC:     // Intentional drop-through
         default:
            retval = false;
      };
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// Real PutInRange(const Real value, const Real min, const Real max, 
//                 bool isReflection)
//------------------------------------------------------------------------------
/**
 * Returns the parameter value that matches the input value for a cyclic 
 * parameter, remapped to fall between the minimum and maximum values allowed.
 * 
 * @param <value>        The input value of the parameter
 * @param <min>          The minimum value of the parameter
 * @param <max>          The maximum value of the parameter
 * @param <isReflection> Flag indicating if the parameter should be reflected 
 *                       into the valid range.  This flag is not yet used.
 * 
 * @return the parameter value, mapped into the valid range.
 */ 
//------------------------------------------------------------------------------
Real StopCondition::PutInRange(const Real value, const Real min, const Real max,
                                bool isReflection)
{
   if (min >= max)
      throw StopConditionException("PutInRange received bad range limits\n");
   Real range = max - min, retval = value;
   
   while (retval < min)
      retval += range;

   while (retval > max)
      retval -= range;
      
   if (isReflection)
   {
      // Not yet needed or implemented
   }
      
   return retval;
}

//------------------------------------------------------------------------------
// void SkipEvaluation(bool shouldSkip)
//------------------------------------------------------------------------------
/**
 * Function used to toggle stopping conditions is active or inactive.
 * 
 * @param <shouldSkip> Flag indicating the status.  If this parameter is true,
 *                     the stopping condition is deactivated; if false, it is
 *                     activated. 
 */ 
//------------------------------------------------------------------------------
void StopCondition::SkipEvaluation(bool shouldSkip)
{
   if (mStopParam->GetName().find("ModJulian") == std::string::npos)
      activated = !shouldSkip;
}


//------------------------------------------------------------------------------
// void UpdateBuffer()
//------------------------------------------------------------------------------
/**
 * Sets the most recent value of the stop parameter and epoch to be the 
 * previously calculated values. 
 */ 
//------------------------------------------------------------------------------
void StopCondition::UpdateBuffer()
{
   previousValue = mStopParam->EvaluateReal();

   // evaluate goal in case needed for cyclics
   if (mAllowGoalParam)
   {
      try
      {
         mGoal = mGoalParam->EvaluateReal();
      }
      catch (BaseException &be)
      {
         // try with rhsWrapper
         if (rhsWrapper != NULL)
            mGoal = rhsWrapper->EvaluateReal();
         else
            throw;
      }
   }
   
   // set current epoch
   Real epoch;
   if (mUseInternalEpoch)
      epoch = mEpoch;
   else
      epoch = mEpochParam->EvaluateReal();
      
      // Force anomalies to handle wrapping
   if (isAngleParameter)
   {
      mGoal = AngleUtil::PutAngleInDegRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
      CheckOnAnomaly(previousValue);
   }
   else if(isCyclicCondition)
   {
      mGoal = PutInRange(mGoal, 0.0, GmatMathUtil::TWO_PI_DEG);
      CheckCyclicCondition(previousValue);
   }
   
   previousEpoch = epoch;

   #ifdef DEBUG_CYCLIC_PARAMETERS
      MessageInterface::ShowMessage(
         "Updated \"previous\" values to [previousValue  previousEpoch]"
         " = [%.12lf  %.12lf]\n", previousValue, previousEpoch);
   #endif
}


//---------------------------------
// private methods
//---------------------------------

//------------------------------------------------------------------------------
// void CopyDynamicData()
//------------------------------------------------------------------------------
void StopCondition::CopyDynamicData(const StopCondition &stopCond)
{
   mNumValidPoints = stopCond.mNumValidPoints;
   mBufferSize = stopCond.mBufferSize;

   if ((Integer)mEpochBuffer.size() < mBufferSize)
      mEpochBuffer.reserve(mBufferSize);
   
   if ((Integer)lhsValueBuffer.size() < mBufferSize)
      lhsValueBuffer.reserve(mBufferSize);
   
   if ((Integer)rhsValueBuffer.size() < mBufferSize)
      rhsValueBuffer.reserve(mBufferSize);
   
   for (int i=0; i<mBufferSize; i++)
   {
      mEpochBuffer[i] = stopCond.mEpochBuffer[i];
      lhsValueBuffer[i] = stopCond.lhsValueBuffer[i];
      rhsValueBuffer[i] = stopCond.rhsValueBuffer[i];
   }
}
