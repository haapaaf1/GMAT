//$Id$
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
 * Implementation of the geometric range measurement.
 */
//------------------------------------------------------------------------------


#include "RangeMeasurement.hpp"
#include "MessageInterface.hpp"
#include "MeasurementException.hpp"

#include "Rvector3.hpp"


//#define DEBUG_RANGE_CALC
//#define SHOW_RANGE_CALC


RangeMeasurement::RangeMeasurement(const std::string &name) :
   GeometricMeasurement          ("Range", name)
{
   objectTypeNames.push_back("RangeMeasurement");

   // Prep value array in measurement
   currentMeasurement.value.push_back(0.0);
   currentMeasurement.typeName = "Range";
   currentMeasurement.type = Gmat::RANGE;
//   parameterCount = GeometricRangeMeasurementParamCount;
}


RangeMeasurement::~RangeMeasurement()
{
}


RangeMeasurement::RangeMeasurement(const RangeMeasurement &rm) :
   GeometricMeasurement          (rm)
{
   currentMeasurement.value.push_back(0.0);
   currentMeasurement.typeName = "Range";
   currentMeasurement.type = Gmat::RANGE;
   currentMeasurement.uniqueID = rm.currentMeasurement.uniqueID;
   currentMeasurement.participantIDs.push_back("NotSet");
   currentMeasurement.participantIDs.push_back("NotSet");
}


RangeMeasurement& RangeMeasurement::operator=(const RangeMeasurement &rm)
{
   if (&rm != this)
   {
      GeometricMeasurement::operator=(rm);

      // Allocate exactly one value in current measurement for range
      currentMeasurement.value.clear();
      currentMeasurement.value.push_back(0.0);
      currentMeasurement.typeName = "Range";
      currentMeasurement.type = Gmat::RANGE;
      currentMeasurement.uniqueID = rm.currentMeasurement.uniqueID;
   }

   return *this;
}


GmatBase* RangeMeasurement::Clone() const
{
   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("Entered RangeMeasurement::Clone() "
            "with %d participants; this = %p\n", participants.size(), this);
   #endif
   GmatBase *retval =  new RangeMeasurement(*this);
   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("   clone address is %p\n", retval);
   #endif
   return retval;
}


bool RangeMeasurement::Initialize()
{
   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("Entered RangeMeasurement::Initialize()"
            "; this = %p\n", this);
   #endif

   bool retval = false;

   if (GeometricMeasurement::Initialize())
      retval = true;

   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("   Initialization %s with %d "
            "participants\n", (retval ? "succeeded" : "failed"),
            participants.size());
   #endif

   return retval;
}


bool RangeMeasurement::Evaluate(bool withDerivatives)
{
   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("Entered RangeMeasurement::Evaluate(%s)\n",
            (withDerivatives ? "true" : "false"));
      MessageInterface::ShowMessage("  ParticipantCount: %d\n",
            participants.size());
   #endif

   CalculateRangeVector();

   currentMeasurement.feasibilityValue = rangeVec * p1Loc;

   if (currentMeasurement.feasibilityValue > 0.0)
   {
      currentMeasurement.isFeasible = true;
      currentMeasurement.value[0] = rangeVec.GetMagnitude();
   }
   else
   {
      currentMeasurement.isFeasible = false;
      currentMeasurement.value[0] = 0.0;
   }

   #ifdef DEBUG_RANGE_CALC
      MessageInterface::ShowMessage("Calculating Range at epoch %.12lf\n",
            currentMeasurement.epoch);
      MessageInterface::ShowMessage("   Location of %s, id = '%s':  %s",
            participants[0]->GetName().c_str(),
            currentMeasurement.participantIDs[0].c_str(),
            p1Loc.ToString().c_str());
      MessageInterface::ShowMessage("   Location of %s, id = '%s':  %s",
            participants[1]->GetName().c_str(),
            currentMeasurement.participantIDs[1].c_str(),
            p2Loc.ToString().c_str());
      MessageInterface::ShowMessage("   Range Vector:  %s\n",
            rangeVec.ToString().c_str());
      MessageInterface::ShowMessage("   R(Groundstation) dot RangeVec =  %lf\n",
            currentMeasurement.feasibilityValue);
      MessageInterface::ShowMessage("   Feasibility:  %s\n",
            (currentMeasurement.isFeasible ? "true" : "false"));
      MessageInterface::ShowMessage("   Range is %.12lf\n",
            currentMeasurement.value[0]);
   #endif

   #ifdef SHOW_RANGE_CALC
      MessageInterface::ShowMessage("Range at epoch %.12lf is ",
            currentMeasurement.epoch);
      if (currentMeasurement.isFeasible)
         MessageInterface::ShowMessage("feasible, value = %.12lf\n",
            currentMeasurement.value[0]);
      else
         MessageInterface::ShowMessage("not feasible\n");
   #endif

   return true;
}
