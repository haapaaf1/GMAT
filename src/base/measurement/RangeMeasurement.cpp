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


#define DEBUG_RANGE_CALC


RangeMeasurement::RangeMeasurement(const std::string &name) :
   GeometricMeasurement          ("Range", name)
{
   objectTypeNames.push_back("RangeMeasurement");

   // Prep value array in measurement
   currentMeasurement.value.push_back(0.0);

//   parameterCount = GeometricRangeMeasurementParamCount;
}


RangeMeasurement::~RangeMeasurement()
{
}


RangeMeasurement::RangeMeasurement(const RangeMeasurement &rm) :
   GeometricMeasurement          (rm)
{
   currentMeasurement.value.push_back(0.0);
   currentMeasurement.participantIDs.push_back("NotSet");
   currentMeasurement.participantIDs.push_back("NotSet");
}


RangeMeasurement& RangeMeasurement::operator=(const RangeMeasurement &rm)
{
   if (&rm != this)
   {
      currentMeasurement.value.push_back(0.0);
   }

   return *this;
}


GmatBase* RangeMeasurement::Clone() const
{
   return new RangeMeasurement(*this);
}


bool RangeMeasurement::Evaluate(bool withDerivatives)
{
   if (this->participants.size() != 2)
      throw MeasurementException("Range measurements require exactly 2 "
            "participants");

   // todo: Replace with parameter ID set at initialization
   currentMeasurement.epoch = participants[1]->GetRealParameter("A1Epoch");


   Rvector3 p1Loc = participants[0]->GetMJ2000Position(currentMeasurement.epoch);
   Rvector3 p2Loc = participants[1]->GetMJ2000Position(currentMeasurement.epoch);

   Rvector3 rangeVec = p2Loc - p1Loc;

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
      MessageInterface::ShowMessage("Location of %s, id = '%s':  %s\n",
            participants[0]->GetName().c_str(),
            currentMeasurement.participantIDs[0].c_str(),
            p1Loc.ToString().c_str());
      MessageInterface::ShowMessage("Location of %s, id = '%s':  %s\n",
            participants[1]->GetName().c_str(),
            currentMeasurement.participantIDs[1].c_str(),
            p2Loc.ToString().c_str());
      MessageInterface::ShowMessage("Range Vector:  %s\n",
            rangeVec.ToString().c_str());
      MessageInterface::ShowMessage("R(Groundstation) dot RangeVec =  %lf\n",
            currentMeasurement.feasibilityValue);
      MessageInterface::ShowMessage("Feasibility:  %s\n",
            (currentMeasurement.isFeasible ? "true" : "false"));
      MessageInterface::ShowMessage("Range is %.12lf\n",
            currentMeasurement.value[0]);
   #endif
   return true;
}
