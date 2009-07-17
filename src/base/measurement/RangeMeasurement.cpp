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
   GeometricMeasurement          ("Range", name),
   satEpochID                    (-1)
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
   GeometricMeasurement          (rm),
   satEpochID                    (rm.satEpochID)
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
      satEpochID = rm.satEpochID;
   }

   return *this;
}


GmatBase* RangeMeasurement::Clone() const
{
   return new RangeMeasurement(*this);
}


bool RangeMeasurement::Initialize()
{
   bool retval = false;

   // Todo: should this call GeometricMeasurement::Initialize()?
   if (GmatBase::Initialize())
   {
      if (participants.size() != 2)
         MessageInterface::ShowMessage("Range measurements require exactly 2 "
               "participants; cannot initialize\n");
      else
      {
         if ((participants[0]->IsOfType(Gmat::SPACE_POINT)) &&
             (participants[1]->IsOfType(Gmat::SPACECRAFT)))
         {
            satEpochID = participants[1]->GetParameterID("A1Epoch");
            retval = true;
         }
         else
         {
            MessageInterface::ShowMessage("Participant mismatch in Range "
                  "measurement: Current code requires one Spacecraft and one other"
                  " SpacePoint participant; cannot initialize\n");
         }
      }
   }

   return retval;
}


bool RangeMeasurement::Evaluate(bool withDerivatives)
{
   // todo: Replace with parameter ID set at initialization
   currentMeasurement.epoch = participants[1]->GetRealParameter(satEpochID);


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
