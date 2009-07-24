//------------------------------------------------------------------------------
//                         GeometricMeasurement
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
 * Implementation of the geometric measurement base class.
 */
//------------------------------------------------------------------------------


#include "GeometricMeasurement.hpp"
#include "MessageInterface.hpp"


//#define DEBUG_MEASUREMENT_INITIALIZATION

//const std::string
//GeometricMeasurement::PARAMETER_TEXT[GeometricMeasurementParamCount -
//                                     GmatBaseParamCount] =
//{
//   "Participants",
//};
//
//
//const Gmat::ParameterType
//GeometricMeasurement::PARAMETER_TYPE[GeometricMeasurementParamCount -
//                                     GmatBaseParamCount] =
//{
//   Gmat::OBJECTARRAY_TYPE,
//};


GeometricMeasurement::GeometricMeasurement(const std::string &type,
      const std::string &nomme) :
   GmatBase          (Gmat::CORE_MEASUREMENT, type, nomme),
   satEpochID        (-1)
{
   objectTypes.push_back(Gmat::CORE_MEASUREMENT);
   objectTypeNames.push_back("CoreMeasurement");  // Move to CoreMeas when built
   objectTypeNames.push_back("GeometricMeasurement");

   parameterCount = GeometricMeasurementParamCount;
}

GeometricMeasurement::~GeometricMeasurement()
{
}

GeometricMeasurement::GeometricMeasurement(const GeometricMeasurement& gm) :
   GmatBase          (gm),
   participants      (gm.participants),
   satEpochID        (gm.satEpochID)
{
}

GeometricMeasurement& GeometricMeasurement::operator=(
      const GeometricMeasurement& gm)
{
   if (&gm != this)
   {
      participants = gm.participants;
      satEpochID   = gm.satEpochID;
   }

   return *this;
}

bool GeometricMeasurement::Initialize()
{
   bool retval = false;

   // todo Switch to CoreMeasurement::Initialize() here
   if (GmatBase::Initialize())
   {
      if (participants.size() < 2)
         MessageInterface::ShowMessage("Range vector calcs require 2 "
               "participants; cannot initialize\n");
      else
      {
         // For now, require specific order for the participants
         // todo: Allow arbitrary participant ordering
         if ((participants[0]->IsOfType(Gmat::SPACE_POINT)) &&
             (participants[1]->IsOfType(Gmat::SPACECRAFT)))
         {
            satEpochID = participants[1]->GetParameterID("A1Epoch");
            retval = true;
         }
         else
         {
            MessageInterface::ShowMessage("Participant mismatch in Range "
                  "measurement: Current code requires one Spacecraft and one "
                  "other SpacePoint participant; cannot initialize\n");
         }
      }
   }

   return retval;
}

// Here are the parameter access shells in case we need them later
//
//std::string GeometricMeasurement::GetParameterText(const Integer id) const
//{
//   if (id >= GmatBaseParamCount && id < GeometricMeasurementParamCount)
//      return PARAMETER_TEXT[id - GmatBaseParamCount];
//   return GmatBase::GetParameterText(id);
//}
//
//Integer GeometricMeasurement::GetParameterID(const std::string &str) const
//{
//   for (Integer i = GmatBaseParamCount; i < GeometricMeasurementParamCount; i++)
//   {
//      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
//         return i;
//   }
//
//   return GmatBase::GetParameterID(str);
//}
//
//Gmat::ParameterType GeometricMeasurement::GetParameterType(const Integer id) const
//{
//   if (id >= GmatBaseParamCount && id < GeometricMeasurementParamCount)
//      return PARAMETER_TYPE[id - GmatBaseParamCount];
//
//   return GmatBase::GetParameterType(id);
//}
//
//std::string GeometricMeasurement::GetParameterTypeString(const Integer id) const
//{
//   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
//}


bool GeometricMeasurement::SetRefObject(GmatBase *obj,
      const Gmat::ObjectType type, const std::string &name)
{
   if (obj->IsOfType(Gmat::SPACE_POINT))
      if (find(participants.begin(), participants.end(), obj) == participants.end())
      {
         // Cheating here for the moment to be sure GroundStation is 1st object
         if (obj->IsOfType(Gmat::GROUND_STATION))
            participants.insert(participants.begin(), (SpacePoint*)obj);
         else
            participants.push_back((SpacePoint*)obj);

         // Set IDs
         currentMeasurement.participantIDs.clear();
         for (std::vector<SpacePoint*>::iterator i = participants.begin();
               i != participants.end(); ++i)
         {
            currentMeasurement.participantIDs.push_back((*i)->
                  GetStringParameter("Id"));
         }

         #ifdef DEBUG_MEASUREMENT_INITIALIZATION
            MessageInterface::ShowMessage(
                  "Added %s named %s to a %s GeometricMeasurement\n",
                  obj->GetTypeName().c_str(), obj->GetName().c_str(),
                  typeName.c_str());

            if (participants.size() == 2)
               CalculateMeasurement(false);
         #endif
      }

   return true;
}

bool GeometricMeasurement::SetRefObject(GmatBase *obj,
      const Gmat::ObjectType type, const std::string &name, const Integer index)
{
   // todo: Manage anchor participant
   return true;
}

MeasurementData* GeometricMeasurement::GetMeasurementDataPointer()
{
   return &currentMeasurement;
}

Rmatrix* GeometricMeasurement::GetDerivativePointer()
{
   return &currentDerivatives;
}


const MeasurementData& GeometricMeasurement::CalculateMeasurement(bool withDerivatives)
{
   if (Evaluate(withDerivatives) == false)
      // throw here
      ;

   return currentMeasurement;
}


const Rmatrix & GeometricMeasurement::CalculateMeasurementDerivatives()
{
   // Add a check to see if current data has been evaluated

   return currentDerivatives;
}


void GeometricMeasurement::CalculateRangeVector()
{
   currentMeasurement.epoch = participants[1]->GetRealParameter(satEpochID);

   p1Loc = participants[0]->GetMJ2000Position(currentMeasurement.epoch);
   p2Loc = participants[1]->GetMJ2000Position(currentMeasurement.epoch);
   rangeVec = p2Loc - p1Loc;
}
