//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#include "ObservationData.hpp"
#include "EstimationDefs.hpp"

ObservationData::ObservationData() :
   typeName          (""),
   type              (Gmat::UNKNOWN_MEASUREMENT),
   uniqueID          (-1),
   epoch             (-1.0)
{
}

ObservationData::~ObservationData()
{
}


ObservationData::ObservationData(const ObservationData& od):
   typeName                (od.typeName),
   type                    (od.type),
   uniqueID                (od.uniqueID),
   epoch                   (od.epoch),
   participantIDs          (od.participantIDs),
   value                   (od.value),
   extraDataDescriptions   (od.extraDataDescriptions),
   extraTypes              (od.extraTypes),
   extraData               (od.extraData)
{
}

ObservationData& ObservationData::operator=(const ObservationData& od)
{
   if (&od != this)
   {
      typeName                = od.typeName;
      type                    = od.type;
      uniqueID                = od.uniqueID;
      epoch                   = od.epoch;
      participantIDs          = od.participantIDs;
      value                   = od.value;
      extraDataDescriptions   = od.extraDataDescriptions;
      extraTypes              = od.extraTypes;
      extraData               = od.extraData;
   }

   return *this;
}


void ObservationData::Clear()
{
   typeName                = "";
   type                    = Gmat::UNKNOWN_MEASUREMENT;
   uniqueID                = -1;
   epoch                   = 0.0;
   participantIDs.clear();
   value.clear();
   extraDataDescriptions.clear();
   extraTypes.clear();
   extraData.clear();
}
