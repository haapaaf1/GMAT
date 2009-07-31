//$Id$
//------------------------------------------------------------------------------
//                         MeasurementData
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/07/15
//
/**
 * Implementation of the MeasurementData class, used to manage calculated
 * measurements
 */
//------------------------------------------------------------------------------


#include "MeasurementData.hpp"

MeasurementData::MeasurementData() :
   type        (Gmat::UNKNOWN_MEASUREMENT),
   typeName    ("Unknown"),
   uniqueID    (-1),
   epoch       (0.0),
   isFeasible  (false)
{
}

MeasurementData::~MeasurementData()
{
}

MeasurementData::MeasurementData(const MeasurementData& md) :
   type           (md.type),
   typeName       (md.typeName),
   uniqueID       (md.uniqueID),
   epoch          (md.epoch),
   participantIDs (md.participantIDs),
   value          (md.value),
   isFeasible     (md.isFeasible)
{
}

MeasurementData MeasurementData::operator=(const MeasurementData& md)
{

   if (&md != this)
   {
      type           = md.type;
      typeName       = md.typeName;
      uniqueID       = md.uniqueID;
      epoch          = md.epoch;
      participantIDs = md.participantIDs;
      value          = md.value;
      isFeasible     = md.isFeasible;
   }

   return *this;
}
