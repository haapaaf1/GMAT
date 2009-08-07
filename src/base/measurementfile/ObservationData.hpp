//$Id$
//------------------------------------------------------------------------------
//                         ObservationData
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/08/06
//
/**
 * Class that contains observation data
 *
 * This class is essentially a struct designed to contain the observation data
 * retrieved from a ObType object.
 */
//------------------------------------------------------------------------------


#ifndef OBSERVATIONDATA_HPP_
#define OBSERVATIONDATA_HPP_

#include "EstimationDefs.hpp"

/// Descriptor here
class ObservationData
{
public:
   ObservationData();
   virtual ~ObservationData();
   ObservationData(const ObservationData& od);
   ObservationData&  operator=(const ObservationData& od);

   void              Clear();

// Explicitly public so that this class acts like a struct
public:
   /// The text name of the data type, if available
   std::string       typeName;
   /// The type of measurement in this record
   Gmat::MeasurementType
                     type;
   /// Unique ID for associated data stream.
   Integer           uniqueID;
   /// The epoch of the measurement
   GmatEpoch         epoch;
   /// Who is involved in the measurement.  First one is the "anchor" node
   StringArray       participantIDs;
   /// The observed value.  Array to handle more than one value, like AZ_EL
   RealArray         value;

   /// Strings describing any ancillary data in the observation source
   StringArray       extraDataDescriptions;
   /// Types for any ancillary data in the observation source
   IntegerArray      extraTypes;
   /// Ancillary data from the observation source, in string format
   StringArray       extraData;
};

#endif /* OBSERVATIONDATA_HPP_ */
