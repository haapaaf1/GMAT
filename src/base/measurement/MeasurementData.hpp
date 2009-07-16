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
 * Definition of the MeasurementData class, used to manage calculated
 * measurements
 */
//------------------------------------------------------------------------------


#ifndef MeasurementData_hpp
#define MeasurementData_hpp

#include "EstimationDefs.hpp"

/**
 *  The measurement data structure
 *
 *  This class acts mostly as a structure which provides the core set of
 *  information used for calculated measurement data.  Each measurement can be
 *  captured in this structure.
 *
 *  This struct presents as a class because it includes code for the assignment
 *  operator and copy constructor.  To me, it's just bad form to call something
 *  a struct and include those pieces.
 */
class MeasurementData
{
public:
   MeasurementData();
   virtual ~MeasurementData();
   MeasurementData(const MeasurementData& md);
   MeasurementData operator=(const MeasurementData& md);


// Explicitly public so that this class acts like a struct
public:
   /// The type of measurement in this record
   Gmat::MeasurementType   type;
   /// The epoch of the measurement
   GmatEpoch         epoch;
   /// Who is involved in the measurement.  First one is the "anchor" node
   StringArray       participantIDs;
   /// The measured value.  Array to handle more than one value, like AZ_EL
   RealArray         value;
   /// Flag indicating if the measurement could be made when it was attempted
   bool              isFeasible;
   /// Value used for root finding
   Real              feasibilityValue;
};

#endif /* MeasurementData_hpp */
