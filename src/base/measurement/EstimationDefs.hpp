//$Id$
//------------------------------------------------------------------------------
//                         EstimationDefs
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/24
//
/**
 * Measurement model definitions used in GMAT's estimation subsystem
 *
 * This file adds structures and enumerations to the Gmat namespace that are
 * used in the estimation subsystem for measurement modeling, both during
 * simulation and during estimation.
 */
//------------------------------------------------------------------------------


#ifndef EstimationDefs_hpp
#define EstimationDefs_hpp

#include "gmatdefs.hpp"

// Temporary while we figure out the measurement model classes
#define CoreMeasurement GeometricMeasurement

namespace Gmat
{
   /// Identifiers for the known measurement data types
   enum MeasurementType
   {
      RANGE = 7000,
      // RANGE_RATE,
      // AZ_EL,
      // RA_DEC,
      UNKNOWN_MEASUREMENT,
      /// Allow for user defined measurements
      CUSTOM_MEASUREMENT_START = 7500,
      CUSTOM_MEASUREMENT_END   = 7800
   };

   /// Identifiers for the known data sources
   enum MeasurementSource
   {
      GMAT_INTERNAL_DATA = 3000,
      // B3_DATA,
      // RINEX_DATA,
      // SLR_DATA,
      // SP3_DATA,
      // TLE_DATA,
      UNKNOWN_SOURCE,
      /// Allow for user defined sources
      CUSTOM_SOURCE_START = 3500,
      CUSTOM_SOURCE_END   = 3800
   };

}


// TBD: Do we want this gorp or not for the early builds?
//// Forward reference to allow for a typedef
//class Datafile;
//
//// Type definitions
//typedef Datafile MeasurementStream;

#endif /* EstimationDefs_hpp */
