//$Header$
//------------------------------------------------------------------------------
//                             DataFormats
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Defines the various data formats for standard measurement data types.
 *
 * This class allows other classes to know what to expect when requesting
 * data from a file reader.
 *
 */
//------------------------------------------------------------------------------

#ifndef DataFormats_hpp
#define	DataFormats_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "B3ObType.hpp"
#include "TLEObType.hpp"
#include "SLRObType.hpp"
#include "CCSDSObType.hpp"
#include "TDMCCSDSObType.hpp"
#include "OPMCCSDSObType.hpp"
#include "OEMCCSDSObType.hpp"
#include "APMCCSDSObType.hpp"
#include "AEMCCSDSObType.hpp"
#include "CCSDSHeader.hpp"
#include "CCSDSMetaData.hpp"
#include "TDMCCSDSMetaData.hpp"
#include "OPMCCSDSMetaData.hpp"
#include "OEMCCSDSMetaData.hpp"
#include "APMCCSDSMetaData.hpp"
#include "AEMCCSDSMetaData.hpp"
#include "CCSDSData.hpp"
#include "SpacecraftInertiaCCSDSData.hpp"
#include "SpacecraftParametersCCSDSData.hpp"
#include "TrackingCCSDSData.hpp"
#include "KeplerianElementsCCSDSData.hpp"
#include "StateVectorCCSDSData.hpp"
#include "OPMStateVectorCCSDSData.hpp"
#include "OEMStateVectorCCSDSData.hpp"
#include "QuaternionCCSDSData.hpp"
#include "APMQuaternionCCSDSData.hpp"
#include "AEMQuaternionCCSDSData.hpp"
#include "EulerAngleCCSDSData.hpp"
#include "APMEulerAngleCCSDSData.hpp"
#include "AEMEulerAngleCCSDSData.hpp"
#include "SpinStabilizedCCSDSData.hpp"
#include "APMSpinStabilizedCCSDSData.hpp"
#include "AEMSpinStabilizedCCSDSData.hpp"
#include "ManeuverCCSDSData.hpp"
#include "AttitudeManeuverCCSDSData.hpp"

#endif	/* _Dataformats_hpp */


