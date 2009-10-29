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
#include "B3Obtype.hpp"
#include "TLEObtype.hpp"
#include "SLRObtype.hpp"
#include "CCSDSObtype.hpp"
#include "CCSDSHeader.hpp"
#include "CCSDSData.hpp"
#include "CCSDSStateVector.hpp"
#include "CCSDSOPMStateVector.hpp"
#include "CCSDSOEMStateVector.hpp"
#include "CCSDSQuaternion.hpp"
#include "CCSDSAPMQuaternion.hpp"
#include "CCSDSAEMQuaternion.hpp"
#include "CCSDSEulerAngle.hpp"
#include "CCSDSAPMEulerAngle.hpp"
#include "CCSDSAEMEulerAngle.hpp"
#include "CCSDSKeplerianElements.hpp"
#include "CCSDSSpinStabilized.hpp"
#include "CCSDSAPMSpinStabilized.hpp"
#include "CCSDSAEMSpinStabilized.hpp"
#include "CCSDSSpacecraftInertia.hpp"
#include "CCSDSSpacecraftParameters.hpp"
#include "CCSDSManeuver.hpp"
#include "CCSDSAttitudeManeuver.hpp"
#include "CCSDSTDMMetaData.hpp"
#include "CCSDSTDMObtype.hpp"
#include "CCSDSOPMMetaData.hpp"
#include "CCSDSOPMObtype.hpp"
#include "CCSDSOEMMetaData.hpp"
#include "CCSDSOEMObtype.hpp"
#include "CCSDSAPMMetaData.hpp"
#include "CCSDSAPMObtype.hpp"
#include "CCSDSAEMMetaData.hpp"
#include "CCSDSAEMObtype.hpp"
//#include "SP3cObtype.hpp"
//#include "RINEXObtype.hpp"

#endif	/* _Dataformats_hpp */


