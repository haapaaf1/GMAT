//$Header$
//------------------------------------------------------------------------------
//                             SLRHeader
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/03
//
/**
 *
 * This utility class specifies the SLR header data type.
 * See the following website for a complete description of the SLR
 * data format. The comments below are taken directly from the ILRS website.
 * http://ilrs.gsfc.nasa.gov/products_formats_procedures/normal_point/np_format.html
 *
 */
//------------------------------------------------------------------------------

#ifndef _SLRHEADER_HPP
#define	_SLRHEADER_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"

using namespace std; // so we don't have to type std::cout and std::endl

class SLRHeader
{

public:

    SLRHeader();
    SLRHeader(const SLRHeader &header);
    const SLRHeader& SLRHeader::operator=(const SLRHeader &header);
    virtual ~SLRHeader();

    Integer GetSLRType();

    friend Integer SLRCheckSum(const std::string &str);

    friend std::ostream& operator<< (std::ostream &output, const SLRHeader *mySLRheader);

    friend class SLRObType;
    friend class SLRDataFile;

    // Data access functions
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    Real        GetRealDataParameter(const Integer id) const;
    Real        GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    
    bool SetDataParameter(const Integer id, const Integer &value);
    bool SetDataParameter(const std::string &label, const Integer &value);
    bool SetDataParameter(const Integer id, const Real &value);
    bool SetDataParameter(const std::string &label, const Real &value);
    bool SetDataParameter(const Integer id, const std::string &value);
    bool SetDataParameter(const std::string &label, const std::string &value);

    enum SLR_HEADERDATA_REPS
    {
        SLR_TYPE_ID,
	SLR_ILRSSATNUM_ID,
	SLR_YEAR_ID,
	SLR_DAYOFYEAR_ID,
	SLR_CDPPADID_ID,
	SLR_CDPSYSNUM_ID,
	SLR_CDPOCCUPANCYSEQUENCENUM_ID,
	SLR_WAVELENGTH_ID,
	SLR_CALSYSDELAY_ID,
	SLR_CALDELAYSHIFT_ID,
	SLR_RMSSYSDELAY_ID,
	SLR_NORMALPOINTWINDOWINDICATOR_ID,
	SLR_EPOCHTIMESCALEINDICATOR_ID,
	SLR_SYSCALMETHODINDICATOR_ID,
	SLR_SCHINDICATOR_ID,
	SLR_SCIINDICATOR_ID,
	SLR_PASSRMS_ID,
	SLR_DATAQUALASSESSMENTINDICATOR_ID,
	SLR_FORMATREVISIONNUM_ID,
        EndSLRHeaderDataReps
    };


protected:

    static const bool SLR_IS_REQUIRED[EndSLRHeaderDataReps];
    static const Gmat::ParameterType SLR_PARAMETER_TYPE[EndSLRHeaderDataReps];
    static const std::string SLR_UNIT_DESCRIPTIONS[EndSLRHeaderDataReps];
    static const std::string SLR_FILEFORMAT_DESCRIPTIONS[EndSLRHeaderDataReps];
    
    // SLR Type
    // 99999 - Standard observation data
    // 88888 - Engineering/Simulated observation data
    Integer slrType;

    // ILRS Satellite Identifier - 7 digit number based on COSPAR
    // ILRS Satellite Identification Format: (YYXXXAA)
    // Example: LAGEOS-1 ILRS Satellite ID is 7603901
    std::string ilrsSatnum;

    // Year, Day of Year of century
    Integer year;
    Integer dayOfYear;

    // Crustal Dynamics Project PAD ID - a 4 digit monument identification
    Integer cdpPadID;

    // Crustal Dynamics Project 2-digit system number
    Integer cdpSysNum;

    // Crustal Dynamics Project 2-digit occupany sequence number
    Integer cdpOccupancySequenceNum;

    // Wavelength of the laser in nanometers. The original spec provides
    // an integer value with the following convention.
    // 3000 - 9999: units are 0.1 nanometer
    // 1000 - 2999: units are 1.0 nanometer
    // For the station generating the data, the rule is:
    // Wavelength in rate 0.3000 - 0.9999 microns: unit 0.1 nanometer
    // Wavelength in rate 1.000 - 2.999 microns: unit 1.0 nanometer
    // We have converted the units appropriately to nanometers.
    Real wavelength;

    // Calibration system delay (two-way value in picoseconds)
    Integer calSysDelay;

    // Calibration delay shift (two-way value in picoseconds)
    Integer calDelayShift;

    // Root Mean Square (RMS) of raw system delay values from the mean.
    // Two-way value in picoseconds. If pre- and post- pass calibrations
    // are made,use the mean of the two RMS values, or the RMS of
    // the combined data set.
    Integer rmsSysDelay;

    // Normal Point window indicator (an integer from 0 to 9)
    // 0: not a normal point
    // 1: 5-second normal point (GFZ-1)
    // 2: LLR normal point
    // 3: 15-second normal point (TOPEX)
    // 4: 20-second normal point
    // 5: 30-second normal point
    // 6: 1-minute normal point
    // 7: 2-minute normal point (LAGEOS)
    // 8: 3-minute normal point
    // 9: 5-minute normal point (ETALON)
    Integer normalPointWindowIndicator;

    // Epoch time scale indicator
    // 3: UTC (USNO)
    // 4: UTC (GPS)
    // 7: UTC (BIPM) (BIH prior to 1988)
    Integer epochTimeScaleIndicator;

    // System calibration method and delay shift indicator.
    // Indicates the type of calibration and the type of
    // calibration shift given in columns 33-38
    //        Pre- to Post-Pass    Minimum to Maximum
    //        Calibration Shift    Calibration Shift
    //  External cal    0        5
    //  Internal cal    1        6
    //  Burst cal        2        7
    //  Some other cal    3        8
    //  Not used        4        9
    Integer sysCalMethodIndicator;

    // System CHange indicator (SCH). A flag to increment for every
    // major change to the system (hardware or software). After the
    // value '9' return to '0', and then continue incrementing. The
    // station and data centers should keep a log in a standard format
    // of the value used, the date of the change, and a description
    // of the change.
    Integer schIndicator;

    // System Configuration Indicator (SCI). A flag used to indicate
    // alternative modes of operation for a system (e.g., choice of
    // alternative timers or detectors, or use of a different mode of
    // operation for high satellites). Each value of the flag indicates
    // a particular configuration, which is described in a log file held
    // at the station and at the data centers. If only a single
    // configuration is used then use a fixed value. If a new
    // configuration is introduced then use the next higher flag value.
    // If value exceeds '9' then return to '0', overwriting a previous
    // configuration flag (it is not likely that a station will have 10
    // current possible configurations).
    Integer sciIndicator;

    // Pass RMS from the mean of raw range values minus the trend function,
    // for accepted ranges (two-way value in picoseconds).
    Integer passRMS;

    // Data quality assessment indicator
    // For LLR data:
    // 0: Undefined or no comment.
    // 1: Clear, easily filtered data, with little or no noise.
    // 2: Clear data with some noise; filtering is slightly compromised by
    //noise level.
    // 3: Clear data with a significant amount of noise, or weak data with
    //little noise. Data are certainly present, but filtering is difficult.
    // 4: Un-clear data; data appear marginally to be present, but are very
    //difficult to separate from noise during filtering. Signal to
    //noise ratio can be less than 1:1.
    // 5: No data apparent.
    Integer dataQualAssessmentIndicator;

    // Format revision number indicator.
    // Value '1' for the 1997 revision.
    // Implied value '0' or 'space' for original 1990 release.
    // Revision 2 and above, use byte 49 in data record to indicate
    // power of ten with which to multiply the number stored in
    // bytes 44-47 of data record.
    Integer formatRevisionNum;

};



#endif	/* _SLRHEADER_HPP */

