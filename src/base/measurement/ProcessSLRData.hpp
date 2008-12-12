//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRData
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
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSLRData_hpp
#define	ProcessSLRData_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "DataFile.hpp"

class ProcessSLRData : public DataFile
{

public:
    
    ProcessSLRData(const std::string &itsName);
    ~ProcessSLRData();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    std::string Ilrs2Cospar(std::string ilrsSatnum);

    enum SLR_DATA_REPS
    {
	SLRTYPE_ID,
	ILRSSATNUM_ID,
	YEAR_ID,
	DAYOFYEAR_ID,
	CDPPADID_ID,
	CDPSYSNUM_ID,
	CDPOCCUPANCYSEQUENCENUM_ID,
	WAVELENGTH_ID,
	CALSYSDELAY_ID,
	CALDELAYSHIFT_ID,
	RMSSYSDELAY_ID,
	NORMALPOINTWINDOWINDICATOR_ID,
	EPOCHTIMESCALEINDICATOR_ID,
	SYSCALMETHODINDICATOR_ID,
	SCHINDICATOR_ID,
	SCIINDICATOR_ID,
	PASSRMS_ID,
	DATAQUALASSESSMENTINDICATOR_ID,
	FORMATREVISIONNUM_ID,
	TIMEOFLASERFIRING_ID,
	TWOWAYTIMEOFFLIGHT_ID,
	BINRMSRANGE_ID,
	SURFACEPRESSURE_ID,
	SURFACETEMP_ID,
	RELATIVEHUMIDITY_ID,
	NUMRAWRANGES_ID,
	DATARELEASEFLAG_ID,
	RAWRANGEFACTOR_ID,
	NORMALPOINTWINDOWINDICATOR2_ID,
	SIGNALTONOISERATIO_ID,
        BURSTCALSYSDELAY_ID,
	SIGNALSTRENGTH_ID,
        ANGLEORIGININDICATOR_ID,
        AZIMUTH_ID,
        ELEVATION_ID,
        EndSLRDataReps
    };

    // Measurement Data Access functions
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;

private:

    static const std::string SLRFILEFORMAT_DESCRIPTIONS[EndSLRDataReps];
    static const Gmat::ParameterType SLRPARAMETER_TYPE[EndSLRDataReps];

    bool GetNextOb(slr_obtype *mySLR);


    // Specific data type processing functions
    bool FindSLRHeaderLine( std::ifstream &theFile,
                            slr_header *mySLRheader, Integer &flag );
    bool GetData(std::ifstream &theFile, slr_header *mySLRheader,
                 slr_obtype *mySLRdata);


    bool GetSLRHeader(std::string &lff, slr_header *mySLRheader);
    bool GetSLRData(std::string &lff, slr_header *mySLRheader,
                    slr_obtype *mySLRdata);

    // Vector containers for the measurement data
    std::vector<slr_header*> slrHeader;
    std::vector<slr_obtype*> slrData;

    //Current iterator pointing at data
    std::vector<slr_obtype*>::iterator i;

    //Current iteratory pointing at header
    std::vector<slr_header*>::iterator i_h;


};

#endif	/* _ProcessSLRData_hpp */

