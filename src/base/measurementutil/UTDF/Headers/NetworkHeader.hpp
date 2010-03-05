/* 
 * File:   NETWORKHEADER.hpp
 * Author: mwilkins
 *
 * Created on March 1, 2010, 9:25 AM
 */

#ifndef _NETWORKHEADER_HPP
#define	_NETWORKHEADER_HPP
//$Header$
//------------------------------------------------------------------------------
//                             NetworkHeader
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the CCSDS Header data that is common to all
 * of the CCSDS data message formats.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSHEADER_HPP
#define	_CCSDSHEADER_HPP


#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"
#include "MessageInterface.hpp"

class NetworkHeader
{

public:

    NetworkHeader();
    NetworkHeader(const NetworkHeader &header);
    const NetworkHeader& operator=(const NetworkHeader &header);
    virtual ~NetworkHeader();

    virtual Real        GetRealDataParameter(const Integer id) const;
    virtual Real        GetRealDataParameter(const std::string &label) const;
    virtual Integer     GetIntegerDataParameter(const Integer id) const;
    virtual Integer     GetIntegerDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    virtual bool        SetDataParameter(const Integer id, const Integer &value);
    virtual bool        SetDataParameter(const std::string &label, const Integer &value);
    virtual bool        SetDataParameter(const Integer id, const Real &value);
    virtual bool        SetDataParameter(const std::string &label, const Real &value);
    virtual bool        SetDataParameter(const Integer id, const std::string &value);
    virtual bool        SetDataParameter(const std::string &label, const std::string &value);
    virtual bool        SetDataParameter(const Integer id, const StringArray &value);
    virtual bool        SetDataParameter(const std::string &label, const StringArray &value);
    virtual bool        SetDataParameter(const Integer id, const bool &value);
    virtual bool        SetDataParameter(const std::string &label, const bool &value);

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    bool IsParameterRequired(const Integer id) const;
    virtual bool IsParameterDefined(std::string value) const;
    virtual bool IsParameterDefined(StringArray value) const;
    virtual bool IsParameterDefined(Real value) const;
    virtual bool IsParameterDefined(Integer value) const;
    virtual bool IsParameterDefined(bool value) const;
    virtual bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const NetworkHeader *myCCSDSheader);

    enum UTDF_NETWORKDATA_REPS
    {
	UTDF_SOURCE_ID,
	UTDF_DESTINATION_ID,
	UTDF_BLOCKSEQUENCENUMBER_ID,
	UTDF_FORMATCODE_ID,
	UTDF_BLOCKSIZE_ID,
	EndNetworkHeaderDataReps
    };

    enum UTDF_FORMATCODE_REPS
    {
        CBAND_ID,
        TDPS_ID,
        TDRSS_ID,
        TDPS_UPDATEDATA_ID,
	EndFormatCodeReps
    };

    friend class UTDFDataFile;

protected:

    static const std::string UTDF_FORMATCODE_DESCRIPTIONS[EndFormatCodeReps];
\    static const bool UTDF_NETWORKHEADER_IS_REQUIRED[EndNetworkHeaderDataReps];
    static const Gmat::ParameterType UTDF_NETWORKHEADER_PARAMETER_TYPE[EndNetworkHeaderDataReps];
    static const std::string UTDF_NETWORKHEADER_FILEFORMAT_DESCRIPTIONS[EndNetworkHeaderDataReps];

    Integer source;
    Integer destination;
    long int blockSequenceNumber;
    Integer formatCode;

    // BlockSize flag
    // 1 = 4800 bit block
    // 0 = 1200 bit block
    bool blockSize;

};

#endif	/* _NETWORKHEADER_HPP */

