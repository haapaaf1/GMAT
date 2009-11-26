//$Header$
//------------------------------------------------------------------------------
//                             CCSDSMetaData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/04
//
/**
 *
 * This class specifies the CCSDS MetaData base class from which the individual
 * meta data constructs for each of the CCSDS data formats is derived
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSMETADATA_HPP
#define	_CCSDSMETADATA_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "MessageInterface.hpp"

class CCSDSMetaData
{

public:

    CCSDSMetaData();
    CCSDSMetaData(const CCSDSMetaData &md);
    const CCSDSMetaData& CCSDSMetaData::operator=(const CCSDSMetaData &md);
    virtual ~CCSDSMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSMetaData *myMetaData);

    std::string GetQuaternionTypeText(const Integer id) const;
    Integer    GetQuaternionTypeID(const std::string &str) const;
    std::string GetAttitudeDirText(const Integer id) const;
    Integer     GetAttitudeDirID(const std::string &str) const;
    std::string GetRateFrameText(const Integer id) const;
    Integer    GetRateFrameID(const std::string &str) const;
    std::string GetAttitudeTypeText(const Integer id) const;
    Integer     GetAttitudeTypeID(const std::string &str) const;

    virtual bool        GetBooleanDataParameter(const Integer id) const;
    virtual bool        GetBooleanDataParameter(const std::string &label) const;
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
    
    virtual const std::string* GetKeywords() const = 0;
    virtual const Integer GetKeywordID(const std::string str) const = 0;
    virtual std::string GetUnits(const Integer &id) const = 0;

    virtual Integer GetDataParameterID(const std::string &str) const = 0;
    virtual std::string GetDataParameterText(const Integer id) const = 0;
    virtual Gmat::ParameterType GetDataParameterType(const Integer id) const = 0;
    virtual std::string GetDataParameterTypeString(const Integer id) const = 0;

    virtual bool IsParameterRequired(const Integer id) const = 0;
    virtual bool Validate() const = 0;
    
    virtual bool IsParameterDefined(std::string value) const;
    virtual bool IsParameterDefined(StringArray value) const;
    virtual bool IsParameterDefined(Real value) const;
    virtual bool IsParameterDefined(Integer value) const;
    virtual bool IsParameterDefined(bool value) const;

    enum CCSDS_METADATA_REPS
    {
        EndCCSDSMetaDataReps = 0
    };

    enum CCSDS_ATTITUDE_TYPE
    {
        CCSDS_QUATERNION_ID = 0,
        CCSDS_QUATERNION_DERIVATIVE_ID,
        CCSDS_QUATERNION_RATE_ID,
        CCSDS_EULER_ANGLE_ID,
        CCSDS_EULER_ANGLE_RATE_ID,
        CCSDS_SPIN_ID,
        CCSDS_SPIN_NUTATION_ID,
        EndCCSDSAttitudeTypeReps
    };

    enum CCSDS_QUATERNION_TYPE
    {
        CCSDS_QUATERNION_FIRST_ID = 0,
        CCSDS_QUATERNION_LAST_ID,
        EndCCSDSQuaternionTypeReps
    };

    enum CCSDS_ATTITUDE_DIR
    {
        CCSDS_ATTITUDE_A2B_ID = 0,
        CCSDS_ATTITUDE_B2A_ID,
        EndCCSDSAttitudeDirReps
    };

    enum CCSDS_RATE_FRAME
    {
        CCSDS_RATE_FRAME_A_ID = 0,
        CCSDS_RATE_FRAME_B_ID,
        EndCCSDSRateFrameReps
    };

    friend class CCSDSDataFile;
    friend class TDMCCSDSDataFile;
    friend class OPMCCSDSDataFile;
    friend class OEMCCSDSDataFile;
    friend class APMCCSDSDataFile;
    friend class AEMCCSDSDataFile;

private:

    static const std::string CCSDS_RATE_FRAME[EndCCSDSRateFrameReps];
    static const std::string CCSDS_ATTITUDE_TYPE[EndCCSDSAttitudeTypeReps];
    static const std::string CCSDS_ATTITUDE_DIR[EndCCSDSAttitudeDirReps];
    static const std::string CCSDS_QUATERNION_TYPE[EndCCSDSQuaternionTypeReps];
    
};

#endif	/* _CCSDSMETADATA_HPP */

