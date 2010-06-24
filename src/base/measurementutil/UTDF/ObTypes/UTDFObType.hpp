//$Header$
//------------------------------------------------------------------------------
//                             UTDFObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2010/03/01
//
/**
 *
 * This class specifies the UTDF base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------

#ifndef _UTDFDATAFIELD_HPP
#define	_UTDFDATAFIELD_HPP

#include "ObType.hpp"

class UTDFObType : public ObType
{

public :

    UTDFObType();
    UTDFObType(const std::string &type, const std::string &name);
    UTDFObType(const UTDFObType &ob);
    const UTDFObType& operator=(const UTDFObType &ob);
    virtual ~UTDFObType();

    GmatBase *Clone() const;

    // Measurement Data Access function

    virtual std::string GetDataParameterText(const Integer id) const;
    virtual std::string GetDataUnits(const Integer id) const;
    virtual Integer     GetDataParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                        GetDataParameterType(const Integer id) const;
    virtual std::string GetDataParameterTypeString(const Integer id) const;

    virtual Integer     GetIntegerDataParameter(const Integer id) const;
    virtual Integer     GetIntegerDataParameter(const std::string &label) const;
    virtual Real        GetRealDataParameter(const Integer id) const;
    virtual Real        GetRealDataParameter(const std::string &label) const;
    virtual bool        GetBoolDataParameter(const Integer id) const;
    virtual bool        GetBoolDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;

    virtual bool        SetDataParameter(const Integer id, const Integer &value);
    virtual bool        SetDataParameter(const std::string &label, const Integer &value);
    virtual bool        SetDataParameter(const Integer id, const Real &value);
    virtual bool        SetDataParameter(const std::string &label, const Real &value);
    virtual bool        SetDataParameter(const Integer id, const std::string &value);
    virtual bool        SetDataParameter(const std::string &label, const std::string &value);
    virtual bool        SetDataParameter(const Integer id, const bool &value);
    virtual bool        SetDataParameter(const std::string &label, const bool &value);

    friend std::ostream& operator<< (std::ostream &output, const UTDFObType *myUTDF);

    // Declare DataFile a friend class so that we have access
    // directly to variables instead of having to use Get/Set
    friend class UTDFDataFile;

    enum ANTENNA
    {
        NONE_ID = 0,
        NORTH_ANTENNA_ID = 9,
        CENTRAL_ANTENNA_ID = 10,
        SOUTH_ANTENNA_ID = 11,
        EndAntenna
    };

    enum TRACKINGDATAROUTER
    {
        GSFC_ID,
        GSFC_CNES,
        GSFC_JAXA,
        GSFC_ESRO,
        GSFC_JSC,
        EndTrackingDataRouter
    };

    enum TDRSSTRACKINGDATAROUTER
    {
        TRACKING_DATA_ID,
        SIMULATED_DATA_ID,
        EndTDRSSTrackingDataRouter
    };

    enum ANTENNASIZE
    {
        LESS_THAN_1M_ID = 0,
        _3_9M_ID,
        _4_3M_ID,
        _9M_ID,
        _12M_ID,
        _26M_ID,
        TDRSS_GROUNDANTENNA_ID,
        _6M_ID,
        _7_3M_ID,
        _8_0M_ID,
        EndAntennaSize
    };

    enum ANTENNAGEOMETRY
    {
        AZEL_ID = 0,
        XY_XSOUTH_ID,
        XY_XEAST_ID,
        RADEC_ID,
        HRDEC_ID,
        EndAntennaGeometry
    };

    enum FREQUENCYBAND
    {
        VHF_ID = 1,
        UHF_ID,
        SBAND_ID,
        CBAND_ID,
        XBAND_ID,
        KUBAND_ID,
        VISIBLE_ID,
        SBANDUP_KUBANDDOWN_ID,
        EndFrequencyBand
    };

    enum DATATRANSMISSIONTYPE
    {
        TEST_ID = 0,
        SPARE_ID,
        SIMULATED_ID,
        RESUBMIT_ID,
        REALTIME_ID,
        PLAYBACK_ID,
        EndDataTransmissionType
    };

    enum TRACKERTYPE
    {
        CBAND_PULSE_TRACK_ID = 0,
        SRE_RER_ID,
        XY_ANGLESONLY_ID,
        SGLS_ID = 4,
        TDRSS_ID = 6,
        STGT_WSGTU_ID,
        TDRSS_TTANDC_ID,
        EndTrackerTrype
    };

    enum TRACKINGSERVICECONFIGURATION
    {
        RETURNLINKONLY_ID,
        SAME_TDRS_LINK_ID,
        HYBRID_TDRS_LINK_ID,
        EndTrackingServiceConfiguration
    };

    enum UTDFOBTYPEREPS
    {
        UTDF_ROUTER_ID = EndObTypeReps,
        UTDF_YEAR_ID,
        UTDF_SIC_ID,
        UTDF_VID_ID,
        UTDF_SECSOFYEAR_ID,
        UTDF_MICROSECSOFSEC_ID,
        UTDF_ANGLE1_ID,
        UTDF_ANGLE2_ID,
        UTDF_RTLT_ID,
        UTDF_FREQUENCY_ID,
        UTDF_DOPPLERCOUNT_ID,
        UTDF_GROUNDTRANSMITANTENNA_ID,
        UTDF_GROUNDRECEIVEANTENNA_ID,
        UTDF_FORWARDLINKTDRSSSIC_ID,
        UTDF_RETURNLINKTDRSSSIC_ID,
        UTDF_MARETURNLINKSIC_ID,
        UTDF_TDRSTRACKINGDATAONLYFLAG_ID,
        UTDF_TRACKINGSERVICECONFIGURATION_ID,
        UTDF_AZELVALIDITY_ID,
        UTDF_DOPPLERVALIDITY_ID,
        UTDF_RANGEVALIDITY_ID,
        UTDF_SERVICETYPE_ID,
        UTDF_FREQUENCYBAND_ID,
        UTDF_TRACKERTYPE_ID,
        UTDF_LASTFRAMEINDICATOR_ID,
        UTDF_SAMPLERATEFIELDFLAG_ID,
        UTDF_SAMPLERATE_ID,
        UTDF_TDRSORIENTATIONDATAVALIDITY_ID,
        UTDF_BEAMORIENTATIONDATAVALIDITY_ID,
        UTDF_FORWARDLINK_ID,
        UTDF_RETURNLINK_ID,
        UTDF_YAW_ID,
        UTDF_ROLL_ID,
        UTDF_PITCH_ID,
        UTDF_BEAMAZ_ID,
        UTDF_BEAMEL_ID,
        UTDF_USERBITRATE_ID,
        UTDF_TDRSTRACKINGDATATRANSPONDER_ID,
        UTDF_DOPPLERCOMPENSATIONFLAG_ID,
        UTDF_PNLOCKATRECEIVERFLAG_ID,
        UTDF_CARRIERLOCKATRECEIVERFLAG_ID,
        UTDF_DOPPLEREXTRACTORNUMBER_ID,
        UTDF_RANGEEXTRACTORNUMBER_ID,
        UTDF_FORWARDGCECHAIN_ID,
        UTDF_RETURNGCECHAIN_ID,
        EndUTDFObTypeReps
    };

protected:

    static const bool UTDF_IS_REQUIRED[EndUTDFObTypeReps];
    static const Gmat::ParameterType UTDF_PARAMETER_TYPE[EndUTDFObTypeReps];
    static const std::string UTDF_UNIT_DESCRIPTIONS[EndUTDFObTypeReps];
    static const std::string UTDF_FILEFORMAT_DESCRIPTIONS[EndUTDFObTypeReps];

    string router;
    Integer year;
    Integer sic;
    Integer vid;
    Real secsOfYear;
    Real microSecsOfSec;
    Real angle1;
    Real angle2;
    Real roundTripLightTime;
    Real frequency;
    Real dopplerCount;
    Integer groundTransmitAntennaID;
    Integer groundReceiveAntennaID;
    Integer forwardLinkTDRSSSIC;
    Integer returnLinkTDRSSSIC;
    Integer maReturnLinkID;
    bool tdrsTrackingDataOnlyFlag;
    Integer trackingServiceConfiguration;
    bool azElValidity;
    bool dopplerValidity;
    bool rangeValidity;
    Integer serviceType;
    Integer frequencyBand;
    Integer trackerType;
    bool lastFrameIndicator;
    bool sampleRateFieldFlag;
    Integer sampleRate;
    bool tdrsOrientationDataValidity;
    bool beamOrientationDataValidity;
    Integer forwardLinkID;
    Integer returnLinkID;
    Integer userBitRate;
    Integer tdrsTrackingDataTransponderID;
    Real yaw;
    Real roll;
    Real pitch;
    Real beamAz;
    Real beamEl;
    bool dopplerCompensationFlag;
    bool pnLockAtReceiverFlag;
    bool carrierLockAtReceiverFlag;
    Integer dopplerExtractorNumber;
    Integer rangeExtractorNumber;
    Integer forwardGCEChain;
    Integer returnGCEChain;

};

#endif	/* _UTDFDATAFIELD_HPP */

