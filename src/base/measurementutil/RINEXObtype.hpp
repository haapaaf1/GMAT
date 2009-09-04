/* 
 * File:   RINEXFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 5:01 AM
 */

#ifndef _RINEXFORMATDESCRIPTION_HPP
#define	_RINEXFORMATDESCRIPTION_HPP


namespace DataFormats
{
    struct rinex_gpsob_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string satSystem;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        std::string antennaMarkerName;
        std::string antennaMarkerNumber;
        std::string observerName;
        std::string agencyName;
        std::string receiverNumber;
        std::string receiverType;
        std::string receiverVersion;
        std::string antennaNumber;
        std::string antennaType;
        Real antennaHeight;
        Real antennaEccentricitiesEast;
        Real antennaEccentricitiesNorth;
        Integer defaultWavelengthFactorL1;
        Integer defaultWavelengthFactorL2;
        IntegerArray specificWavelengthFactorL1;
        IntegerArray specificWavelengthFactorL2;
        StringArray satPRNForSpecificWaveFactors;
        Integer numObsTypes;
        StringArray obsTypeCodes;
        Real observationInterval;
        Integer firstObYear, firstObMonth, firstObDay;
        Integer firstObHour, firstObMinute;
        Real firstObSeconds;
        std::string timeSystem;
        Integer lastObYear, lastObMonth, lastObDay;
        Integer lastObHour, lastObMinute;
        Real lastObSeconds;
        bool receiverClockOffsetApplied;
        Integer numLeapSecondsSince6JAN1980;
        Integer numSatsInFile;
        // This needs to be fixed... numObs should be a mxn matrix
        StringArray satPRNlist;
        IntegerArray numObs;
    };

    class rinex_gpsob_obtype : public Obtype
    {
    
    public :
        std::vector<rinex_gpsob_header*>::iterator headerVectorIndex;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Integer epochFlag;
        Integer numSatsThisEpoch;
        StringArray satPRNlist;
        Real receiverClockOffset;
        RealArray observation;
        IntegerArray lli;
        IntegerArray signalStrength;
    };

    struct rinex_gpsnav_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        RealArray ionAlpha;
        RealArray ionBeta;
        Real A0, A1;
        Real T;
        Integer W;
        Integer leapSeconds;
    };

    class rinex_gpsnav_obtype : public Obtype
    {
    
    public :
        Integer satPRN;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias, svClockDrift, svClockDriftRate;
        Real iodeIssueOfData, crs, deltaN, M0;
        Real cuc, e, cus, sqrtA;
        Real toe, cic, capOmega, cis;
        Real i0, crc, omega, capOmegaDot;
        Real iDot,codesOnL2Channel,gpsWeek,l2PDataFlag;
        Real svAccuracy, svHealth, tgd, iodcIssueOfData;
        Real transmissionTimeOfMessage, fitInterval;
    };

    struct rinex_met_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        std::string markerName, markerNumber;
        Integer numObsTypes;
        StringArray obsTypes;
        std::string sensorModel, sensorType;
        Real sensorAccuracy;
        std::string sensorObType;
        Real sensorGeocentricX, sensorGeocentricY, sensorGeocentricZ;
        Real sensorEllipsoidalHeight;
        std::string sensorObType2;
    };

    class rinex_met_obtype : public Obtype
    {
    
    public :
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        RealArray metData;
    };

    struct rinex_glonassnav_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        Integer refYear, refMonth, refDay;
        Real sysTimeCorrection;
        Real numLeapSecondsSince6JAN1980;
    };

    class rinex_glonassnav_obtype : public Obtype
    {
    
    public :
        Integer satNum;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias,svRelativeFrequencyBias;
        Real messageTimeFrame;
        Real x, xDot, xAcc, health;
        Real y, yDot, yAcc, frequencyNum;
        Real z, zDot, zAcc, ageOfOperInfo;
    };

    struct rinex_geonav_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        Integer refSysTimeCorrYear, refSysTimeCorrMonth, refSysTimeCorrDay;
        Real geoTimeToUTC;
        Real A0, A1;
        Real T;
        Integer W,U;
        std::string S;
        Integer numLeapSecondsSince6JAN1980;
    };

    class rinex_geonav_obtype : public Obtype
    {
    
    public :
        Integer satNum;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias,svRelativeFrequencyBias;
        Real transmissionTimeofMessage;
        Real x, xDot, xAcc, health;
        Real y, yDot, yAcc, accuracyCode;
        Real z, zDot, zAcc, IODN;
    };

    enum RINEX_2_11_DATA_REPS
    {
        RINEX_2_11_TYPE_ID,
        RINEX_2_11_SECURITYCLASSIFICATION_ID,
        RINEX_2_11_SATELLITE_ID,
        RINEX_2_11_SENSORID_ID,
        RINEX_2_11_YEAR_ID,
        RINEX_2_11_DAYOFYEAR_ID,
        RINEX_2_11_HOUR_ID,
        RINEX_2_11_MINUTE_ID,
        RINEX_2_11_SECONDS_ID,
        RINEX_2_11_ELEVATION_ID,
        RINEX_2_11_DECLINATION_ID,
        RINEX_2_11_RIGHTASCENSION_ID,
        RINEX_2_11_AZIMUTH_ID,
        RINEX_2_11_RANGE_ID,
        RINEX_2_11_RANGERATE_ID,
        RINEX_2_11_ECFX_ID,
        RINEX_2_11_ECFY_ID,
        RINEX_2_11_ECFZ_ID,
        EndRinex211DataReps
    };

    static const std::string RINEX_2_11_FILEFORMAT_DESCRIPTIONS[EndRinex211DataReps] =
    {
        "B3Type",
        "SecurityClassification",
        "SatelliteID",
        "SensorID",
        "Year",
        "DayOfYear",
        "Hour",
        "Minute",
        "Seconds",
        "Elevation",
        "Declination",
        "RightAscension",
        "Azimuth",
        "Range",
        "RangeRate",
        "Ecf_X",
        "Ecf_Y",
        "Ecf_Z"
    };

    static const std::string RINEX_2_11_UNIT_DESCRIPTIONS[EndRinex211DataReps] =
    {
        "",
        "",
        "",
        "",
        "year",
        "DayOfYear",
        "hrs",
        "min",
        "sec",
        "deg",
        "deg",
        "deg",
        "deg",
        "km",
        "km/sec",
        "km",
        "km",
        "km"
    };


    static const Gmat::ParameterType RINEX_2_11_PARAMETER_TYPE[EndRinex211DataReps] =
    {

    };
}

#endif	/* _RINEXFORMATDESCRIPTION_HPP */

