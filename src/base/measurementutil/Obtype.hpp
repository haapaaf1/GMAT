/* 
 * File:   Obtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 2, 2009, 7:33 AM
 */

#ifndef _OBTYPE_HPP
#define	_OBTYPE_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "StringUtil.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include "sstream"
#include "A1Date.hpp"

class Obtype
{
    
public:
    
    Obtype();
    virtual ~Obtype();
    
    enum OBTYPE_REPS
    {
	RANGE_ID = 0,
	RANGERATE_ID,
	AZIMUTH_ID,
	AZIMUTHRATE_ID,
	ELEVATION_ID,
	ELEVATIONRATE_ID,
	RIGHTASCENSION_ID,
	RIGHTASCENSIONRATE_ID,
	DECLINATION_ID,
	DECLINATIONRATE_ID,
	TWOWAYTIMEOFFLIGHT_ID,
	CARTESIANSTATE_ID,
	X_ID,
	XDOT_ID,
	Y_ID,
	YDOT_ID,
	Z_ID,
	ZDOT_ID,
	ORBITELEMENTSTATE_ID,
	SEMIMAJAXIS_ID,
	ECCENTRICITY_ID,
	INCLINATION_ID,
	ARGPER_ID,
	RAAN_ID,
	TRUEANOM_ID,
	EndObtypeReps
    };

    // Measurement Data Access function

    virtual std::string GetDataParameterText(const Integer id) const;
    virtual std::string GetDataUnits(const Integer id) const;
    virtual Integer     GetDataParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                        GetDataParameterType(const Integer id) const;
    virtual std::string GetDataParameterTypeString(const Integer id) const;

    virtual Integer     GetIntegerDataParameter(const Integer id) const;
    virtual Integer     GetIntegerDataParameter(const std::string &label) const;
    virtual IntegerArray     GetIntegerArrayDataParameter(const Integer id) const;
    virtual IntegerArray     GetIntegerArrayDataParameter(const std::string &label) const;
    virtual Real        GetRealDataParameter(const Integer id) const;
    virtual Real        GetRealDataParameter(const std::string &label) const;
    virtual bool        GetBoolDataParameter(const Integer id) const;
    virtual bool        GetBoolDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    virtual bool CheckDataAvailability(const std::string str) const;
           
    // Test if data format requires this parameter to be
    // defined in order to have a valid data record
    virtual bool IsParameterRequired(const Integer id) const;
    bool IsParameterRequired(const std::string &label) const;
     
    const std::string* GetObtypeKeywords();
    std::string GetObtypeKeyword(Integer myID);
    Integer GetObtypeID(std::string keyword);
    
    A1Date GetEpoch();
    Integer GetSatelliteID();
    std::string GetInternationalDesignator();
    Integer GetSensorID();
    
    std::string Ilrs2Cospar(std::string ilrsSatnum);
    std::string Cospar2Ilrs(std::string cosparSatnum);

    // This is the GMAT epoch time in the Goddard A1 time system
    A1Date epoch;
    
    // This is the NORAD satellite ID
    Integer satelliteID;
    
    // This is the international designator
    std::string internationalDesignator;
    
    // This is the sensor ID
    Integer sensorID;


private:

    
    static const std::string Obtype::OBTYPE_KEYWORDS[EndObtypeReps];
    
    template <class T> bool from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&));
    
};

//------------------------------------------------------------------------------
// template <class T> bool from_string(T& t, const std::string& s,
//                 std::ios_base& (*f)(std::ios_base&))
//------------------------------------------------------------------------------
/**
 * Typesafe conversion from string to integer, float, etc
 */
//------------------------------------------------------------------------------
template <class T> bool Obtype::from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}

#endif	/* _OBTYPE_HPP */

