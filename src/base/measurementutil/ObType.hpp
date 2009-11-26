//$Header$
//------------------------------------------------------------------------------
//                             CCSDSObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/02
//
/**
 *
 * This class specifies the base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------

#ifndef _OBTYPE_HPP
#define	_OBTYPE_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"
#include "A1Date.hpp"
#include "MessageInterface.hpp"

using namespace std; // so we don't have to type std::cout and std::endl

class ObType : public GmatBase
{
    
public:
    
    // default constructor
    ObType(const std::string &type, const std::string &name);
    // copy constructor
    ObType(const ObType &ob);
    // operator =
    const ObType& operator=(const ObType &ob);
    // destructor
    virtual ~ObType();
    
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
	EndObTypeReps
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

    virtual bool        SetDataParameter(const Integer id, const Integer &value);
    virtual bool        SetDataParameter(const std::string &label, const Integer &value);
    virtual bool        SetDataParameter(const Integer id, const IntegerArray &value);
    virtual bool        SetDataParameter(const std::string &label, const IntegerArray &value);
    virtual bool        SetDataParameter(const Integer id, const Real &value);
    virtual bool        SetDataParameter(const std::string &label, const Real &value);
    virtual bool        SetDataParameter(const Integer id, const std::string &value);
    virtual bool        SetDataParameter(const std::string &label, const std::string &value);
    virtual bool        SetDataParameter(const Integer id, const StringArray &value);
    virtual bool        SetDataParameter(const std::string &label, const StringArray &value);
    virtual bool        SetDataParameter(const Integer id, const bool &value);
    virtual bool        SetDataParameter(const std::string &label, const bool &value);
    
    // Function to validate data
    virtual bool Validate() const;
           
    // Test if data format requires this parameter to be
    // defined in order to have a valid data record
    virtual bool IsParameterRequired(const Integer id) const;
    bool IsParameterRequired(const std::string &label) const;
     
    const std::string* GetObTypes();
    std::string GetObType(Integer myID);
    Integer GetObTypeID(std::string keyword);
    
    A1Date& GetEpoch();
    void SetEpoch(A1Date &myDate);
    Integer GetSatID();
    void SetSatID(Integer &mySatID);
    std::string GetInternationalDesignator();
    void SetInternationalDesignator(std::string &myID);
    Integer GetSensorID();
    void SetSensorID(Integer &mySensorID);
    
    friend std::string Ilrs2Cospar(std::string ilrsSatnum);
    friend std::string Cospar2Ilrs(std::string cosparSatnum);
    friend std::string Overpunch(const Real &number );

    template <class T> bool from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&));


protected:

    static const std::string OBTYPES[EndObTypeReps];


    // This is the GMAT epoch time in the Goddard A1 time system
    A1Date epoch;
    
    // This is the NORAD satellite ID
    Integer satelliteID;
    
    // This is the international designator
    std::string internationalDesignator;
    
    // This is the sensor ID
    Integer sensorID;

};

//------------------------------------------------------------------------------
// template <class T> bool from_string(T& t, const std::string& s,
//                 std::ios_base& (*f)(std::ios_base&))
//------------------------------------------------------------------------------
/**
 * Typesafe conversion from string to integer, float, etc
 */
//------------------------------------------------------------------------------
template <class T> bool from_string(T& t, const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}

//------------------------------------------------------------------------------
/**
 * A vector based class to contain our ObType items
 */
//------------------------------------------------------------------------------
class ObTypeVector : public std::vector<ObType*>
{
public:
     // Make sure that the vector items are de-allocated so we don't leak

     ~ObTypeVector()
     {
          for (iterator pItem=begin(); pItem != end(); ++pItem)
               delete *pItem;
     }
};    

#endif	/* _OBTYPE_HPP */

