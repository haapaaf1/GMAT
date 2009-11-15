//$Header$
//------------------------------------------------------------------------------
//                                GpsDate
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew Wilkins
// Created: 2009/09/18
//
/**
 * Provides conversions among various ways representing GPS dates and times.
 */
//------------------------------------------------------------------------------
#include <cmath>
#include "gmatdefs.hpp"
#include "RealUtilities.hpp"   // for Round() 
#include "A1Mjd.hpp"     // for A1Mjd
#include "GpsDate.hpp"   // for GpsDate
//  #include "A1Date.hpp"    // for A1Date

using namespace GmatTimeUtil;
using namespace GmatMathUtil;

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  GpsDate()
//------------------------------------------------------------------------------
/**
 * @note Calls GpsDate default constructor which creates an object with 0
 *       second from GPS reference date 1980-JAN-06 00:00:19 TAI
 */
//------------------------------------------------------------------------------
GpsDate::GpsDate()
  : Date(1980,01,06,0,0,19.0),
    gpsWeek(0),
    gpsSecondsOfWeek(0)
{
}

//------------------------------------------------------------------------------
//  GpsDate (Integer week, Real secondsOfWeek)
//------------------------------------------------------------------------------
/**
 * @note Assumes input date is in Gps time system.
 */
//------------------------------------------------------------------------------
GpsDate::GpsDate (Integer week, Real secondsOfWeek) :
    gpsWeek(week),
    gpsSecondsOfWeek(secondsOfWeek)
{
    // Julian date starting from GPS zero date of Jan 6, 1980 00:00:00 UTC

    Real gpsUTCJD = TimeTypes::JD_JAN_6_1980 + week*7*TimeTypes::SECS_PER_WEEK +
                 secondsOfWeek;

    Real a1Mjd = ToA1Mjd(gpsUTCJD-TimeTypes::JD_JAN_5_1941);

    A1Mjd newA1Mjd = A1Mjd(a1Mjd);
    A1Date newA1Date = newA1Mjd.ToA1Date();
    yearD = newA1Date.GetYear();
    monthD = newA1Date.GetMonth();
    dayD = newA1Date.GetDay();
    secondsOfDayD = newA1Date.GetSecondsOfDay() - TimeTypes::A1_TAI_OFFSET;
}


//------------------------------------------------------------------------------
//  GpsDate (Integer week, Real dayOfWeek, Real secondsOfWeek)
//------------------------------------------------------------------------------
/**
 * @note Assumes input date is in Gps time system.
 */
//------------------------------------------------------------------------------
GpsDate::GpsDate (Integer week, Real dayOfWeek, Real secondsOfWeek)
  : Date(),
    gpsWeek(week)
{
    gpsSecondsOfWeek = secondsOfWeek + dayOfWeek*TimeTypes::SECS_PER_DAY;
}

//------------------------------------------------------------------------------
//  GpsDate (const GpsDate &date)
//------------------------------------------------------------------------------
GpsDate::GpsDate (const GpsDate &date)
  : Date(date)
    gpsWeek(date.gpsWeek),
    gpsSecondsOfWeek(date.gpsSecondsOfWeek)
{
}

//------------------------------------------------------------------------------
//  GpsDate operator= (const GpsDate &date)
//------------------------------------------------------------------------------
GpsDate GpsDate::operator= (const GpsDate &date)
{
    yearD = date.yearD;
    monthD = date.monthD;
    dayD = date.dayD;
    secondsOfDayD = date.secondsOfDayD;
    gpsWeek = date.gpsWeek;
    gpsSecondsOfWeek = date.gpsSecondsOfWeek;
    return *this;
}

//------------------------------------------------------------------------------
//  ~GpsDate ()
//------------------------------------------------------------------------------
GpsDate::~GpsDate ()
{
}

//------------------------------------------------------------------------------
//  A1Date ToA1Date() const
//------------------------------------------------------------------------------
/**
 * Converts from GpsDate to A1Date.
 *
 * @return A1 Date
 */
//------------------------------------------------------------------------------
A1Date GpsDate::ToA1Date() const
{
   Integer year   = GetYear();
   Integer month  = GetMonth();
   Integer day    = GetDay();
   Integer hour   = GetHour();
   Integer minute = GetMinute();
   Real second    = GetSecond();

   // Convert to TAI Modified Julian date
   // Account for difference between TAI and A1
   Real a1Mjd = ModifiedJulianDate(year,month,day,hour,minute,second) +
                TimeTypes::A1_TAI_OFFSET;

   // Check for the tolerance then round-off
   Real testGps = fabs(a1Mjd - (Integer)a1Mjd);
   if (testGps < 1.0e-07)
      a1Mjd = Round(a1Mjd);

   return a1Mjd.ToA1Date();

}

//------------------------------------------------------------------------------
//  Real ToA1Mjd() const
//------------------------------------------------------------------------------
/**
 * Converts from GpsDate to A1 Modified Julian date.
 *
 * @return A1 Modified Julian Date
 */
//------------------------------------------------------------------------------
Real GpsDate::ToA1Mjd() const
{
   Integer year   = GetYear();
   Integer month  = GetMonth();
   Integer day    = GetDay();
   Integer hour   = GetHour();
   Integer minute = GetMinute();
   Real second    = GetSecond();

   // Convert to TAI Modified Julian date
   // Account for difference between TAI and A1
   Real a1Mjd = ModifiedJulianDate(year,month,day,hour,minute,second) +
                TimeTypes::A1_TAI_OFFSET;

   // Check for the tolerance then round-off 
   Real testGps = fabs(a1Mjd - (Integer)a1Mjd);
   if (testGps < 1.0e-07)
      a1Mjd = Round(a1Mjd);
      
   return a1Mjd;

}
