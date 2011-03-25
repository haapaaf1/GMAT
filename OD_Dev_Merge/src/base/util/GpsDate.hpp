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
#ifndef GpsDate_hpp
#define GpsDate_hpp

#include "gmatdefs.hpp"
#include "TimeTypes.hpp"
#include "Date.hpp"
#include "DateUtil.hpp"

 class DateUtil;

class GpsDate : public Date
{
public:
    GpsDate();
    GpsDate(Integer week, Real secondsOfWeek);
    GpsDate(Integer week, Real dayOfWeek, Real secondsOfWeek);
    GpsDate(const GpsDate &date);
    GpsDate operator= (const GpsDate &date);
    ~GpsDate();

    //Real     operator- (const UtcDate &date) const;
    //GpsDate  operator+ (const Real seconds) const;
    //GpsDate& operator+=(const Real seconds);
    //GpsDate  operator- (const Real seconds) const;
    //GpsDate& operator-=(const Real seconds);

    A1Date ToA1Date();
    Real  ToA1Mjd() const;

protected:

  Real gpsWeek;
  Real gpsSecondsOfWeek;

private:
};
#endif // GpsDate_hpp
