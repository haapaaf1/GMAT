//$Header$
//------------------------------------------------------------------------------
//                                    DateUtil
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 1995/10/18 for GSS project (originally GSSDate)
// Modified: 2003/10/02 Linda Jun - See DateUtil.hpp
//           2004/05/06 J. Gurganus - See DateUtil.hpp
//
/**
 * This class provides conversions among various ways of representing calendar
 * dates and times.
 */
//------------------------------------------------------------------------------
#include "TimeTypes.hpp"
#include "Date.hpp"
#include "DateUtil.hpp"
#include "RealUtilities.hpp"  // for namespace GmatMathUtil
#include "StringUtil.hpp"
#include "ElapsedTime.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_DATE_VALIDATE 1

using namespace GmatMathUtil; // for Floor(), Mod()

//---------------------------------
// static
//---------------------------------

//------------------------------------------------------------------------------
// ElapsedDays DateUtil::JulianDay(YearNumber year, MonthOfYear month,
//                                 DayOfMonth day) 
//------------------------------------------------------------------------------
Integer DateUtil::JulianDay(YearNumber year, MonthOfYear month,
                            DayOfMonth day) 
{
   //Calculate julian date using Fliegel and Van Flandern algorithm
   const Integer L = (month - 14) / 12;
   return (day - 32075 + 1461 * (year + 4800 + L) / 4 + 367 *
           (month - 2 - L * 12) / 12 - 3 *
           ((year + 4900 + L) / 100) /4 );
}



//---------------------------------
// friend
//---------------------------------

//------------------------------------------------------------------------------
// Real JulianDate(YearNumber year, MonthOfYear month, DayOfMonth day, 
//                 Integer hour, Integer minute, Real second)
//  
//------------------------------------------------------------------------------
/**
 * Friend function.   Converted from calendar date to Julian Date.
 *
 * @param <year>    - year of calendar
 * @param <month>   - month in calendar format
 * @param <day>     - day of month in calendar format 
 * @param <hour>    - hour of day 
 * @param <minute>  - minute of hour
 * @param <second>  - seconds including millisecond 
 *
 * @return           Julian date   
 * 
 * @note:   The algorithm is used in the Vallado book.
 */
//------------------------------------------------------------------------------
Real JulianDate(YearNumber year, MonthOfYear month, DayOfMonth day, 
                Integer hour, Integer minute, Real second)
{
   Integer computeYearMon = (7*(year + (Integer)((month + 9)/12)))/4;
   Integer computeMonth = (275 * month)/9;
   Real fractionalDay = ((second/60.0 + minute)/60 + hour)/24.0;

   // Compute the Julian Date
   return ( 367*year - computeYearMon + computeMonth + day +
            1721013.5 + fractionalDay);
}

//------------------------------------------------------------------------------
// Real ModifiedJulianDate(YearNumber year, MonthOfYear month, DayOfMonth day,
//                         Integer hour, Integer minute, Real second)
//
//------------------------------------------------------------------------------
/**
 * Friend function.   Converted from calendar date to Modified Julian Date.
 *
 * @param <year>    - year of calendar
 * @param <month>   - month in calendar format
 * @param <day>     - day of month in calendar format
 * @param <hour>    - hour of day
 * @param <minute>  - minute of hour
 * @param <second>  - seconds including millisecond
 *
 * @return           Modified Julian date
 *
 */
//------------------------------------------------------------------------------
Real ModifiedJulianDate(YearNumber year, MonthOfYear month, DayOfMonth day,
                        Integer hour, Integer minute, Real second,
                        Real refEpochJD)
{
    // 5/23/06 - commented out
    // Need to subtract out Julian date offset before adding the fraction
    // of a day term to gain significant digits after the decimal place.
    // The computation is copied from the Julian Date method.

   Integer computeYearMon = ( 7*(year + (Integer)((month + 9)/12)) )/4;
   Integer computeMonth = (275 * month)/9;
   Real fractionalDay = ((second/60.0 + minute)/60.0 + hour)/24.0;

   Real ModJulianDay = 367*year - computeYearMon + computeMonth + day +
               1721013.5 -  refEpochJD;
   Real modJulianDate = ModJulianDay  + fractionalDay;

   return modJulianDate;
}

//------------------------------------------------------------------------------
// Real ModifiedJulianDate(YearNumber year, MonthOfYear month, DayOfMonth day, 
//                         Integer hour, Integer minute, Real second)
//  
//------------------------------------------------------------------------------
/**
 * Friend function.   Converted from calendar date to Modified Julian Date.
 *
 * @param <year>    - year of calendar
 * @param <month>   - month in calendar format
 * @param <day>     - day of month in calendar format 
 * @param <hour>    - hour of day 
 * @param <minute>  - minute of hour
 * @param <second>  - seconds including millisecond 
 *
 * @return           Modified Julian date   
 * 
 */
//------------------------------------------------------------------------------
Real ModifiedJulianDate(YearNumber year, MonthOfYear month, DayOfMonth day, 
                        Integer hour, Integer minute, Real second)
{
    // 5/23/06 - commented out
    // Need to subtract out Julian date offset before adding the fraction
    // of a day term to gain significant digits after the decimal place.
    // The computation is copied from the Julian Date method.
//   Real jd =  JulianDate(year, month, day, hour, minute, second);
//   return (jd - GmatTimeUtil::JULIAN_DATE_OF_010541);

   //Integer computeYearMon = ( 7*(year + (Integer)(month + 9)/12 ) )/4;
   Integer computeYearMon = ( 7*(year + (Integer)((month + 9)/12)) )/4;
   Integer computeMonth = (275 * month)/9;
   Real fractionalDay = ((second/60.0 + minute)/60.0 + hour)/24.0;

   Real ModJulianDay = 367*year - computeYearMon + computeMonth + day +
               1721013.5 -  GmatTimeUtil::JULIAN_DATE_OF_010541;
   Real modJulianDate = ModJulianDay  + fractionalDay;

   return modJulianDate;
}

                
//------------------------------------------------------------------------------
//  void UnpackDate (Real packedDate, Integer& year, Integer& month, Integer& day)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Input date in YYYYMMDD
 */
//------------------------------------------------------------------------------
void UnpackDate (Real packedDate, Integer& year, Integer& month, Integer& day)
{
   Real remainder = packedDate;
   year = (Integer)(GmatMathUtil::Floor(remainder / 10000.0));
   remainder = Mod(remainder, 10000.0);
   month = (Integer)(GmatMathUtil::Floor(remainder / 100.0));
   remainder = Mod(remainder, 100.0);
   day = (Integer)(GmatMathUtil::Floor(remainder + 0.5));
   
   if ( month < 0 || month > 12 || day < 0 || day > 31)
      throw Date::TimeRangeError();    
}

//------------------------------------------------------------------------------
//  void UnpackDateWithDOY (Real packedDate, Integer& year, Integer& day)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Input date is in YYYYDDD; Day is rounded because of potential floating
 *       point representation problem.  Do not pre-correct the input by adding 0.5.
 */
//------------------------------------------------------------------------------
void UnpackDateWithDOY (Real packedDate, Integer& year, Integer& day)
{
   Real remainder;
   year = (Integer)(GmatMathUtil::Floor(packedDate / 1000.0));
   remainder = Mod (packedDate, 1000.0);
   day = (Integer)(GmatMathUtil::Floor(remainder + 0.5));
   
   if ( day < 0 || day > 366)
      throw Date::TimeRangeError();    
}

//------------------------------------------------------------------------------
//  void UnpackTime (Real packedTime, Integer& hour, Integer& minute, Real& second)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Input time is in hhmmssnnn; added 20 seconds to the input time, 
 *       to be subtracted later, to avoid gross errors around minute boundaries.
 */
//------------------------------------------------------------------------------
void UnpackTime (Real packedTime, Integer& hour, Integer& minute,
                 Real& second)
{
   Real remainder = packedTime;
   remainder = remainder + 20000.0;      // 20000 milliseconds (hhmmssnnn.)
   hour = (Integer)(GmatMathUtil::Floor(remainder / 10000000.0));
   remainder = Mod (remainder, 10000000); 
   minute = (Integer)(GmatMathUtil::Floor(remainder / 100000.0));
   remainder = Mod (remainder, 100000.0); 
   second = (remainder / 1000.0) - 20.0;
   
   if (hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0.0
       || second > 61.0)
   {
      throw Date::TimeRangeError();    
   }     
}

//-----------------------------------------------------------------------------
//  void ToMonthDayFromYearDOY (Integer year, Integer dayOfYear, Integer& month,
//                              Integer& day)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Year is needed to determine if it is a leap year.
 */
//------------------------------------------------------------------------------
void ToMonthDayFromYearDOY (Integer year, Integer dayOfYear, Integer& month,
                            Integer& day)
{
   Integer i;
   const Integer *ptrDaysList;

   if ( dayOfYear < 0 || dayOfYear > 366 )
      throw Date::TimeRangeError();    
   
   if (IsLeapYear(year))
      ptrDaysList = GmatTimeUtil::LEAP_YEAR_DAYS_BEFORE_MONTH;
   else
      ptrDaysList = GmatTimeUtil::DAYS_BEFORE_MONTH;
      
   for (i=1; i<12; i++)
      if (dayOfYear <= ptrDaysList[i])
         break;
   
   month = i;
   day = dayOfYear - ptrDaysList[i-1];
}

//------------------------------------------------------------------------------
//  Integer ToDOYFromYearMonthDay (Integer year, Integer month, Integer day)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Year is needed to determine if it is a leap year
 */
//------------------------------------------------------------------------------
Integer ToDOYFromYearMonthDay (Integer year, Integer month, Integer day)
{
   Integer d;
   
   if ( month < 0 || month > 12 || day < 0 || day > 31)
      throw Date::TimeRangeError();    

   if (IsLeapYear(year) == true)
      d = GmatTimeUtil::LEAP_YEAR_DAYS_BEFORE_MONTH[month-1] + day;
   else
      d = GmatTimeUtil::DAYS_BEFORE_MONTH[month-1] + day;

   return d;
}

//------------------------------------------------------------------------------
//  Real ToSecondsOfDayFromHMS (Integer hour, Integer minute, Real second)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 * @note Seconds of day constrained to 0.0..86401.0; assumes only 1 leap 
 *       second per day maximum.
 */
//------------------------------------------------------------------------------
Real ToSecondsOfDayFromHMS (Integer hour, Integer minute, Real second)
{
   ElapsedTime secondsOfDay;
   
   if (hour < 0 || hour > 23 || minute < 0 || minute > 59 || 
       second < 0.0 || second > 61.0) 
   {
      throw Date::TimeRangeError();
   }        

   secondsOfDay = (Real)hour * 3600.0 + (Real)minute * 60.0 + second;
   return secondsOfDay.Get();
}

//------------------------------------------------------------------------------
//  void ToHMSFromSecondsOfDay (Real secsOfDay, Integer& hour, Integer& minute, 
//                              Real& second)
//------------------------------------------------------------------------------
/**
 * Friend function.
 *
 *  Notes: Seconds are Real to permit fractions; seconds of day constrained to 
 *         0.0 .. 86401.0; the last second of a leap second day will be 
 *         23:59:60; assumes only 1 leap second per day maximum.
 */
//------------------------------------------------------------------------------
void ToHMSFromSecondsOfDay (Real secsOfDay, Integer& hour, Integer& minute, 
                                Real& second)
{
   const Real maxSecondsPerDay = 86401.0;
   Real    remainder;
   Integer h;
   Integer m;
   
   if (secsOfDay  < 0.0 || secsOfDay > maxSecondsPerDay)
      throw Date::TimeRangeError();

   h = (Integer)(GmatMathUtil::Floor(secsOfDay/3600.0));  // constrain hours to 0..23
   if (h >= 24)
      h = 23;

   hour = h;
   remainder = secsOfDay - (Real)h * 3600.0;
   m = (Integer)(GmatMathUtil::Floor(remainder / 60.0));  // constrain minutes to 0..59
   if (m >= 60)
      m = 59;
   
   minute = m;
   second = remainder - (Real)m  * 60.0;
}

//------------------------------------------------------------------------------
//  bool IsValidTime (Integer year, Integer month, Integer day, 
//                    Integer hour, Integer minute, Real second)
//------------------------------------------------------------------------------
/**
 * Friend function.
 */
//------------------------------------------------------------------------------
bool IsValidTime (Integer year, Integer month, Integer day, 
                  Integer hour, Integer minute, Real second)
{
   bool valid = false;
   const Integer *ptrDaysList;
   
   if (IsLeapYear(year))
      ptrDaysList = GmatTimeUtil::LEAP_YEAR_DAYS_IN_MONTH;
   else
      ptrDaysList = GmatTimeUtil::DAYS_IN_MONTH;
   
   // check month and day
   if ((month >= 1 && month <= 12) && (day >=1 && day <= ptrDaysList[month-1]))
      valid = true;
   
   // check hour, minute, and seconds
   if (valid)
   {
      valid = false;
      if ((hour >= 0 && hour <= 23) && (minute >= 0 && minute <= 59) &&
          (second >= 0.0 && second < 61.0))
      {
         valid = true;
      }
   }
   
   return valid;
}


//------------------------------------------------------------------------------
// bool IsValidGregorian(const std::string &str)
//------------------------------------------------------------------------------
/**
 * Determines if input date string is valid Gregorian or not.
 *   Valid format is dd mmm yyyy hh:mm:ss.mmm.
 *   For example, 01 Jan 2000 12:00:00.000
 *
 * @param  greg  input gregorian string
 *
 * @return true if time is in valid Gregorian format; otherwise, false
 */
//---------------------------------------------------------------------------
bool DateUtil::IsValidGregorian(const std::string &str)
{
   StringArray parts = GmatStringUtil::SeparateBy(str, " ");
   if (parts.size() != 4)
      return false;

   #if DEBUG_DATE_VALIDATE
   MessageInterface::ShowMessage
      ("DateUtil::IsValidGregorian() parts=%s, %s, %s, %s\n", parts[0].c_str(),
       parts[1].c_str(), parts[2].c_str(), parts[3].c_str());
   #endif
   
   StringArray timeParts = GmatStringUtil::SeparateBy(parts[3], ":");
   if (timeParts.size() != 3)
      return false;
   
   #if DEBUG_DATE_VALIDATE
   MessageInterface::ShowMessage
      ("DateUtil::IsValidGregorian() timeParts=%s, %s, %s\n", timeParts[0].c_str(),
       timeParts[1].c_str(), timeParts[2].c_str());
   #endif
   
   Integer year, month, day, hour, min;
   Real sec;
   
   // check for valid month name
   if (!GmatTimeUtil::IsValidMonthName(parts[1]))
      return false;
   
   month = GmatTimeUtil::GetMonth(parts[1]);
   
   if (!GmatStringUtil::ToInteger(parts[0], day))
      return false;
   
   if (!GmatStringUtil::ToInteger(parts[2], year))
      return false;
   
   if (!GmatStringUtil::ToInteger(timeParts[0], hour))
      return false;
   
   if (!GmatStringUtil::ToInteger(timeParts[1], min))
      return false;
   
   if (!GmatStringUtil::ToReal(timeParts[2], sec))
      return false;
   
   #if DEBUG_DATE_VALIDATE
   MessageInterface::ShowMessage
      ("DateUtil::IsValidGregorian() year=%d, month=%d, day=%d, hour=%d, "
       "min=%d, sec=%f\n", year, month, day, hour, min, sec);
   #endif
   
   // check for date
   return IsValidTime(year, month, day, hour, min, sec);
}


//------------------------------------------------------------------------------
//  bool IsLeapYear (Integer year)
//------------------------------------------------------------------------------
/**
 * Friend function.
 */
//------------------------------------------------------------------------------
bool IsLeapYear(Integer year)
{
   bool result = false;
    
   if ((year % 100 == 0) && (year % 400 == 0))
      result = true;
   else if (year % 4 == 0)
      result = true;

   return result;
}
