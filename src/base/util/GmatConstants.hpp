//$Id:
//------------------------------------------------------------------------------
//                           GmatConstants
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: W. Shoan/NASA/GSFC/583
// Created: 2011.02.15
//
/**
 * Provides declarations for commonly used physical/computation/time/conversion
 * constants.
 */
//------------------------------------------------------------------------------
#ifndef GmatConstants_hpp
#define GmatConstants_hpp

// Physical Constants
namespace GmatPhysicalConstants
{
    //Speed Of Light Constant
    const Real SPEED_OF_LIGHT_VACUUM            = 299792458.0;  // m/s
    const Real c                                = 299792458.0;  // m/s

    // gravitational constant (units: km^3/(kg s^2))
    const Real UNIVERSAL_GRAVITATIONAL_CONSTANT = 6.673e-20;

    //Astronomical Constants
    const Real ASTRONOMICAL_UNIT                = 1.49597870e8;  // km

    // Temperature constants
    const Real ABSOLUTE_ZERO_K                  = 0.0;           // K
    const Real ABSOLUTE_ZERO_C                  = -273.15;       // degrees C
}

// Math Constants and Conversion for angles, mass, and length
namespace GmatMathConstants
{
   //Math constants
   static const Real PI_DEG = 180.0;
   static const Real TWO_PI_DEG = 360.0;
   static const Real PI = 3.14159265358979323846264338327950288419716939937511;
   static const Real TWO_PI = 6.28318530717958647692528676655900576839433879875022;
   static const Real PI_OVER_TWO = 1.57079632679489661923132169163975144209858469968756;
   static const Real E = 2.71828182845904523536028747135266249775724709369996;

   //Angle conversion
   static const Real RAD_PER_DEG =
      3.14159265358979323846264338327950288419716939937511 / 180.0;
   static const Real DEG_PER_RAD=
      180.0 / 3.14159265358979323846264338327950288419716939937511;

   static const Real ARCSEC_PER_DEGREE = 3600.0;
   static const Real DEG_PER_ARCSEC = 1.0 / 3600.0;
   static const Real RAD_PER_ARCSEC = DEG_PER_ARCSEC * RAD_PER_DEG;

   //Mass (kilogram)
   static const Real LBM_TO_KG = 0.45359237;
   static const Real SLUG_TO_KG = 14.59390294;

   //Length (metre)
   static const Real INCH_TO_M = 0.0254;
   static const Real FOOT_TO_M = 0.3048;
   static const Real STATUTE_MILE_TO_M = 1609.344;
   static const Real M_TO_KM = 0.001;
   static const Real KM_TO_M = 1000.0;

   enum SIGN {PLUS = 1, MINUS = -1 };

}

// Time Constants
namespace GmatTimeConstants
{
   const Real SECS_PER_DAY             = 86400.0;
   const Real SECS_PER_HOUR            = 3600.0;
   const Real SECS_PER_MINUTE          = 60.0;

   const Real DAYS_PER_YEAR            = 365.25;
   const Real DAYS_PER_JULIAN_CENTURY  = 36525.00;

   const Real TIME_OF_J2000  = 883655990.850000; // 2000/01/01 43167.85
   const Real JD_OF_J2000    = 2451545.0;        // JD of J2000 epoch
   const Real MJD_OF_J2000   = 21545.00000000;   // MJD of J2000 epoch
   const Real A1MJD_OF_J2000 = 21545.00000000;   // 2000/01/01 11:59:27.965622
   const Real JD_MJD_OFFSET  = 2400000.5;        // Vallado page 187 (= JD_NOV_17_1858)
   const Real TT_TAI_OFFSET  = 32.184;           // GMAT Math Spec section 2.3
   const Real A1_TAI_OFFSET  = 0.0343817;        // GMAT Math Spec section 2.1
   const Real JD_JAN_5_1941  = 2430000.0;        // old name JULIAN_DATE_OF_010541
   const Real JD_NOV_17_1858 = 2400000.5;        // old name JD_MJD_OFFSET

   const Integer DAYS_BEFORE_MONTH[12] =
   {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
   const Integer LEAP_YEAR_DAYS_BEFORE_MONTH[12] =
   {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335};
   const Integer DAYS_IN_MONTH[12] =
   {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
   const Integer LEAP_YEAR_DAYS_IN_MONTH[12] =
   {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
   const Integer JULIAN_DATE_OF_010541 = 2430000;

   enum DayName {SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY};

   enum MonthName {JANUARY = 1, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST,
                   SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER};

   static const std::string MONTH_NAME_TEXT[12] =
   {
      "Jan",  "Feb",  "Mar",  "Apr",  "May",  "Jun",
      "Jul",  "Aug",  "Sep",  "Oct",  "Nov",  "Dec"
   };

}
#endif // GmatConstants_hpp
