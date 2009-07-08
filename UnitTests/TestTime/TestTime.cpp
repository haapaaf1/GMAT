//$Header: /cygdrive/p/dev/cvs/test/TestUtil/TestTime.cpp,v 1.2 2005/02/22 18:44:56 agreene Exp $
//------------------------------------------------------------------------------
//                                  TestTime
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2005/02/14
// Modification:
//   2005/02/14  L. Jun - Moved time related test cases from TestUtil.cpp
//   2005/02/14  A. Greene - Added time conversion testing
//
/**
 * Test driver for time related classes.
 */
//------------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "gmatdefs.hpp"
#include "RealTypes.hpp"
#include "TimeTypes.hpp"
#include "ArrayTemplate.hpp"
#include "ElapsedTime.hpp"
#include "A1Mjd.hpp"
#include "LeapSecsFileReader.hpp"
#include "TimeSystemConverter.hpp"
#include "EopFile.hpp"
#include "TestOutput.hpp"

using namespace std;
using namespace TimeConverterUtil;


//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{

   const std::string *descs;
   std::string *vals;
   Integer size;
   
   out.Put("\n============================== test A1Mjd()");
   A1Mjd a1mjd = A1Mjd(21545);
   size = a1mjd.GetNumData();
   descs = a1mjd.GetDataDescriptions();
   vals = a1mjd.ToValueStrings();
   for (int i=0; i<size; i++)
      out.Put(descs[i], " = ", vals[i]);

   out.Put("\n============================== test A1Date");
   A1Date a1date = A1Date(2003, 10, 2, 10, 30, 20);
   size = a1date.GetNumData();
   descs = a1date.GetDataDescriptions();
   vals = a1date.ToValueStrings();
   for (int i=0; i<size; i++)
      out.Put(descs[i], " = ", vals[i]);

   out.Put("\n============================== test UtcDate()");
   UtcDate utcdate = UtcDate(2003, 10, 2, 10, 30, 20);
   size = utcdate.GetNumData();
   descs = utcdate.GetDataDescriptions();
   vals = utcdate.ToValueStrings();
   for (int i=0; i<size; i++)
      out.Put(descs[i], " = ", vals[i]);
   
   out.Put("\n============================== test ElapsedTime()");
   ElapsedTime elapsedTime = ElapsedTime(100.0);
   size = elapsedTime.GetNumData();
   descs = elapsedTime.GetDataDescriptions();
   vals = elapsedTime.ToValueStrings();
   for (int i=0; i<size; i++)
      out.Put(descs[i], " = ", vals[i]);

   out.Put("\n============================== test A1Mjd.ToA1Date()");
   try
   {
      A1Date a1 = a1mjd.ToA1Date();
      size = a1.GetNumData();
      descs = a1.GetDataDescriptions();
      vals = a1.ToValueStrings();
      for (int i=0; i<size; i++)
         out.Put(descs[i], " = ", vals[i]);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetMessage());
   }

   out.Put("\n============================== test A1Mjd.ToUtcDate()");
   try
   {
      UtcDate u1 = a1mjd.ToUtcDate();
      size = u1.GetNumData();
      descs = u1.GetDataDescriptions();
      vals = u1.ToValueStrings();
      for (int i=0; i<size; i++)
         out.Put(descs[i], " = ", vals[i]);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetMessage());
   }
   
   //---------------------------------------------------------------------------
   out.Put("\n============================== Test Time Coeff File Reader");
   try
   {
      LeapSecsFileReader *tcfr = new LeapSecsFileReader();
      tcfr->Initialize();

      UtcMjd utcmMjd = a1mjd.ToUtcMjd() + 30000;
      // greater then last table position
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
//      cout << "The number of leap seconds from the analytic = "
//           << a1mjd.UtcMjdToA1Mjd(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 29000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 28000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 27000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 26000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 25000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 24000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 23000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 22000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 21000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 20000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 19000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 18000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 17000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      utcmMjd = a1mjd.ToUtcMjd() + 16000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
      // less than first position in table
      utcmMjd = a1mjd.ToUtcMjd() + 15000;
      cout << utcmMjd << " => number of leap seconds = "
           << tcfr->NumberOfLeapSecondsFrom(utcmMjd) << endl;
   }
   catch (BaseException &e)
   {
      cout << e.GetMessage() << endl;
   }

   out.Put("");
   //---------------------------------------------------------------------------
   out.Put("\n============================== test TimeSystemConverter()");
   try
   {
//      SetJdMjdOffset(2400000.5);
      LeapSecsFileReader *tcfr = new LeapSecsFileReader();
      tcfr->Initialize();
      SetLeapSecsFileReader(tcfr);

      EopFile *ef = new EopFile("finals.data", GmatEop::FINALS);
      ef->Initialize();
      SetEopFile(ef);

      Real taiMjd = 53180.5;
//      Real taiMjd = 38334.4;
      cout.precision(15);
      Real a1Mjd = ConvertFromTaiMjd("A1Mjd", taiMjd);
      cout << taiMjd << " => to A1Mjd = " << a1Mjd
           << " => to taimjd = " << ConvertToTaiMjd("A1Mjd", a1Mjd)
           << endl;

      Real ttMjd = ConvertFromTaiMjd("TtMjd", taiMjd);
      cout << taiMjd << " => to TtMjd = " << ttMjd
           << " => to taimjd = " << ConvertToTaiMjd("TtMjd", ttMjd)
           << endl;

      Real utcMjd = ConvertFromTaiMjd("UtcMjd", taiMjd);
      cout << taiMjd << " => to UtcMjd = " << utcMjd
           << " => to taimjd = " << ConvertToTaiMjd("UtcMjd", utcMjd)
           << endl;

      Real ut1Mjd = ConvertFromTaiMjd("Ut1Mjd", taiMjd);
      cout << taiMjd << " => to Ut1Mjd = " << ut1Mjd
           << " => to taimjd = " << ConvertToTaiMjd("Ut1Mjd", ut1Mjd)
           << endl;

      Real tcbMjd = ConvertFromTaiMjd("TcbMjd", taiMjd);
      cout << taiMjd << " => to TcbMjd = " << tcbMjd
//           << " => to taimjd = " << ConvertToTaiMjd("TcbMjd", tcbMjd)
           << endl;

      Real tdbMjd = ConvertFromTaiMjd("TdbMjd", taiMjd);
      cout << taiMjd << " => to TdbMjd = " << tdbMjd
//           << " => to taimjd = " << ConvertToTaiMjd("TdbMjd", tdbMjd)
           << endl;

//      EopFile *eopFile = new EopFile("finals.data", GmatEop::FINALS);
//      SetEopFile(eopFile);

//      Real ut1Mjd = ConvertFromTaiMjd("Ut1Mjd", taiMjd);
//      cout << taiMjd << " => to Ut1Mjd = " << ut1Mjd
//           << " => to taimjd = " << ConvertToTaiMjd("Ut1Mjd", ut1Mjd)
//           << endl;

      cout << "\n=============== Test Convert Method" << endl;
      Real ttMjd2 = Convert(a1Mjd, "A1Mjd", "TtMjd");
      cout << "A1Mjd: " << a1Mjd << " ==> to TtMjd: " << ttMjd2 << endl;
      cout << "A1Mjd: " << a1Mjd << " ==> to UtcMjd: "
           << Convert(a1Mjd, "A1Mjd", "UtcMjd") << endl;

      cout << "\n=============== Test Convert Method with diff offset" << endl;
      Real a1Mjd2 = a1Mjd + 500000;
      Real utcMjd2 = Convert(a1Mjd2, "A1Mjd", "UtcMjd", GmatTimeUtil::JD_JAN_5_1941);
      cout << "A1Mjd: " << a1Mjd2 << " ==> to UtcMjd: " << utcMjd2 << endl;

      cout << "UtcMjd: " << utcMjd2 << " ==> to TtMjd: "
           << Convert(utcMjd2, "UtcMjd", "TtMjd", GmatTimeUtil::JD_JAN_5_1941)
           << endl;

   }
   catch (BaseException &e)
   {
      out.Put(e.GetMessage());
   }

   out.Put("");
}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   TestOutput out("..\\..\\test\\TestUtil\\TestTime.out");
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of time classes!!");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetMessage());
   }
   catch (...)
   {
      out.Put("Unknown error occurred\n");
   }
   
   cout << endl;
   cout << "Hit enter to end" << endl;
   cin.get();
}
