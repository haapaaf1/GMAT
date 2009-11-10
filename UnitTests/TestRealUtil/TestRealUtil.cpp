//$Id$
//------------------------------------------------------------------------------
//                                  TestRealUtil
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2005/02/14
//
/**
 * Test driver for RealUtilities.
 */
//------------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "gmatdefs.hpp"
#include "RealTypes.hpp"
#include "TimeTypes.hpp"
#include "PhysicalConstants.hpp"
#include "RealUtilities.hpp"
#include "TestOutput.hpp"
#include "MessageInterface.hpp"
#include "ConsoleMessageReceiver.hpp"

using namespace std;
using namespace GmatMathUtil;

//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{
   Real x = 0.0;
   Real tol = 1.0e-11;
   
   out.Put("");

   out.Put("============================== test GmatTimeUtil:: constants");
   Real rval = GmatTimeUtil::SECS_PER_DAY;
   out.Put("GmatTimeUtil::SECS_PER_DAY = ", rval);
   out.Put("");
   
   out.Put("============================== test GmatRealConst constants");
   rval = GmatRealConst::REAL_TOL;
   out.Put("GmatRealConst::REAL_TOL = ", rval);
   out.Put("");
   
   out.Put("============================== test GmatPhysicalConst constants");
   rval = GmatPhysicalConst::c;
   out.Put("GmatPhysicalConst::c = ", rval);
   out.Put("");
   
   out.Put("============================== test RealUtilities");
   out.Put("");
   
   out.Put("========================= Tan(90)");
   out.Put(Tan(90.0 * RAD_PER_DEG));
   out.Put("");
   
   out.Put("========================= Ln(e)");
   out.Put(Ln(E));
   out.Put("");
   
   out.Put("========================= Log(e)");
   out.Put(Log(E));
   out.Put("");
   
   out.Put("========================= Ln(10)");
   out.Put(Ln(10.0));
   out.Put("");
   
   out.Put("========================= Log(10)");
   out.Put(Log(10.0));
   out.Put("");
   
   out.Put("========================= Log(10, E)");
   out.Put(Log(10.0, E));
   out.Put("");
   
   out.Put("========================= Log10(10)");
   out.Put(Log10(10.0));
   out.Put("");
   
   out.Put("========================= Log(10, 10)");
   out.Put(Log(10.0, 10.0));
   out.Put("");
   
   out.Put("========================= Log(100, 10)");
   out.Put(Log(100.0, 10.0));
   out.Put("");
   
   out.Put("========================= Exp(2)");
   out.Put(Exp(2.0));
   out.Put("");
   
   out.Put("========================= Exp10(2)");
   out.Put(Exp10(2.0));
   out.Put("");
   
   out.Put("========================= Pow(2, 3)");
   out.Put(Pow(2.0, 3.0));
   out.Put("");
   
   x = 1.0000000000000002;
   out.Put("========================= Acos(1.0000000000000002)");
   out.Put(ACos(x, tol));
   out.Put("");
   
   x = -1.0000000000000002;
   out.Put("========================= Acos(-1.0000000000000002)");
   out.Put(ACos(x, tol));
   out.Put("");

   try
   {
      x = -1.0000000000000002;
      out.Put("========================= Acos(-1.0000000000000002)");
      out.Put(ACos(x));
      out.Put("");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }

   
   try
   {
      x = 1.00000000002;
      out.Put("========================= Acos(1.00000000002)");
      out.Put(ACos(x, tol));
      out.Put("");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }

   try
   {
      x = -1.00000000002;
      out.Put("========================= Acos(-1.00000000002)");
      out.Put(ACos(x, tol));
      out.Put("");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   
}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   ConsoleMessageReceiver *consoleMsg = ConsoleMessageReceiver::Instance();
   MessageInterface::SetMessageReceiver(consoleMsg);
   std::string outPath = "../../TestRealUtil/";
   MessageInterface::SetLogFile(outPath + "GmatLog.txt");
   std::string outFile = outPath + "TestRealUtil.txt";
   TestOutput out(outFile);
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of RealUtilities!!");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   catch (...)
   {
      out.Put("Unknown error occurred\n");
   }
   
   cout << endl;
   cout << "Hit enter to end" << endl;
   cin.get();
}
