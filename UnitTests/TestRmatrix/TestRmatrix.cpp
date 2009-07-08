//$Id$
//------------------------------------------------------------------------------
//                               TestRmatrix
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2005/02/14
// Modification:
//   2005/02/14  L. Jun - Moved linear algebra related test cases from TestUtil.cpp
//
/**
 * Test driver for Rmatrix operations.
 */
//------------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "gmatdefs.hpp"
#include "RealTypes.hpp"
#include "Linear.hpp"
#include "Rvector.hpp"
#include "Rvector3.hpp"
#include "Rvector6.hpp"
#include "Rmatrix.hpp"
#include "Rmatrix33.hpp"
#include "TestOutput.hpp"
#include "MessageInterface.hpp"
#include "ConsoleMessageReceiver.hpp"

using namespace std;

//------------------------------------------------------------------------------
//int OutputRmatrix(const Rmatrix &rmat, const Real *array, TestOutput &out)
//------------------------------------------------------------------------------
int OutputRmatrix(const Rmatrix &rmat, const Real *array, TestOutput &out)
{
   out.Put("=============== In OutputRmatrix()");
   std::string matStr = rmat.ToString();
   out.Put(matStr);

   array[0] = 999.99;
   
   Integer row, col;
   rmat.GetSize(row, col);
   out.Put("row = ", row, "col = ", col);
   
   Real *newArray = new Real(row*col);
   memcpy(newArray, array, 8*row*col);
   out.Put("=============== after memcpy");
   
   Rmatrix newRmat = Rmatrix(2, 2, newArray[0], newArray[1], newArray[2], newArray[4]);
   matStr = newRmat.ToString();
   out.Put(matStr);
   delete newArray;
}


//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{

   const std::string *descs;
   std::string *vals;
   Integer size;
   
   //---------------------------------------------------------------------------
   out.Put("========================= Test GetDataVector()");
   Rmatrix mat1(5, 3,
                1.1, 1.2, 1.3,
                2.1, 2.2, 2.3,
                3.1, 3.2, 3.3,
                4.1, 4.2, 4.3,
                5.1, 5.2, 5.3);
   
   const Real *mat1Data = mat1.GetDataVector();
   
   OutputRmatrix(mat1, mat1Data, out);
   //OutputRmatrix(mat1, const_cast<double *>mat1Data, out);
   
   //---------------------------------------------------------------------------

}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   ConsoleMessageReceiver *consoleMsg = ConsoleMessageReceiver::Instance();
   MessageInterface::SetMessageReceiver(consoleMsg);
   MessageInterface::SetLogFile("../../test/TestUtil/GmatLog.txt");
   std::string outFile = "../../test/TestUtil/TestRmatrixOut.txt";
   TestOutput out(outFile);
   
   char *buffer;
   buffer = getenv("OS");
   if (buffer  != NULL)
      printf("Current OS is %s\n", buffer);
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of Rmatrix!!");
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
