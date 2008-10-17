//$Id$
//------------------------------------------------------------------------------
//                               MatlabInterface
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2002/11/04
//
/**
 * Implements MatlabInterface functions. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#include <stdlib.h>         // for NULL
#include <string.h>         // for memcpy()
#include <sstream>          // for std::stringstream
#if defined __USE_MATLAB__
#include "engine.h"         // for Matlab Engine
#include "matrix.h"         // for Matlab mxArray
#endif

#ifdef __WXMAC__
#include <stdlib.h>         // for system() to launch X11 application
#include <unistd.h>         // for gethostname()
#endif

#include "MatlabInterface.hpp"
#include "MessageInterface.hpp"
#include "InterfaceException.hpp"

//#define DEBUG_MATLAB_OPEN_CLOSE
//#define DEBUG_MATLAB_PUT_REAL
//#define DEBUG_MATLAB_GET_REAL
//#define DEBUG_MATLAB_GET_STRING
//#define DEBUG_MATLAB_EVAL

//--------------------------------------
//  initialize static variables
//--------------------------------------
#ifdef __USE_MATLAB__
MatlabInterface* MatlabInterface::instance = NULL;
#endif

//--------------------------------------
//  public functions
//--------------------------------------

//------------------------------------------------------------------------------
// MatlabInterface* Instance()
//------------------------------------------------------------------------------
/*
 * Returns this instance
 */
//------------------------------------------------------------------------------
MatlabInterface* MatlabInterface::Instance()
{
#if defined __USE_MATLAB__
   if (instance == NULL)
      instance = new MatlabInterface;
   
   return instance;
#endif

   return NULL;
}


//------------------------------------------------------------------------------
//  int Open()
//------------------------------------------------------------------------------
//  Purpose:
//     Opens Matlab engine.
//
//  Returns:
//     1 = no error, 0 = error
//------------------------------------------------------------------------------
int MatlabInterface::Open()
{
#if defined __USE_MATLAB__
   
   #ifdef DEBUG_MATLAB_OPEN_CLOSE
   MessageInterface::ShowMessage
      ("MatlabInterface::Open() enginePtr=%p\n", enginePtr);
   #endif
   
   if (enginePtr == NULL)
      MessageInterface::ShowMessage("Please wait while MATLAB opens...\n");
   
#ifdef __WXMAC__

   /// Check if MATLAB is still running then doesn't need to re-launch
   if (enginePtr != NULL)
   {
      accessCount++;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
         MessageInterface::ShowMessage(
         "Attempting to reopen MATLAB connection ... accessCount = %d\n", accessCount);
      #endif
      return 1;
   }
   
   // open the X11 application before launching the matlab
   system("open -a X11");
   // need to get IP address or hostname here
   char hName[128];
   int OK = gethostname(hName, 128);
   if (OK != 0) MessageInterface::ShowMessage("Error getting host name\n");
   std::string hNameStr(hName);
   // -desktop now causes MATLAB desktop to come up (as of 2008.01.11) but it 
   // hangs both MATLAB and GMAT
   //std::string runString = "matlab -display " + hNameStr + ":0.0 -desktop";
   //std::string runString = "matlab -display " + hNameStr + ":0.0";
   std::string runString = "matlab ";
   if ((enginePtr = engOpen(runString.c_str())))
   {
      MessageInterface::ShowMessage(
               "Successfully opened MATLAB engine using startcmd \"%s\"\n",
               runString.c_str());
      accessCount++;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
         MessageInterface::ShowMessage(
         "Attempting to open MATLAB connection ... accessCount = %d\n", accessCount);
      #endif
      //engSetVisible(enginePtr,1);  // rats!
      return 1;
   }
   else
   {
      MessageInterface::ShowMessage(
               "Failed to open MATLAB engine using startcmd \"%s\"\n",
               runString.c_str());
      return 0;
   }
   
#else

   if (enginePtr != NULL)
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage("Connecting to current MATLAB session\n");
      #endif
      
      return 1;
   }
   
   // open new MATLAB engine
   if ((enginePtr = engOpen(NULL)))
   {
      accessCount++;
      
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("Attempting to open MATLAB connection ... accessCount = %d, "
          "enginePtr=%p\n", accessCount, enginePtr);
      #endif
      
      // set precision to long
      EvalString("format long");
      return 1;
   }
   else
   {
      MessageInterface::ShowMessage("Failed to open MATLAB engine ...\n");
      return 0;
   }
#endif  // End-ifdef __WXMAC__

   return 0;
#endif // End-ifdef __USE_MATLAB__
   
   return 0;

} // end Open()


//------------------------------------------------------------------------------
//  int Close()
//------------------------------------------------------------------------------
//  Purpose:
//     Closes Matlab engine.
//
//  Returns:
//     1 = no error, 0 = error
//------------------------------------------------------------------------------
int MatlabInterface::Close()
{
#if defined __USE_MATLAB__
   // Check if MATLAB is still running then close it.
   if (enginePtr != NULL)
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("MatlabInterface::Close() enginePtr=%p\n", enginePtr);
      #endif
      
      accessCount--;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
         MessageInterface::ShowMessage(
         "Attempting to close MATLAB connection ... accessCount = %d\n", accessCount);
      #endif
         
      //if (accessCount <= 0)
      //{
         #ifdef __WXMAC__
            // need to close X11 here ------------ **** TBD ****
            MessageInterface::ShowMessage(
            "Closing connection to MATLAB ... please close X11 ...\n");
         #endif
            
         if (engClose(enginePtr) != 0)
               MessageInterface::ShowMessage("\nError closing MATLAB\n");
         
         enginePtr = NULL;      // set to NULL, so it can be reopened
         MessageInterface::ShowMessage("MATLAB has been closed ...\n");
      //}
      //else
      //{
      //   MessageInterface::ShowMessage(
      //   "\nGMAT still accessing MATLAB ... not closing connection at this time\n");
      //}
   }
   else
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("\nUnable to close MATLAB because it is not currently running\n");
      #endif
      
      return 0;
   }
   
#endif

   return 1;

} // end Close()


//------------------------------------------------------------------------------
// int PutRealArray(const std::string &matlabVarName, int numRow, int numCol,
//                  const double *inArray)
//------------------------------------------------------------------------------
/**
 * Put arrays to Matlab workspace.
 *
 * @param <matlabVarName> variable name in the MATLAB workspace
 * @param <numRows> number of rows in input array
 * @param <numCols> number of columns in input array
 * @return 1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::PutRealArray(const std::string &matlabVarName,
                                  int numRows, int numCols, const double *inArray)
{
#ifdef __USE_MATLAB__
   
   #ifdef DEBUG_MATLAB_PUT_REAL
   MessageInterface::ShowMessage
      ("MatlabInterface::PutRealArray() entered, matlabVarName='%s', numRows=%d, "
       "numCols=%d, inArray=%p\n", matlabVarName.c_str(), numRows, numCols, inArray);
   #endif
   
   // create a matlab variable
   mxArray *mxArrayPtr = NULL;
   mxArrayPtr = mxCreateDoubleMatrix(numRows, numCols, mxREAL);
   
   #ifdef DEBUG_MATLAB_GET_REAL
   MessageInterface::ShowMessage
      ("   mxArrayPtr <%p> created, now copying from inArray <%p>\n", mxArrayPtr, inArray);
   #endif
   
   memcpy((char*)mxGetPr(mxArrayPtr), (char*)inArray, numRows*numCols*sizeof(double));
   
   // place the variable mxArrayPtr into the MATLAB workspace
   engPutVariable(enginePtr, matlabVarName.c_str(), mxArrayPtr);
   
   // Should I destroy after put to matlab?
   #ifdef DEBUG_MATLAB_GET_REAL
   MessageInterface::ShowMessage("   Now destroying mxArrayPtr <%p>\n", mxArrayPtr);
   #endif
   mxDestroyArray(mxArrayPtr);
   
   return 1;
#endif
   
   return 0;
} // end PutRealArray()


//------------------------------------------------------------------------------
// int GetRealArray(const std::string &matlabVarName, int numElements,
//                  double outArray[])
//------------------------------------------------------------------------------
/**
 * Get arrays from Matlab workspace.
 *
 * @param <matlabVarName> variable name in the MATLAB workspace
 * @param <numElements> number of elements to receive from MATLAB
 * @param <outArray> array to receive double array from MATLAB
 * @Return 1 = no error, 0 = error
 * @exception <InterfaceException> thrown if empty output was received
 */
//------------------------------------------------------------------------------
int MatlabInterface::GetRealArray(const std::string &matlabVarName,
                                  int numElements, double outArray[])
{
#if defined __USE_MATLAB__
   
   #ifdef DEBUG_MATLAB_GET_REAL
   MessageInterface::ShowMessage
      ("MatlabInterface::GetRealArray() entered, matlabVarName=%s, numElements=%d\n",
       matlabVarName.c_str(), numElements);
   #endif
   
   // set precision to long
   //EvalString("format long");
   
   // get the variable from the MATLAB workspace
   mxArray *mxArrayPtr = NULL;
   mxArrayPtr = engGetVariable(enginePtr, matlabVarName.c_str());
   
   #ifdef DEBUG_MATLAB_GET_REAL
   MessageInterface::ShowMessage("   mxArrayPtr is <%p>\n", mxArrayPtr);
   #endif
   
   if (mxArrayPtr == NULL)
   {
      #ifdef DEBUG_MATLAB_GET_REAL
      MessageInterface::ShowMessage
         ("MatlabInterface::GetRealArray(): mxArrayPtr is NULL\n");
      #endif
      return 0;
   }
   
   if (mxIsDouble(mxArrayPtr))
   {
      #ifdef DEBUG_MATLAB_GET_REAL
      MessageInterface::ShowMessage
         ("MatlabInterface::GetRealArray() matlab double array pointer found <%p>, "
          "so calling mxGetPr()\n", mxArrayPtr);
      #endif
      
      // If real numeric pointer is NULL, throw an exception
      double *realPtr = mxGetPr(mxArrayPtr);
      if (realPtr == NULL)
         throw InterfaceException("Received empty output from MATLAB");
      
      memcpy((char*)outArray, (char*)mxGetPr(mxArrayPtr),
             numElements*sizeof(double));
      
      #ifdef DEBUG_MATLAB_GET_REAL
      MessageInterface::ShowMessage("      outArray  = \n");
      for (Integer ii=0; ii < numElements; ii++)
         MessageInterface::ShowMessage("         %.12f\n", outArray[ii]);
      MessageInterface::ShowMessage("   Now destroying mxArrayPtr <%p>\n", mxArrayPtr);
      #endif
      
      mxDestroyArray(mxArrayPtr);
      return 1;
   }
   else
   {
      #ifdef DEBUG_MATLAB_GET_REAL
      MessageInterface::ShowMessage
         ("MatlabInterface::GetRealArray() Matlab variable is not a double array\n");
      #endif
      
      return 0;
   }
#endif

   return 0;
} // end GetRealArray()


//------------------------------------------------------------------------------
// int GetString(const std::string &matlabVarName, std::string &outStr)
//------------------------------------------------------------------------------
/**
 * Get char array from Matlab workspace.
 *
 * @param <matlabVarName> variable name in the MATLAB workspace
 * @param <outStr> string to receive char array from MATLAB
 * @Return 1 if matlab string variable found, 0 otherwise
 * @exception <InterfaceException> thrown if empty output was received
 */
//------------------------------------------------------------------------------
int MatlabInterface::GetString(const std::string &matlabVarName,
                               std::string &outStr)
{
#ifdef __USE_MATLAB__
   
   #ifdef DEBUG_MATLAB_GET_STRING
   MessageInterface::ShowMessage
      ("MatlabInterface::GetString() entered, matlabVarName='%s', outStr='%s'\n",
       matlabVarName.c_str(), outStr.c_str());
   #endif
   
   // get the variable from the MATLAB workspace
   mxArray *mxArrayPtr = NULL;
   mxArrayPtr = engGetVariable(enginePtr, matlabVarName.c_str());
   
   // if pointer is NULL, variable not found
   if (mxArrayPtr == NULL)
   {
      #ifdef DEBUG_MATLAB_GET_STRING
      MessageInterface::ShowMessage
         ("MatlabInterface::GetString() mxArrayPtr is NULL\n");
      #endif
      return 0;
   }
   
   // check if it is string
   if (mxIsChar(mxArrayPtr))
   {
      #ifdef DEBUG_MATLAB_GET_STRING
      MessageInterface::ShowMessage
         ("MatlabInterface::GetString() matlab string pointer found <%p>, "
          "so calling mxGetString()\n", mxArrayPtr);
      #endif
      
      char outArray[512];
      mxGetString(mxArrayPtr, outArray, 512);
      outStr.assign(outArray);
      
      #ifdef DEBUG_MATLAB_GET_STRING
      MessageInterface::ShowMessage
         ("MatlabInterface::GetString() outStr =\n%s\n", outStr.c_str());
      MessageInterface::ShowMessage("   Now destroying mxArrayPtr <%p>\n", mxArrayPtr);
      #endif
      
      mxDestroyArray(mxArrayPtr);
      return 1;
   }
   else
   {
      #ifdef DEBUG_MATLAB_GET_STRING
      MessageInterface::ShowMessage
         ("MatlabInterface::GetString() Matlab variable is not a char array\n");
      #endif
      
      return 0;
   }
#endif
   return 0;
} // end GetString()


//------------------------------------------------------------------------------
//  int EvalString(const std::string &evalString)
//------------------------------------------------------------------------------
//  Purpose:
//     Evaluates matlab string.
//
//  Returns:
//     0, if string evaluated was successful
//     nonzero, if unsuccessful
//------------------------------------------------------------------------------
int MatlabInterface::EvalString(const std::string &evalString)
{
#ifdef __USE_MATLAB__
   
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage
      ("MatlabInterface::EvalString() calling engEvalString with\n"
       "======================================================================\n"
       "%s\n\n", evalString.c_str());
   #endif
   
   // return value is 0 if the command was evaluated by the MATLAB engine session,
   // and a nonzero value if unsuccessful. Possible reasons for failure include
   // the engine session is no longer running or the engine pointer is invalid or NULL.
   int retval = engEvalString(enginePtr, evalString.c_str());
   
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage("MatlabInterface::EvalString() exiting with %d\n", retval);
   #endif
   return retval;
#endif
   return 0;
}


//------------------------------------------------------------------------------
//  int OutputBuffer(const char *buffer, int size)
//------------------------------------------------------------------------------
//  Purpose:
//     outputs matlab results to buffer
//
//  Returns:
//     1 = no error, 0 = error
//------------------------------------------------------------------------------
int MatlabInterface::OutputBuffer(char *buffer, int size)
{
#if defined __USE_MATLAB__
   #ifdef DEBUG_OUTPUT_BUFFER
   MessageInterface::ShowMessage
      ("MatlabInterface::OutputBuffer() entered, buffer=%p, size=%d\n", buffer, size);
   #endif
   if (buffer == NULL)
   {
      return 0;
   }
   else
   {
      //Ensure first that the buffer is always NULL terminated.
      buffer[0] = '\0';
      engOutputBuffer(enginePtr, buffer, size-1);
      return 1;
   }
#endif
   return 0;
}

//------------------------------------------------------------------------------
// bool IsOpen()
//------------------------------------------------------------------------------
bool MatlabInterface::IsOpen() 
{  
   #ifdef __USE_MATLAB__
      return (MatlabInterface::enginePtr != NULL);
   #endif
   return false;
}


//------------------------------------------------------------------------------
// void RunMatlabString(std::string evalString)
//------------------------------------------------------------------------------
void MatlabInterface::RunMatlabString(std::string evalString)
{
   #ifndef __USE_MATLAB__
   return;
   #endif
   
   
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage
      ("MatlabInterface::RunMatlabString() entered\n"
       "======================================================================\n"
       "%s\n\n", evalString.c_str());
   #endif
   
   if (!MatlabInterface::IsOpen())
   {
      // Let's try to open it first (loj: 2008.03.06)
      if (MatlabInterface::Open() == 0)
         throw InterfaceException("**** ERROR **** Failed to open MATLAB engine\n");
   }
   
   // add try/catch to string to evaluate
   evalString = "try,\n  " + evalString + "\ncatch\n  errormsg = lasterr;\nend";
   
   bool errorReturned = false;
   std::string errorStr;
   // call to evaluate string
   if (EvalString(evalString) == 0)
   {
      // if there was an error throw an exception
      #ifdef DEBUG_MATLAB_EVAL
      MessageInterface::ShowMessage("   Now checking if there was an error\n");
      #endif
      if (GetString("errormsg", errorStr) == 1)
      {
         #ifdef DEBUG_MATLAB_EVAL
         MessageInterface::ShowMessage("Error occurred in Matlab\n'%s'\n", errorStr.c_str());
         #endif
         errorReturned = true;
      }
      #ifdef DEBUG_MATLAB_EVAL
      MessageInterface::ShowMessage("   No error found\n");
      #endif
   }
   else
   {
      errorReturned = true;
      errorStr = "Error returned from " + evalString;
   }
   
   if (errorReturned)
      throw InterfaceException(errorStr);
   
   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage("MatlabInterface::RunMatlabString() exiting\n\n");
   #endif
}


//--------------------------------------
//  private functions
//--------------------------------------


//------------------------------------------------------------------------------
//  <constructor>
//  MatlabInterface()
//------------------------------------------------------------------------------
MatlabInterface::MatlabInterface()
{
#if defined __USE_MATLAB__
   enginePtr = 0;
   accessCount = 0;
#endif
}


//------------------------------------------------------------------------------
//  <destructor>
//  MatlabInterface()
//------------------------------------------------------------------------------
MatlabInterface::~MatlabInterface()
{
#if defined __USE_MATLAB__
   if (enginePtr != NULL)
      Close();
#endif
}


