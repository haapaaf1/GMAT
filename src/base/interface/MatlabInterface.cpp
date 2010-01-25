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
#include "GmatGlobal.hpp"         // for GetMatlabMode()
#include "StringUtil.hpp"         // for ToString()

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
const int MatlabInterface::MAX_OUT_SIZE = 8192;
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
// int Open(const std::string &engineName = ""))
//------------------------------------------------------------------------------
/**
 * Opens Matlab engine
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::Open(const std::string &engineName)
{
#if defined __USE_MATLAB__

   //=======================================================
   #ifdef __WXMAC__
   //=======================================================
   
   return OpenEngineOnMac();
   
   //=======================================================
   #else
   //=======================================================
   
   // If opening MATLAB for single use
   if (GmatGlobal::Instance()->GetMatlabMode() == GmatGlobal::SINGLE_USE)
      return OpenSingleEngine(engineName);
   else
      return OpenSharedEngine();
   
   //=======================================================
   #endif  // End-ifdef __WXMAC__
   //=======================================================
   
#else
   return 0;
#endif // End-ifdef __USE_MATLAB__
} // end Open()


//------------------------------------------------------------------------------
//  int Close(const std::string &engineName = "")
//------------------------------------------------------------------------------
/**
 * Closes Matlab engine
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::Close(const std::string &engineName)
{
#if defined __USE_MATLAB__
   
   //=======================================================
   #ifdef __WXMAC__
   //=======================================================
   
   return CloseEngineOnMac();
   
   //=======================================================
   #else
   //=======================================================
   
   if (GmatGlobal::Instance()->GetMatlabMode() == GmatGlobal::SINGLE_USE)
      return CloseSingleEngine(engineName);
   else
      return CloseSharedEngine();
   
   //=======================================================
   #endif
   //=======================================================
   
#else
   return 0;
#endif
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

      if (outBuffer == NULL)
         throw InterfaceException
            ("**** ERROR **** Failed to get string from MATLAB, output buffer is NULL\n");

      outBuffer[0] = '\0';
      mxGetString(mxArrayPtr, outBuffer, MAX_OUT_SIZE);
      outBuffer[MAX_OUT_SIZE] = '\0';
      outStr.assign(outBuffer);

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
//  int SetOutputBuffer(int size)
//------------------------------------------------------------------------------
/**
 * Sets outBuffer to input size. All result using EvalString() will use this
 * buffer.
 *
 * @param size   size of the buffer, it will use size-1.
 *
 * @return size of the buffer used, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::SetOutputBuffer(int size)
{
#if defined __USE_MATLAB__
   outBuffer[0] = '\0';
   int sizeToUse = size-1;

   if (sizeToUse > MAX_OUT_SIZE)
      sizeToUse = MAX_OUT_SIZE;

   engOutputBuffer(enginePtr, outBuffer, sizeToUse);
   return sizeToUse;
#endif

   return 0;
}


//------------------------------------------------------------------------------
// char* GetOutputBuffer()
//------------------------------------------------------------------------------
/*
 * Returns Matlab output buffer pointer which has a pointer to Matlab result
 * using EvalStaring()
 */
//------------------------------------------------------------------------------
char* MatlabInterface::GetOutputBuffer()
{
#ifdef __USE_MATLAB__
   return outBuffer;
#else
   return "";
#endif
}


//------------------------------------------------------------------------------
// bool IsOpen(const std::string &engineName = "")
//------------------------------------------------------------------------------
/**
 * Checks if engine is open. If engineName is blank for single use, it will
 * return true if there are more than one engine is opened.
 */
//------------------------------------------------------------------------------
bool MatlabInterface::IsOpen(const std::string &engineName)
{
   #ifdef __USE_MATLAB__
   if (GmatGlobal::Instance()->GetMatlabMode() == GmatGlobal::SINGLE_USE)
   {
      if (engineName != "")
      {
         std::map<std::string, Engine*>::iterator pos = matlabEngineMap.find(engineName);
         if (pos == matlabEngineMap.end())
            return false;
         else
            return true;
      }
      else
      {
         if (matlabEngineMap.empty())
            return false;
         else
            return true;
      }
   }
   else
   {
      return (MatlabInterface::enginePtr != NULL);
   }
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

   if (!IsOpen())
   {
      // Let's try to open it first (loj: 2008.03.06)
      if (Open() == 0)
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
   outBuffer = new char[MAX_OUT_SIZE+1];
   for (int i=0; i<=MAX_OUT_SIZE; i++)
      outBuffer[i] = '\0';
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
   delete [] outBuffer;
#endif
}


#if defined __USE_MATLAB__
//------------------------------------------------------------------------------
// int OpenEngineOnMac()
//------------------------------------------------------------------------------
/**
 * Opens Matlab engine on Mac.
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::OpenEngineOnMac()
{
#ifdef __WXMAC__
   #ifdef DEBUG_MATLAB_OPEN_CLOSE
   MessageInterface::ShowMessage
      ("MatlabInterface::Open() enginePtr=%p\n", enginePtr);
   #endif
   
   if (enginePtr == NULL)
      MessageInterface::ShowMessage("Please wait while MATLAB engine opens...\n");
   
   /// Check if MATLAB is still running then doesn't need to re-launch
   if (enginePtr != NULL)
   {
      accessCount++;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
         MessageInterface::ShowMessage(
         "Attempting to reopen MATLAB engine ... accessCount = %d\n", accessCount);
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
         "Attempting to open MATLAB engine ... accessCount = %d\n", accessCount);
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
   return 0;
#endif
}


//------------------------------------------------------------------------------
// int CloseEngineOnMac()
//------------------------------------------------------------------------------
/**
 * Closes shared MATLAB engine on Mac.
 *
 * @return 1 if successfully closed, 0 otherwise
 */
//------------------------------------------------------------------------------
int MatlabInterface::CloseEngineOnMac()
{
#ifdef __WXMAC__

   int retval = 1; // set to success
   
   // Check if MATLAB is still running then close it.
   if (enginePtr != NULL)
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("MatlabInterface::Close() enginePtr=%p\n", enginePtr);
      #endif
      
      accessCount--;
      
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("Attempting to close MATLAB engine ... accessCount = %d\n", accessCount);
      #endif
      
      // need to close X11 here ------------ **** TBD ****
      MessageInterface::ShowMessage
         ("Closing MATLAB engine ... please close X11 ...\n");
      
      //==============================================================
      // int engClose(Engine *ep);
      // 0 on success, and 1 otherwise. Possible failure includes
      // attempting to terminate a MATLAB engine session that was
      // already terminated.
      //==============================================================
      retval = engClose(enginePtr);
      if (retval == 0)
      {
         retval = 1;
         MessageInterface::ShowMessage("MATLAB successfully closed\n");
      }
      else
      {
         retval = 0;;
         MessageInterface::ShowMessage("\nError closing MATLAB\n");
      }
      
      enginePtr = NULL;      // set to NULL, so it can be reopened
   }
   else
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("\nUnable to close MATLAB because it is not currently running\n");
      #endif
   }
   return retval;
#else
   return 0;
#endif
}


//------------------------------------------------------------------------------
// int OpenSharedEngine()
//------------------------------------------------------------------------------
/**
 * Opens shared Matlab engine on Windows.
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::OpenSharedEngine()
{
   #ifdef DEBUG_MATLAB_OPEN_CLOSE
   MessageInterface::ShowMessage
      ("MatlabInterface::Open() enginePtr=%p\n", enginePtr);
   #endif
   
   if (enginePtr == NULL)
   {
      MessageInterface::ShowMessage("Please wait while MATLAB engine opens...\n");
   }
   else
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
         ("Attempting to open MATLAB engine ... accessCount = %d, "
          "enginePtr=%p\n", accessCount, enginePtr);
      #endif
      
      // set precision to long
      EvalString("format long");
      MessageInterface::ShowMessage("MATLAB engine successfully opened\n");
      return 1;
   }
   else
   {
      MessageInterface::ShowMessage("Failed to open MATLAB engine ...\n");
      return 0;
   }
}


//------------------------------------------------------------------------------
// int CloseSharedEngine()
//------------------------------------------------------------------------------
/**
 * Closes shared MATLAB engine on Windows.
 *
 * @return 1 if successfully closed, 0 otherwise
 */
//------------------------------------------------------------------------------
int MatlabInterface::CloseSharedEngine()
{
   int retval = 0; // set to failed
   
   // Check if MATLAB is still running then close it.
   if (enginePtr != NULL)
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("MatlabInterface::Close() enginePtr=%p\n", enginePtr);
      #endif
      
      accessCount--;
      
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("Attempting to close MATLAB engine ... accessCount = %d\n", accessCount);
      #endif
      
      //==============================================================
      // int engClose(Engine *ep);
      // 0 on success, and 1 otherwise. Possible failure includes
      // attempting to terminate a MATLAB engine session that was
      // already terminated.
      //==============================================================
      retval = engClose(enginePtr);
      if (retval == 0)
      {
         retval = 1;
         MessageInterface::ShowMessage("MATLAB uccessfuly closed\n");
      }
      else
      {
         retval = 0;
         MessageInterface::ShowMessage("\nError closing MATLAB\n");
      }
      
      enginePtr = NULL; // set to NULL, so it can be reopened
   }
   else
   {
      retval = 0;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("\nUnable to close MATLAB because it is not currently running\n");
      #endif
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// int OpenSingleEngine(const std::string &engineName);
//------------------------------------------------------------------------------
/**
 * Opens single Matlab engine on Windows.
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::OpenSingleEngine(const std::string &engineName)
{
   //=================================================================
   //Engine* engOpenSingleUse(const char *startcmd, void *dcom, int *retstatus);
   //startcmd
   //   String to start MATLAB process. On Microsoft Windows systems,
   //   the startcmd string must be NULL.
   //dcom
   //   Reserved for future use; must be NULL.
   //retstatus
   //   Return status; possible cause of failure.
   //      0 = success
   //     -2 = error - second argument must be NULL
   //     -3 = error - engOpenSingleUse failed
   //=================================================================
   
   // Check for the engine name first      
   if (engineName == "")
      lastEngineName = "matlabEngine_" + GmatStringUtil::ToString(accessCount+1, 1);
   else
      lastEngineName = engineName;
   
   #ifdef DEBUG_MATLAB_OPEN_CLOSE
   MessageInterface::ShowMessage
      ("Attempting to open MATLAB engine '%s' for single use ... accessCount = %d\n",
       lastEngineName.c_str(), accessCount+1);
   #endif
   
   if (matlabEngineMap.find(lastEngineName) != matlabEngineMap.end())
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("'%s' already opened for single use\n", lastEngineName.c_str());
      #endif
      
      enginePtr = matlabEngineMap[lastEngineName];
      return 1;
   }
   
   // Set current engine pointer to enginePtr
   int retval = -99;
   enginePtr = engOpenSingleUse(NULL, NULL, &retval);
   
   if (retval != 0)
   {
      MessageInterface::ShowMessage
         ("Failed to open MATLAB engine for single use...\n");
      return 0;
   }
   
   accessCount++;
   matlabEngineMap.insert(std::make_pair(lastEngineName, enginePtr));
   
   #ifdef DEBUG_MATLAB_OPEN_CLOSE
   MessageInterface::ShowMessage
      ("Added <%p>'%s' to matlabEngineMap\n", enginePtr, lastEngineName.c_str());
   #endif
   
   // set precision to long
   EvalString("format long");
   MessageInterface::ShowMessage
      ("MATLAB engine '%s' successfully opened\n", lastEngineName.c_str());
   
   return 1;
}


//------------------------------------------------------------------------------
// int CloseSingleEngine(const std::string &engineName)
//------------------------------------------------------------------------------
/**
 * Closes single MATLAB engine on Windows.
 *
 * @return 1 if successfully closed, 0 otherwise
 */
//------------------------------------------------------------------------------
int MatlabInterface::CloseSingleEngine(const std::string &engineName)
{
   int retval = 1; // set to success
   bool failedToClose = false;
   
   if (engineName != "")
   {
      std::map<std::string, Engine*>::iterator pos = matlabEngineMap.find(engineName);
      if (pos != matlabEngineMap.end())
      {
         //==============================================================
         // int engClose(Engine *ep);
         // 0 on success, and 1 otherwise. Possible failure includes
         // attempting to terminate a MATLAB engine session that was
         // already terminated.
         //==============================================================
         retval = engClose(pos->second);
         if (retval == 0)
         {
            MessageInterface::ShowMessage
               ("MATLAB engine '%s' successfully closed\n", engineName.c_str());
         }
         else
         {
            failedToClose = true;
            MessageInterface::ShowMessage
               ("\nError closing MATLAB engine '%s'\n", engineName.c_str());
         }
      }
      else
      {
         MessageInterface::ShowMessage
            ("\nError closing MATLAB engine '%s'\n", engineName.c_str());
      }
   }
   else
   {
      // Close all matlab engines
      for (std::map<std::string, Engine*>::iterator pos = matlabEngineMap.begin();
           pos != matlabEngineMap.end(); ++pos)
      {
         #ifdef DEBUG_MATLAB_OPEN_CLOSE
         MessageInterface::ShowMessage
            ("MatlabInterface::Close() about to close engine <%p>'%s'\n",
             pos->second, (pos->first).c_str());
         #endif
         
         retval = engClose(pos->second);
         if (retval == 0)
         {
            MessageInterface::ShowMessage
               ("MATLAB engine '%s' successfully closed\n", (pos->first).c_str());
         }
         else
         {
            failedToClose = true;
            MessageInterface::ShowMessage
               ("\nError closing MATLAB engine '%s'\n", (pos->first).c_str());
         }
      }
   }
   
   if (failedToClose)
      retval = 0;
   
   return retval;
}
#endif
