//$Id$
//------------------------------------------------------------------------------
//                               MatlabInterface
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2010/03/31
//
/**
 * Implements MatlabInterface functions. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#include <stdlib.h>         // for NULL
#include <string.h>         // for memcpy()
#include <sstream>          // for std::stringstream
#ifdef __USE_MATLAB__
#include "engine.h"         // for Matlab Engine
#include "matrix.h"         // for Matlab mxArray
#endif

#ifdef __WXMAC__
#include <stdlib.h>         // for system() to launch X11 application
#include <unistd.h>         // for gethostname()
#endif

#include "MatlabInterface.hpp"
#include "InterfaceException.hpp"
#include "MessageInterface.hpp"

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
#ifdef __USE_MATLAB__
   if (instance == NULL)
      instance = new MatlabInterface("OneInstance");
   
   return instance;
#endif

   return NULL;
}


//------------------------------------------------------------------------------
//  <constructor>
//  MatlabInterface()
//------------------------------------------------------------------------------
MatlabInterface::MatlabInterface(const std::string &name) :
   Interface ("MatlabInterface", name)
{
#ifdef __USE_MATLAB__
   enginePtr = 0;
   accessCount = 0;
   matlabMode = SHARED;
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
#ifdef __USE_MATLAB__
   if (enginePtr != NULL)
      Close();
   delete [] outBuffer;
#endif
}


//------------------------------------------------------------------------------
// MatlabInterface(const MatlabInterface &mi)
//------------------------------------------------------------------------------
MatlabInterface::MatlabInterface(const MatlabInterface &mi) :
   Interface (mi)
{
}


//------------------------------------------------------------------------------
// MatlabInterface& operator=(const MatlabInterface& mi)
//------------------------------------------------------------------------------
MatlabInterface& MatlabInterface::operator=(const MatlabInterface& mi)
{
   if (&mi != this)
   {
      Interface::operator=(mi);
   }
   
   return *this;
}


//------------------------------------------------------------------------------
// int Open(const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * Opens Matlab engine
 *
 * @param  name  Name used in identifying matlab engine
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::Open(const std::string &name)
{
#ifdef __USE_MATLAB__

   //=======================================================
   #ifdef __WXMAC__
   //=======================================================
   
   return OpenEngineOnMac();
   
   //=======================================================
   #else
   //=======================================================
   
   // If opening MATLAB engine for single use
   if (matlabMode == SINGLE_USE)
      return OpenSingleEngine(name);
   else if (matlabMode == SHARED)
      return OpenSharedEngine();
   else
   {
      message = "matlab mode is invalid, expecting 20 or 21";
      return 0;
   }
   
   //=======================================================
   #endif  // End-ifdef __WXMAC__
   //=======================================================
   
#else
   return 0;
#endif
} // end Open()


//------------------------------------------------------------------------------
//  int Close(const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * Closes Matlab engine
 *
 * @param  name  Name used in identifying matlab engine
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::Close(const std::string &name)
{
#ifdef __USE_MATLAB__
   
   //=======================================================
   #ifdef __WXMAC__
   //=======================================================
   
   return CloseEngineOnMac();
   
   //=======================================================
   #else
   //=======================================================
   
   if (matlabMode == SINGLE_USE)
      return CloseSingleEngine(name);
   else if (matlabMode == SHARED)
      return CloseSharedEngine();
   else
   {
      message = "matlab mode is invalid, expecting 20 or 21";
      return 0;
   }
   
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
#else
   return 0;
#endif
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
#ifdef __USE_MATLAB__

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
#else
   return 0;
#endif
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
#else
   return 0;
#endif
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

   // return value is 0 if the command was evaluated by the MATLAB engine,
   // and a nonzero value if unsuccessful. Possible reasons for failure include
   // the engine session is no longer running or the engine pointer is invalid or NULL.
   int retval = engEvalString(enginePtr, evalString.c_str());

   #ifdef DEBUG_MATLAB_EVAL
   MessageInterface::ShowMessage("MatlabInterface::EvalString() exiting with %d\n", retval);
   #endif
   return retval;
#else
   return 0;
#endif
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
#ifdef __USE_MATLAB__
   
   outBuffer[0] = '\0';
   int sizeToUse = size-1;

   if (sizeToUse > MAX_OUT_SIZE)
      sizeToUse = MAX_OUT_SIZE;

   engOutputBuffer(enginePtr, outBuffer, sizeToUse);
   return sizeToUse;

#else
   return 0;
#endif
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
// bool IsOpen(const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * Checks if engine is open. If name is blank for single use, it will
 * return true if there are more than one engine is opened.
 */
//------------------------------------------------------------------------------
bool MatlabInterface::IsOpen(const std::string &name)
{
#ifdef __USE_MATLAB__
   
   if (matlabMode == SINGLE_USE)
   {
      if (name != "")
      {
         std::map<std::string, Engine*>::iterator pos = matlabEngineMap.find(name);
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
#else
   return false;
#endif
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


//------------------------------------------------------------------------------
// void SetMatlabMode(int mode)
//------------------------------------------------------------------------------
/**
 * Sets matlab run mode.
 *
 * @param  mode  The matlab run mode
 *               (0 = shared, 1 = single use, -1 = no MATLAB installed)
 */
//------------------------------------------------------------------------------
void MatlabInterface::SetMatlabMode(int mode)
{
#ifdef __USE_MATLAB__
   matlabMode = mode;
#endif
}

//------------------------------------------------------------------------------
// int GetMatlabMode()
//------------------------------------------------------------------------------
/**
 * Return matlab run mode.
 *
 * @return  The matlab run mode (0 = shared, 1 = single use)
 */
//------------------------------------------------------------------------------
int MatlabInterface::GetMatlabMode()
{
#ifdef __USE_MATLAB__
   return matlabMode;
#else
   return -1;
#endif
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the MatlabInterface.
 *
 * @return clone of the MatlabInterface.
 */
//------------------------------------------------------------------------------
GmatBase* MatlabInterface::Clone() const
{
   return (new MatlabInterface(*this));
}


//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void MatlabInterface::Copy(const GmatBase* orig)
{
   operator=(*((MatlabInterface *)(orig)));
}


//--------------------------------------
//  private functions
//--------------------------------------


#ifdef __USE_MATLAB__
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
      ("MatlabInterface::OpenEngineOnMac() enginePtr=%p\n", enginePtr);
   #endif
   
   if (enginePtr == NULL)
      MessageInterface::ShowMessage("Please wait while MATLAB engine opens...\n");
      message = "Please wait while MATLAB engine opens...\n";
   
   /// Check if MATLAB engine is still running then doesn't need to re-launch
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
   
   if (OK != 0)
   {
      //MessageInterface::ShowMessage("Error getting host name\n");
      return 0;
   }
   
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
      message = "Successfully opened MATLAB engine using startcmd " + runString + "\n";
      
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
      message = "Failed to open MATLAB engine using startcmd " + runString + "\n";
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
   
   // Check if MATLAB engine is still running then close it.
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
      // attempting to terminate a MATLAB engine that was
      // already terminated.
      //==============================================================
      retval = engClose(enginePtr);
      if (retval == 0)
      {
         retval = 1;
         MessageInterface::ShowMessage("MATLAB engine successfully closed\n");
         message = "MATLAB engine successfully closed\n";
      }
      else
      {
         retval = 0;;
         MessageInterface::ShowMessage("\nError closing MATLAB\n");
         message = "\nError closing MATLAB\n";
      }
      
      enginePtr = NULL;      // set to NULL, so it can be reopened
   }
   else
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("\nUnable to close MATLAB engine because it is not currently running\n");
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
      ("MatlabInterface::OpenSharedEngine() enginePtr=%p\n", enginePtr);
   #endif
   
   if (enginePtr == NULL)
   {
      MessageInterface::ShowMessage("Please wait while MATLAB engine opens...\n");
      // This message is useless since it will be overwritten by the next message.
      //message = "Please wait while MATLAB engine opens...\n";
   }
   else
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage("Connecting to current MATLAB engine\n");
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
      message = "MATLAB engine successfully opened\n";
      return 1;
   }
   else
   {
      MessageInterface::ShowMessage("Failed to open MATLAB engine ...\n");
      message = "Failed to open MATLAB engine ...\n";
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
   
   // Check if MATLAB engine is still running then close it.
   if (enginePtr != NULL)
   {
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("MatlabInterface::CloseSharedEngine() enginePtr=%p\n", enginePtr);
      #endif
      
      accessCount--;
      
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("Attempting to close MATLAB engine ... accessCount = %d\n", accessCount);
      #endif
      
      //==============================================================
      // int engClose(Engine *ep);
      // 0 on success, and 1 otherwise. Possible failure includes
      // attempting to terminate a MATLAB engine that was
      // already terminated.
      //==============================================================
      retval = engClose(enginePtr);
      if (retval == 0)
      {
         retval = 1;
         MessageInterface::ShowMessage("MATLAB engine successfuly closed\n");
         message = "MATLAB engine successfuly closed\n";
      }
      else
      {
         retval = 0;
         MessageInterface::ShowMessage("\nError closing MATLAB\n");
         message = "\nError closing MATLAB\n";
      }
      
      enginePtr = NULL; // set to NULL, so it can be reopened
   }
   else
   {
      retval = 0;
      #ifdef DEBUG_MATLAB_OPEN_CLOSE
      MessageInterface::ShowMessage
         ("\nUnable to close MATLAB engine because it is not currently running\n");
      #endif
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// int OpenSingleEngine(const std::string &name);
//------------------------------------------------------------------------------
/**
 * Opens single Matlab engine on Windows.
 *
 * @return  1 = no error, 0 = error
 */
//------------------------------------------------------------------------------
int MatlabInterface::OpenSingleEngine(const std::string &name)
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
   if (name == "")
   {
      std::stringstream ss("");
      ss.width(1);
      ss << accessCount+1;
      lastEngineName = "matlabEngine_" + ss.str();
   }
   else
   {
      lastEngineName = name;
   }
   
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
      message = "Failed to open MATLAB engine for single use...\n";
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
   message = "MATLAB engine " + lastEngineName + " successfully opened\n";
   return 1;
}


//------------------------------------------------------------------------------
// int CloseSingleEngine(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Closes single MATLAB engine on Windows.
 *
 * @param  name  Name used in identifying matlab engine
 * @return 1 if successfully closed, 0 otherwise
 */
//------------------------------------------------------------------------------
int MatlabInterface::CloseSingleEngine(const std::string &name)
{
   int retval = 1; // set to success
   bool failedToClose = false;
   
   if (name != "")
   {
      std::map<std::string, Engine*>::iterator pos = matlabEngineMap.find(name);
      if (pos != matlabEngineMap.end())
      {
         //==============================================================
         // int engClose(Engine *ep);
         // 0 on success, and 1 otherwise. Possible failure includes
         // attempting to terminate a MATLAB engine that was
         // already terminated.
         //==============================================================
         retval = engClose(pos->second);
         if (retval == 0)
         {
            MessageInterface::ShowMessage
               ("MATLAB engine '%s' successfully closed\n", name.c_str());
            message = "MATLAB engine " + name + " successfully closed\n";
         }
         else
         {
            failedToClose = true;
            MessageInterface::ShowMessage
               ("\nError closing MATLAB engine '%s'\n", name.c_str());
            message = "\nError closing MATLAB engine " + name + "\n";
         }
      }
      else
      {
         MessageInterface::ShowMessage
            ("\nError closing MATLAB engine '%s'\n", name.c_str());
         message = "\nError closing MATLAB engine " + name + "\n";
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
            ("MatlabInterface::CloseSingleEngine() about to close engine <%p>'%s'\n",
             pos->second, (pos->first).c_str());
         #endif
         
         retval = engClose(pos->second);
         if (retval == 0)
         {
            MessageInterface::ShowMessage
               ("MATLAB engine '%s' successfully closed\n", (pos->first).c_str());
            message = "MATLAB engine " + pos->first + " successfully closed\n";
         }
         else
         {
            failedToClose = true;
            MessageInterface::ShowMessage
               ("\nError closing MATLAB engine '%s'\n", (pos->first).c_str());
            message = "\nError closing MATLAB engine " + pos->first + "\n";
         }
      }
   }
   
   if (failedToClose)
      retval = 0;
   
   return retval;
}
#endif