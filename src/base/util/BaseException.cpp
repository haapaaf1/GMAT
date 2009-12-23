//$Id$
//------------------------------------------------------------------------------
//                             BaseException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P.
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2007/1/18
//
/**
 * Exception class used by the GmatBase base class.
 */
//------------------------------------------------------------------------------

#include "BaseException.hpp"
#include <stdarg.h>                // for va_start(), va_end()

#include <cstdlib>                      // Required for GCC 4.3
#include <string.h>                     // Required for GCC 4.3


//------------------------------------------------------------------------------
// std::string GetFullMessage() const 
//------------------------------------------------------------------------------
std::string BaseException::GetFullMessage() const 
{
   return theMessage + theDetails;
}

//------------------------------------------------------------------------------
// std::string GetDetails() const 
//------------------------------------------------------------------------------
std::string BaseException::GetDetails() const 
{
   return theDetails;
}

//------------------------------------------------------------------------------
// bool IsFatal() const
//------------------------------------------------------------------------------
bool BaseException::IsFatal() const
{
   return isFatal;
}

//------------------------------------------------------------------------------
// void BaseException::SetMessage(const std::string &message)  
//------------------------------------------------------------------------------
void BaseException::SetMessage(const std::string &message)  
{
   theMessage = message;
}

//------------------------------------------------------------------------------
// void SetDetails(const std::string &details)  
//------------------------------------------------------------------------------
void BaseException::SetDetails(const std::string &details)  
{
   theDetails = details;
}

//------------------------------------------------------------------------------
// void SetFatal(bool fatal)
//------------------------------------------------------------------------------
void BaseException::SetFatal(bool fatal)
{
   isFatal = fatal;
}

//------------------------------------------------------------------------------
// const BaseException& operator=(const std::string &newMessage) 
//------------------------------------------------------------------------------
const BaseException& BaseException::operator=(const std::string &newMessage) 
{
   theMessage = newMessage;
   return *this;
}

//------------------------------------------------------------------------------
// void SetDetails(const char *details, ...)
//------------------------------------------------------------------------------
/**
 * constructor taking variable arguments
 */
//------------------------------------------------------------------------------
void BaseException::SetDetails(const char *details, ...)
{
   short    ret;
   short    size;
   va_list  marker;
   char     *msgBuffer;

   size = strlen(details) + MAX_MESSAGE_LENGTH;

   if ( (msgBuffer = (char *)malloc(size)) != NULL )
   {
      va_start(marker, details);

      #ifdef _MSC_VER  // Microsoft Visual C++
      // _vscprintf doesn't count terminating '\0'
      int len = _vscprintf( details, marker ) + 1;
      ret = vsprintf_s(msgBuffer, len, details, marker);
      #else
      ret = vsprintf(msgBuffer, details, marker);
      #endif

      va_end(marker);
   }

   theDetails = std::string(msgBuffer);
   free(msgBuffer);
}

//---------------------------------
// protected
//---------------------------------

//------------------------------------------------------------------------------
// BaseException(const std::string& message = "", const std::string &details = "") 
//------------------------------------------------------------------------------
BaseException::BaseException(const std::string& message, const std::string &details) 
{
   theMessage = message;
   theDetails = details;
   isFatal = false;
}

//------------------------------------------------------------------------------
// BaseException(const BaseException& be) 
//------------------------------------------------------------------------------
BaseException::BaseException(const BaseException& be) 
{
   theMessage = be.theMessage;
   theDetails = be.theDetails;
   isFatal = be.isFatal;
}

//------------------------------------------------------------------------------
// virtual ~BaseException() 
//------------------------------------------------------------------------------
BaseException::~BaseException() 
{
}

//------------------------------------------------------------------------------
// const BaseException& operator=(const BaseException& be) 
//------------------------------------------------------------------------------
const BaseException& BaseException::operator=(const BaseException& be) 
{
   theMessage = be.theMessage;
   theDetails = be.theDetails;
   isFatal = be.isFatal;
   return *this;
}
