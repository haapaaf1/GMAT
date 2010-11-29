//$Id$
//------------------------------------------------------------------------------
//                              BaseException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Darrel J. Conway
// Created: 2003/09/09
//
/**
 * This class provides base exception class, from which all GMAT exceptions must 
 * be derived.
 */
//------------------------------------------------------------------------------
#ifndef BaseException_hpp
#define BaseException_hpp

#include "gmatdefs.hpp"
#include <string>

class GMAT_API BaseException
{
public:
   virtual std::string GetFullMessage() const;
   virtual std::string GetDetails() const;
   virtual bool IsFatal() const;
   virtual void SetMessage(const std::string &message);
   virtual void SetDetails(const std::string &details);
   virtual void SetFatal(bool fatal);
   const BaseException& operator=(const std::string &newMessage);
   
   virtual void SetDetails(const char *details, ...);
   
   static const int MAX_MESSAGE_LENGTH = 3000;
   
protected:
   BaseException(const std::string& message = "",
         const std::string &details = "",
         Gmat::MessageType mt = Gmat::ERROR_);
         // Change to this if it's problematic:
         // Gmat::GENERAL_);
   BaseException(const BaseException& be);
   virtual ~BaseException();
   const BaseException& operator=(const BaseException& be);
   
private:
   std::string theMessage;
   std::string theDetails;
   Gmat::MessageType msgType;
   bool isFatal;
};
#endif // BaseException_hpp

