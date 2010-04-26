//$Header$
//------------------------------------------------------------------------------
//                            UtilityException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/04/19
//
/**
 * Declares utility exception.
 */
//------------------------------------------------------------------------------
#ifndef UtilityException_hpp
#define UtilityException_hpp

#include "BaseException.hpp"

// General utility exception
class GMAT_API UtilityException : public BaseException
{
public:
   UtilityException(const std::string& details = "") 
      : BaseException("Utility Exception: ", details) {};
};


// Gravity File exception
class GMAT_API GravityFileException : public BaseException
{
public:
   GravityFileException(const std::string& details = "") 
      : BaseException("Gravity File Exception: ", details) {};
};


// Time utility exception
class GMAT_API TimeException : public BaseException
{
public:
   TimeException(const std::string& details = "") 
      : BaseException("Time Exception: ", details) {};
};
#endif