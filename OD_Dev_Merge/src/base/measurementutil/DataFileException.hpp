//$Header$
//------------------------------------------------------------------------------
//                             DataFileException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/11/24
//
/**
 *
 * Exceptions thrown in the data file object
 *
 */
//------------------------------------------------------------------------------


#ifndef DataFileException_hpp
#define DataFileException_hpp

#include "BaseException.hpp"
#include "gmatdefs.hpp"

class GMAT_API DataFileException : public BaseException
{
public:
   DataFileException(const std::string &details);
   virtual ~DataFileException();
   DataFileException(const DataFileException &dfe);
};

#endif // DataFileException_hpp
