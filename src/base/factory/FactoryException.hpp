//$Id$
//------------------------------------------------------------------------------
//                              FactoryException
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy Shoan
// Created: 2003/09/24
//
/**
 * This class provides an exception class for the Factory classes
 */
//------------------------------------------------------------------------------
#ifndef FactoryException_hpp
#define FactoryException_hpp

#include "gmatdefs.hpp"
#include "BaseException.hpp"

class GMAT_API FactoryException : public BaseException
{
public:

   FactoryException(std::string details = "");

protected:
   
private:
};
#endif // FactoryException_hpp
