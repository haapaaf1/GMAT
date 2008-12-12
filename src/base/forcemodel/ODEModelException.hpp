//$Header$
//------------------------------------------------------------------------------
//                             ODEModelException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway
// Created: 2004/02/29
//
/**
 * Exceptions thrown in the force models
 */
//------------------------------------------------------------------------------


#ifndef ODEModelException_hpp
#define ODEModelException_hpp

#include "BaseException.hpp"
#include "gmatdefs.hpp"

class GMAT_API ODEModelException : public BaseException
{
public:
   ODEModelException(const std::string &details);
   virtual ~ODEModelException();
   ODEModelException(const ODEModelException &fme);
};

#endif // ODEModelException_hpp
