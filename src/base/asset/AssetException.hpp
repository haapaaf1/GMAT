//$Id$
//------------------------------------------------------------------------------
//                              AssetException
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy Shoan
// Created: 2008.09.22
//
/**
 * This class provides an exception class for the Asset classes.
 */
//------------------------------------------------------------------------------
#ifndef AssetException_hpp
#define AssetException_hpp

#include "gmatdefs.hpp"
#include "BaseException.hpp"

class GMAT_API AssetException : public BaseException
{
public:

   AssetException(std::string details = "");

protected:

private:
};
#endif // AssetException_hpp
