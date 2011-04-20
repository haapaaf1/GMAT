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
 * Implementation code for the AssetException class.
 */
//------------------------------------------------------------------------------
#include "AssetException.hpp"


//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  AssetException(std::string details)
//------------------------------------------------------------------------------
/**
* Constructs an AssetException object (default constructor).
 */
//------------------------------------------------------------------------------

AssetException::AssetException(std::string details) :
BaseException  ("Asset exception: ", details)
{

}

