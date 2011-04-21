//$Id$
//------------------------------------------------------------------------------
//                                  SandboxException
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Author: Darrel J. Conway
// Created: 2003/mm/dd
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Insert descriptive text here.
 *
 * @note Any notes here.
 */
//------------------------------------------------------------------------------




// Class automatically generated by Dev-C++ New Class wizard

#include "SandboxException.hpp" // class's header file

//------------------------------------------------------------------------------
// SandboxException(std::string details, Gmat::MessageType mt = Gmat::ERROR_);
//------------------------------------------------------------------------------
/**
 * class constructor
 */
//------------------------------------------------------------------------------
SandboxException::SandboxException(std::string details, Gmat::MessageType mt) :
   BaseException("Sandbox Exception: ", details, mt)
{
}

// class destructor
SandboxException::~SandboxException()
{
}

