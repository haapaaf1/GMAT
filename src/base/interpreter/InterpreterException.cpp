//$Id$
//------------------------------------------------------------------------------
//                                  ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/11/04
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




#include "InterpreterException.hpp" // class's header file

// class constructor
InterpreterException::InterpreterException(const std::string &details,
      Gmat::MessageType mt) :
    BaseException       ("Interpreter Exception: ", details, mt)
{
}

// class destructor
InterpreterException::~InterpreterException(void)
{
}

