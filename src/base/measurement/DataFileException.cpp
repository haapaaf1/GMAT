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


#include "DataFileException.hpp"


//------------------------------------------------------------------------------
//  DataFileException(std::string details)
//------------------------------------------------------------------------------
/**
 * Constructs DataFileException instance (default constructor).
 * 
 * @param details A message providing the details of the exception. 
 */
//------------------------------------------------------------------------------
DataFileException::DataFileException(const std::string &details) :
    BaseException           ("DataFile Exception Thrown: ", details)
{
}


//------------------------------------------------------------------------------
//  ~CommandException()
//------------------------------------------------------------------------------
/**
 * Class destructor.
 */
//------------------------------------------------------------------------------
DataFileException::~DataFileException()
{
}


//------------------------------------------------------------------------------
//  CommandException(const CommandException &ce)
//------------------------------------------------------------------------------
/**
 * Constructs DataFileException instance (copy constructor). 
 */
//------------------------------------------------------------------------------
DataFileException::DataFileException(const DataFileException &dfe) :
    BaseException       (dfe)
{
}
