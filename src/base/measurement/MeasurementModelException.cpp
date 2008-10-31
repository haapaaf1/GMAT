//$Header$
//------------------------------------------------------------------------------
//                             MeasurementModelException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/31
//
/**
 *
 * Exceptions thrown in the measurement models
 *
 */
//------------------------------------------------------------------------------


#include "MeasurementModelException.hpp"


//------------------------------------------------------------------------------
//  MeasurementModelException(std::string details)
//------------------------------------------------------------------------------
/**
 * Constructs MeasurementModelException instance (default constructor).
 * 
 * @param details A message providing the details of the exception. 
 */
//------------------------------------------------------------------------------
MeasurementModelException::MeasurementModelException(const std::string &details) :
    BaseException           ("MeasurementModel Exception Thrown: ", details)
{
}


//------------------------------------------------------------------------------
//  ~CommandException()
//------------------------------------------------------------------------------
/**
 * Class destructor.
 */
//------------------------------------------------------------------------------
MeasurementModelException::~MeasurementModelException()
{
}


//------------------------------------------------------------------------------
//  CommandException(const CommandException &ce)
//------------------------------------------------------------------------------
/**
 * Constructs MeasurementModelException instance (copy constructor). 
 */
//------------------------------------------------------------------------------
MeasurementModelException::MeasurementModelException(const MeasurementModelException &mme) :
    BaseException       (mme)
{
}
