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


#ifndef MeasurementModelException_hpp
#define MeasurementModelException_hpp

#include "BaseException.hpp"
#include "gmatdefs.hpp"

class GMAT_API MeasurementModelException : public BaseException
{
public:
   MeasurementModelException(const std::string &details);
   virtual ~MeasurementModelException();
   MeasurementModelException(const MeasurementModelException &mme);
};

#endif // MeasurementModelException_hpp
