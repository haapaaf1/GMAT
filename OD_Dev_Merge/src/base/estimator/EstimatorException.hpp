//$Header$
//------------------------------------------------------------------------------
//                            EstimatorException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/20
//
//
/**
 * Definition for exceptions thrown by the Estimator subsystem. 
 */
//------------------------------------------------------------------------------


#ifndef EstimatorException_hpp
#define EstimatorException_hpp

#include "BaseException.hpp"
#include "gmatdefs.hpp"          // For GMAT_API


class GMAT_API EstimatorException : public BaseException
{
public:
   //---------------------------------------------------------------------------
   // EstimatorException(const std::string &details, const std::string &message)
   //---------------------------------------------------------------------------
   /**
    * Default constructor.
    *
    * @param <details> Message explaining why the exception was thrown.
    * @param <message> Initial part of the exception message.
    */
   //---------------------------------------------------------------------------
   EstimatorException(const std::string &details,
                   const std::string &message = "Estimator subsystem exception: "):
      BaseException       (message, details)
   {
   }
   
   //---------------------------------------------------------------------------
   // ~EstimatorException()
   //---------------------------------------------------------------------------
   /**
    * Destructor.
    */
   //---------------------------------------------------------------------------
   virtual ~EstimatorException()
   {
   }
};


#endif // EstimatorException_hpp
