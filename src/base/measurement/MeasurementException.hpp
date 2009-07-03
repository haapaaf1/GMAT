//$Header$
//------------------------------------------------------------------------------
//                                SpaceObjectException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2004/07/26
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Exception class used by the SpaceObject hierarchy.
 */
//------------------------------------------------------------------------------


#ifndef MeasurementException_hpp
#define MeasurementException_hpp

#include "BaseException.hpp"


class GMAT_API MeasurementException : public BaseException
{
public:
   //---------------------------------------------------------------------------
   // MeasurementException(const std::string &details)
   //---------------------------------------------------------------------------
   /**
    * Default constructor.
    *
    * @param <details> Message explaining why the exception was thrown.
    */
   //---------------------------------------------------------------------------
   MeasurementException(const std::string &details = "") :
      BaseException("Measurement Exception Thrown: ", details)
   {
   }

   //---------------------------------------------------------------------------
   // ~MeasurementException()
   //---------------------------------------------------------------------------
   /**
    * Destructor.
    */
   //---------------------------------------------------------------------------
   ~MeasurementException()
   {
   }

   //---------------------------------------------------------------------------
   // MeasurementException(const MeasurementException &soe)
   //---------------------------------------------------------------------------
   /**
    * Copy constructor.
    *
    * @param <soe> Exception used to generate this one.
    */
   //---------------------------------------------------------------------------
   MeasurementException(const MeasurementException &soe) :
      BaseException(soe)
   {
   }
};

#endif // MeasurementException_hpp
