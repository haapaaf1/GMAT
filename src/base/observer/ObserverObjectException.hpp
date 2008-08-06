//$Header$
//------------------------------------------------------------------------------
//                                ObserverObjectException
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 * Exception class used by the ObserverObject hierarchy.
 */
//------------------------------------------------------------------------------


#ifndef ObserverObjectException_hpp
#define ObserverObjectException_hpp

#include "BaseException.hpp"


class GMAT_API ObserverObjectException : public BaseException
{
public:
   //---------------------------------------------------------------------------
   // ObserverObjectException(const std::string &details)
   //---------------------------------------------------------------------------
   /**
    * Default constructor.
    *
    * @param <details> Message explaining why the exception was thrown.
    */
   //---------------------------------------------------------------------------
   ObserverObjectException(const std::string &details = "") :
      BaseException("ObserverObject Exception Thrown: ", details)
   {
   }

   //---------------------------------------------------------------------------
   // ~ObserverObjectException()
   //---------------------------------------------------------------------------
   /**
    * Destructor.
    */
   //---------------------------------------------------------------------------
   ~ObserverObjectException()
   {
   }

   //---------------------------------------------------------------------------
   // ObserverObjectException(const ObserverObjectException &soe)
   //---------------------------------------------------------------------------
   /**
    * Copy constructor.
    *
    * @param <soe> Exception used to generate this one.
    */
   //---------------------------------------------------------------------------
   ObserverObjectException(const ObserverObjectException &soe) :
      BaseException(soe)
   {
   }
};

#endif // ObserverObjectException_hpp
