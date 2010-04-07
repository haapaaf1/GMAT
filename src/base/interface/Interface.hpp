//$Id$
//------------------------------------------------------------------------------
//                                  Interface
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun/GSFC
// Created: 2010/04/02
//
/**
 * Declaration of Interface class.
 */
//------------------------------------------------------------------------------
#ifndef Interface_hpp
#define Interface_hpp

#include "GmatBase.hpp" // inheriting class's header file

/**
 * Exceptions thrown from the interface subsystem
 */
class GMAT_API Interface : public GmatBase
{
public:
   Interface(const std::string &type, const std::string &name);
   Interface(const Interface &interface);
   virtual ~Interface();
   
   virtual Integer      Open(const std::string &name = "");
   virtual Integer      Close(const std::string &name = "");
   
   // required method for all subclasses
   //virtual GmatBase*    Clone() const;
};

#endif // Interface_hpp

