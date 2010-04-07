//$Id$
//------------------------------------------------------------------------------
//                            MatlabInterfaceFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Linda Jun
// Created: 2010/03/30
//
/**
 *  Declaration code for the MatlabInterfaceFactory class.
 */
//------------------------------------------------------------------------------
#ifndef MatlabInterfaceFactory_hpp
#define MatlabInterfaceFactory_hpp

#include "Factory.hpp"
#include "Interface.hpp"

class MatlabInterfaceFactory : public Factory
{
public:
   virtual Interface* CreateInterface(const std::string &ofType,
                                      const std::string &withName);
   
   /// default constructor
   MatlabInterfaceFactory();
   /// constructor
   MatlabInterfaceFactory(StringArray createList);
   /// copy constructor
   MatlabInterfaceFactory(const MatlabInterfaceFactory& fact);
   /// assignment operator
   MatlabInterfaceFactory& operator=(const MatlabInterfaceFactory& fact);
   
   virtual ~MatlabInterfaceFactory();
   
};

#endif // MatlabInterfaceFactory_hpp
