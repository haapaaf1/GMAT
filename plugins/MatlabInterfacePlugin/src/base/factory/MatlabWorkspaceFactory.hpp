//$Id$
//------------------------------------------------------------------------------
//                            MatlabWorkspaceFactory
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
 *  Declaration code for the MatlabWorkspaceFactory class. With this class user can
 *  write data to MATLAB workspace as data is published from the Publisher.
 */
//------------------------------------------------------------------------------
#ifndef MatlabWorkspaceFactory_hpp
#define MatlabWorkspaceFactory_hpp


#include "Factory.hpp"
#include "Subscriber.hpp"

class MatlabWorkspaceFactory : public Factory
{
public:
   virtual Subscriber* CreateSubscriber(const std::string &ofType,
                                        const std::string &withName = "",
                                        const std::string &fileName = "");
   
   // default constructor
   MatlabWorkspaceFactory();
   // constructor
   MatlabWorkspaceFactory(StringArray createList);
   // copy constructor
   MatlabWorkspaceFactory(const MatlabWorkspaceFactory& fact);
   // assignment operator
   MatlabWorkspaceFactory& operator=(const MatlabWorkspaceFactory& fact);
   
   virtual ~MatlabWorkspaceFactory();
   
};

#endif // MatlabWorkspaceFactory_hpp
