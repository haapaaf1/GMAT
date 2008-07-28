//$Header$
//------------------------------------------------------------------------------
//                         ObserverFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 *  This class is the factory class for ground stations.  
 */
//------------------------------------------------------------------------------
#ifndef ObserverFactory_hpp
#define ObserverFactory_hpp

#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "Observer.hpp"

class GMAT_API ObserverFactory : public Factory
{
public:
   SpaceObject* CreateObserver(const std::string &ofType,
                                 const std::string &withName = "");

   // default constructor
   ObserverFactory();
   // constructor
   ObserverFactory(StringArray createList);
   // copy constructor
   ObserverFactory(const ObserverFactory &fact);
   // assignment operator
   ObserverFactory& operator= (const ObserverFactory &fact);

   // destructor
   ~ObserverFactory();

protected:
   // protected data

private:
   // private data


};

#endif // ObserverFactory_hpp
