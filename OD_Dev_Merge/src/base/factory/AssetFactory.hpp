//$Id$
//------------------------------------------------------------------------------
//                            AssetFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel Conway
// Created: 2008/08/12
//
/**
 *  This class is the factory class for assets.  
 */
//------------------------------------------------------------------------------

#ifndef AssetFactory_hpp
#define AssetFactory_hpp

#include "gmatdefs.hpp"
#include "Factory.hpp"


class AssetFactory : public Factory
{
public:
   AssetFactory();
   virtual ~AssetFactory();
   AssetFactory(StringArray createList);
   AssetFactory(const AssetFactory& af);
   AssetFactory& operator=(const AssetFactory& af);
   
   virtual SpacePoint*      CreateSpacePoint(const std::string &ofType,
                                             const std::string &withName = "");        

};

#endif /*AssetFactory_hpp*/
