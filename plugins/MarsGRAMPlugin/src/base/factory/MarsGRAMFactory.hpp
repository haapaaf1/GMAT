//$Id: MarsGRAMFactory.hpp 9490 2011-04-27 16:28:40Z tdnguye2 $
//------------------------------------------------------------------------------
//                            MarsGRAMFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Tuan Nguyen
// Created: 2011/6/13
//
/**
 *  Declaration code for the MarsGRAMFactory class.
 */
//------------------------------------------------------------------------------
#ifndef MarsGRAMFactory_hpp
#define MarsGRAMFactory_hpp


#include "marsgram_defs.hpp"
#include "AtmosphereFactory.hpp"
#include "AtmosphereModel.hpp"

class MARSGRAM_API MarsGRAMFactory : public AtmosphereFactory
{
public:
   AtmosphereModel* CreateAtmosphereModel(
		   const std::string &ofType,
           const std::string &withName,
           const std::string &forBody);
   
   // default constructor
   MarsGRAMFactory();
   // constructor
   MarsGRAMFactory(StringArray createList);
   // copy constructor
   MarsGRAMFactory(const MarsGRAMFactory& fact);
   // assignment operator
   MarsGRAMFactory& operator=(const MarsGRAMFactory& fact);
   
   virtual ~MarsGRAMFactory();
   
};

#endif // MarsGRAMFactory_hpp
