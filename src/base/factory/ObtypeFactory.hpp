//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#ifndef OBTYPEFACTORY_HPP_
#define OBTYPEFACTORY_HPP_

#include "Factory.hpp"
#include "Obtype.hpp"

/// Descriptor here
class ObtypeFactory : public Factory
{
public:
   ObtypeFactory();
   virtual ~ObtypeFactory();

   ObtypeFactory(StringArray createList);
   ObtypeFactory(const ObtypeFactory& fact);
   ObtypeFactory& operator= (const ObtypeFactory& fact);

   virtual Obtype *CreateObtype(const std::string &ofType,
         const std::string &withName = "");
};

#endif /* OBTYPEFACTORY_HPP_ */
