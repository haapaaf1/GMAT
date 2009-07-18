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


#include "ObtypeFactory.hpp"

// Supported Obtypes
#include "GmatObtype.hpp"


ObtypeFactory::ObtypeFactory() :
   Factory     (Gmat::OBTYPE)
{
   if (creatables.empty())
   {
      creatables.push_back("GMATInternal");
   }
}

ObtypeFactory::~ObtypeFactory()
{
}


ObtypeFactory::ObtypeFactory(StringArray createList) :
   Factory        (createList, Gmat::OBTYPE)
{
   if (creatables.empty())
   {
      creatables.push_back("GMATInternal");
   }
}
ObtypeFactory::ObtypeFactory(const ObtypeFactory& fact) :
   Factory        (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("GMATInternal");
   }
}

ObtypeFactory& ObtypeFactory::operator= (const ObtypeFactory& fact)
{
   if (&fact != this)
   {
      Factory::operator=(fact);

      if (creatables.empty())
      {
         creatables.push_back("GMATInternal");
      }
   }

   return *this;
}

Obtype* ObtypeFactory::CreateObtype(const std::string &ofType,
      const std::string &withName)
{
   Obtype *retval = NULL;

   if (ofType == "GMATInternal")
      retval = new GmatObtype(withName);

   return retval;
}
