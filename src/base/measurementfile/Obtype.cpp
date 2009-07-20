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
//         Based on code by Matthew P. Wilkins, Shafer Corporation
// Created: 2009/07/06
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#include "Obtype.hpp"

Obtype::Obtype(const std::string &obType, const std::string &name) :
   GmatBase          (Gmat::OBTYPE, obType, name)
{
   objectTypes.push_back(Gmat::OBTYPE);
   objectTypeNames.push_back("Obtype");

//   parameterCount = ObtypeParamCount;
}

Obtype::~Obtype()
{
}


Obtype::Obtype(const Obtype& ot) :
   GmatBase       (ot)
{
}


Obtype& Obtype::operator=(const Obtype& ot)
{
   if (this != &ot)
   {

   }

   return *this;
}
