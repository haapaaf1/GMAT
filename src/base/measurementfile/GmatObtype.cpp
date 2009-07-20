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


#include "GmatObtype.hpp"
#include "MessageInterface.hpp"


GmatObtype::GmatObtype(const std::string withName) :
   Obtype         ("GMATInternal", withName)
{
   MessageInterface::ShowMessage("Creating a GMATInternal obtype\n");
}

GmatObtype::~GmatObtype()
{
}


GmatObtype::GmatObtype(const GmatObtype& ot) :
   Obtype         (ot)
{
   MessageInterface::ShowMessage("Copying a GMATInternal obtype\n");
}


GmatObtype& GmatObtype::operator=(const Obtype& ot)
{
   MessageInterface::ShowMessage("Assigning one GMATInternal obtype to another\n");

   if (this != &ot)
   {

   }

   return *this;
}


GmatBase* GmatObtype::Clone() const
{
   MessageInterface::ShowMessage("Cloning a GMATInternal obtype\n");
   return new GmatObtype(*this);
}
