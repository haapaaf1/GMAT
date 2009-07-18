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

GmatObtype::GmatObtype(const std::string withName) :
   Obtype         ("GMATInternal", withName)
{
   // TODO Auto-generated constructor stub

}

GmatObtype::~GmatObtype()
{
   // TODO Auto-generated destructor stub
}


GmatBase* GmatObtype::Clone() const
{
   return new GmatObtype(*this);
}
