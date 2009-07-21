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


//#define DEBUG_OBTYPE_CREATION_INITIALIZATION

GmatObtype::GmatObtype(const std::string withName) :
   Obtype         ("GMATInternal", withName)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Creating a GMATInternal obtype\n");
   #endif
}

GmatObtype::~GmatObtype()
{
}


GmatObtype::GmatObtype(const GmatObtype& ot) :
   Obtype         (ot)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Copying a GMATInternal obtype\n");
   #endif
}


GmatObtype& GmatObtype::operator=(const Obtype& ot)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Assigning one GMATInternal obtype to another\n");
   #endif

   if (this != &ot)
   {

   }

   return *this;
}


GmatBase* GmatObtype::Clone() const
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Cloning a GMATInternal obtype\n");
   #endif

   return new GmatObtype(*this);
}


bool GmatObtype::Initialize()
{
   bool retval = false;
   return retval;
}

bool GmatObtype::AddMeasurement(MeasurementData *md)
{
   bool retval = false;
   return retval;
}

bool GmatObtype::Finalize()
{
   bool retval = false;
   return retval;
}
