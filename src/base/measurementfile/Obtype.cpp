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
#include "MessageInterface.hpp"


Obtype::Obtype(const std::string &obType, const std::string &name) :
   GmatBase          (Gmat::OBTYPE, obType, name),
   streamName        (""),
   header            (""),
   openForRead       (true),
   openForWrite      (false)
{
   objectTypes.push_back(Gmat::OBTYPE);
   objectTypeNames.push_back("Obtype");

//   parameterCount = ObtypeParamCount;
}

Obtype::~Obtype()
{
}


Obtype::Obtype(const Obtype& ot) :
   GmatBase          (ot),
   streamName        (ot.streamName),
   header            (ot.header),
   openForRead       (ot.openForRead),
   openForWrite      (ot.openForWrite)
{
}


Obtype& Obtype::operator=(const Obtype& ot)
{
   if (this != &ot)
   {
      streamName   = ot.streamName;
      header       = ot.header;
      openForRead  = ot.openForRead;
      openForWrite = ot.openForWrite;
   }

   return *this;
}

void Obtype::SetStreamName(std::string name)
{
   streamName = name;
}

bool Obtype::Initialize()
{
   return false;
}

bool Obtype::Open(bool forRead, bool forWrite, bool append)
{
   return false;
}

bool Obtype::IsOpen()
{
   return false;
}

bool Obtype::Close()
{
   return false;
}

bool Obtype::Finalize()
{
   return false;
}
