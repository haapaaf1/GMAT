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


#ifndef GMATOBTYPE_HPP_
#define GMATOBTYPE_HPP_

/// Descriptor here
#include "Obtype.hpp"
#include <fstream>

class GmatObtype: public Obtype
{
public:
   GmatObtype(const std::string withName = "");
   virtual ~GmatObtype();
   GmatObtype(const GmatObtype& ot);
   GmatObtype& operator=(const Obtype& ot);

   GmatBase*         Clone() const;

private:
   std::fstream      theStream;
};

#endif /* GMATOBTYPE_HPP_ */
