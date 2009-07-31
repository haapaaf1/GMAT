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


#ifndef Obtype_hpp
#define Obtype_hpp

#include "GmatBase.hpp"
#include "MeasurementData.hpp"


/**
 * The Obtype classes are structures designed to contain observation data --
 * either calculated or observed -- in a single packet designed to be passed
 * between the different consumers of the contained data.  Derived classes are
 * implemented for each data file type that is supported.
 */
class Obtype : public GmatBase
{
public:
   Obtype(const std::string &obType = "NotSet", const std::string &name = "");
   virtual ~Obtype();
   Obtype(const Obtype& ot);
   Obtype&           operator=(const Obtype& ot);

   virtual bool      Initialize();
   virtual bool      Open(bool forRead = true, bool forWrite= false,
                          bool append = false);
   virtual bool      IsOpen();
   virtual bool      AddMeasurement(MeasurementData *md) = 0;
   virtual bool      Close();
   virtual bool      Finalize();

   void              SetStreamName(std::string name);

protected:
   std::string       streamName;
   std::string       header;
   bool              openForRead;
   bool              openForWrite;
};

#endif /* Obtype_hpp */
