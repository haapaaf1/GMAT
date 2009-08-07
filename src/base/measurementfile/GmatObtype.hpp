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
#include <fstream>         // Should we have a file specific intermediate class?

class GmatObtype: public Obtype
{
public:
   GmatObtype(const std::string withName = "");
   virtual ~GmatObtype();
   GmatObtype(const GmatObtype& ot);
   GmatObtype& operator=(const GmatObtype& ot);

   GmatBase*         Clone() const;

   virtual bool      Initialize();
   virtual bool      Open(bool forRead = true, bool forWrite= false,
                          bool append = false);
   virtual bool      IsOpen();
   virtual bool      AddMeasurement(MeasurementData *md);
   virtual ObservationData *
                     ReadObservation();
   virtual bool      Close();
   virtual bool      Finalize();

private:
   std::fstream      theStream;
   Integer           epochPrecision;
   Integer           dataPrecision;
   ObservationData   currentObs;
};

#endif /* GMATOBTYPE_HPP_ */
