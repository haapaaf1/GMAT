//$Id:$
//------------------------------------------------------------------------------
//                              SpiceKernelWriter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2009.12.07
//
/**
 * Implementation of the SpiceKernelWriter, which writes SPICE data (kernel) files.
 *
 * This code creates a temporary text file, required in order to include META-Data
 * (commentary) in the SPK file.  The file is deleted from the system after the
 * commentary is added to the SPK file.  The name of this temporary text file
 * takes the form
 *       GMATtmpSPKcmmnt<objName>.txt
 * where <objName> is the name of the object for whom the SPK file is created
 *
 * If the code is unable to create the temporary file (e.g., because of a permission
 * problem), the SPK file will still be generated but will contain no META-data.
 *
 */
//------------------------------------------------------------------------------
#include <stdio.h>
#include <sstream>

#include "SpiceKernelWriter.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "UtilityException.hpp"
#include "RealUtilities.hpp"

//#define DEBUG_SPICE_KERNEL_WRITER

//---------------------------------
// static data
//---------------------------------
// none

//---------------------------------
// public methods
//---------------------------------
SpiceKernelWriter::SpiceKernelWriter() :
   SpiceInterface()
{
}

SpiceKernelWriter::SpiceKernelWriter(const SpiceKernelWriter &copy) :
   SpiceInterface(copy)
{
}

SpiceKernelWriter& SpiceKernelWriter::operator=(const SpiceKernelWriter &copy)
{
   if (&copy != this)
   {
      SpiceInterface::operator=(copy);
   }

   return *this;
}

SpiceKernelWriter::~SpiceKernelWriter()
{
}

