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
#include "FileManager.hpp"

//#define DEBUG_OBTYPE_CREATION_INITIALIZATION
//#define DEBUG_FILE_WRITE


GmatObtype::GmatObtype(const std::string withName) :
   Obtype         ("GMATInternal", withName),
   epochPrecision (16),
   dataPrecision  (6)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Creating a GMATInternal obtype\n");
   #endif

   header = "\% GMAT Internal Measurement Data File\n\n";
}

GmatObtype::~GmatObtype()
{
}


GmatObtype::GmatObtype(const GmatObtype& ot) :
   Obtype         (ot),
   epochPrecision (ot.epochPrecision),
   dataPrecision  (ot.dataPrecision)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Copying a GMATInternal obtype\n");
   #endif
}


GmatObtype& GmatObtype::operator=(const GmatObtype& ot)
{
   #ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
      MessageInterface::ShowMessage("Assigning one GMATInternal obtype to another\n");
   #endif

   if (this != &ot)
   {
      epochPrecision = ot.epochPrecision;
      dataPrecision  = ot.dataPrecision;
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
#ifdef DEBUG_OBTYPE_CREATION_INITIALIZATION
   MessageInterface::ShowMessage("GmatObtype::Initialize() Executing\n");
#endif

   bool retval = false;

   return retval;
}


bool GmatObtype::Open(bool forRead, bool forWrite, bool append)
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("GmatObtype::Open() Executing\n");
   #endif
   bool retval = false;

   // temporary
   retval = true;
   std::ios_base::openmode mode;

   if (forRead && forWrite)
      mode = std::fstream::in | std::fstream::out;

   else
   {
      if (forRead)
         mode = std::fstream::in;
      if (forWrite)
         mode = std::fstream::out;
   }

   if (append)
      mode = mode | std::fstream::app;

   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("   Opening the stream %s, mode = %d\n",
            streamName.c_str(), mode);
   #endif

   if (streamName != "")
   {
      // todo: Clean up the path for the measurement file
      FileManager *fm = FileManager::Instance();
      std::string outputPath = fm->GetPathname(FileManager::MEASUREMENT_PATH);
      outputPath += streamName;

      theStream.open(outputPath.c_str(), mode);
   }

   retval = theStream.is_open();
   if (retval && forWrite)
      theStream << header;
   return retval;
}

bool GmatObtype::IsOpen()
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("GmatObtype::IsOpen() Executing\n");
   #endif
   return theStream.is_open();
}



bool GmatObtype::AddMeasurement(MeasurementData *md)
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("GmatObtype::AddMeasurement() Executing\n");
   #endif
   bool retval = false;

   std::stringstream dataLine;
   char databuffer[20];
   // char dataSpec[10];

   dataLine.precision(epochPrecision);
   dataLine << md->epoch << "    " << md->typeName
            << "    " << md->type << "    ";
   for (UnsignedInt j = 0; j < md->participantIDs.size(); ++j)
      dataLine << md->participantIDs[j] << "    ";

   // sprintf(dataSpec, "%%%dlf", dataPrecision);
   for (UnsignedInt k = 0; k < md->value.size(); ++k)
   {
      sprintf(databuffer, "%9.5lf", md->value[k]);
      dataLine << databuffer;
      if (k < md->value.size()-1)
         dataLine << "    ";
   }

   theStream << dataLine.str() << "\n";

   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("Datafile::WriteMeasurement: \"%s\"\n",
            dataLine.str().c_str());
   #endif

   // temporary
   retval = true;

   return retval;
}


bool GmatObtype::Close()
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("GmatObtype::Close() Executing\n");
   #endif
   bool retval = false;

   theStream.flush();
   theStream.close();
   retval = !(theStream.is_open());

   return retval;
}


bool GmatObtype::Finalize()
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("GmatObtype::Finalize() Executing\n");
   #endif
   bool retval = true;

   return retval;
}
