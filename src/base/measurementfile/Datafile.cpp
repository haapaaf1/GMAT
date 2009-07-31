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


#include "Datafile.hpp"
#include "GmatBase.hpp"
#include "MessageInterface.hpp"
#include <sstream>

//#define DEBUG_FILE_WRITE


const std::string Datafile::PARAMETER_TEXT[] =
{
   "Filename",
   "Format"
};

const Gmat::ParameterType Datafile::PARAMETER_TYPE[] =
{
   Gmat::STRING_TYPE,
   Gmat::OBJECT_TYPE
};



Datafile::Datafile(const std::string name) :
   GmatBase          (Gmat::DATASTREAM, "Datafile", name),
   theDatastream     (NULL),
   streamName        ("ObsData.gmd"),
   obsType           ("GMATInternal")
{
   objectTypes.push_back(Gmat::DATASTREAM);
   objectTypeNames.push_back("Datafile");

   parameterCount = DatafileParamCount;
}


Datafile::~Datafile()
{
   if (theDatastream)
      delete theDatastream;
}


Datafile::Datafile(const Datafile& df) :
   GmatBase          (df),
   streamName        (df.streamName),
   obsType           (df.obsType)
{
   if (df.theDatastream != NULL)
      theDatastream = (Obtype*)df.theDatastream->Clone();
   else
      theDatastream = NULL;
}


Datafile& Datafile::operator=(const Datafile& df)
{
   if (this != &df)
   {
      GmatBase::operator=(df);

      streamName = df.streamName;
      obsType    = df.obsType;

      // todo: the stream management here
      if (df.theDatastream)
         theDatastream = (Obtype*)df.theDatastream->Clone();
      else
         theDatastream = NULL;
   }

   return *this;
}


GmatBase* Datafile::Clone() const
{
   return new Datafile(*this);
}

bool Datafile::Initialize()
{
   bool retval = false;

   if (theDatastream)
   {
      retval = theDatastream->Initialize();
   }

   return retval;
}

bool Datafile::Finalize()
{
   bool retval = false;

   if (theDatastream)
   {
      retval = theDatastream->Finalize();
   }

   return retval;
}

std::string Datafile::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < DatafileParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
}

std::string Datafile::GetParameterUnit(const Integer id) const
{
   return GmatBase::GetParameterUnit(id);
}

Integer Datafile::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < DatafileParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

Gmat::ParameterType Datafile::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < DatafileParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

std::string Datafile::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

std::string Datafile::GetStringParameter(const Integer id) const
{
   if (id == ObsType)
      return obsType;

   if (id == StreamName)
      return streamName;

   return GmatBase::GetStringParameter(id);
}

bool Datafile::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == ObsType)
   {
      obsType = value;
      return true;
   }

   if (id == StreamName)
   {
      streamName = value;
      return true;
   }

   return GmatBase::SetStringParameter(id, value);
}

std::string Datafile::GetStringParameter(const Integer id, const Integer index) const
{
   return GmatBase::GetStringParameter(id, index);
}

bool Datafile::SetStringParameter(const Integer id, const std::string &value, const Integer index)
{
   return GmatBase::SetStringParameter(id, value, index);
}

std::string Datafile::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}

bool Datafile::SetStringParameter(const std::string &label, const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}

std::string Datafile::GetStringParameter(const std::string &label, const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}

bool Datafile::SetStringParameter(const std::string &label, const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


bool Datafile::SetStream(Obtype *thisStream)
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("Setting Obtype to a %s object\n",
            thisStream->GetTypeName().c_str());
   #endif

   theDatastream = thisStream;
   return true;
}

bool Datafile::OpenStream(bool simulate)
{
   bool retval = false;

   if (theDatastream)
   {
      theDatastream->SetStreamName(streamName);

      // todo: Currently opens either to simulate or to estimate, but not both
      // at the same time
      if (simulate)
         retval = theDatastream->Open(false, true);
      else
         retval = theDatastream->Open(true, false);
   }

   return retval;
}

void Datafile::WriteMeasurement(MeasurementData* theMeas)
{
   if (theDatastream)
      theDatastream->AddMeasurement(theMeas);
}

MeasurementData* Datafile::ReadMeasurement()
{
   // todo: Currently set for simulation; needs to be implemented for estimation
   return NULL;
}


bool Datafile::CloseStream()
{
   bool retval = false;

   if (theDatastream)
      retval = theDatastream->Close();

   return retval;
}
