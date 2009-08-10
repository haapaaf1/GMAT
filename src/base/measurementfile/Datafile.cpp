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
//#define DEBUG_OBSERVATION_READ


//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

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



//------------------------------------------------------------------------------
// Datafile(const std::string name)
//------------------------------------------------------------------------------
/**
 * Constructor for Datafile objects
 *
 * @param name The name of the object
 */
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// ~Datafile()
//------------------------------------------------------------------------------
/**
 * Datafile destructor
 */
//------------------------------------------------------------------------------
Datafile::~Datafile()
{
   if (theDatastream)
      delete theDatastream;
}


//------------------------------------------------------------------------------
// Datafile(const Datafile& df)
//------------------------------------------------------------------------------
/**
 * Copy constructor for a Datafile
 *
 * @param df The Datafile object that provides data for the new one
 */
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// Datafile& operator=(const Datafile& df)
//------------------------------------------------------------------------------
/**
 * Datafile assignment operator
 *
 * @param df The Datafile object that provides data for the this one
 *
 * @return This object, configured to match df
 */
//------------------------------------------------------------------------------
Datafile& Datafile::operator=(const Datafile& df)
{
   if (this != &df)
   {
      GmatBase::operator=(df);

      streamName = df.streamName;
      obsType    = df.obsType;

      if (df.theDatastream)
         theDatastream = (Obtype*)df.theDatastream->Clone();
      else
         theDatastream = NULL;
   }

   return *this;
}


//------------------------------------------------------------------------------
// GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Clone method for Datafiles
 *
 * @return A clone of this object.
 */
//------------------------------------------------------------------------------
GmatBase* Datafile::Clone() const
{
   return new Datafile(*this);
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Code fired in the Sandbox when the Sandbox initializes objects prior to a run
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::Initialize()
{
   bool retval = false;

   if (theDatastream)
   {
      retval = theDatastream->Initialize();
   }

   return retval;
}


//------------------------------------------------------------------------------
// bool Finalize()
//------------------------------------------------------------------------------
/**
 * Code that executes after a run completes
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::Finalize()
{
   bool retval = false;

   if (theDatastream)
   {
      retval = theDatastream->Finalize();
   }

   return retval;
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves the text string used to script a Datafile property
 *
 * @param id The ID of the property
 *
 * @return The string
 */
//------------------------------------------------------------------------------
std::string Datafile::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < DatafileParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterUnit(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves the units used for a property
 *
 * @param id The ID of the property
 *
 * @return The text string specifying the property's units
 */
//------------------------------------------------------------------------------
std::string Datafile::GetParameterUnit(const Integer id) const
{
   return GmatBase::GetParameterUnit(id);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Retrieves the ID associated with a scripted property string
 *
 * @param str The scripted string used for the property
 *
 * @return The associated ID
 */
//------------------------------------------------------------------------------
Integer Datafile::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < DatafileParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves the parameter type for a Datafile property
 *
 * @param id The ID of the property
 *
 * @return The ParameterType of the property
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Datafile::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < DatafileParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves a string describing the type of a property
 *
 * @param id The ID of the property
 *
 * @return The text description of the property type
 */
//------------------------------------------------------------------------------
std::string Datafile::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves a string property of a Datafile
 *
 * @param id The ID of the property
 *
 * @return The property value
 */
//------------------------------------------------------------------------------
std::string Datafile::GetStringParameter(const Integer id) const
{
   if (id == ObsType)
      return obsType;

   if (id == StreamName)
      return streamName;

   return GmatBase::GetStringParameter(id);
}

//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets a string property
 *
 * @param id The ID of the property
 * @param value The new value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves a string property of a Datafile contained in an array
 *
 * @note This method is provided to keep the base class version visible to the
 *       compiler.
 *
 * @param id The ID of the property
 * @param index The array index for the property
 *
 * @return The property value
 */
//------------------------------------------------------------------------------
std::string Datafile::GetStringParameter(const Integer id,
      const Integer index) const
{
   return GmatBase::GetStringParameter(id, index);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value,
//                         const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets a string property of a Datafile contained in an array
 *
 * @note This method is provided to keep the base class version visible to the
 *       compiler.
 *
 * @param id The ID of the property
 * @param value The new property value
 * @param index The array index for the property
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::SetStringParameter(const Integer id, const std::string &value,
      const Integer index)
{
   return GmatBase::SetStringParameter(id, value, index);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves a string property of a Datafile
 *
 * @param label The text description of the property
 *
 * @return The property value
 */
//------------------------------------------------------------------------------
std::string Datafile::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets a string property
 *
 * @param label The text description of the property
 * @param value The new value
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::SetStringParameter(const std::string &label,
      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label,
//                                const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieves a string property of a Datafile contained in an array
 *
 * @note This method is provided to keep the base class version visible to the
 *       compiler.
 *
 * @param label The text description of the property
 * @param index The array index for the property
 *
 * @return The property value
 */
//------------------------------------------------------------------------------
std::string Datafile::GetStringParameter(const std::string &label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value,
//                         const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets a string property of a Datafile contained in an array
 *
 * @note This method is provided to keep the base class version visible to the
 *       compiler.
 *
 * @param label The text description of the property
 * @param value The new property value
 * @param index The array index for the property
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::SetStringParameter(const std::string &label,
      const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// bool SetStream(Obtype *thisStream)
//------------------------------------------------------------------------------
/**
 * Sets the data stream used for the measurement data
 *
 * @param thisStream The Obtype that provides the stream interfaces
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::SetStream(Obtype *thisStream)
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("Setting Obtype to a %s object\n",
            thisStream->GetTypeName().c_str());
   #endif

   bool retval = false;

   if (thisStream->IsOfType(Gmat::OBTYPE))
   {
      theDatastream = thisStream;
      retval = true;
   }
   return retval;
}


//------------------------------------------------------------------------------
// bool OpenStream(bool simulate)
//------------------------------------------------------------------------------
/**
 * Opens the data stream used for the measurement data
 *
 * @param simulate A flag indicating if the stream should be opened to receive
 *                 simulated data
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::OpenStream(bool simulate)
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
            "Entered Datafile::OpenStream(%s)\n",
            (simulate ? "true" : "false"));
   #endif

   bool retval = false;

   if (theDatastream)
   {
      theDatastream->SetStreamName(streamName);

      // todo: Currently opens either to simulate or to estimate, but not both
      // at the same time.
      if (simulate)
         retval = theDatastream->Open(false, true);
      else
         retval = theDatastream->Open(true, false);
   }

   return retval;
}

//------------------------------------------------------------------------------
// bool IsOpen()
//------------------------------------------------------------------------------
/**
 * Reports the status of a datastream
 *
 * @return true if the stream is open, false if not
 */
//------------------------------------------------------------------------------
bool Datafile::IsOpen()
{
   if (theDatastream)
      return theDatastream->IsOpen();

   return false;
}

//------------------------------------------------------------------------------
// void WriteMeasurement(MeasurementData* theMeas)
//------------------------------------------------------------------------------
/**
 * Sends a measurement to a data stream so it can be written
 *
 * This method is used during simulation to pass a calculated measurement to
 * the measurement stream.
 *
 * @param theMeas The measurement that needs to be written
 */
//------------------------------------------------------------------------------
void Datafile::WriteMeasurement(MeasurementData* theMeas)
{
   if (theDatastream)
      theDatastream->AddMeasurement(theMeas);
}


//------------------------------------------------------------------------------
// MeasurementData* ReadMeasurement()
//------------------------------------------------------------------------------
/**
 * Retrieves an observation from a data stream so it can be processed
 *
 * This method is used during estimation to retrieve the measurement
 * observations from the measurement stream.
 *
 * @return The measurement observation from the file, or NULL if no more
 *         observations are available
 */
//------------------------------------------------------------------------------
ObservationData* Datafile::ReadObservation()
{
   ObservationData *theObs = NULL;
   if (theDatastream)
   {
      theObs = theDatastream->ReadObservation();

      #ifdef DEBUG_OBSERVATION_READ
         if (theObs)
         {
            MessageInterface::ShowMessage("Observation:\n");
            MessageInterface::ShowMessage("   Epoch:          %.12lf\n",
                  theObs->epoch);
            MessageInterface::ShowMessage("   Type:           %s\n",
                  theObs->typeName.c_str());
            MessageInterface::ShowMessage("   TypeID:         %d\n",
                  theObs->type);
            for (UnsignedInt i = 0; i < theObs->participantIDs.size(); ++i)
               MessageInterface::ShowMessage("   Participant %d: %s\n", i,
                     theObs->participantIDs[i].c_str());
            for (UnsignedInt i = 0; i < theObs->value.size(); ++i)
               MessageInterface::ShowMessage("   Value[%d]:         %.12lf\n",
                     i, theObs->value[i]);

            // Now the extra data
            for (UnsignedInt i = 0; i < theObs->extraData.size(); ++i)
            {
               MessageInterface::ShowMessage("   ExtraData[%d]:   %s (type %d)"
                     " = %s\n", i, theObs->extraDataDescriptions[i].c_str(),
                     theObs->extraTypes[i], theObs->extraData[i].c_str());
            }
         }
         else
            MessageInterface::ShowMessage("*** Reached End of Observations\n");
      #endif
   }

   return theObs;
}


//------------------------------------------------------------------------------
// bool CloseStream()
//------------------------------------------------------------------------------
/**
 * Closes the data stream
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Datafile::CloseStream()
{
   bool retval = false;

   if (theDatastream)
      retval = theDatastream->Close();

   return retval;
}
