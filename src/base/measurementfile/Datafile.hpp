//$Id$
//------------------------------------------------------------------------------
//                                 Datafile
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/07/16
//
/**
 * Definition for the Datafile class used in GMAT measurement simulator and
 * estimators
 */
//------------------------------------------------------------------------------


#ifndef DataFile_hpp
#define DataFile_hpp

#include "GmatBase.hpp"
#include "Obtype.hpp"
#include "MeasurementData.hpp"


/**
 * Datafile is the container class for measurement data streams.
 *
 * The Datafile class provides the interfaces needed to script observation data
 * into GMAT.  Instances of the class identify the type of data stream used and
 * the identifier for that stream.
 */
class Datafile : public GmatBase
{
public:
   Datafile(const std::string name);
   virtual ~Datafile();
   Datafile(const Datafile& df);
   Datafile& operator=(const Datafile& df);

   virtual GmatBase* Clone() const;
   virtual bool Initialize();
   virtual bool Finalize();

   // Access methods derived classes can override
   virtual std::string  GetParameterText(const Integer id) const;
   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const Integer id,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const std::string &label,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);

   virtual bool         SetStream(Obtype *thisStream);
   virtual bool         OpenStream(bool simulate = false);
   virtual void         WriteMeasurement(MeasurementData* theMeas);
   virtual MeasurementData*
                        ReadMeasurement();
   virtual bool         CloseStream();

protected:
   /// The stream for this Datafile
   Obtype         *theDatastream;

   std::string    streamName;
   std::string    obsType;

   enum
   {
       StreamName = GmatBaseParamCount,
       ObsType,
       DatafileParamCount
   };

   // Start with the parameter IDs and associates strings
   static const std::string
                PARAMETER_TEXT[DatafileParamCount - GmatBaseParamCount];
   static const Gmat::ParameterType
                PARAMETER_TYPE[DatafileParamCount - GmatBaseParamCount];
};

#endif /* DataFile_hpp */
