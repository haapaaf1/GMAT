//$Id$
//------------------------------------------------------------------------------
//                           DataInterface
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under the FDSS 
// contract, Task Order 28
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: May 2, 2013
/**
 * 
 */
//------------------------------------------------------------------------------

#ifndef DataInterface_hpp
#define DataInterface_hpp

#include "DataInterfaceDefs.hpp"
#include "Interface.hpp"
#include "DataReader.hpp"


class DATAINTERFACE_API DataInterface: public Interface
{
public:
   DataInterface(const std::string &type, const std::string &name);
   virtual ~DataInterface();
   DataInterface(const DataInterface& di);
   DataInterface& operator=(const DataInterface& di);

   // Access methods derived classes can override
   virtual std::string  GetParameterText(const Integer id) const;
//   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;

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

   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id,
                                                const Integer index) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label,
                                                const Integer index) const;

   const StringArray& GetSupportedFieldNames() const;

   virtual Integer      Open(const std::string &name = "");
   virtual bool         LoadData() = 0;
   virtual Integer      Close(const std::string &name = "");

protected:
   /// Format identifier for the Reader this interface uses
   std::string readerFormat;
   /// The reader that this interface uses
   DataReader *theReader;

   /// Parameter IDs
   enum
   {
      FORMAT = InterfaceParamCount,
      SELECTED_FIELD_NAMES,
      SUPPORTED_FIELD_NAMES,        // Read only list of parms
      DataInterfaceParamCount,
   };

   /// Spacecraft parameter types
   static const Gmat::ParameterType PARAMETER_TYPE[DataInterfaceParamCount - InterfaceParamCount];
   /// Spacecraft parameter labels
   static const std::string PARAMETER_LABEL[DataInterfaceParamCount - InterfaceParamCount];
};

#endif /* DataInterface_hpp */
