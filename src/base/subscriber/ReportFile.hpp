//$Id$
//------------------------------------------------------------------------------
//                                  ReportFile
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: LaMont Ruley
// Created: 2003/10/23
// Modified:  2004/7/16 by Allison Greene to include copy constructor
//                         and operator=
//
/**
 * Definition for the ReportFile class.
 */
//------------------------------------------------------------------------------


#ifndef ReportFile_hpp
#define ReportFile_hpp


#include "Subscriber.hpp"
#include <fstream>

#include "Parameter.hpp"
#include <map>
#include <iostream>
#include <iomanip>

class GMAT_API ReportFile : public Subscriber
{
public:
   ReportFile(const std::string &typeName, const std::string &name,
              const std::string &fileName = "", 
              Parameter *firstParam = NULL);
   
   virtual ~ReportFile(void);
   
   ReportFile(const ReportFile &);
   ReportFile& operator=(const ReportFile&);
   
   // methods for this class
   std::string          GetDefaultFileName();
   std::string          GetPathAndFileName();
   Integer              GetNumParameters();
   bool                 AddParameter(const std::string &paramName, Integer index);
   bool                 AddParameterForTitleOnly(const std::string &paramName);
   bool                 WriteData(WrapperArray dataArray);
   
   // methods inherited from GmatBase
   virtual bool         Initialize();
   
   virtual GmatBase*    Clone(void) const;
   virtual void         Copy(const GmatBase* orig);
   
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");
   
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;
   
   virtual bool         GetBooleanParameter(const Integer id) const;
   virtual bool         SetBooleanParameter(const Integer id,
                                            const bool value);
   virtual bool         GetBooleanParameter(const std::string &label) const;
   virtual bool         SetBooleanParameter(const std::string &label,
                                            const bool value);
   virtual Integer      GetIntegerParameter(const Integer id) const;
   virtual Integer      SetIntegerParameter(const Integer id,
                                            const Integer value);
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   
   virtual std::string  GetOnOffParameter(const Integer id) const;
   virtual bool         SetOnOffParameter(const Integer id, 
                                          const std::string &value);
   virtual std::string  GetOnOffParameter(const std::string &label) const;
   virtual bool         SetOnOffParameter(const std::string &label, 
                                          const std::string &value);
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual bool         HasRefObjectTypeArray();
   virtual const ObjectTypeArray&
                        GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   
   // methods for setting up the items to subscribe
   virtual const StringArray&
                        GetWrapperObjectNameArray();
   
protected:
   /// Name of the output path
   std::string          outputPath;
   /// Name of the report file
   std::string          filename;
   /// Default file name of the report file when it is not set
   std::string          defFileName;
   /// Full file name with path
   std::string          fullPathName;
   /// Precision for output of real data
   Integer              precision;  
   /// Width of column
   Integer              columnWidth;   
   /// Write the headers on the top of the column
   bool                 writeHeaders;
   /// Left justify
   bool                 leftJustify;
   /// Fill right field with 0
   bool                 zeroFill;
   
   /// output data stream
   std::ofstream        dstream;
   std::vector<Parameter*> mParams;
   
   Integer              mNumParams;
   StringArray          mParamNames;
   StringArray          mAllRefObjectNames;
   Integer              lastUsedProvider;
   Real                 mLastReportTime;
   bool                 usedByReport;
   bool                 calledByReport;
   bool                 initial;
   
   virtual bool         OpenReportFile(void);
   void                 ClearParameters();
   void                 WriteHeaders();
   Integer              WriteMatrix(StringArray *output, Integer param,
                                    const Rmatrix &rmat, UnsignedInt &maxRow,
                                    Integer defWidth);
   
   // methods inherited from Subscriber
   virtual bool         Distribute(Integer len);
   virtual bool         Distribute(const Real * dat, Integer len);
   
   enum
   {
      FILENAME = SubscriberParamCount,
      PRECISION,
      ADD,
      WRITE_HEADERS,
      LEFT_JUSTIFY,
      ZERO_FILL,
      COL_WIDTH,
      WRITE_REPORT,
      ReportFileParamCount  /// Count of the parameters for this class
   };

private:

   static const std::string
      PARAMETER_TEXT[ReportFileParamCount - SubscriberParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[ReportFileParamCount - SubscriberParamCount];
   
};


#endif
