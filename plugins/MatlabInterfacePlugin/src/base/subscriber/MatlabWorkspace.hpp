//$Id$
//------------------------------------------------------------------------------
//                                  MatlabWorkspace
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/09/08
//
/**
 * Declares MatlabWorkspace class which evaluates parameters to be sent to MATLAB
 * workspace.
 */
//------------------------------------------------------------------------------
#ifndef MatlabWorkspace_hpp
#define MatlabWorkspace_hpp

#include "Subscriber.hpp"

class Parameter;
class MatlabInterface;

class MatlabWorkspace : public Subscriber
{
public:
   MatlabWorkspace(const std::string &name, Parameter *firstParam = NULL);
   virtual ~MatlabWorkspace(void);
   MatlabWorkspace(const MatlabWorkspace &copy);
   MatlabWorkspace& operator=(const MatlabWorkspace &right);
   
   Integer              GetNumParameters();
   bool                 AddParameter(const std::string &paramName, Integer index);
   
   // methods inherited from Subscriber
   virtual bool         Initialize();
   
   // inherited from GmatBase
   virtual GmatBase*    Clone(void) const;
   
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");
   
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual Integer      GetIntegerParameter(const Integer id) const;
   virtual Integer      SetIntegerParameter(const Integer id, const Integer value);
   virtual Integer      GetIntegerParameter(const std::string &label) const;
   virtual Integer      SetIntegerParameter(const std::string &label,
                                            const Integer value);
   
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   
   virtual bool         SetStringParameter(const Integer id, const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);
   
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   
protected:
   Integer mUpdateFrequency;
   Integer mNumParams;
   Integer mDataCount;
   Integer mSendCount;
   
   std::vector<Parameter*> mParams;
   StringArray mParamNames;
   StringArray mAllRefObjectNames;
   
   MatlabInterface *matlabIf;
   
   virtual bool         Distribute(Integer len);
   virtual bool         Distribute(const Real * dat, Integer len);
   
private:
   void ClearParameters();
   
   enum
   {
      ADD = SubscriberParamCount,
      UPDATE_FREQUENCY,
      MatlabWorkspaceParamCount  /// Count of the parameters for this class
   };
   
   static const std::string
      PARAMETER_TEXT[MatlabWorkspaceParamCount - SubscriberParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[MatlabWorkspaceParamCount - SubscriberParamCount];
   
};
#endif
