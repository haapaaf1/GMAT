//$Header$
//------------------------------------------------------------------------------
//                              Formation
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CI63P
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2004/7/24
//
/**
 * Defines the class used for formations. 
 */
//------------------------------------------------------------------------------


#ifndef FORMATION_HPP
#define FORMATION_HPP

#include "SpaceObject.hpp"

class GMAT_API Formation : public SpaceObject{
public:
   Formation(Gmat::ObjectType typeId, const std::string &typeStr, 
             const std::string &nomme);
   virtual ~Formation();
   Formation(const Formation& orig);
   Formation&           operator=(const Formation& orig);
   
   virtual GmatBase*    Clone() const;
   
   // Access methods derived classes can override
   virtual Integer      GetParameterCount(void) const;
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual bool GetBooleanParameter(const Integer id) const;
   virtual bool GetBooleanParameter(const std::string &label) const;
   virtual bool SetBooleanParameter(const Integer id, const bool value);
   virtual bool SetBooleanParameter(const std::string &label,
                                    const bool value);
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const Integer id,
                                          const Integer index) const;
   virtual Real         SetRealParameter(const Integer id, const Real value);
   virtual Real         SetRealParameter(const std::string &label, 
                                         const Real value);
   virtual bool         SetStringParameter(const Integer id, 
                                          const std::string &value);
   virtual bool         SetStringParameter(const Integer id, 
                                          const std::string &value,
                                          const Integer index);
   virtual const StringArray& 
                        GetStringArrayParameter(const Integer id) const;
                       
//   virtual std::string GetStringParameter(const std::string &label) const;
//   virtual std::string GetStringParameter(const std::string &label,
//                                          const Integer index) const;
//
//   virtual bool        SetStringParameter(const std::string &label, 
//                                          const std::string &value);
//   virtual bool        SetStringParameter(const std::string &label, 
//                                          const std::string &value,
//                                          const Integer index);
//   virtual const StringArray& 
//                       GetStringArrayParameter(const std::string &label)const;

   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                    const std::string &name,
                                    const Integer index);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                    const std::string &name = "");
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                    const std::string &name,
                                    const Integer index);
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);

   virtual void         BuildState();
   virtual void         UpdateElements();
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");

protected:
   /// List of the object names used in the formation
   StringArray                      componentNames;
   /// Pointers to the formation members
   std::vector <SpaceObject *>      components;
   /// Size of the state vector used in propagation
   Integer                          dimension;
//   /// Flag indicating if the PropState is ready for propagation
//   bool                             initialized;

   /// Enumerated parameter IDs   
   enum
   {
      ADDED_SPACECRAFT = SpaceObjectParamCount,
      REMOVED_SPACECRAFT,
      CLEAR_NAMES,
      FormationParamCount
   }; 
   /// Array of supported parameters
   static const std::string PARAMETER_TEXT[FormationParamCount - 
                                           SpaceObjectParamCount];
   /// Array of parameter types
   static const Gmat::ParameterType PARAMETER_TYPE[FormationParamCount - 
                                                   SpaceObjectParamCount];
                                                   
   bool                 ClearSpacecraftList();
   bool                 RemoveSpacecraft(const std::string &name);
};

#endif // FORMATION_HPP
