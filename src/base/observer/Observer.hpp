//$Id$
//------------------------------------------------------------------------------
//                                 Observer
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 * Definition of the Ground Station class base
 */
//------------------------------------------------------------------------------

#ifndef Observer_hpp
#define Observer_hpp

#include <valarray>
#include "ObserverObject.hpp"
#include "Rvector6.hpp"
#include "ObserverPropState.hpp"
//include "CoordinateSystem.hpp"
//include "CoordinateConverter.hpp"
//include "TimeSystemConverter.hpp"
//include "StateConverter.hpp"
#include <map>

class GMAT_API Observer : public ObserverObject
{
public:
   Observer(const std::string &name, 
      const std::string &typeStr = "Observer");
   Observer(const Observer &a);
   Observer&          operator=(const Observer &a);
   
   // Destructor
   virtual              ~Observer();

   CoordinateSystem*    GetInternalCoordSystem();
   void                 SetInternalCoordSystem(CoordinateSystem *cs);   
   void                 SetState(const Rvector6 &cartState);
   void                 SetState(const std::string &elementType, Real *instate);
   void                 SetState(const Real s1, const Real s2, const Real s3, 
                                 const Real s4, const Real s5, const Real s6);
   
   virtual ObserverPropState&   GetState();
   virtual Rvector6     GetState(std::string rep);
   virtual Rvector6     GetState(Integer rep);
   Rvector6             GetCartesianState();
   
   // inherited from GmatBase
   virtual GmatBase*    Clone(void) const;
   virtual void         Copy(const GmatBase* orig);
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   virtual std::string  GetRefObjectName(const Gmat::ObjectType type) const;
   
   virtual const        ObjectTypeArray& GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObjectName(const Gmat::ObjectType type,
                                         const std::string &name);
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);
   
   // Parameter accessor methods -- overridden from GmatBase
   virtual Integer      GetParameterID(const std::string &str) const;
   
   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;
   
   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         GetRealParameter(const std::string &label) const;
   virtual Real         SetRealParameter(const Integer id, const Real value);
   virtual Real         SetRealParameter(const std::string &label, const Real value);
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id, const std::string &value);
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value);
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value,
                                           const Integer index);
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value,
                                           const Integer index);
   
   const StringArray&   GetStringArrayParameter(const Integer id) const;
   
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual bool         Initialize();
   
   virtual bool         TakeAction(const std::string &action, 
                                   const std::string &actionData = "");
   virtual bool         IsOwnedObject(Integer id) const;
   virtual GmatBase*    GetOwnedObject(Integer whichOne);
   
   
   virtual const std::string&  
                        GetGeneratingString(Gmat::WriteMode mode = Gmat::SCRIPTING,
                                            const std::string &prefix = "",
                                            const std::string &useName = "");
   
   std::string GetEpochString();
   void SetDateFormat(const std::string &dateType);   
   void SetEpoch(const std::string &ep);
   void SetEpoch(const std::string &type, const std::string &ep, Real a1mjd);
   void SetState(const std::string &type, const Rvector6 &cartState);
   void SetAnomaly(const std::string &type, const Anomaly &ta);
   
protected:

   std::map <std::string, std::string> elementLabelMap;
   
   /// State element labels
   StringArray       stateElementLabel;
   /// State element units
   StringArray       stateElementUnits;
   /// Possible state representations
   StringArray       representations;
   
   /// Epoch string, specifying the text form of the epoch
   std::string       scEpochStr;
   /// String specifying the epoch time system (A1, TAI, UTC, or TT)
   std::string       epochSystem;
   /// String specifying the epoch time format (Gregorian or ModJulian)
   std::string       epochFormat;
   /// String specifying the epoch system and format used for scEpochStr (TAIModJulian, etc)
   std::string       epochType;
   std::string       stateType;
   std::string       displayStateType;

  /*
   std::string       anomalyType;
   Anomaly           trueAnomaly;
  */
 
   /// Base coordinate system for the Observer
   CoordinateSystem  *internalCoordSystem;
   /// Coordinate system used for the input and output to the GUI
   CoordinateSystem  *coordinateSystem;
   
   std::string       coordSysName;

  /*
   /// Pointer to the object that manages the attitude of the observer
   Attitude          *attitude;
  */ 
   // for non-internal Observer information
   StateConverter    stateConverter;
   CoordinateConverter coordConverter;
   
   // New constructs needed to preserve interfaces
   Rvector6          rvState;

   // protected methods

   bool              initialDisplay;
   bool              csSet;

   virtual void      WriteParameters(Gmat::WriteMode mode, std::string &prefix, 
                        std::stringstream &stream);
                                
   virtual void      UpdateElementLabels();
   Rvector6            GetStateInRepresentation(std::string rep = "");
   Rvector6            GetStateInRepresentation(Integer rep = CARTESIAN_ID);
   void             SetStateFromRepresentation(std::string rep, Rvector6 &st);
   
   Real                GetElement(const std::string &label);
   bool            SetElement(const std::string &label, const Real &value);
   Integer             LookUpLabel(const std::string &label, std::string &rep);
   Integer             LookUpID(const Integer id, std::string &label, std::string &rep);
   void            BuildElementLabelMap();
};

#endif // Observer_hpp
