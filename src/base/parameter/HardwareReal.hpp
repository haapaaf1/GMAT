//$Id$
//------------------------------------------------------------------------------
//                                  HardwareReal
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc.
//
// Author: Linda Jun
// Created: 2009.03.20
//
/**
 * Declares BallisticMass real data class.
 */
//------------------------------------------------------------------------------
#ifndef HardwareReal_hpp
#define HardwareReal_hpp

#include "gmatdefs.hpp"
#include "RealVar.hpp"
#include "SpacecraftData.hpp"


class GMAT_API HardwareReal : public RealVar, public SpacecraftData
{
public:
   
   HardwareReal(const std::string &name, const std::string &typeStr, 
                GmatBase *obj, const std::string &desc,
                const std::string &unit);
   HardwareReal(const HardwareReal &copy);
   HardwareReal& operator=(const HardwareReal &right);
   virtual ~HardwareReal();
   
   // methods inherited from Parameter
   virtual Real EvaluateReal();
   
   virtual Integer GetNumRefObjects() const;
   virtual bool AddRefObject(GmatBase*obj, bool replaceName = false);
   virtual bool Validate();
   virtual bool Initialize();
   
   // methods inherited from GmatBase
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   virtual std::string  GetRefObjectName(const Gmat::ObjectType type) const;
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObjectName(const Gmat::ObjectType type,
                                         const std::string &name);
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
protected:

};


#endif /*HardwareReal_hpp*/