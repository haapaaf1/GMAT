//$Id$
//------------------------------------------------------------------------------
//                                  SpacecraftData
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
 * Declares Spacecraft Spacecraft related data class.
 */
//------------------------------------------------------------------------------
#ifndef SpacecraftData_hpp
#define SpacecraftData_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "RefData.hpp"
#include "Spacecraft.hpp"

class GMAT_API SpacecraftData : public RefData
{
public:

   SpacecraftData();
   SpacecraftData(const SpacecraftData &data);
   SpacecraftData& operator= (const SpacecraftData& right);
   virtual ~SpacecraftData();
   
   Real GetReal(Integer item);
   
   // The inherited methods from RefData
   virtual bool ValidateRefObjects(GmatBase *param);
   virtual const std::string* GetValidObjectList() const;
   
   const static Real BALLISTIC_REAL_UNDEFINED;
   
protected:
   
   // The inherited methods from RefData
   virtual void InitializeRefObjects();
   virtual bool IsValidObjectType(Gmat::ObjectType type);
   
   Spacecraft *mSpacecraft;
   
   enum 
   {
      DRY_MASS,
      DRAG_COEFF,
      REFLECT_COEFF,
      DRAG_AREA,
      SRP_AREA,
      TOTAL_MASS,
   };
   
   enum
   {
      SPACECRAFT = 0,
      SpacecraftDataObjectCount
   };
   
   static const std::string VALID_OBJECT_TYPE_LIST[SpacecraftDataObjectCount];
};

#endif /*SpacecraftData_hpp*/
