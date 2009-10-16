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
 * Implements Spacecraft Spacecraft related data class.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "SpacecraftData.hpp"
#include "ParameterException.hpp"
#include "StringUtil.hpp"          // ToString()
#include "MessageInterface.hpp"


const std::string
SpacecraftData::VALID_OBJECT_TYPE_LIST[SpacecraftDataObjectCount] =
{
   "Spacecraft"
}; 

const Real SpacecraftData::BALLISTIC_REAL_UNDEFINED = -9876543210.1234;

//------------------------------------------------------------------------------
// SpacecraftData()
//------------------------------------------------------------------------------
SpacecraftData::SpacecraftData(const std::string &name)
   : RefData(name)
{
   mSpacecraft = NULL;
}


//------------------------------------------------------------------------------
// SpacecraftData(const SpacecraftData &copy)
//------------------------------------------------------------------------------
SpacecraftData::SpacecraftData(const SpacecraftData &copy)
   : RefData(copy)
{
   mSpacecraft = copy.mSpacecraft;
}


//------------------------------------------------------------------------------
// SpacecraftData& operator= (const SpacecraftData& right)
//------------------------------------------------------------------------------
SpacecraftData& SpacecraftData::operator= (const SpacecraftData& right)
{
   if (this == &right)
      return *this;
   
   RefData::operator=(right);
   
   mSpacecraft = right.mSpacecraft;

   return *this;
}


//------------------------------------------------------------------------------
// ~SpacecraftData()
//------------------------------------------------------------------------------
SpacecraftData::~SpacecraftData()
{
}


//------------------------------------------------------------------------------
// Real GetReal(Integer item)
//------------------------------------------------------------------------------
/**
 * Retrives Spacecraft element.
 */
//------------------------------------------------------------------------------
Real SpacecraftData::GetReal(Integer item)
{
   if (mSpacecraft == NULL)
      InitializeRefObjects();
   
   switch (item)
   {
   case DRY_MASS:
      return mSpacecraft->GetRealParameter("DryMass");
   case DRAG_COEFF:
      return mSpacecraft->GetRealParameter("Cd");
   case REFLECT_COEFF:
      return mSpacecraft->GetRealParameter("Cr");
   case DRAG_AREA:
      return mSpacecraft->GetRealParameter("DragArea");
   case SRP_AREA:      
      return mSpacecraft->GetRealParameter("SRPArea");
   case TOTAL_MASS:
      return mSpacecraft->GetRealParameter("TotalMass");
   
   // for Spacecraft owned FuelTank
   case FUEL_MASS:
      return GetOwnedObjectProperty(Gmat::FUEL_TANK, "FuelMass");
   case PRESSURE:
      return GetOwnedObjectProperty(Gmat::FUEL_TANK, "Pressure");
   case TEMPERATURE:
      return GetOwnedObjectProperty(Gmat::FUEL_TANK, "Temperature");
   case VOLUME:
      return GetOwnedObjectProperty(Gmat::FUEL_TANK, "Volume");
   case FUEL_DENSITY:
      return GetOwnedObjectProperty(Gmat::FUEL_TANK, "FuelDensity");
      
   // for Spacecraft owned Thruster
   case DUTY_CYCLE:
      return GetOwnedObjectProperty(Gmat::THRUSTER, "DutyCycle");
   case THRUSTER_SCALE_FACTOR:
      return GetOwnedObjectProperty(Gmat::THRUSTER, "ThrustScaleFactor");
   case GRAVITATIONAL_ACCEL:
      return GetOwnedObjectProperty(Gmat::THRUSTER, "GravitationalAccel");
   default:
      // otherwise, there is an error   
      throw ParameterException
         ("SpacecraftData::GetReal() Unknown parameter id: " +
          GmatStringUtil::ToString(item));
   }
}

//-------------------------------------
// Inherited methods from RefData
//-------------------------------------

//------------------------------------------------------------------------------
// virtual const std::string* GetValidObjectList() const
//------------------------------------------------------------------------------
const std::string* SpacecraftData::GetValidObjectList() const
{
   return VALID_OBJECT_TYPE_LIST;
}


//------------------------------------------------------------------------------
// bool ValidateRefObjects(GmatBase *param)
//------------------------------------------------------------------------------
/**
 * Validates reference objects for given parameter
 */
//------------------------------------------------------------------------------
bool SpacecraftData::ValidateRefObjects(GmatBase *param)
{
   int objCount = 0;
   for (int i=0; i<SpacecraftDataObjectCount; i++)
   {
      if (HasObjectType(VALID_OBJECT_TYPE_LIST[i]))
         objCount++;
   }

   if (objCount == SpacecraftDataObjectCount)
      return true;
   else
      return false;
}

//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// virtual void InitializeRefObjects()
//------------------------------------------------------------------------------
void SpacecraftData::InitializeRefObjects()
{
   #if DEBUG_BALLISTICDATA_INIT
   MessageInterface::ShowMessage
      ("SpacecraftData::InitializeRefObjects() entered.\n");
   #endif
   
   mSpacecraft = (Spacecraft*)FindFirstObject(VALID_OBJECT_TYPE_LIST[SPACECRAFT]);
   
   if (mSpacecraft == NULL)
   {
      // Just write a message since Parameters in GmatFunction may not have ref. object
      // set until execution
      #if DEBUG_BALLISTICDATA_INIT
      MessageInterface::ShowMessage
         ("SpacecraftData::InitializeRefObjects() Cannot find Spacecraft object.\n");
      #endif
      
      //throw ParameterException
      //   ("SpacecraftData::InitializeRefObjects() Cannot find Spacecraft object.\n");
   }
   
   #if DEBUG_BALLISTICDATA_INIT
   MessageInterface::ShowMessage
      ("SpacecraftData::InitializeRefObjects() mSpacecraft=%s\n",
       mSpacecraft->GetName().c_str())
   #endif
}


//------------------------------------------------------------------------------
// virtual bool IsValidObjectType(Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Checks reference object type.
 *
 * @return return true if object is valid object, false otherwise
 */
//------------------------------------------------------------------------------
bool SpacecraftData::IsValidObjectType(Gmat::ObjectType type)
{
   for (int i=0; i<SpacecraftDataObjectCount; i++)
   {
      if (GmatBase::GetObjectTypeString(type) == VALID_OBJECT_TYPE_LIST[i])
         return true;
   }
   
   return false;

}


//------------------------------------------------------------------------------
// Real GetOwnedObjectProperty(Gmat::ObjectType objType, const std::string &propName)
//------------------------------------------------------------------------------
Real SpacecraftData::GetOwnedObjectProperty(Gmat::ObjectType objType,
                                            const std::string &propName)
{
   std::string type, owner, dep;
   GmatStringUtil::ParseParameter(mName, type, owner, dep);

   #ifdef DEBUG_SC_OWNED_OBJ
   MessageInterface::ShowMessage
      ("SpacecraftData::GetOwnedObjectProperty() name='%s', type='%s', owner='%s', "
       "dep='%s',\n", mName.c_str(), type.c_str(), owner.c_str(), dep.c_str());
   #endif
   
   GmatBase *ownedObj = mSpacecraft->GetRefObject(objType, dep);
   if (ownedObj == NULL)
   {
      ParameterException pe;
      pe.SetDetails("SpacecraftData::GetOwnedObjectProperty() %s \"%s\" is not "
                    "attached to Spacecraft \"%s\"",
                    GmatBase::GetObjectTypeString(objType).c_str(), dep.c_str(),
                    mSpacecraft->GetName().c_str());
      throw pe;
   }
   else
      return ownedObj->GetRealParameter(propName);
}

