//$Id$
//------------------------------------------------------------------------------
//                            File: BallisticMassParameters
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
 * Implements BallisticMass related parameter classes.
 *    DryMass, DragCoeff, ReflectCoeff, DragArea, SRPArea, TotalMass
 */
//------------------------------------------------------------------------------

#include "BallisticMassParameters.hpp"
#include "ColorTypes.hpp"


//==============================================================================
//                              DryMass
//==============================================================================
/**
 * Implements DryMass class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DryMass(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
DryMass::DryMass(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "DryMass", obj, "Dry Mass", "")
{
   mColor = GmatColor::RED32;
}


//------------------------------------------------------------------------------
// DryMass(const DryMass &copy)
//------------------------------------------------------------------------------
DryMass::DryMass(const DryMass &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// DryMass& operator=(const DryMass &right)
//------------------------------------------------------------------------------
DryMass& DryMass::operator=(const DryMass &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~DryMass()
//------------------------------------------------------------------------------
DryMass::~DryMass()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool DryMass::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(DRY_MASS);
   
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* DryMass::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* DryMass::Clone(void) const
{
   return new DryMass(*this);
}


//==============================================================================
//                              DragCoeff
//==============================================================================
/**
 * Implements DragCoeff class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DragCoeff(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
DragCoeff::DragCoeff(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "Cd", obj, "Drag Coefficient", "")
{
   mColor = GmatColor::YELLOW32;
}


//------------------------------------------------------------------------------
// DragCoeff(const DragCoeff &copy)
//------------------------------------------------------------------------------
DragCoeff::DragCoeff(const DragCoeff &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// DragCoeff& operator=(const DragCoeff &right)
//------------------------------------------------------------------------------
DragCoeff& DragCoeff::operator=(const DragCoeff &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~DragCoeff()
//------------------------------------------------------------------------------
DragCoeff::~DragCoeff()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool DragCoeff::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(DRAG_COEFF);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* DragCoeff::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* DragCoeff::Clone(void) const
{
   return new DragCoeff(*this);
}


//==============================================================================
//                              ReflectCoeff
//==============================================================================
/**
 * Implements ReflectCoeff class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// ReflectCoeff(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
ReflectCoeff::ReflectCoeff(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "Cr", obj, "Reflectivity Coefficient", "")
{
   mColor = GmatColor::BLUE32;
}


//------------------------------------------------------------------------------
// ReflectCoeff(const ReflectCoeff &copy)
//------------------------------------------------------------------------------
ReflectCoeff::ReflectCoeff(const ReflectCoeff &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// ReflectCoeff& operator=(const ReflectCoeff &right)
//------------------------------------------------------------------------------
ReflectCoeff& ReflectCoeff::operator=(const ReflectCoeff &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~ReflectCoeff()
//------------------------------------------------------------------------------
ReflectCoeff::~ReflectCoeff()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool ReflectCoeff::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(REFLECT_COEFF);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* ReflectCoeff::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* ReflectCoeff::Clone(void) const
{
   return new ReflectCoeff(*this);
}


//==============================================================================
//                              DragArea
//==============================================================================
/**
 * Implements DragArea class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DragArea(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
DragArea::DragArea(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "DragArea", obj, "Drag Area", "")
{
   mColor = GmatColor::GREEN32;
}


//------------------------------------------------------------------------------
// DragArea(const DragArea &copy)
//------------------------------------------------------------------------------
DragArea::DragArea(const DragArea &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// DragArea& operator=(const DragArea &right)
//------------------------------------------------------------------------------
DragArea& DragArea::operator=(const DragArea &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~DragArea()
//------------------------------------------------------------------------------
DragArea::~DragArea()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool DragArea::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(DRAG_AREA);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* DragArea::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* DragArea::Clone(void) const
{
   return new DragArea(*this);
}


//==============================================================================
//                              SRPArea
//==============================================================================
/**
 * Implements SRPArea class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// SRPArea(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
SRPArea::SRPArea(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "SRPArea", obj, "SRP Area", "")
{
   mColor = GmatColor::ORANGE32;
}


//------------------------------------------------------------------------------
// SRPArea(const SRPArea &copy)
//------------------------------------------------------------------------------
SRPArea::SRPArea(const SRPArea &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// SRPArea& operator=(const SRPArea &right)
//------------------------------------------------------------------------------
SRPArea& SRPArea::operator=(const SRPArea &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~SRPArea()
//------------------------------------------------------------------------------
SRPArea::~SRPArea()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool SRPArea::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(SRP_AREA);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* SRPArea::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* SRPArea::Clone(void) const
{
   return new SRPArea(*this);
}


//==============================================================================
//                              TotalMass
//==============================================================================
/**
 * Implements TotalMass class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TotalMass(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
TotalMass::TotalMass(const std::string &name, GmatBase *obj)
   : BallisticMassReal(name, "TotalMass", obj, "Total Mass", "")
{
   mColor = GmatColor::CHESTNUT; 
}


//------------------------------------------------------------------------------
// TotalMass(const TotalMass &copy)
//------------------------------------------------------------------------------
TotalMass::TotalMass(const TotalMass &copy)
   : BallisticMassReal(copy)
{
}


//------------------------------------------------------------------------------
// TotalMass& operator=(const TotalMass &right)
//------------------------------------------------------------------------------
TotalMass& TotalMass::operator=(const TotalMass &right)
{
   if (this != &right)
      BallisticMassReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~TotalMass()
//------------------------------------------------------------------------------
TotalMass::~TotalMass()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool TotalMass::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(TOTAL_MASS);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* TotalMass::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* TotalMass::Clone(void) const
{
   return new TotalMass(*this);
}

