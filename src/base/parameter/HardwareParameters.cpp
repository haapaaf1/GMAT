//$Id$
//------------------------------------------------------------------------------
//                            File: HardwareParameters
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
 * Implements Hardware related parameter classes.
 *    FuelTak : FuelMass, Pressure, Temperature, FuelVolume, FuelDensity
 *    Thruster: DutyCycle, ThrustScaleFactor, GravitationalAccel
 */
//------------------------------------------------------------------------------

#include "HardwareParameters.hpp"
#include "ColorTypes.hpp"


//==============================================================================
//                              FuelMass
//==============================================================================
/**
 * Implements FuelMass class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FuelMass(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
FuelMass::FuelMass(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "FuelMass", obj, "Fuel Mass", "")
{
   mColor = GmatColor::RED32;
}


//------------------------------------------------------------------------------
// FuelMass(const FuelMass &copy)
//------------------------------------------------------------------------------
FuelMass::FuelMass(const FuelMass &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// FuelMass& operator=(const FuelMass &right)
//------------------------------------------------------------------------------
FuelMass& FuelMass::operator=(const FuelMass &right)
{
   if (this != &right)
      HardwareReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~FuelMass()
//------------------------------------------------------------------------------
FuelMass::~FuelMass()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool FuelMass::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(FUEL_MASS);
   
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* FuelMass::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* FuelMass::Clone(void) const
{
   return new FuelMass(*this);
}


//==============================================================================
//                              Pressure
//==============================================================================
/**
 * Implements Pressure class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Pressure(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Pressure::Pressure(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "Pressure", obj, "Pressure", "")
{
   mColor = GmatColor::YELLOW32;
}


//------------------------------------------------------------------------------
// Pressure(const Pressure &copy)
//------------------------------------------------------------------------------
Pressure::Pressure(const Pressure &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// Pressure& operator=(const Pressure &right)
//------------------------------------------------------------------------------
Pressure& Pressure::operator=(const Pressure &right)
{
   if (this != &right)
      HardwareReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Pressure()
//------------------------------------------------------------------------------
Pressure::~Pressure()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Pressure::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(PRESSURE);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Pressure::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Pressure::Clone(void) const
{
   return new Pressure(*this);
}


//==============================================================================
//                              Temperature
//==============================================================================
/**
 * Implements Temperature class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Temperature(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Temperature::Temperature(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "Temperature", obj, "Temperature", "")
{
   mColor = GmatColor::BLUE32;
}


//------------------------------------------------------------------------------
// Temperature(const Temperature &copy)
//------------------------------------------------------------------------------
Temperature::Temperature(const Temperature &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// Temperature& operator=(const Temperature &right)
//------------------------------------------------------------------------------
Temperature& Temperature::operator=(const Temperature &right)
{
   if (this != &right)
      HardwareReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Temperature()
//------------------------------------------------------------------------------
Temperature::~Temperature()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Temperature::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(TEMPERATURE);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Temperature::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Temperature::Clone(void) const
{
   return new Temperature(*this);
}


//==============================================================================
//                              Volume
//==============================================================================
/**
 * Implements Volume class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Volume(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Volume::Volume(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "Volume", obj, "Volume", "")
{
   mColor = GmatColor::GREEN32;
}


//------------------------------------------------------------------------------
// Volume(const Volume &copy)
//------------------------------------------------------------------------------
Volume::Volume(const Volume &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// Volume& operator=(const Volume &right)
//------------------------------------------------------------------------------
Volume& Volume::operator=(const Volume &right)
{
   if (this != &right)
      HardwareReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Volume()
//------------------------------------------------------------------------------
Volume::~Volume()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Volume::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(VOLUME);
    
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Volume::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Volume::Clone(void) const
{
   return new Volume(*this);
}


//==============================================================================
//                              FuelDensity
//==============================================================================
/**
 * Implements FuelDensity class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FuelDensity(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
FuelDensity::FuelDensity(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "FuelDensity", obj, "FuelDensity", "")
{
   mColor = GmatColor::ORANGE32;
}


//------------------------------------------------------------------------------
// FuelDensity(const FuelDensity &copy)
//------------------------------------------------------------------------------
FuelDensity::FuelDensity(const FuelDensity &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// FuelDensity& operator=(const FuelDensity &right)
//------------------------------------------------------------------------------
FuelDensity& FuelDensity::operator=(const FuelDensity &right)
{
   if (this != &right)
      HardwareReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~FuelDensity()
//------------------------------------------------------------------------------
FuelDensity::~FuelDensity()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool FuelDensity::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(FUEL_DENSITY);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* FuelDensity::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* FuelDensity::Clone(void) const
{
   return new FuelDensity(*this);
}


//==============================================================================
//                              DutyCycle
//==============================================================================
/**
 * Implements DutyCycle class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DutyCycle(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
DutyCycle::DutyCycle(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "DutyCycle", obj, "DutyCycle", "")
{
   mColor = GmatColor::CHESTNUT; 
}


//------------------------------------------------------------------------------
// DutyCycle(const DutyCycle &copy)
//------------------------------------------------------------------------------
DutyCycle::DutyCycle(const DutyCycle &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// DutyCycle& operator=(const DutyCycle &right)
//------------------------------------------------------------------------------
DutyCycle& DutyCycle::operator=(const DutyCycle &right)
{
   if (this != &right)
      HardwareReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~DutyCycle()
//------------------------------------------------------------------------------
DutyCycle::~DutyCycle()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool DutyCycle::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(DUTY_CYCLE);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* DutyCycle::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* DutyCycle::Clone(void) const
{
   return new DutyCycle(*this);
}


//==============================================================================
//                              ThrustScaleFactor
//==============================================================================
/**
 * Implements ThrustScaleFactor class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// ThrustScaleFactor(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
ThrustScaleFactor::ThrustScaleFactor(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "ThrustScaleFactor", obj, "ThrustScaleFactor", "")
{
   mColor = GmatColor::CHESTNUT; 
}


//------------------------------------------------------------------------------
// ThrustScaleFactor(const ThrustScaleFactor &copy)
//------------------------------------------------------------------------------
ThrustScaleFactor::ThrustScaleFactor(const ThrustScaleFactor &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// ThrustScaleFactor& operator=(const ThrustScaleFactor &right)
//------------------------------------------------------------------------------
ThrustScaleFactor& ThrustScaleFactor::operator=(const ThrustScaleFactor &right)
{
   if (this != &right)
      HardwareReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~ThrustScaleFactor()
//------------------------------------------------------------------------------
ThrustScaleFactor::~ThrustScaleFactor()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool ThrustScaleFactor::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(THRUSTER_SCALE_FACTOR);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* ThrustScaleFactor::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* ThrustScaleFactor::Clone(void) const
{
   return new ThrustScaleFactor(*this);
}


//==============================================================================
//                              GravitationalAccel
//==============================================================================
/**
 * Implements GravitationalAccel class.
 */
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// GravitationalAccel(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
GravitationalAccel::GravitationalAccel(const std::string &name, GmatBase *obj)
   : HardwareReal(name, "GravitationalAccel", obj, "GravitationalAccel", "")
{
   mColor = GmatColor::CHESTNUT; 
}


//------------------------------------------------------------------------------
// GravitationalAccel(const GravitationalAccel &copy)
//------------------------------------------------------------------------------
GravitationalAccel::GravitationalAccel(const GravitationalAccel &copy)
   : HardwareReal(copy)
{
}


//------------------------------------------------------------------------------
// GravitationalAccel& operator=(const GravitationalAccel &right)
//------------------------------------------------------------------------------
GravitationalAccel& GravitationalAccel::operator=(const GravitationalAccel &right)
{
   if (this != &right)
      HardwareReal::operator=(right);
   
   return *this;
}


//------------------------------------------------------------------------------
// ~GravitationalAccel()
//------------------------------------------------------------------------------
GravitationalAccel::~GravitationalAccel()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool GravitationalAccel::Evaluate()
{
   mRealValue = SpacecraftData::GetReal(GRAVITATIONAL_ACCEL);
   
   if (mRealValue == GmatBase::REAL_PARAMETER_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* GravitationalAccel::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* GravitationalAccel::Clone(void) const
{
   return new GravitationalAccel(*this);
}


