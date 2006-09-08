//$Header$
//------------------------------------------------------------------------------
//                                  AttitudeParameters
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Daniel Hunter
// Created: 2006/6/26
//
/**
 * Implements Attitude related parameter classes.
 *    Quat1, Quat2, Quat3, Quat4
 */
//------------------------------------------------------------------------------

#include "AttitudeParameters.hpp"
#include "ColorTypes.hpp"

//------------------------------------------------------------------------------
// Quat1(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Quat1::Quat1(const std::string &name, GmatBase *obj)
   : AttitudeReal(name, "Quat1", obj, "Quaternion 1", "")
{
   mColor = GmatColor::RED32;
}


//------------------------------------------------------------------------------
// Quat1(const Quat1 &copy)
//------------------------------------------------------------------------------
Quat1::Quat1(const Quat1 &copy)
   : AttitudeReal(copy)
{
}


//------------------------------------------------------------------------------
// Quat1& operator=(const Quat1 &right)
//------------------------------------------------------------------------------
Quat1& Quat1::operator=(const Quat1 &right)
{
   if (this != &right)
      AttitudeReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Quat1()
//------------------------------------------------------------------------------
Quat1::~Quat1()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Quat1::Evaluate()
{
   mRealValue = AttitudeData::GetAttitudeReal(QUAT1);
    
   if (mRealValue == AttitudeData::ATTITUDE_REAL_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Quat1::Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Quat1::Clone(void) const
{
   return new Quat1(*this);
}


//------------------------------------------------------------------------------
// Quat2(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Quat2::Quat2(const std::string &name, GmatBase *obj)
   : AttitudeReal(name, "Quat2", obj, "Quaternion 2", "")
{
   mColor = GmatColor::YELLOW32;
}


//------------------------------------------------------------------------------
// Quat2(const Quat2 &copy)
//------------------------------------------------------------------------------
Quat2::Quat2(const Quat2 &copy)
   : AttitudeReal(copy)
{
}


//------------------------------------------------------------------------------
// Quat2& operator=(const Quat2 &right)
//------------------------------------------------------------------------------
Quat2& Quat2::operator=(const Quat2 &right)
{
   if (this != &right)
      AttitudeReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Quat2()
//------------------------------------------------------------------------------
Quat2::~Quat2()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Quat2::Evaluate()
{
   mRealValue = AttitudeData::GetAttitudeReal(QUAT2);
    
   if (mRealValue == AttitudeData::ATTITUDE_REAL_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Quat2::Clone(void) const
{
   return new Quat2(*this);
}


//------------------------------------------------------------------------------
// Quat3(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Quat3::Quat3(const std::string &name, GmatBase *obj)
   : AttitudeReal(name, "Quat3", obj, "Quaternion 3", "")
{
   mColor = GmatColor::BLUE32;
}


//------------------------------------------------------------------------------
// Quat3(const Quat3 &copy)
//------------------------------------------------------------------------------
Quat3::Quat3(const Quat3 &copy)
   : AttitudeReal(copy)
{
}


//------------------------------------------------------------------------------
// Quat3& operator=(const Quat3 &right)
//------------------------------------------------------------------------------
Quat3& Quat3::operator=(const Quat3 &right)
{
   if (this != &right)
      AttitudeReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Quat3()
//------------------------------------------------------------------------------
Quat3::~Quat3()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Quat3::Evaluate()
{
   mRealValue = AttitudeData::GetAttitudeReal(QUAT3);
    
   if (mRealValue == AttitudeData::ATTITUDE_REAL_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Quat3::Clone(void) const
{
   return new Quat3(*this);
}


//------------------------------------------------------------------------------
// Quat4::Quat4(const std::string &name, GmatBase *obj)
//------------------------------------------------------------------------------
Quat4::Quat4(const std::string &name, GmatBase *obj)
   : AttitudeReal(name, "Quat4", obj, "Quaternion 4", "")
{
   mColor = GmatColor::GREEN32;
}


//------------------------------------------------------------------------------
// Quat4(const Quat4 &copy)
//------------------------------------------------------------------------------
Quat4::Quat4(const Quat4 &copy)
   : AttitudeReal(copy)
{
}


//------------------------------------------------------------------------------
// Quat4& operator=(const Quat4 &right)
//------------------------------------------------------------------------------
Quat4& Quat4::operator=(const Quat4 &right)
{
   if (this != &right)
      AttitudeReal::operator=(right);

   return *this;
}


//------------------------------------------------------------------------------
// ~Quat4()
//------------------------------------------------------------------------------
Quat4::~Quat4()
{
}


//------------------------------------------------------------------------------
// bool Evaluate()
//------------------------------------------------------------------------------
bool Quat4::Evaluate()
{
   mRealValue = AttitudeData::GetAttitudeReal(QUAT4);
    
   if (mRealValue == AttitudeData::ATTITUDE_REAL_UNDEFINED)
      return false;
   else
      return true;
}


//------------------------------------------------------------------------------
// GmatBase* Clone(void) const
//------------------------------------------------------------------------------
GmatBase* Quat4::Clone(void) const
{
   return new Quat4(*this);
}
