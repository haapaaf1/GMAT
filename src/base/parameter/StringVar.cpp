//$Header$
//------------------------------------------------------------------------------
//                                  StringVar
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2005/1/7
//
/**
 * Implements StringVar class which handles std::string value. The string value
 * is stored in Parameter::mExpr.
 */
//------------------------------------------------------------------------------

#include "StringVar.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_STRINGVAR 1

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// StringVar(const std::string &name = "", const std::string &desc = "")
//------------------------------------------------------------------------------
/**
 * Constructor.
 *
 * @param <name> parameter name
 * @param <desc> parameter description
 *
 */
//------------------------------------------------------------------------------
StringVar::StringVar(const std::string &name, const std::string &desc)
   : Parameter(name, "String", GmatParam::USER_PARAM, NULL, desc, "",
               GmatParam::NO_DEP, Gmat::UNKNOWN_OBJECT, false)
{   
   // Parameter data
   mIsPlottable = false;
}


//------------------------------------------------------------------------------
// StringVar(const StringVar &copy)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <copy> the object being copied.
 */
//------------------------------------------------------------------------------
StringVar::StringVar(const StringVar &copy)
   : Parameter(copy)
{
}


//------------------------------------------------------------------------------
// StringVar& operator= (const StringVar& right)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <right> the object being copied.
 *
 * @return reference to this object
 */
//------------------------------------------------------------------------------
StringVar& StringVar::operator= (const StringVar& right)
{
   if (this != &right)
   {
      Parameter::operator=(right);
   }

   return *this;
}


//------------------------------------------------------------------------------
// ~StringVar()
//------------------------------------------------------------------------------
/**
 * Destructor
 */
//------------------------------------------------------------------------------
StringVar::~StringVar()
{
}


//------------------------------------
// methods inherited from Parameter
//------------------------------------

//------------------------------------------------------------------------------
// bool operator==(const StringVar &right) const
//------------------------------------------------------------------------------
/**
 * Equal operator.
 *
 * @return true if input object's type and name are the same as this object.
 */
//------------------------------------------------------------------------------
bool StringVar::operator==(const StringVar &right) const
{
   return Parameter::operator==(right);
}


//------------------------------------------------------------------------------
// bool operator!=(const StringVar &right) const
//------------------------------------------------------------------------------
/**
 * Not equal operator.
 *
 * @return true if input object's type and name are not the same as this object.
 */
//------------------------------------------------------------------------------
bool StringVar::operator!=(const StringVar &right) const
{
   return Parameter::operator!=(right);
}


//------------------------------------------------------------------------------
// std::string ToString()
//------------------------------------------------------------------------------
/**
 * Retrieves string value of parameter.
 *
 * @return string value of parameter.
 */
//------------------------------------------------------------------------------
std::string StringVar::ToString()
{
   return mExpr;
}


//------------------------------------------------------------------------------
// const std::string& GetString() const
//------------------------------------------------------------------------------
/**
 * Retrieves string value of parameter.
 *
 * @return string value.
 */
//------------------------------------------------------------------------------
const std::string& StringVar::GetString() const
{
   return mExpr;
}


//------------------------------------
// methods inherited from GmatBase
//------------------------------------

//------------------------------------------------------------------------------
// virtual GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Method used to create a copy of the object.
 *
 * @return cloned object pointer.
 */
//------------------------------------------------------------------------------
GmatBase* StringVar::Clone() const
{
   return new StringVar(*this);
}

