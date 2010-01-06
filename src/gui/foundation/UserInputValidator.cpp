//$Id$
//------------------------------------------------------------------------------
//                              UserInputValidator
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/02
//
/**
 * Implements UserInputValidator class.
 */
//------------------------------------------------------------------------------

#include "UserInputValidator.hpp"
#include "GmatPanel.hpp"
#include "GmatDialog.hpp"
#include "MessageInterface.hpp"

//------------------------------------------------------------------------------
// UserInputValidator()
//------------------------------------------------------------------------------
UserInputValidator::UserInputValidator()
{
   mObject = NULL;
   mGuiManager = NULL;
   mWindow = NULL;
   mIsInputValid = true;
   
   mMsgFormat =
      "The value of \"%s\" for field \"%s\" is not an allowed value. \n"
      "The allowed values are: [%s].";
}


//------------------------------------------------------------------------------
// ~UserInputValidator()
//------------------------------------------------------------------------------
UserInputValidator::~UserInputValidator()
{
}


//------------------------------------------------------------------------------
// void SetObject(GmatBase *obj)
//------------------------------------------------------------------------------
void UserInputValidator::SetObject(GmatBase *obj)
{
   if (obj != NULL)
   {
      mObject = obj;
      if (mObject->IsOfType(Gmat::COMMAND))
      {
         mMsgFormat =
            "The value of \"%s\" for field \"%s\" on command \""
            + mObject->GetTypeName() +  "\" is not an allowed value. \n"
            "The allowed values are: [%s].";
      }
      else
      {
         mMsgFormat =
            "The value of \"%s\" for field \"%s\" on object \""
            + mObject->GetName() +  "\" is not an allowed value. \n"
            "The allowed values are: [%s].";
      }
   }
}


//------------------------------------------------------------------------------
// void SetGuiManager(GuiItemManager *manager)
//------------------------------------------------------------------------------
void UserInputValidator::SetGuiManager(GuiItemManager *manager)
{
   mGuiManager = manager;
}


//------------------------------------------------------------------------------
// void SetWindow(wxWindow *window)
//------------------------------------------------------------------------------
void UserInputValidator::SetWindow(wxWindow *window)
{
   mWindow = window;
}


//------------------------------------------------------------------------------
// bool IsInputValid()
//------------------------------------------------------------------------------
bool UserInputValidator::IsInputValid()
{
   return mIsInputValid;
}


//------------------------------------------------------------------------------
// bool CheckReal(Real &rvalue, const std::string &str,
//                const std::string &field, const std::string &expRange,
//                bool onlyMsg, bool checkRange, bool positive bool zeroOk)
//------------------------------------------------------------------------------
/*
 * This method checks if input string is valid real number. It uses
 * GmatStringUtil::ToReal() to convert string to Real value. This method
 * pops up the error message and return false if input string is
 * not a real number.
 *
 * @param  rvalue     Real value to be set if input string is valid
 * @param  str        Input string value
 * @param  field      Field name should be used in the error message
 * @param  expRange   Expected value range to be used in the error message
 * @param  onlyMsg    if true, it only shows error message (false)
 * @param  checkRange if true, it will check for positive and zero (false)
 * @param  positive   if true, the value must be positive (false)
 * @param  zeroOk     if true, zero is allowed (false)
 *
 * @return true if value is valid, false otherwise
 */
//------------------------------------------------------------------------------
bool UserInputValidator::CheckReal(Real &rvalue, const std::string &str,
                          const std::string &field, const std::string &expRange,
                          bool onlyMsg, bool checkRange, bool positive, bool zeroOk)
{
   #ifdef DEBUG_CHECK_REAL
   MessageInterface::ShowMessage
      ("UserInputValidator::CheckReal() str='%s', field='%s', expRange='%s'\n", str.c_str(),
       field.c_str(), expRange.c_str());
   #endif
   
   if (onlyMsg)
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, mMsgFormat.c_str(), str.c_str(), field.c_str(),
          expRange.c_str());

      SetErrorFlag();
      return false;
   }
   
   // check for real value
   Real rval;
   if (GmatStringUtil::ToReal(str, &rval))
   {
      rvalue = rval;
      
      if (checkRange)
      {
         if (!positive || (positive && rval > 0) || (zeroOk && rval >= 0))
            return true;
      }
      else
         return true;
   }
   
   MessageInterface::PopupMessage
      (Gmat::ERROR_, mMsgFormat.c_str(), str.c_str(), field.c_str(),
       expRange.c_str());

   SetErrorFlag();
   return false;
}


//------------------------------------------------------------------------------
// bool CheckInteger(Integer &ivalue, const std::string &str,
//                   const std::string &field, const std::string &expRange,
//                   bool onlyMsg = false, bool positive, bool zeroOk)
//------------------------------------------------------------------------------
/*
 * This method checks if input string is valid integer number. It uses
 * GmatStringUtil::ToInteger() to convert string to Integer value. This method
 * pops up the error message and return false if input string is
 * not an integer number.
 *
 * @param  ivalue     Integer value to be set if input string is valid
 * @param  str        Input string value
 * @param  field      Field name should be used in the error message
 * @param  expRange   Expected value range to be used in the error message
 * @param  onlyMsg    if true, it only shows error message (false)
 * @param  checkRange if true, it will check for positive and zero (false)
 * @param  positive   if true, the value must be positive (false)
 * @param  zeroOk     if true, zero is allowed (false)
 *
 * @return true if value is valid, false otherwise
 */
//------------------------------------------------------------------------------
bool UserInputValidator::CheckInteger(Integer &ivalue, const std::string &str,
                                      const std::string &field,
                                      const std::string &expRange, bool onlyMsg,
                                      bool checkRange, bool positive, bool zeroOk)
{
   if (onlyMsg)
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, mMsgFormat.c_str(), str.c_str(), field.c_str(),
          expRange.c_str());
      
      SetErrorFlag();
      return false;
   }
   
   // check for integer value
   Integer ival;
   if (GmatStringUtil::ToInteger(str, &ival))
   {
      ivalue = ival;

      if (checkRange)
      {
         if (!positive || (positive && ival > 0) || (zeroOk && ival >= 0))
            return true;
      }
      else
         return true;
   }
   
   MessageInterface::PopupMessage
      (Gmat::ERROR_, mMsgFormat.c_str(), str.c_str(), field.c_str(),
       expRange.c_str());
   
   SetErrorFlag();
   return false;
}


//------------------------------------------------------------------------------
// bool CheckVariable(const std::string &varName, Gmat::ObjectType ownerType,
//                    const std::string &field, const std::string &expRange,
//                    bool allowNumber  = true, bool allowNonPlottable = false)
//------------------------------------------------------------------------------
/*
 * Checks if input variable is a Number, Variable, Array element, or parameter of
 * input owner type.
 *
 * @param  varName    Input variable name to be checked
 * @param  ownerType  Input owner type (such as Gmat::SPACECRAFT)
 * @param  field      Field name should be used in the error message
 * @param  expRange   Expected value range to be used in the error message
 * @param  allowNumber  true if varName can be a Real number 
 * @param  allowNonPlottable  true if varName can be a non-plottable
 *
 * @return true if varName is valid
 */
//------------------------------------------------------------------------------
bool UserInputValidator::CheckVariable(const std::string &varName, Gmat::ObjectType ownerType,
                              const std::string &field, const std::string &expRange,
                              bool allowNumber, bool allowNonPlottable)
{
   if (mGuiManager == NULL)
   {
      MessageInterface::ShowMessage
         ("==> UserInputValidator::CheckVariable() mGuiManager is NULL\n");
      return false;
   }
   
   int retval = mGuiManager->
      IsValidVariable(varName.c_str(), Gmat::SPACECRAFT, allowNumber,
                      allowNonPlottable);
   
   if (retval == -1)
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "The variable \"%s\" for field \"%s\" "
          "does not exist.\nPlease create it first from the ParameterSelectDialog or "
          "from the Resource Tree.\n", varName.c_str(), field.c_str());
      
      SetErrorFlag();
      return false;
   }
   else if (retval == 3)
   {
      std::string type, ownerName, depObj;
      GmatStringUtil::ParseParameter(varName, type, ownerName, depObj);
      
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "The Parameter \"%s\" for field \"%s\" "
          "has undefined object \"%s\".\nPlease create proper object first "
          "from the Resource Tree.\n", varName.c_str(), field.c_str(), ownerName.c_str());
      
      SetErrorFlag();
      return false;
   }
   else if (retval == 4)
   {
      std::string type, ownerName, depObj;
      GmatStringUtil::ParseParameter(varName, type, ownerName, depObj);
      
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "The Parameter \"%s\" for field \"%s\" "
          "has unknown Parameter type \"%s\".\n", varName.c_str(), field.c_str(),
          type.c_str());
      
      SetErrorFlag();
      return false;
   }
   else if (retval == 0)
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, mMsgFormat.c_str(), varName.c_str(), field.c_str(),
          expRange.c_str());
      
      SetErrorFlag();
      return false;
   }
   
   return true;
}


//------------------------------------------------------------------------------
// bool IsValidName(const wxString &name)
//------------------------------------------------------------------------------
/*
 * Checks for a valid name.
 *
 * @param  name  Input name to be valdated
 *
 * @return true if input name is valid, false otherwise
 */
//------------------------------------------------------------------------------
bool UserInputValidator::IsValidName(const wxString &name)
{
   if (name == "")
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "The name is blank, please enter a valid name");
      
      SetErrorFlag();
      return false;
   }
   
   if (!GmatStringUtil::IsValidName(name.c_str()))
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "\"%s\" is not a valid name. Please reenter a name.\n\n"
          "[Name cannot be a GMAT keyword, such as \"GMAT\", \"Create\", \"function\" and \n"
          "must begin with a letter, which may be followed by any combination "
          "of letters, \ndigits, and underscores.]", name.c_str());
      SetErrorFlag();
      return false;
   }
   
   return true;
}


//------------------------------------------------------------------------------
// wxArrayString ToWxArrayString(const StringArray &array)
//------------------------------------------------------------------------------
/**
 * Converts std::string array to wxString array.
 */
//------------------------------------------------------------------------------
wxArrayString UserInputValidator::ToWxArrayString(const StringArray &array)
{
   wxArrayString newArray;
   for (UnsignedInt i=0; i<array.size(); i++)
      newArray.Add(array[i].c_str());
   
   return newArray;
}


//------------------------------------------------------------------------------
// wxString ToWxString(const wxArrayString &names)
//------------------------------------------------------------------------------
/**
 * Converts wxString array to wxString separated by comma.
 */
//------------------------------------------------------------------------------
wxString UserInputValidator::ToWxString(const wxArrayString &names)
{
   wxString str = "";
   wxString delimiter = ", ";
   if (names.Count() > 0)
   {
      str = names[0];
      
      for (unsigned int i=1; i<names.Count(); i++)
         str = str + delimiter + names[i];
   }
   
   return str;
}


//------------------------------------------------------------------------------
// void SetErrorFlag()
//------------------------------------------------------------------------------
/*
 * Sets canClose flag to window passed to this class
 */
//------------------------------------------------------------------------------
void UserInputValidator::SetErrorFlag()
{
   if (mWindow->IsKindOf(CLASSINFO(wxPanel)))
      ((GmatPanel*)mWindow)->SetCanClose(false);
   else if (mWindow->IsKindOf(CLASSINFO(wxDialog)))
      ((GmatDialog*)mWindow)->SetCanClose(false);
   
   mIsInputValid = false;
}

