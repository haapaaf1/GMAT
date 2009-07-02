//$Id$
//------------------------------------------------------------------------------
//                              UserInputValidator
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/02
//
/** Validates user input entered via the GUI.
 * Declares UserInputValidator class.
 */
//------------------------------------------------------------------------------

#ifndef UserInputValidator_hpp
#define UserInputValidator_hpp

#include "gmatwxdefs.hpp"
#include "GuiItemManager.hpp"
#include "GmatBase.hpp"

class UserInputValidator
{
public:
   
   // constructors
   UserInputValidator();
   ~UserInputValidator();
   
   void SetObject(GmatBase *obj);
   void SetGuiManager(GuiItemManager *manager);
   void SetWindow(wxWindow *window);
   bool IsInputValid();
   
   bool CheckReal(Real &rvalue, const std::string &str,
                  const std::string &field, const std::string &expRange,
                  bool onlyMsg = false, bool checkRange = false, 
                  bool positive = false, bool zeroOk = false);
   
   bool CheckInteger(Integer &ivalue, const std::string &str,
                     const std::string &field, const std::string &expRange,
                     bool onlyMsg = false, bool checkRange = false,
                     bool positive = false, bool zeroOk = false);
   
   bool CheckVariable(const std::string &varName, Gmat::ObjectType ownerType,
                      const std::string &field, const std::string &expRange,
                      bool allowNumber = true, bool allowNonPlottable = false);
   
   bool IsValidName(const wxString &name);
   
   wxArrayString ToWxArrayString(const StringArray &array);
   wxString ToWxString(const wxArrayString &names);
   
protected:
   
   GmatBase       *mObject;
   GuiItemManager *mGuiManager;
   wxWindow       *mWindow;
   bool           mIsInputValid;;
   std::string    mObjectName;
   std::string    mMsgFormat;
   
   void SetErrorFlag();
   
};

#endif // UserInputValidator_hpp
