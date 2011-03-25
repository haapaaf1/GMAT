//$Id$
//------------------------------------------------------------------------------
//                              BeginFiniteBurnPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc.
// Author: Linda Jun
// Created: 2006/07/14
//
/**
 * This class contains the BeginFiniteBurn Setup window.
 */
//------------------------------------------------------------------------------

#ifndef BeginFiniteBurnPanel_hpp
#define BeginFiniteBurnPanel_hpp

#include "gmatwxdefs.hpp"
#include "GmatPanel.hpp"
#include "GmatCommand.hpp"

class BeginFiniteBurnPanel : public GmatPanel
{
public:
   // constructors
   BeginFiniteBurnPanel(wxWindow *parent, GmatCommand *cmd);
   ~BeginFiniteBurnPanel();
   
protected:
   // member data
   GmatCommand *theCommand;
   wxArrayString mObjectTypeList;
   wxArrayString mSpacecraftList;
   
   wxComboBox *mFiniteBurnComboBox;
   wxTextCtrl *mSatTextCtrl;
   
   wxArrayString ToWxArrayString(const StringArray &array);
   wxString ToWxString(const wxArrayString &names);
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // event handling
   void OnButtonClicked(wxCommandEvent& event);
   void OnComboBoxChange(wxCommandEvent& event);
   void OnTextUpdate(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 80000,
      ID_TEXTCTRL,
      ID_BUTTON,
      ID_COMBOBOX,
   };
};

#endif // BeginFiniteBurnPanel_hpp
