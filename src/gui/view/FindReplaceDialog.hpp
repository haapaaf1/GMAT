//$Id$
//------------------------------------------------------------------------------
//                              FindReplaceDialog
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc.
//
// Author: Linda Jun, NASA/GSFC
// Created: 2009/01/09
//
/**
 * Declares FindReplaceDialog class.
 */
//------------------------------------------------------------------------------
#ifndef FindReplaceDialog_hpp
#define FindReplaceDialog_hpp

#include "gmatwxdefs.hpp"
#include <wx/button.h>

#include "GmatAppData.hpp"
#include "GuiInterpreter.hpp"
#include "GuiItemManager.hpp"

class Editor;

class FindReplaceDialog : public wxDialog
{
public:
   
   // constructors
   FindReplaceDialog(wxWindow *parent, wxWindowID id, const wxString& title,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDEFAULT_DIALOG_STYLE);
   
   void SetEditor(Editor *editor) { mEditor = editor; };
   wxString GetFindText() { return mFindText; };
   
protected:
   
   Editor *mEditor;
   wxArrayString mFindArray;
   wxArrayString mReplaceArray;
   wxString   mFindText;
   wxString   mReplaceText;
   
   wxComboBox *mFindComboBox;
   wxComboBox *mReplaceComboBox;
   wxButton   *mFindNextButton;
   wxButton   *mFindPrevButton;
   wxButton   *mReplaceButton;
   wxButton   *mReplaceAllButton;
   wxButton   *mCloseButton;
   
   void Create();
   
   void OnButtonClick(wxCommandEvent &event);
   void OnComboBoxEnter(wxCommandEvent &event);
   void OnKeyDown(wxKeyEvent &event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_BUTTON = 500,
      ID_COMBOBOX,
   };

};

#endif // FindReplaceDialog_hpp