//$Header$
//------------------------------------------------------------------------------
//                              GmatDialog
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/02
//
/**
 * Implements GmatDialog class.
 */
//------------------------------------------------------------------------------

#include "GmatDialog.hpp"
#include "GmatAppData.hpp"
#include "MessageInterface.hpp"

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(GmatDialog, wxDialog)
   EVT_BUTTON(ID_BUTTON_OK, GmatDialog::OnOK)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatDialog::OnCancel)
   EVT_BUTTON(ID_BUTTON_HELP, GmatDialog::OnHelp)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// GmatDialog(wxWindow *parent)
//------------------------------------------------------------------------------
/**
 * Constructs GmatDialog object.
 *
 * @param <parent> parent window
 *
 */
//------------------------------------------------------------------------------
GmatDialog::GmatDialog(wxWindow *parent, wxWindowID id, const wxString& title)
   : wxDialog(parent, id, title)
{
   int borderSize = 2;
    
   theGuiInterpreter = GmatAppData::GetGuiInterpreter();
   theGuiManager = GuiItemManager::GetInstance();
   theParent = parent;
    
   wxStaticBox *middleStaticBox = new wxStaticBox(this, -1, wxT(""));
   wxStaticBox *bottomStaticBox = new wxStaticBox(this, -1, wxT(""));
    
    // create sizers
   theDialogSizer = new wxBoxSizer(wxVERTICAL);
   theMiddleSizer = new wxStaticBoxSizer(middleStaticBox, wxVERTICAL);
   theBottomSizer = new wxStaticBoxSizer(bottomStaticBox, wxVERTICAL);
   theButtonSizer = new wxBoxSizer(wxHORIZONTAL); //loj: 10/19/04 Made theButtonSizer member data
  
    // create bottom buttons
   theOkButton =
      new wxButton(this, ID_BUTTON_OK, "OK", wxDefaultPosition, wxDefaultSize, 0);
   
   theCancelButton =
      new wxButton(this, ID_BUTTON_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize, 0);
   
   theHelpButton =
      new wxButton(this, ID_BUTTON_HELP, "Help", wxDefaultPosition, wxDefaultSize, 0);
        
   // adds the buttons to button sizer    
   theButtonSizer->Add(theOkButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   theButtonSizer->Add(theCancelButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   theButtonSizer->Add(theHelpButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   
   theBottomSizer->Add(theButtonSizer, 0, wxALIGN_CENTER | wxALL, borderSize);
}

//-------------------------------
// protected methods
//-------------------------------

//------------------------------------------------------------------------------
// void Show()
//------------------------------------------------------------------------------
/**
 * Shows the panel.
 */
//------------------------------------------------------------------------------
void GmatDialog::Show()
{
   // add items to middle sizer
    
   theDialogSizer->Add(theMiddleSizer, 0, wxGROW | wxALL, 1);
   theDialogSizer->Add(theBottomSizer, 0, wxGROW | wxALL, 1);
    
   // tells the enclosing window to adjust to the size of the sizer
   SetAutoLayout(TRUE);
   SetSizer(theDialogSizer); //use the sizer for layout
   theDialogSizer->Fit(this); //loj: if theParent is used it doesn't show the scroll bar
   theDialogSizer->SetSizeHints(this); //set size hints to honour minimum size

   CenterOnScreen(wxBOTH);
   
   LoadData();

   theOkButton->Disable();
   theHelpButton->Disable(); //loj: for future build
}

//------------------------------------------------------------------------------
// void OnOK()
//------------------------------------------------------------------------------
/**
 * Saves the data and closes the page
 */
//------------------------------------------------------------------------------
void GmatDialog::OnOK()
{
   SaveData();
   Close();
}

//------------------------------------------------------------------------------
// void OnCancel()
//------------------------------------------------------------------------------
/**
 * Close page.
 */
//------------------------------------------------------------------------------
void GmatDialog::OnCancel()
{
   ResetData();
   Close();
}

//------------------------------------------------------------------------------
// void OnHelp()
//------------------------------------------------------------------------------
/**
 * Shows Helps
 */
//------------------------------------------------------------------------------
void GmatDialog::OnHelp()
{
   // open separate window to show help
}
