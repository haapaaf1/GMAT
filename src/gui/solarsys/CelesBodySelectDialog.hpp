//$Id$
//------------------------------------------------------------------------------
//                              CelesBodySelectDialog
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/25
//
/**
 * Declares CelesBodySelectDialog class. This class shows dialog window where
 * celestial bodies can be selected.
 * 
 */
//------------------------------------------------------------------------------
#ifndef CelesBodySelectDialog_hpp
#define CelesBodySelectDialog_hpp

#include "gmatwxdefs.hpp"
#include "GmatDialog.hpp"
#include "gmatdefs.hpp"
#include <wx/string.h> // for wxArrayString

class CelesBodySelectDialog : public GmatDialog
{
public:
    
   CelesBodySelectDialog(wxWindow *parent, wxArrayString &bodiesToExclude,
                         wxArrayString &bodiesToHide, bool showCalPoints = false);
   ~CelesBodySelectDialog();
   
   wxArrayString& GetBodyNames()
      { return mBodyNames; }
   bool IsBodySelected()
      { return mIsBodySelected; }
      
private:
   wxArrayString mBodyNames;
   wxArrayString mBodiesToExclude;
   wxArrayString mBodiesToHide;
      
   bool mIsBodySelected;
   bool mShowCalPoints;
   
   wxString mSelBodyName;
   
   wxButton *mAddBodyButton;
   wxButton *mRemoveBodyButton;
   wxButton *mClearBodyButton;
   
   wxListBox *mBodyListBox;
   wxListBox *mBodySelectedListBox;
   
   // methods inherited from GmatDialog
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   virtual void ResetData();
   
   void ShowBodyOption(const wxString &name, bool show = true);
   
   // event handling
   void OnButton(wxCommandEvent& event);
   void OnSelectBody(wxCommandEvent& event);
   
   DECLARE_EVENT_TABLE()
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 9300,
      ID_LISTBOX,
      ID_BODY_SEL_LISTBOX,
      ID_BUTTON,
   };
};

#endif
