//$Header$
//------------------------------------------------------------------------------
//                              Universe
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Monisha Butler
// Created: 2003/09/10
// Modified: 2003/09/29
/**
 * This class allows user to specify where Universe information is 
 * coming from
 */
//------------------------------------------------------------------------------
#ifndef Universe_hpp
#define Universe_hpp

#include "gmatwxdefs.hpp"
#include "wx/sizer.h"


#include <wx/image.h>
#include <wx/statline.h>
#include <wx/spinbutt.h>
#include <wx/spinctrl.h>
#include <wx/splitter.h>
#include <wx/listctrl.h>
#include <wx/treectrl.h>
#include <wx/notebook.h>
#include <wx/grid.h>

// Declare window functions

#define ID_TEXT          10003
#define ID_LISTBOX       10004
#define ID_BUTTON        10005
#define ID_BUTTON_ADD    10008
#define ID_BUTTON_SORT   10009
#define ID_BUTTON_REMOVE 10010

class Universe: public wxPanel
{
public:
  Universe(wxWindow *parent);
  //wxSizer *CreateSolarSystem(wxWindow *parent, bool call_fit=TRUE, bool set_sizer=TRUE);

 
   
private:
   wxBoxSizer *item0;
   wxGridSizer *item1;
   wxBoxSizer *item2;
   wxBoxSizer *item5;
   wxBoxSizer *item9;
   
   wxStaticText *item3;
   wxStaticText *item10;
   
   wxListBox *item4;
   wxListBox *item11;
   
   wxButton *item6;
   wxButton *item7;
   wxButton *item8;
    
   void CreateUniverse(wxWindow *parent);
   void OnAddButton(wxCommandEvent& event);
   void OnSortButton(wxCommandEvent& event);
   void OnRemoveButton(wxCommandEvent& event);
   
   DECLARE_EVENT_TABLE();
};
#endif
