//$Header$
//------------------------------------------------------------------------------
//                              PropagatorSetupPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2003/09/08
// Copyright: (c) 2003 NASA/GSFC. All rights reserved.
//
/**
 * This class contains the Propagator Setup window.
 */
//------------------------------------------------------------------------------

#ifndef PropagatorSetupPanel_hpp
#define PropagatorSetupPanel_hpp

#include <wx/sizer.h>
#include <wx/control.h>
#include <wx/textctrl.h>
#include <wx/combobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/grid.h>

#include "gmatwxdefs.hpp"

class PropagatorSetupPanel : public wxPanel
{
public:
    // constructors
    PropagatorSetupPanel( wxWindow *parent );
    
private:
    wxBoxSizer *item0;
    wxBoxSizer *item1;
    wxBoxSizer *item31;
    wxBoxSizer *item35;

    wxStaticBox *item6;
    wxStaticBox *item11;
    wxStaticBox *item14;

    wxStaticBoxSizer *item5;
    wxStaticBoxSizer *item10;
    wxStaticBoxSizer *item13;

    wxFlexGridSizer *item15;

    wxStaticText *item2;
    wxStaticText *item7;
    wxStaticText *item16;
    wxStaticText *item17;
    wxStaticText *item18;
    wxStaticText *item23;
    wxStaticText *item24;
    wxStaticText *item25;
    wxStaticText *item26;
    wxStaticText *item32;
        
    wxTextCtrl *item3;
    wxTextCtrl *item22;
    wxTextCtrl *item27;
    wxTextCtrl *item28;
    wxTextCtrl *item33;

    wxButton *item4;
    wxButton *item34;
    wxButton *item36;
    wxButton *item37;
    wxButton *item38;
    //wxButton *item39;

    wxComboBox *item8;
    wxComboBox *item19;
    wxComboBox *item20;
    wxComboBox *item21;
    wxComboBox *item29;
    wxComboBox *item30;

    wxGrid *item9;
    wxGrid *item12; 

    void OnButton(wxCommandEvent& event);
    void OnRightClick();
    void OnOK();
    void OnApply();
    void OnCancel();
    void Setup(wxWindow *parent);
    void GetData();
    void SetData();

    // any class wishing to process wxWindows events must use this macro
    DECLARE_EVENT_TABLE();
    
    // IDs for the controls and the menu commands
    enum
    {     
        ID_TEXT = 8000,
        ID_TEXTCTRL,
        ID_COMBO,
        ID_BUTTON,
        ID_CHECKBOX,
        ID_GRID
    };
};

#endif // PropagatorSetupPanel_hpp
