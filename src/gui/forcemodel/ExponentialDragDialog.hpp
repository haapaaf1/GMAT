//$Header:
//------------------------------------------------------------------------------
//                              ExponentialDragDialog
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2004/04/01
// Modified:
/**
 * This class allows user to edit Exponential drag parameters.
 */
//------------------------------------------------------------------------------

#ifndef ExponentialDragDialog_hpp
#define ExponentialDragDialog_hpp

#include "gmatwxdefs.hpp"
#include "gmatdefs.hpp"
#include "GuiInterpreter.hpp"
#include "GmatAppData.hpp"
#include "GmatDialog.hpp"

class ExponentialDragDialog : public GmatDialog
{
public:
    ExponentialDragDialog(wxWindow *parent, wxString name);
    ~ExponentialDragDialog();
    
private:   
    wxTextCtrl *expDrag1TextCtrl;
    wxTextCtrl *expDrag2TextCtrl;
    wxTextCtrl *expDrag3TextCtrl;

    // Methods inherited from GmatDialog
    virtual void Create();
    virtual void LoadData();
    virtual void SaveData();
    virtual void ResetData();
    
    // Event-handling Methods
    void OnTextChange();

    DECLARE_EVENT_TABLE();

    // IDs for the controls and the menu commands
    enum
    {     
        ID_TEXT = 10003,
        ID_TEXTCTRL
    };
};

#endif

