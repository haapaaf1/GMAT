//$Id$
//------------------------------------------------------------------------------
//                           SolverGoalsPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2004/01/20
// Modified: 2004/05/06 by Allison Greene to inherit from GmatPanel
/**
 * This class contains the Solver Goals window.
 */
//------------------------------------------------------------------------------

#ifndef SolverGoalsPanel_hpp
#define SolverGoalsPanel_hpp

#include "gmatwxdefs.hpp"

// base includes
#include "gmatdefs.hpp"
#include "GuiInterpreter.hpp"
#include "GmatPanel.hpp"

class SolverGoalsPanel : public GmatPanel
{
public:
    // constructors
    SolverGoalsPanel(wxWindow *parent);
    ~SolverGoalsPanel(); 
    
private:             
    wxGrid *goalsGrid;
    
    wxStaticText *descStaticText;
    wxStaticText *solverStaticText;
    wxStaticText *varStaticText;
    wxStaticText *valueStaticText;
    wxStaticText *tolStaticText;
    
    wxTextCtrl *descTextCtrl;
    wxTextCtrl *varTextCtrl;
    wxTextCtrl *valueTextCtrl;
    wxTextCtrl *tolTextCtrl;
    
    wxComboBox *solverComboBox;
    
    wxButton *editButton;
    wxButton *updateButton;
    
    wxString solverString;
    wxString propertyString;
    wxString descriptionString;
    
    Integer numOfGoals;
    Integer nextRow;
    
    // methods inherited from GmatPanel
    virtual void Create();
    virtual void LoadData();
    virtual void SaveData();
    //loj: 2/27/04 commented out
    //virtual void OnHelp();
    //virtual void OnScript();
    
    // Layout & data handling methods
    void Initialize();
    void Setup(wxWindow *parent);
    
    // Grid table event method
    void OnCellValueChanged(wxGridEvent &event);
    
    // Text control event method
    void OnTextUpdate(wxCommandEvent& event);    
    
    // Combobox event method
    void OnSolverSelection(wxCommandEvent &event);
    
    // Button event method
    void OnButton(wxCommandEvent& event);


    // any class wishing to process wxWindows events must use this macro
    DECLARE_EVENT_TABLE();
    
    // IDs for the controls and the menu commands
    enum
    {     
        ID_TEXT = 52000,
        ID_TEXTCTRL,
        ID_BUTTON,
        ID_COMBO,
        ID_GRID,
        SOL_COL,
        PRO_COL,
        DES_COL
    };
};

#endif // SolverGoalsPanel_hpp

