//$Header$
//------------------------------------------------------------------------------
//                            SpacecraftPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P.
//
//
// Author: Monisha Butler
// Created: 2003/09/10
// Modified: 2003/09/29
/**
 * This class contains information needed to setup users spacecraft through GUI
 * 
 */
//------------------------------------------------------------------------------
#ifndef SpacecraftPanel_hpp
#define SpacecraftPanel_hpp

#include "gmatwxdefs.hpp"
#include "GmatPanel.hpp"
#include "GuiInterpreter.hpp"

#include "Spacecraft.hpp"
#include "PhysicalConstants.hpp"   // for mu
#include "GmatPanel.hpp"
#include "BallisticsMassPanel.hpp"
#include "OrbitPanel.hpp"


//loj: to remove warnings about duplicate defines
// Declare window functions
//  #define ID_TEXT 10000
//  #define ID_COMBO 10001
//  #define ID_TEXTCTRL 10002

//  #define ID_NOTEBOOK 20000
//  #define ID_BUTTON   20001
//  #define ID_ELEMENT_TAG1     20006
//  #define ID_ELEMENT_TAG2     20007
//  #define ID_ELEMENT_TAG3     20008
//  #define ID_ELEMENT_TAG4     20009
//  #define ID_ELEMENT_TAG5     20010
//  #define ID_ELEMENT_TAG6     20011

//  #define ID_ELEMENT_VALUE1     20012
//  #define ID_ELEMENT_VALUE2     20013
//  #define ID_ELEMENT_VALUE3     20014
//  #define ID_ELEMENT_VALUE4     20015
//  #define ID_ELEMENT_VALUE5     20016
//  #define ID_ELEMENT_VALUE6     20017

//  #define ID_CHOICE_BODY  20018
//  #define ID_CHOICE_FRAME 20019
//  #define ID_CHOICE_EPOCH 20020
//  #define ID_CHOICE_STATE 20021

//  #define ID_EPOCH_VALUE 10011

//  #define ID_BUTTON_OK     20022
//  #define ID_BUTTON_APPLY  20023
//  #define ID_SC_BUTTON_CANCEL 20024
//  #define ID_BUTTON_HELP   20025


class SpacecraftPanel: public GmatPanel
{
public:
    SpacecraftPanel(wxWindow *parent, const wxString &scName);
    ~SpacecraftPanel();   
       
private:
    void CreateNotebook(wxWindow *parent, const wxString &scName);
    
    // methods inherited from GmatPanel
    virtual void Create();
    virtual void LoadData();
    virtual void SaveData();
    //loj: 2/27/04 commented out
    //virtual void OnHelp();
    //virtual void OnScript();

    GuiInterpreter *theGuiInterpreter;
    Spacecraft *theSpacecraft;
    Spacecraft *currentSpacecraft;
    
    wxStaticText *description1;
    wxStaticText *description2;
    wxStaticText *description3;
    wxStaticText *description4;
    wxStaticText *description5;
    wxStaticText *description6;
    
    wxTextCtrl *textCtrl1;
    wxTextCtrl *textCtrl2;
    wxTextCtrl *textCtrl3;
    wxTextCtrl *textCtrl4;
    wxTextCtrl *textCtrl5;
    wxTextCtrl *textCtrl6;
    
    wxStaticText *label1;
    wxStaticText *label2;
    wxStaticText *label3;
    wxStaticText *label4;
    wxStaticText *label5;
    wxStaticText *label6;

    wxNotebook *mainNotebook;
    wxNotebookSizer *sizer;
    wxPanel *attitude;
    wxPanel *actuators;
    wxPanel *sensors;
    wxPanel *tanks;
    wxPanel *visuals;
    
    BallisticsMassPanel *theBallisticMassPanel;
    OrbitPanel *theOrbitPanel;
       
    // IDs for the controls and the menu commands
    enum
    {
        ID_TEXT = 30000,
        ID_COMBO,
        ID_TEXTCTRL,
        ID_NOTEBOOK,
        ID_BUTTON,
        ID_ELEMENT_TAG1,
        ID_ELEMENT_TAG2,
        ID_ELEMENT_TAG3,
        ID_ELEMENT_TAG4,
        ID_ELEMENT_TAG5,
        ID_ELEMENT_TAG6,
        
        ID_ELEMENT_VALUE1,
        ID_ELEMENT_VALUE2,
        ID_ELEMENT_VALUE3,
        ID_ELEMENT_VALUE4,
        ID_ELEMENT_VALUE5,
        ID_ELEMENT_VALUE6,
        
        ID_STATIC_COORD,
        ID_STATIC_ORBIT,
        ID_STATIC_ELEMENT,
        
        ID_CB_BODY,
        ID_CB_FRAME,
        ID_CB_EPOCH,
        ID_CB_STATE,
        
        ID_EPOCH_VALUE,
        
        ID_BUTTON_OK,
        ID_BUTTON_APPLY,
        ID_BUTTON_CANCEL,
        ID_BUTTON_HELP
    };
};
#endif

