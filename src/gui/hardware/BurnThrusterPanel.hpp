//$Id$
//------------------------------------------------------------------------------
//                            BurnThrusterPanel
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
// Author: Linda Jun
// Created: 2009.02.11
/**
 * This class sets up Thruster or ImpulsiveBurn parameters.
 */
//------------------------------------------------------------------------------
#ifndef BurnThrusterPanel_hpp
#define BurnThrusterPanel_hpp

#include "gmatwxdefs.hpp"
#include "GmatPanel.hpp"
#include "gmatdefs.hpp"

class BurnThrusterPanel: public GmatPanel
{
public:
   BurnThrusterPanel(wxWindow *parent, const wxString &name);
   ~BurnThrusterPanel();
   
   // Event Handling
   void OnTextChange(wxCommandEvent &event);
   void OnCheckBoxChange(wxCommandEvent &event);
   void OnComboBoxChange(wxCommandEvent &event);
   void OnButtonClick(wxCommandEvent &event);
   void UpdateOriginAxes();
   
protected:
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   GmatBase* theObject;
   
   std::string coordSysName;
   std::string tankName;
   std::string thrustDir1;
   std::string thrustDir2;
   std::string thrustDir3;
   
   bool isCoordSysChanged;
   bool isTankChanged;
   bool isTankEmpty;
   
   wxStaticText *originLabel;
   wxStaticText *axisLabel;
   wxStaticText *tankLabel;
   wxStaticText *ispLabel;
   wxStaticText *ispUnit;
   
   wxButton *cCoefButton;
   wxButton *kCoefButton;
   
   wxComboBox *coordSysComboBox;
   wxComboBox *originComboBox;
   wxComboBox *axesComboBox;
   wxComboBox *tankComboBox;
   
   wxTextCtrl *elem1TextCtrl;
   wxTextCtrl *elem2TextCtrl;
   wxTextCtrl *elem3TextCtrl;
   wxTextCtrl *dutyCycleTextCtrl;
   wxTextCtrl *scaleFactorTextCtrl;
   wxTextCtrl *ispTextCtrl;
   wxTextCtrl *gravityAccelTextCtrl;
   
   wxCheckBox *decMassCheckBox;
   
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 30250,
      ID_TEXTCTRL,
      ID_CHECKBOX,
      ID_BUTTON,
      ID_COMBOBOX,
   };
};
#endif

