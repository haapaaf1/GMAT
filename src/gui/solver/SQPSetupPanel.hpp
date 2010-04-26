#ifndef SQPSETUPPANEL_HPP_
#define SQPSETUPPANEL_HPP_

#include "gmatwxdefs.hpp"

#include <wx/variant.h>

// base includes
#include "GuiInterpreter.hpp"
#include "GmatPanel.hpp"
#include "Solver.hpp"

class SQPSetupPanel : public GmatPanel
{
public:
   // constructors
   SQPSetupPanel(wxWindow *parent, const wxString &name);
   ~SQPSetupPanel();  
   
private:     
   
   Solver *theSolver;
   bool isTextModified;
   
// GradObj, GradConstr, DerivativeCheck, Diagnostics, and Display parameters are no longer been used:
//   wxStaticText *displayStaticText;
//   wxComboBox *displayComboBox;
   
//   wxCheckBox *gradObjCB;
//   wxCheckBox *gradConstrCB;
//   wxCheckBox *derivativeCheckCB;
//   wxCheckBox *diagnosticsCB;
   
   wxStaticText *tolFunStaticText;
   wxStaticText *tolConStaticText;
   wxStaticText *tolXStaticText;
   wxStaticText *maxFunEvalsStaticText;
   wxStaticText *maxIterStaticText;
   wxStaticText *diffMinChangeStaticText;
   wxStaticText *diffMaxChangeStaticText;
   
   wxTextCtrl *tolFunTextCtrl;
   wxTextCtrl *tolConTextCtrl;
   wxTextCtrl *tolXTextCtrl;
   wxTextCtrl *maxFunEvalsTextCtrl;
   wxTextCtrl *maxIterTextCtrl;
   wxTextCtrl *diffMinChangeTextCtrl;
   wxTextCtrl *diffMaxChangeTextCtrl;
      
   wxCheckBox *showProgressCheckBox;
   wxStaticText *reportfileStaticText;
   wxTextCtrl *reportfileTextCtrl;
   wxStaticText *reportStyleStaticText;
   wxComboBox *styleComboBox;

   wxButton	  *browseButton;
   
   wxFileDialog	*fileDialog;

   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // Layout 
   void Setup(wxWindow *parent);
   
   // Text control event method
   void OnTextUpdate(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   void OnComboBoxChange(wxCommandEvent& event);
   void OnTextChange(wxCommandEvent& event);
   void OnCheckboxChange(wxCommandEvent& event);
   void OnBrowse(wxCommandEvent &event);
       
//   static const wxString DISPLAY_SCHEMES[4];				// made a change here
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 55000,
      ID_TEXTCTRL,
      ID_BUTTON,
      ID_COMBOBOX,
      ID_CHECKBOX,
      ID_NOTEBOOK,
	  ID_BUTTON_BROWSE
   };
};

#endif /*SQPSETUPPANEL_HPP_*/