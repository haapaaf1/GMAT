//$Header$
//------------------------------------------------------------------------------
//                           SolverCreatePanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2004/01/21
// Modified: 2004/05/06 by Allison Greene to inherit from GmatPanel
/**
 * This class contains the Solver Create window.
 */
//------------------------------------------------------------------------------
#include "GmatAppData.hpp"
#include "DCSetupPanel.hpp"

#include <wx/variant.h>

// base includes
#include "gmatdefs.hpp"
#include "GuiInterpreter.hpp"
#include "GuiInterpreter.hpp"
#include "Solver.hpp"
#include "DifferentialCorrector.hpp"
#include "StringUtil.hpp"  // for ToInteger()

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------
BEGIN_EVENT_TABLE(DCSetupPanel, GmatPanel)
    EVT_TEXT(ID_TEXTCTRL, DCSetupPanel::OnTextUpdate)
    EVT_COMBOBOX(ID_COMBOBOX, DCSetupPanel::OnComboBoxChange)
    EVT_CHECKBOX(ID_CHECKBOX, DCSetupPanel::OnCheckBoxChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// DCSetupPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
DCSetupPanel::DCSetupPanel(wxWindow *parent, const wxString &name)
    : GmatPanel(parent)
{
    theGuiInterpreter = GmatAppData::GetGuiInterpreter();
    
    theSolver =
        theGuiInterpreter->GetSolver(std::string(name.c_str()));
        
    theDC = (DifferentialCorrector *)theSolver;
    
    reportStyle = "";

    if (theDC != NULL)
    {
        Create();
        Show();
    }
}

//------------------------------------------------------------------------------
// void ~DCSetupPanel()
//------------------------------------------------------------------------------
DCSetupPanel::~DCSetupPanel()
{
}

//-------------------------------
// private methods
//-------------------------------

//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void DCSetupPanel::Create()
{        
    Setup(this);
}

//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void DCSetupPanel::LoadData()
{
    mObject = theDC;
    
    Integer id;
	 
    id = theDC->GetParameterID("MaximumIterations");
    maxTextCtrl->SetValue(wxVariant((long)theDC->GetIntegerParameter(id)));
    
    id = theDC->GetParameterID("ReportStyle");
    reportStyle = theDC->GetStringParameter(id).c_str();
    styleComboBox->SetValue(reportStyle.c_str());
    
    id = theDC->GetParameterID("TargeterTextFile");
    textfileTextCtrl->SetValue(theDC->GetStringParameter(id).c_str());
    
    id = theDC->GetParameterID("ShowProgress");
    showProgressCheckBox->SetValue(theDC->GetBooleanParameter(id));
   
    id = theDC->GetParameterID("UseCentralDifferences");
    centralDifferencesCheckBox->SetValue(theDC->GetBooleanParameter(id));
    
    theApplyButton->Disable();
}

//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void DCSetupPanel::SaveData()
{   
   try
   {
      Integer id;
      Integer ivalue;

      canClose = true;
      
      std::string inputString;
      std::string msg = "The value of \"%s\" for field \"%s\" on object \"" + 
                         theDC->GetName() + "\" is not an allowed value. \n"
                        "The allowed values are: [%s].";                        

//      // save maximum iterations
//      id = theDC->GetParameterID("MaximumIterations");
//      theDC->SetIntegerParameter(id, (Integer)atof(maxTextCtrl->GetValue()));

      // save maximum iterations
	  id = theDC->GetParameterID("MaximumIterations");
      inputString = maxTextCtrl->GetValue();      

         // check to see if input is a real
      if (GmatStringUtil::ToInteger(inputString,&ivalue))      
         theDC->SetIntegerParameter(id, ivalue);
      else
      {
         MessageInterface::PopupMessage(Gmat::ERROR_, msg.c_str(), 
            inputString.c_str(), "MaximumIterations","Integer > 0");

         canClose = false;
      }

//      // check to see if inout is a real
//	  wxString maxStr = maxTextCtrl->GetValue();
//      if (maxStr.ToDouble(&rval))
//         theDC->SetIntegerParameter(id, (Integer)rval);
////         theBurn->SetRealParameter(id, rval);
//      else
//      {
//         wxLogError(
//            "The value you entered for the maximum iterations is not allowed.\n" 
//            "The allowed value is: [Integer > 0].");
//         canClose = false;
//      }
    
      id = theDC->GetParameterID("ReportStyle");
      theDC->SetStringParameter(id, reportStyle.c_str());
    
      id = theDC->GetParameterID("TargeterTextFile");
      theDC->SetStringParameter(id, textfileTextCtrl->GetValue().c_str());
    
      id = theDC->GetParameterID("ShowProgress");
      theDC->SetBooleanParameter(id, showProgressCheckBox->GetValue());
    
      id = theDC->GetParameterID("UseCentralDifferences");
      theDC->SetBooleanParameter(id, centralDifferencesCheckBox->GetValue());

      theApplyButton->Disable();
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("DCSetupPanel:SaveData() error occurred!\n%s\n", e.GetMessage().c_str());
      canClose = false;
      return;
   }
}

//------------------------------------------------------------------------------
// void Setup()
//------------------------------------------------------------------------------
void DCSetupPanel::Setup( wxWindow *parent)
{   
    // wxStaticText
    maxStaticText = new wxStaticText( parent, ID_TEXT, wxT("Max Iterations"), 
                     wxDefaultPosition, wxDefaultSize, 0 );
    textfileStaticText = new wxStaticText( parent, ID_TEXT, wxT("Targeter Text File"),
                            wxDefaultPosition,wxDefaultSize, 0);
    reportStyleStaticText = new wxStaticText( parent, ID_TEXT, wxT("Report Style"),
                            wxDefaultPosition,wxDefaultSize, 0);
                            
    // wxTextCtrl
    maxTextCtrl = new wxTextCtrl( parent, ID_TEXTCTRL, wxT(""), 
                  wxDefaultPosition, wxSize(50,-1), 0 );
    textfileTextCtrl = new wxTextCtrl( parent, ID_TEXTCTRL, wxT(""), 
                            wxDefaultPosition, wxSize(200,-1), 0 );
                            
    // wxCheckBox
    showProgressCheckBox = new wxCheckBox( parent, ID_CHECKBOX, 
                               wxT("Show Progress"),wxDefaultPosition, 
                               wxDefaultSize, 0 );
    centralDifferencesCheckBox = new wxCheckBox( parent, ID_CHECKBOX, 
                               wxT("Use Central Differences"),wxDefaultPosition, 
                               wxDefaultSize, 0 );
                               
    // wxString
   wxString *styleArray = new wxString[4];
   styleArray[0] = "Normal";
   styleArray[1] = "Concise";
   styleArray[2] = "Verbose";
   styleArray[3] = "Debug";

    // wxComboBox
    styleComboBox =
      new wxComboBox( this, ID_COMBOBOX, wxT(reportStyle), wxDefaultPosition, 
         wxSize(200,-1), 4, styleArray, wxCB_DROPDOWN|wxCB_READONLY );
                                
    // wx*Sizer
    wxBoxSizer *boxsizerMain = new wxBoxSizer( wxVERTICAL );
    wxBoxSizer *boxsizer1 = new wxBoxSizer( wxVERTICAL );
    wxFlexGridSizer *flexGridSizer1 = new wxFlexGridSizer( 2, 0, 0 );
    flexGridSizer1->AddGrowableCol( 1 );
 
    // Add to wx*Sizer
    flexGridSizer1->Add( maxStaticText, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( maxTextCtrl, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( textfileStaticText, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( textfileTextCtrl, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( showProgressCheckBox, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( 0, 0, wxALIGN_CENTRE|wxALL, 5);
    flexGridSizer1->Add( centralDifferencesCheckBox, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( 0, 0, wxALIGN_CENTRE|wxALL, 5);
    flexGridSizer1->Add( reportStyleStaticText, 0, wxALIGN_LEFT|wxALL, 5 );
    flexGridSizer1->Add( styleComboBox, 0, wxALIGN_LEFT|wxALL, 5 );
    
    boxsizer1->Add( flexGridSizer1, 0, wxALIGN_CENTER|wxALL, 5 );
    boxsizerMain->Add( boxsizer1, 0, wxALIGN_CENTER|wxALL, 5 );

    theMiddleSizer->Add(boxsizerMain, 0, wxGROW, 5);
}

//------------------------------------------------------------------------------
// void OnTextUpdate()
//------------------------------------------------------------------------------
void DCSetupPanel::OnTextUpdate(wxCommandEvent& event)
{
   theApplyButton->Enable();
}

//------------------------------------------------------------------------------
// void OnComboBoxChange()
//------------------------------------------------------------------------------
void DCSetupPanel::OnComboBoxChange(wxCommandEvent &event)
{
	reportStyle = styleComboBox->GetStringSelection().c_str();
	theApplyButton->Enable();
}

//------------------------------------------------------------------------------
// void OnTextChange()
//------------------------------------------------------------------------------
void DCSetupPanel::OnCheckBoxChange(wxCommandEvent &event)
{
    theApplyButton->Enable();
}    