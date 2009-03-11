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
#include "BurnThrusterPanel.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"
#include "ThrusterCoefficientDialog.hpp"
#include <wx/variant.h>

//#define DEBUG_BURNPANEL_CREATE
//#define DEBUG_BURNPANEL_LOAD
//#define DEBUG_BURNPANEL_SAVE

//------------------------------
// event tables for wxWindows
//------------------------------
BEGIN_EVENT_TABLE(BurnThrusterPanel, GmatPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   EVT_TEXT(ID_TEXTCTRL, BurnThrusterPanel::OnTextChange)
   EVT_TEXT(ID_COMBOBOX, BurnThrusterPanel::OnTextChange)
   EVT_CHECKBOX(ID_CHECKBOX, BurnThrusterPanel::OnCheckBoxChange)
   EVT_COMBOBOX(ID_COMBOBOX, BurnThrusterPanel::OnComboBoxChange)
   EVT_BUTTON(ID_BUTTON, BurnThrusterPanel::OnButtonClick)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// BurnThrusterPanel(wxWindow *parent, const wxString &name)
//------------------------------------------------------------------------------
/**
 * Constructs BurnThrusterPanel object.
 */
//------------------------------------------------------------------------------
BurnThrusterPanel::BurnThrusterPanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent, true, true)
{
   theObject = theGuiInterpreter->GetConfiguredObject(name.c_str());

   #ifdef DEBUG_BURNPANEL_CREATE
   MessageInterface::ShowMessage
      ("BurnThrusterPanel() constructor entered, theObject=<%p>'%s'\n",
       theObject, theObject->GetTypeName().c_str());
   #endif
   
   isCoordSysChanged = false;
   isTankChanged = false;
   isTankEmpty = false;
   coordSysName = "";
   tankName = "";
}


//------------------------------------------------------------------------------
// ~Burnthrusterpanel()
//------------------------------------------------------------------------------
BurnThrusterPanel::~BurnThrusterPanel()
{
   theGuiManager->UnregisterComboBox("CoordinateSystem", coordSysComboBox);
   theGuiManager->UnregisterComboBox("CelestialBody", originComboBox);
   theGuiManager->UnregisterComboBox("FuelTank", tankComboBox);
}

//-------------------------------
// private methods
//-------------------------------

//----------------------------------
// methods inherited from GmatPanel
//----------------------------------

//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void BurnThrusterPanel::Create()
{
   #ifdef DEBUG_BURNPANEL_CREATE
   MessageInterface::ShowMessage("BurnThrusterPanel::Create() entered\n");
   #endif
   
   Integer bsize = 2; // border size
   
   // Coordinate Systems 
   wxStaticText *coordSysLabel =
      new wxStaticText(this, ID_TEXT, wxT("Coordinate System"));
   coordSysComboBox  =
      theGuiManager->GetCoordSysComboBox(this, ID_COMBOBOX, wxSize(150,-1));
   
   // Addd Local to CoordinateSystem list
   coordSysComboBox->Insert("Local", 0);
   
   // Origin
   originLabel = new wxStaticText(this, ID_TEXT, wxT("Origin"));
   originComboBox =
      theGuiManager->GetCelestialBodyComboBox(this, ID_COMBOBOX,
                                              wxSize(150,-1));
   
   // Axes 
   ///@todo Needs to be implemented in the base code
   StringArray axesLabels = theObject->GetStringArrayParameter("Axes");
   wxArrayString wxAxesLabels = ToWxArrayString(axesLabels);
   
   axisLabel = new wxStaticText(this, ID_TEXT, wxT("Axes"));
   
   axesComboBox = 
      new wxComboBox(this, ID_COMBOBOX, wxT(""), wxDefaultPosition, 
                     wxSize(150,-1), wxAxesLabels, wxCB_DROPDOWN|wxCB_READONLY);
   axesComboBox->SetSelection(0);
   
   // Element1
   wxStaticText *elem1Unit = new wxStaticText(this, ID_TEXT, wxT(" km/s"));
   wxStaticText *XLabel = new wxStaticText(this, ID_TEXT, wxT("Element1"));
   elem1TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                      wxDefaultPosition, wxSize(150,-1), 0);
   
   // Element2
   wxStaticText *elem2Unit =
      new wxStaticText(this, ID_TEXT, wxT(" km/s"));
   wxStaticText *YLabel =
      new wxStaticText(this, ID_TEXT, wxT("Element2"),
                        wxDefaultPosition,wxDefaultSize, 0);
   elem2TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                      wxDefaultPosition, wxSize(150,-1), 0);
   
   // Element3
   wxStaticText *elem3Unit = new wxStaticText(this, ID_TEXT, wxT(" km/s"));
   wxStaticText *ZLabel = new wxStaticText(this, ID_TEXT, wxT("Element3"));
   elem3TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                     wxDefaultPosition, wxSize(150,-1), 0);
   
   wxStaticText *dutyCycleLabel = NULL;
   wxStaticText *scaleFactorLabel = NULL;
   
   if (theObject->IsOfType(Gmat::THRUSTER))
   {
      // Thruster Duty Cycle
      dutyCycleLabel =
         new wxStaticText(this, ID_TEXT, wxT("Duty Cycle"));
      dutyCycleTextCtrl =
         new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                        wxDefaultPosition, wxSize(150,-1), 0);
      
      // Thruster Scale Factor
      scaleFactorLabel =
         new wxStaticText(this, ID_TEXT, wxT("Thrust Scale Factor"));
      scaleFactorTextCtrl =
         new wxTextCtrl(this, ID_TEXTCTRL, wxT(""),
                        wxDefaultPosition, wxSize(150,-1), 0);
   }
   
   // Decrement mass
   decMassCheckBox =
      new wxCheckBox(this, ID_CHECKBOX, wxT("Decrement Mass"),
                     wxDefaultPosition, wxSize(-1, -1), bsize);
   
   //Tank
   tankLabel =
      new wxStaticText(this, ID_TEXT, wxT("Tank"));
   tankComboBox =
      theGuiManager->GetFuelTankComboBox(this, ID_COMBOBOX, wxSize(150,-1));

   ispLabel = NULL;
   ispTextCtrl = NULL;
   ispUnit = NULL;
   // Isp for ImpulsiveBurn only
   if (theObject->IsOfType(Gmat::IMPULSIVE_BURN))
   {
      ispLabel =
         new wxStaticText(this, ID_TEXT, wxT("Isp"));
      ispTextCtrl =
         new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                        wxDefaultPosition, wxSize(150,-1), 0);
      ispUnit =
         new wxStaticText(this, ID_TEXT, wxT(" s"));
   }
   
   // Gravitational Acceleration
   wxStaticText *gravityAccelLabel =
      new wxStaticText(this, ID_TEXT, wxT("GravitationalAccel"));
   gravityAccelTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), 
                     wxDefaultPosition, wxSize(150,-1), 0);
   wxStaticText *gravityAccelUnit =
      new wxStaticText(this, ID_TEXT, wxT(" m/s"));
   
   // Coefficients for Thruster only
   if (theObject->IsOfType(Gmat::THRUSTER))
   {
      cCoefButton = new wxButton(this, ID_BUTTON, wxT("Edit Thruster Coef."));
      kCoefButton = new wxButton(this, ID_BUTTON, wxT("Edit Impulse Coef."));
   }
   
   wxBoxSizer *coefSizer = new wxBoxSizer(wxHORIZONTAL);
   if (theObject->IsOfType(Gmat::THRUSTER))
   {
      coefSizer->Add(cCoefButton, 0, wxALIGN_CENTER|wxALL, 5);
      coefSizer->Add(kCoefButton, 0, wxALIGN_CENTER|wxALL, 5);
   }
   
   // Add to wx*Sizers
   wxFlexGridSizer *flexGridSizer1 = new wxFlexGridSizer(3, 0, 0);
   flexGridSizer1->Add(coordSysLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(coordSysComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(20,20);
   
   flexGridSizer1->Add(originLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(originComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(20,20);
   
   flexGridSizer1->Add(axisLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(axesComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(20,20);
   
   flexGridSizer1->Add(XLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(elem1TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(elem1Unit, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   
   flexGridSizer1->Add(YLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(elem2TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(elem2Unit, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   
   flexGridSizer1->Add(ZLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(elem3TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(elem3Unit, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   
   if (theObject->IsOfType(Gmat::THRUSTER))
   {
      flexGridSizer1->Add(dutyCycleLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
      flexGridSizer1->Add(dutyCycleTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
      flexGridSizer1->Add(20,20);
      
      flexGridSizer1->Add(scaleFactorLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
      flexGridSizer1->Add(scaleFactorTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
      flexGridSizer1->Add(20,20);
   }
   
   flexGridSizer1->Add(decMassCheckBox, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(20,20);
   flexGridSizer1->Add(20,20);
   
   flexGridSizer1->Add(tankLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(tankComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(20,20);
   
   if (theObject->IsOfType(Gmat::IMPULSIVE_BURN))
   {
      flexGridSizer1->Add(ispLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
      flexGridSizer1->Add(ispTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
      flexGridSizer1->Add(ispUnit, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   }
   
   flexGridSizer1->Add(gravityAccelLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   flexGridSizer1->Add(gravityAccelTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(gravityAccelUnit, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   
   wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
   pageSizer->Add(flexGridSizer1, 0, wxALIGN_CENTER|wxALL, bsize);
   
   if (theObject->IsOfType(Gmat::THRUSTER))
   {      
      flexGridSizer1->Add(20, 20, 0, wxALIGN_LEFT|wxALL, bsize);
      flexGridSizer1->Add(20, 20, 0, wxALIGN_LEFT|wxALL, bsize);
      flexGridSizer1->Add(20, 20, 0, wxALIGN_LEFT|wxALL, bsize);
      
      pageSizer->Add(coefSizer, 0, wxALIGN_CENTER|wxALL, bsize);
   }
   
   theMiddleSizer->Add(pageSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   #ifdef DEBUG_BURNPANEL_CREATE
   MessageInterface::ShowMessage("BurnThrusterPanel::Create() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void BurnThrusterPanel::LoadData()
{
   #ifdef DEBUG_BURNPANEL_LOAD
   MessageInterface::ShowMessage("BurnThrusterPanel::LoadData() entered\n");
   #endif
   
   // Set object pointer for "Show Script"
   mObject = theObject;
   
   Integer paramID;
   
   try
   {
      paramID = theObject->GetParameterID("CoordinateSystem");
      coordSysName = theObject->GetStringParameter(paramID);
      coordSysComboBox->SetValue(coordSysName.c_str());
      
      paramID = theObject->GetParameterID("Origin");
      std::string objName = theObject->GetStringParameter(paramID);
      originComboBox->SetValue(objName.c_str());
      
      paramID = theObject->GetParameterID("Axes");
      objName = theObject->GetStringParameter(paramID);
      axesComboBox->SetValue(objName.c_str());
      
      paramID = theObject->GetParameterID("Element1");
      elem1TextCtrl->SetValue(wxVariant(theObject->GetRealParameter(paramID)));
      
      paramID = theObject->GetParameterID("Element2");
      elem2TextCtrl->SetValue(wxVariant(theObject->GetRealParameter(paramID)));
      
      paramID = theObject->GetParameterID("Element3");
      elem3TextCtrl->SetValue(wxVariant(theObject->GetRealParameter(paramID)));
      
      paramID = theObject->GetParameterID("DecrementMass");
      decMassCheckBox->SetValue((wxVariant(theObject->GetBooleanParameter(paramID))));
      
      paramID = theObject->GetParameterID("GravitationalAccel");
      gravityAccelTextCtrl->SetValue((wxVariant(theObject->GetRealParameter(paramID))));
      
      paramID = theObject->GetParameterID("Tank");
      StringArray tanks = theObject->GetStringArrayParameter(paramID);   
      
      if (tanks.empty())
      {
         if (theGuiManager->GetNumFuelTank() > 0)
         {
            tankComboBox->Insert("No Fuel Tank Selected", 0);
            tankComboBox->SetSelection(0);
         }
      }
      else
      {
         tankName = tanks[0];
         tankComboBox->SetValue(tankName.c_str());
         isTankEmpty = false;
      }
      
      // Disable tank combo box if decrement mass is not checked
      if (!decMassCheckBox->IsChecked())
      {
         tankLabel->Disable();
         tankComboBox->Disable();

         if (theObject->GetType() == Gmat::IMPULSIVE_BURN)
         {
            ispLabel->Disable();
            ispTextCtrl->Disable();
            ispUnit->Disable();
         }
      }
      
      // Update Origin and Axes
      UpdateOriginAxes();
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
   
   #ifdef DEBUG_BURNPANEL_LOAD
   MessageInterface::ShowMessage("BurnThrusterPanel::LoadData() exiting\n");
   #endif
}

//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void BurnThrusterPanel::SaveData()
{
   #ifdef DEBUG_BURNPANEL_SAVE
   MessageInterface::ShowMessage("BurnThrusterPanel::SaveData() entered\n");
   #endif
   
   canClose = true;
   std::string str;
   Real elem1, elem2, elem3, gravityAccel;
   bool realDataChanged = false;
   
   //-----------------------------------------------------------------
   // check values from text field
   //-----------------------------------------------------------------
   if (elem1TextCtrl->IsModified() || elem2TextCtrl->IsModified() ||
       elem3TextCtrl->IsModified() || gravityAccelTextCtrl->IsModified())
   {
      str = elem1TextCtrl->GetValue();
      CheckReal(elem1, str, "Element1", "Real Number");
      
      str = elem2TextCtrl->GetValue();
      CheckReal(elem2, str, "Element2", "Real Number");
      
      str = elem3TextCtrl->GetValue();
      CheckReal(elem3, str, "Element3", "Real Number");
      
      str = gravityAccelTextCtrl->GetValue();
      CheckReal(gravityAccel, str, "GravitationalAccel", "Real Number");
      
      realDataChanged = true;      
   }
   
   if (!canClose)
      return;
   
   try 
   {
      Integer paramID;
      
      // Coordinate System      
      if (isCoordSysChanged)
      {
         paramID = theObject->GetParameterID("CoordinateSystem");
         theObject->SetStringParameter(paramID, coordSysName);
         isCoordSysChanged = false;
      }
      
      // Origin
      paramID = theObject->GetParameterID("Origin");
      theObject->SetStringParameter(paramID, originComboBox->GetValue().c_str());
      
      // Axes
      paramID = theObject->GetParameterID("Axes");
      theObject->SetStringParameter(paramID, axesComboBox->GetValue().c_str());
      
      // Save Elements
      if (realDataChanged)
      {
         paramID = theObject->GetParameterID("Element1");
         theObject->SetRealParameter(paramID, elem1);
         
         paramID = theObject->GetParameterID("Element2");
         theObject->SetRealParameter(paramID, elem2);
         
         paramID = theObject->GetParameterID("Element3");
         theObject->SetRealParameter(paramID, elem3);
         
         paramID = theObject->GetParameterID("GravitationalAccel");
         theObject->SetRealParameter(paramID, gravityAccel);
      }
      
      // Always save DecrementMass
      // @todo If some base code computation involved, have separate flag (LOJ)
      paramID = theObject->GetParameterID("DecrementMass");
      if (decMassCheckBox->IsChecked())
         theObject->SetBooleanParameter(paramID, true);
      else
         theObject->SetBooleanParameter(paramID, false);
      
      // Save Tank
      if (isTankChanged)
      {
         isTankChanged = false;
         paramID = theObject->GetParameterID("Tank");
         
         if (theObject->TakeAction("ClearTanks", ""))
            if (tankName != "")
               theObject->SetStringParameter(paramID, tankName.c_str());
      }
   }
   catch(BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   #ifdef DEBUG_BURNPANEL_SAVE
   MessageInterface::ShowMessage("BurnThrusterPanel::SaveData() exiting\n");
   #endif
}

//------------------------------------------------------------------------------
// void OnTextChange()
//------------------------------------------------------------------------------
void BurnThrusterPanel::OnTextChange(wxCommandEvent &event)
{
   EnableUpdate(true);
}

//------------------------------------------------------------------------------
// void OnCheckBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void BurnThrusterPanel::OnCheckBoxChange(wxCommandEvent& event)
{
   if (decMassCheckBox->IsChecked())
   {
      tankLabel->Enable();
      tankComboBox->Enable();
      if (theObject->GetType() == Gmat::IMPULSIVE_BURN)
      {
         ispLabel->Enable();
         ispTextCtrl->Enable();
         ispUnit->Enable();
      }
   }
   else
   {
      tankLabel->Disable();
      tankComboBox->Disable();
      if (theObject->GetType() == Gmat::IMPULSIVE_BURN)
      {
         ispLabel->Disable();
         ispTextCtrl->Disable();
         ispUnit->Disable();
      }
   }
   
   EnableUpdate(true);
}

//------------------------------------------------------------------------------
// void OnComboBoxChange()
//------------------------------------------------------------------------------
void BurnThrusterPanel::OnComboBoxChange(wxCommandEvent &event)
{
   if (event.GetEventObject() == coordSysComboBox)
   {
      UpdateOriginAxes();      
      isCoordSysChanged =  true;
      coordSysName = coordSysComboBox->GetStringSelection().c_str();
      EnableUpdate(true);
   }
   else if (event.GetEventObject() == tankComboBox)
   {
      isTankChanged = true;
      tankName = tankComboBox->GetStringSelection().c_str();
      if (tankName == "No Fuel Tank Selected")
         tankName = "";
      
      // remove "No Tank Selected" once tank is selected
      int pos = tankComboBox->FindString("No Fuel Tank Selected");
      if (pos != wxNOT_FOUND)
         tankComboBox->Delete(pos);
      
      EnableUpdate(true);
   }
}

//------------------------------------------------------------------------------
// void OnButtonClick()
//------------------------------------------------------------------------------
void BurnThrusterPanel::OnButtonClick(wxCommandEvent &event)
{  
    if (event.GetEventObject() == cCoefButton)
    {
       ThrusterCoefficientDialog tcDlg(this, -1, "ThrusterCoefficientDialog", theObject, "C");
       tcDlg.ShowModal(); 
       EnableUpdate(true);      
    } 
    else if (event.GetEventObject() == kCoefButton)
    {
       ThrusterCoefficientDialog tcDlg(this, -1, "ImpulseCoefficientDialog", theObject, "K");
       tcDlg.ShowModal();
       EnableUpdate(true);
    }            
}


//------------------------------------------------------------------------------
// void UpdateOriginAxes()
//------------------------------------------------------------------------------
void BurnThrusterPanel::UpdateOriginAxes()
{
   if (coordSysComboBox->GetValue() == "Local")
   {
      originLabel->Enable();
      originComboBox->Enable();
      axisLabel->Enable();
      axesComboBox->Enable();
   }
   else
   {
      originLabel->Disable();
      originComboBox->Disable();
      axisLabel->Disable();
      axesComboBox->Disable();
   }
}
