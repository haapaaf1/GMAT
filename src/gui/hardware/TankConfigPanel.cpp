//$Id$
//------------------------------------------------------------------------------
//                            TankConfigPanel
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
// Author: Waka Waktola
// Created: 2004/11/19
// Modified: 2009.05.27 Linda Jun - To derive from GmatBaseSetupPanel
/**
 * This class contains information needed to setup users spacecraft tank 
 * parameters.
 */
//------------------------------------------------------------------------------
#include "TankConfigPanel.hpp"
#include "MessageInterface.hpp"


//====================================================================
#ifndef __USE_OLD_TANK_PANEL__
//====================================================================

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// TankConfigPanel(wxWindow *parent, const wxString &name)
//------------------------------------------------------------------------------
/**
 * Constructs TankConfigPanel object.
 */
//------------------------------------------------------------------------------
TankConfigPanel::TankConfigPanel(wxWindow *parent, const wxString &name)
   : GmatBaseSetupPanel(parent, name)
{           
}


//------------------------------------------------------------------------------
// ~TankConfigPanel()
//------------------------------------------------------------------------------
TankConfigPanel::~TankConfigPanel()
{
}


//====================================================================
#else
//====================================================================


#include "StringUtil.hpp"
#include <wx/variant.h>

//------------------------------
// event tables for wxWindows
//------------------------------
BEGIN_EVENT_TABLE(TankConfigPanel, wxPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   EVT_TEXT(ID_TEXTCTRL, TankConfigPanel::OnTextChange)
   EVT_COMBOBOX(ID_COMBOBOX, TankConfigPanel::OnComboBoxChange)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// TankConfigPanel(wxWindow *parent, const wxString &name)
//------------------------------------------------------------------------------
/**
 * Constructs TankConfigPanel object.
 */
//------------------------------------------------------------------------------
TankConfigPanel::TankConfigPanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent)
{           
   std::string tankName = std::string(name.c_str());
   theFuelTank = (FuelTank*)theGuiInterpreter->GetConfiguredObject(tankName);
   
   if (theFuelTank)
   {
      Create();
      Show();
   }
   else
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "Cannot find the FuelTank object named " + tankName);
   }
}

//------------------------------------------------------------------------------
// ~TankConfigPanel()
//------------------------------------------------------------------------------
TankConfigPanel::~TankConfigPanel()
{
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
void TankConfigPanel::Create()
{
   // Border size
   Integer bsize = 2;
   
   // Fuel Mass
   wxStaticText *fuelMassLabel =
      new wxStaticText( this, ID_TEXT, wxT("Fuel Mass"));
   fuelMassTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *fuelMassUnit =
      new wxStaticText( this, ID_TEXT, wxT("kg"));
   
   // Pressure
   wxStaticText *pressureLabel =
      new wxStaticText( this, ID_TEXT, wxT("Pressure"));
   pressureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *pressureUnit =
      new wxStaticText( this, ID_TEXT, wxT("kPa"));
   
   // Temperature
   wxStaticText *temperatureLabel =
      new wxStaticText( this, ID_TEXT, wxT("Temperature"));
   temperatureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *temperatureUnit =
      new wxStaticText( this, ID_TEXT, wxT("C"));
   
   // Reference Temperature
   wxStaticText *refTemperatureLabel =
      new wxStaticText( this, ID_TEXT, wxT("Reference Temperature"));
   refTemperatureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *refTemperatureUnit =
      new wxStaticText( this, ID_TEXT, wxT("C"));
   
   // Volume
   wxStaticText *volumeLabel =
      new wxStaticText( this, ID_TEXT, wxT("Volume")); 
   volumeTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *volumeUnit =
      new wxStaticText( this, ID_TEXT, wxT("m^3"));
   
   // Fuel Density
   wxStaticText *fuelDensityLabel =
      new wxStaticText( this, ID_TEXT, wxT("Fuel Density"));
   fuelDensityTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0 );
   wxStaticText *fuelDensityUnit =
      new wxStaticText( this, ID_TEXT, wxT("kg/m^3"));
   
   // Pressure Model
   wxStaticText *pressureModelLabel =
      new wxStaticText( this, ID_TEXT, wxT("Pressure Model"));
   Integer id = theFuelTank->GetParameterID("PressureModel");
   StringArray pressModelList =
      theFuelTank->GetPropertyEnumStrings(id);
   wxArrayString wxPressModelLabels = ToWxArrayString(pressModelList);
   pressureModelComboBox = 
      new wxComboBox( this, ID_COMBOBOX, wxT(""), wxDefaultPosition, wxSize(120,-1),
                      wxPressModelLabels, wxCB_DROPDOWN|wxCB_READONLY);
   
   //-----------------------------------------------------------------
   // Add to sizer
   //-----------------------------------------------------------------
   wxFlexGridSizer *flexGridSizer1 = new wxFlexGridSizer( 3, 0, 0 );
   
   flexGridSizer1->Add(fuelMassLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(fuelMassTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(fuelMassUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(pressureLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(pressureTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(pressureUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(temperatureLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(temperatureTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(temperatureUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(refTemperatureLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(refTemperatureTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(refTemperatureUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(volumeLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(volumeTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(volumeUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(fuelDensityLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(fuelDensityTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(fuelDensityUnit, 0, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(pressureModelLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(pressureModelComboBox, 0, wxALIGN_CENTER|wxALL, bsize);
   flexGridSizer1->Add(0, 0, wxALIGN_LEFT|wxALL, bsize);
   
   theMiddleSizer->Add(flexGridSizer1, 0, wxALIGN_CENTRE|wxALL, bsize);
}

//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void TankConfigPanel::LoadData()
{ 
   if (theFuelTank == NULL)
      return; 
   
   Integer paramID;
   
   // Set object pointer for "Show Script"
   mObject = theFuelTank;

   try
   {
      paramID = theFuelTank->GetParameterID("FuelMass");
      fuelMassTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("Pressure");
      pressureTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("Temperature");
      temperatureTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("RefTemperature");
      refTemperatureTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("Volume");
      volumeTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("FuelDensity");
      fuelDensityTextCtrl->SetValue(wxVariant(theFuelTank->GetRealParameter(paramID)));
      
      paramID = theFuelTank->GetParameterID("PressureModel");      
      pressureModelComboBox->SetValue(theFuelTank->GetStringParameter(paramID));
      
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void TankConfigPanel::SaveData()
{
   canClose = true;
   
   Integer paramID;
   Real fuelMass, pressure, temp, refTemp, volume, fuelDensity;
   std::string inputString;
   
   //-----------------------------------------------------------------
   // validate user input for non-Real value
   //-----------------------------------------------------------------
   // Fuel Mass 
   inputString = fuelMassTextCtrl->GetValue();
   CheckReal(fuelMass, inputString, "FuelMass", "Real Number >= 0.0");
   
   // Pressure 
   inputString = pressureTextCtrl->GetValue();
   CheckReal(pressure, inputString, "Pressure", "Real Number >= 0.0");
   
   // Temperature
   inputString = temperatureTextCtrl->GetValue(); 
   CheckReal(temp, inputString, "Temperature", "Real Number");
   
   // Reference Temperature
   inputString = refTemperatureTextCtrl->GetValue();
   CheckReal(refTemp, inputString, "RefTemperature", "Real Number");
   
   // Volume
   inputString = volumeTextCtrl->GetValue();
   CheckReal(volume, inputString, "Volume", "Real Number >= 0.0");
   
   // Fuel Density 
   inputString = fuelDensityTextCtrl->GetValue();
   CheckReal(fuelDensity, inputString, "FuelDensity", "Real Number >= 0.0");
   
   
   if (!canClose)
      return;
   
   //-----------------------------------------------------------------
   // save values to base, base code should do the range checking
   //-----------------------------------------------------------------
   try
   {
      // Fuel Mass
      paramID = theFuelTank->GetParameterID("FuelMass");
      theFuelTank->SetRealParameter(paramID, fuelMass);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   try
   {
      // Pressure 
      paramID = theFuelTank->GetParameterID("Pressure");
      theFuelTank->SetRealParameter(paramID, pressure);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   try
   {
      // Temperature
      paramID = theFuelTank->GetParameterID("Temperature");
      theFuelTank->SetRealParameter(paramID, temp);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   try
   {
      // Reference Temperature
      paramID = theFuelTank->GetParameterID("RefTemperature");
      theFuelTank->SetRealParameter(paramID, refTemp);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   try
   {
      // Volume 
      paramID = theFuelTank->GetParameterID("Volume");
      theFuelTank->SetRealParameter(paramID, volume);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   try
   {
      // Pressure Model
      paramID = theFuelTank->GetParameterID("PressureModel");
      std::string modelName = pressureModelComboBox->GetValue().c_str();
      theFuelTank->SetStringParameter(paramID, modelName);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, ex.GetFullMessage());
      canClose = false;
   }
   
   EnableUpdate(false);
}


//------------------------------------------------------------------------------
// void OnTextChange()
//------------------------------------------------------------------------------
void TankConfigPanel::OnTextChange(wxCommandEvent &event)
{
    EnableUpdate(true);
}    


//------------------------------------------------------------------------------
// void OnTextChange()
//------------------------------------------------------------------------------
void TankConfigPanel::OnComboBoxChange(wxCommandEvent &event)
{
    EnableUpdate(true);
}    

//====================================================================
#endif
//====================================================================
