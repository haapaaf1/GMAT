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
// Modified: 
//              2010.02.12 Thomas Grubb 
//                      - Added tooltips & accelerator keys
//                      - Added validators to numeric text controls
//                      - Added GmatStaticBoxSizers: fuelPropertiesSizer and tankPropertiesSizer
//                        and reordered controls
//              2009.05.27 Linda Jun - To derive from GmatBaseSetupPanel
/**
 * This class contains information needed to setup users spacecraft tank 
 * parameters.
 */
//------------------------------------------------------------------------------
#include "TankConfigPanel.hpp"
#include "MessageInterface.hpp"
#include "GmatStaticBoxSizer.hpp"


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
   int minLabelSize;
   Integer bsize = 2;
   Integer labelSizeProportion = 0;
   Integer ctrlSizeProportion = 1;
   Integer unitSizeProportion = 0;
   
   //-----------------------------------------------------------------
   // Create controls in tab order
   //-----------------------------------------------------------------
   // Volume
   wxStaticText *volumeLabel =
      new wxStaticText( this, ID_TEXT, wxT("&Volume")); 
   volumeTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   volumeTextCtrl->SetToolTip(wxT("The original volume of the fuel in the tank"));
   wxStaticText *volumeUnit =
      new wxStaticText( this, ID_TEXT, wxT("m^3"));
   
   // Pressure Model
   wxStaticText *pressureModelLabel =
      new wxStaticText( this, ID_TEXT, wxT("Pressure &Model"));
   Integer id = theFuelTank->GetParameterID("PressureModel");
   StringArray pressModelList =
      theFuelTank->GetPropertyEnumStrings(id);
   wxArrayString wxPressModelLabels = ToWxArrayString(pressModelList);
   pressureModelComboBox = 
      new wxComboBox( this, ID_COMBOBOX, wxT(""), wxDefaultPosition, wxSize(120,-1),
                      wxPressModelLabels, wxCB_DROPDOWN|wxCB_READONLY);
   pressureModelComboBox->SetToolTip(wxT("The pressure model for the fuel.  \n"
           "Pressure Regulated means that the pressure will be maintained as a constant during the duration of a thrust.  \n"
           "Blow down means that the pressure in the tank will decrease with time as more fuel is released by a thruster. "));
   
   // Fuel Mass
   wxStaticText *fuelMassLabel =
      new wxStaticText( this, ID_TEXT, wxT("&Fuel Mass"));
   fuelMassTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   fuelMassTextCtrl->SetToolTip(wxT("The total mass of fuel available in the fuel tank"));
   wxStaticText *fuelMassUnit =
      new wxStaticText( this, ID_TEXT, wxT("kg"));
   
   // Fuel Density
   wxStaticText *fuelDensityLabel =
      new wxStaticText( this, ID_TEXT, wxT("Fuel &Density"));
   fuelDensityTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   fuelDensityTextCtrl->SetToolTip(wxT("The density of the fuel"));
   wxStaticText *fuelDensityUnit =
      new wxStaticText( this, ID_TEXT, wxT("kg/m^3"));
   
   // Temperature
   wxStaticText *temperatureLabel =
      new wxStaticText( this, ID_TEXT, wxT("&Temperature"));
   temperatureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   temperatureTextCtrl->SetToolTip(wxT("The temperature of the fuel in the tank"));
   wxStaticText *temperatureUnit =
      new wxStaticText( this, ID_TEXT, wxT("C"));
   
   // Reference Temperature
   wxStaticText *refTemperatureLabel =
      new wxStaticText( this, ID_TEXT, wxT("&Reference Temperature"));
   refTemperatureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   wxStaticText *refTemperatureUnit =
      new wxStaticText( this, ID_TEXT, wxT("C"));
   
   // Pressure
   wxStaticText *pressureLabel =
      new wxStaticText( this, ID_TEXT, wxT("&Pressure"));
   pressureTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0, wxTextValidator(wxFILTER_NUMERIC) );
   pressureTextCtrl->SetToolTip(wxT("The pressure of the fuel in the tank"));
   wxStaticText *pressureUnit =
      new wxStaticText( this, ID_TEXT, wxT("kPa"));

   // set the min width for one of the labels for each GmatStaticBoxSizer
   minLabelSize = volumeLabel->GetBestSize().x;
   minLabelSize = (minLabelSize < pressureModelLabel->GetBestSize().x) ? pressureModelLabel->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < fuelMassLabel->GetBestSize().x) ? fuelMassLabel->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < fuelDensityLabel->GetBestSize().x) ? fuelDensityLabel->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < temperatureLabel->GetBestSize().x) ? temperatureLabel->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < refTemperatureLabel->GetBestSize().x) ? refTemperatureLabel->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < pressureLabel->GetBestSize().x) ? pressureLabel->GetBestSize().x : minLabelSize;

   volumeLabel->SetMinSize(wxSize(minLabelSize, volumeLabel->GetMinHeight()));
   fuelMassLabel->SetMinSize(wxSize(minLabelSize, fuelMassLabel->GetMinHeight()));

   // set the min width for one of the UNIT labels for each GmatStaticBoxSizer
   minLabelSize = volumeUnit->GetBestSize().x;
   minLabelSize = (minLabelSize < fuelMassUnit->GetBestSize().x) ? fuelMassUnit->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < fuelDensityUnit->GetBestSize().x) ? fuelDensityUnit->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < temperatureUnit->GetBestSize().x) ? temperatureUnit->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < refTemperatureUnit->GetBestSize().x) ? refTemperatureUnit->GetBestSize().x : minLabelSize;
   minLabelSize = (minLabelSize < pressureUnit->GetBestSize().x) ? pressureUnit->GetBestSize().x : minLabelSize;

   volumeUnit->SetMinSize(wxSize(minLabelSize, volumeUnit->GetMinHeight()));
   fuelMassUnit->SetMinSize(wxSize(minLabelSize, fuelMassUnit->GetMinHeight()));

   //-----------------------------------------------------------------
   // Add to fuel properties sizer
   //-----------------------------------------------------------------
   wxFlexGridSizer *flexGridSizer1 = new wxFlexGridSizer( 3, 0, 0 );
   //flexGridSizer1->AddGrowableCol(1);
   
   flexGridSizer1->Add(fuelMassLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(fuelMassTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer1->Add(fuelMassUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(fuelDensityLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(fuelDensityTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer1->Add(fuelDensityUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);

   flexGridSizer1->Add(temperatureLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(temperatureTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer1->Add(temperatureUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(refTemperatureLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(refTemperatureTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer1->Add(refTemperatureUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer1->Add(pressureLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer1->Add(pressureTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer1->Add(pressureUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   
   // create the Fuel Properties group box
   fuelPropertiesSizer = new GmatStaticBoxSizer( wxVERTICAL, this, "Fuel Properties" );
   fuelPropertiesSizer->Add(flexGridSizer1, 0, wxEXPAND|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Add to tank properties sizer
   //-----------------------------------------------------------------
   wxFlexGridSizer *flexGridSizer2 = new wxFlexGridSizer( 3, 0, 0 );
   //flexGridSizer2->AddGrowableCol(1);
   
   flexGridSizer2->Add(volumeLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer2->Add(volumeTextCtrl, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer2->Add(volumeUnit, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   
   flexGridSizer2->Add(pressureModelLabel, labelSizeProportion, wxALIGN_LEFT|wxALL, bsize);
   flexGridSizer2->Add(pressureModelComboBox, ctrlSizeProportion, wxGROW|wxALL, bsize);
   flexGridSizer2->Add(0, unitSizeProportion, wxALIGN_LEFT|wxALL, bsize);

   // create the Tank Properties group box
   tankPropertiesSizer = new GmatStaticBoxSizer( wxVERTICAL, this, "Tank Properties" );
   tankPropertiesSizer->Add(flexGridSizer2, 0, wxEXPAND|wxALL, bsize);

   //-----------------------------------------------------------------
   // Now put tank & fuel properties sizers into the middle sizer
   //-----------------------------------------------------------------
   theMiddleSizer->Add(tankPropertiesSizer, 0, wxEXPAND|wxALL, bsize);
   theMiddleSizer->Add(fuelPropertiesSizer, 1, wxEXPAND|wxALL, bsize);
   theMiddleSizer->SetSizeHints(this);
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
