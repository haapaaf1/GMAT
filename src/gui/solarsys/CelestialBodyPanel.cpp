//$Header$
//------------------------------------------------------------------------------
//                              CelestialBodyPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Wendy C. Shoan (original author: Allison Greene 2005.07.06)
// Created: 2009.01.26
//
/**
 * Implements CelestialBodyPanel class.
 */
//------------------------------------------------------------------------------

#include "CelestialBodyPanel.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"
#include "RealUtilities.hpp"
#include <stdlib.h>


//#define DEBUG_PARAM_PANEL 1
//#define DEBUG_CELESBODY_SAVE 1

//const std::string
//CelestialBodyPanel::KEP_ELEMENT_NAMES[6] =
//{
//   "SMA",
//   "ECC",
//   "INC",
//   "RAAN",
//   "AOP",
//   "TA",
//};
//
//const std::string
//CelestialBodyPanel::KEP_ELEMENT_UNITS[6] =
//{
//   "km",
//   "",
//   "deg",
//   "deg",
//   "deg",
//   "deg",
//};

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(CelestialBodyPanel, GmatPanel)
//   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
//   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
//   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
//   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
//   
////   EVT_TEXT(ID_TEXTCTRL, CelestialBodyPanel::OnTextUpdate)
////   EVT_COMBOBOX(ID_COMBO, CelestialBodyPanel::OnComboBoxChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// CelestialBodyPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
CelestialBodyPanel::CelestialBodyPanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent, true)
{
   theCelestialBody = (CelestialBody*)
      theGuiInterpreter->GetConfiguredObject(name.c_str());
           
   bodyName = name.c_str();
   
   if (theCelestialBody)
   {
      isUserDefined = theCelestialBody->IsUserDefined();
            
      Create();
      Show();
   }
}

//------------------------------------------------------------------------------
// ~CelestialBodyPanel()
//------------------------------------------------------------------------------
/**
 * The destructor.
 */
//------------------------------------------------------------------------------
CelestialBodyPanel::~CelestialBodyPanel()
{
   //delete child panels (tabs) in the Mainframe
}


//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void CelestialBodyPanel::Create()
{
   try
   {
      cbNotebook = new wxNotebook(this, ID_NOTEBOOK, wxDefaultPosition, 
                   wxDefaultSize, wxGROW);
      cbNotebook->SetBackgroundColour(GetBackgroundColour());
      cbNotebook->SetForegroundColour(GetForegroundColour());
      
      // create properties, orbit, and orientation panels
      properties       = new CelestialBodyPropertiesPanel(this, cbNotebook, theCelestialBody);
      orbit            = new CelestialBodyOrbitPanel(this, cbNotebook, theCelestialBody);
      orientation      = new CelestialBodyOrientationPanel(this, cbNotebook, theCelestialBody);
      
      // add panels to notebook
      cbNotebook->AddPage(properties, wxT("Properties"));
      cbNotebook->AddPage(orbit, wxT("Orbit"));
      cbNotebook->AddPage(orientation, wxT("Orientation"));
      
      theMiddleSizer->Add(cbNotebook, 1, wxGROW, 1);  // 3?
   }
   catch (BaseException &be)
   {
      MessageInterface::ShowMessage("Error creating the CelestialBodyPanel: %s\n",
                                   (be.GetFullMessage()).c_str());
   }
}

//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void CelestialBodyPanel::LoadData()
{
   try
   {
      properties->LoadData();
      orbit->LoadData();
      orientation->LoadData();
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("CelestialBodyPanel:LoadData() error occurred!\n%s\n",
            e.GetFullMessage().c_str());
   }

   // Activate "ShowScript"
   mObject = theCelestialBody;
   
   EnableUpdate(false);
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void CelestialBodyPanel::SaveData()
{
   #if DEBUG_CELESBODY_SAVE
//   MessageInterface::ShowMessage
//      ("CelestialBodyPanel::SaveData() isTextModified=%d, isStateTextModified=%d,"
//       " isRotDataSourceChanged=%d\n", isTextModified, isStateTextModified,
//       isRotDataSourceChanged);
   #endif
   
   canClose = true;
   
   if (properties->IsDataChanged())
   {
      properties->SaveData();
      canClose = properties->CanClosePanel();
   }
   if (orbit->IsDataChanged())
   {
      orbit->SaveData();
      canClose = canClose && orbit->CanClosePanel();
   }
   if (orientation->IsDataChanged())
   {
      orientation->SaveData();
      canClose = canClose && orientation->CanClosePanel();
   }
   
   if (!canClose)   // why do this???? spaceraft panel did this ....
   {
      EnableUpdate(true);
      return;
   }
   EnableUpdate(false);

}

void CelestialBodyPanel::OnPageChange(wxCommandEvent &event)
{
   properties->LoadData();
   orbit->LoadData();
   orientation->LoadData();
}
