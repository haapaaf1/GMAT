//$Id$
//------------------------------------------------------------------------------
//                              EndFiniteBurnPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc.
// Author: Linda Jun
// Created: 2006/07/14
//
/**
 * This class contains the EndFiniteBurn Setup window.
 */
//------------------------------------------------------------------------------

#include "EndFiniteBurnPanel.hpp"
#include "ParameterSelectDialog.hpp"
#include "StringTokenizer.hpp"          // for GetAllTokens()
#include "MessageInterface.hpp"
#include <algorithm>                    // for sort(), set_difference()

//#define DEBUG_ENDFBPANEL_CREATE
//#define DEBUG_ENDFBPANEL_SAVE

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EndFiniteBurnPanel, GmatPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   EVT_BUTTON(ID_BUTTON, EndFiniteBurnPanel::OnButtonClicked)
   EVT_COMBOBOX(ID_COMBOBOX, EndFiniteBurnPanel::OnComboBoxChange)
   EVT_TEXT(ID_TEXTCTRL, EndFiniteBurnPanel::OnTextUpdate)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// EndFiniteBurnPanel(wxWindow *parent)
//------------------------------------------------------------------------------
/**
 * Constructs EndFiniteBurnPanel object.
 *
 * @param <parent> input parent.
 *
 * @note Creates the maneuver dialog box
 */
//------------------------------------------------------------------------------
EndFiniteBurnPanel::EndFiniteBurnPanel(wxWindow *parent, GmatCommand *cmd)
   : GmatPanel(parent)

{
   theCommand = cmd;
   mObjectTypeList.Add("Spacecraft");
   
   if (theCommand != NULL)
   {
      Create();
      Show();
   }
}


//------------------------------------------------------------------------------
// ~EndFiniteBurnPanel()
//------------------------------------------------------------------------------
EndFiniteBurnPanel::~EndFiniteBurnPanel()
{
   theGuiManager->UnregisterComboBox("FiniteBurn", mFiniteBurnComboBox);
}


//-------------------------------
// protected methods
//-------------------------------

//------------------------------------------------------------------------------
// wxArrayString ToWxArrayString(const StringArray &array)
//------------------------------------------------------------------------------
/**
 * Converts std::string array to wxString array.
 */
//------------------------------------------------------------------------------
wxArrayString EndFiniteBurnPanel::ToWxArrayString(const StringArray &array)
{
   wxArrayString newArray;
   for (UnsignedInt i=0; i<array.size(); i++)
      newArray.Add(array[i].c_str());

   return newArray;
}


//------------------------------------------------------------------------------
// wxString ToWxString(const wxArrayString &names)
//------------------------------------------------------------------------------
/**
 * Converts wxString array to wxString separated by comma.
 */
//------------------------------------------------------------------------------
wxString EndFiniteBurnPanel::ToWxString(const wxArrayString &names)
{
   wxString str = "";
   wxString delimiter = ", ";
   if (names.Count() > 0)
   {
      str = names[0];
      
      for (unsigned int i=1; i<names.Count(); i++)
         str = str + delimiter + names[i];
   }
   
   return str;
}


//----------------------------------
// methods inherited from GmatPanel
//----------------------------------

//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
/**
 *
 * @note Creates the panel for the Maneuver Command
 */
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::Create()
{
   #ifdef DEBUG_ENDFBPANEL_CREATE
   MessageInterface::ShowMessage
      ("EndFiniteBurnPanel::Create() Entered. command=%s\n",
       theCommand->GetTypeName().c_str());
   #endif
   
   int bsize = 3;
   
   //----------------------------------------------------------------------
   // Burns
   //----------------------------------------------------------------------
   // create burn label
   wxStaticText *burnLabel =
      new wxStaticText(this, ID_TEXT,
                       wxT("Burn"), wxDefaultPosition, wxSize(50, -1));
   
   #ifdef DEBUG_ENDFBPANEL_CREATE
   MessageInterface::ShowMessage
      ("EndFiniteBurnPanel::Create() Calling theGuiManager->"
       "GetFiniteBurnComboBox()\n");
   #endif
   
   // create finite burn combo box
   mFiniteBurnComboBox =
      theGuiManager->GetFiniteBurnComboBox(this, ID_COMBOBOX, wxSize(150,-1));
   
   wxBoxSizer *burnSizer = new wxBoxSizer(wxHORIZONTAL);
   burnSizer->Add(burnLabel, 0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, bsize);
   burnSizer->Add(mFiniteBurnComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //----------------------------------------------------------------------
   // Spacecraft
   //----------------------------------------------------------------------
   
   wxStaticText *satLabel =
      new wxStaticText(this, ID_TEXT,
                       wxT("Spacecraft"), wxDefaultPosition, wxSize(50,-1));
   mSatTextCtrl =
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(150,-1));
   
   wxButton *selectSatButton =
      new wxButton(this, ID_BUTTON, wxT("Select"), wxDefaultPosition, wxDefaultSize);
   
   // add spacecraft textbox and select button to sizer
   wxBoxSizer *satSelectSizer = new wxBoxSizer(wxVERTICAL);   
   satSelectSizer->Add(mSatTextCtrl, 0, wxALIGN_CENTER|wxGROW|wxALL, bsize);
   satSelectSizer->Add(selectSatButton, 0, wxALIGN_CENTER|wxALL, bsize);
   
   wxBoxSizer *satSizer = new wxBoxSizer(wxHORIZONTAL);
   satSizer->Add(satLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   satSizer->Add(satSelectSizer, 1, wxALIGN_LEFT|wxALL, bsize);
   
   // add items to page sizer
   wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
   pageSizer->Add(burnSizer, 0, wxGROW|wxALIGN_LEFT|wxALL, 6);
   pageSizer->Add(satSizer, 1, wxGROW|wxALIGN_LEFT|wxALL, bsize);
   
   // add to middle sizer
   theMiddleSizer->Add(pageSizer, 0, wxGROW|wxALIGN_CENTRE|wxALL, bsize);     
   
   #ifdef DEBUG_ENDFBPANEL_CREATE
   MessageInterface::ShowMessage("EndFiniteBurnPanel::Create() Exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// virtual void LoadData()
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::LoadData()
{
   // Set the pointer for the "Show Script" button
   mObject = theCommand;
   
   try
   {
      // Get FiniteBurn from the command
      std::string burnName = theCommand->GetRefObjectName(Gmat::FINITE_BURN);
      
      #if DEBUG_BEGIN_FINITE_BURN
      MessageInterface::ShowMessage
         ("EndFiniteBurnPanel::LoadData() burnName=<%s>\n", burnName.c_str());
      #endif
      
      mFiniteBurnComboBox->SetValue(burnName.c_str());
      
      // Get spacecraft list from the command
      StringArray scNames = theCommand->GetRefObjectNameArray(Gmat::SPACECRAFT);
      mSpacecraftList = ToWxArrayString(scNames);
      wxString scList = ToWxString(mSpacecraftList);
      mSatTextCtrl->SetValue(scList);
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
}


//------------------------------------------------------------------------------
// virtual void SaveData()
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::SaveData()
{
   #ifdef DEBUG_ENDFBPANEL_SAVE
   MessageInterface::ShowMessage("EndFiniteBurnPanel::SaveData() entered\n");
   #endif
   
   canClose = true;
   wxString satNames = mSatTextCtrl->GetValue();
   
   //-----------------------------------------------------------------
   // check empty spacecraft list
   //-----------------------------------------------------------------
   if (satNames == "")
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "Please enter Spacecrafts to begin maneuver\n");
      canClose = false;
      return;
   }
   
   // In case user typed in spacecraft names, get value from textbox and
   // parse by blank or comma
   std::string scNames = satNames.c_str();
   StringTokenizer st(scNames, " ,");
   StringArray scList = st.GetAllTokens();
   #ifdef DEBUG_ENDFBPANEL_SAVE
   for (UnsignedInt i=0; i<scList.size(); i++)
      MessageInterface::ShowMessage
         ("   selected spacecraft[%d] = '%s'\n", i, scList[i].c_str());
   #endif
   
   //-----------------------------------------------------------------
   // check unknown spacecraft names
   //-----------------------------------------------------------------
   StringArray configList = theGuiInterpreter->GetListOfObjects(Gmat::SPACECRAFT);
   StringArray result;
   sort(scList.begin(), scList.end());
   sort(configList.begin(), configList.end());
   set_difference(scList.begin(), scList.end(), configList.begin(),
                  configList.end(), back_inserter(result));
   
   #ifdef DEBUG_ENDFBPANEL_SAVE
   for (UnsignedInt i=0; i<result.size(); i++)
      MessageInterface::ShowMessage("   sc not configured[%d] = '%s'\n", i, result[i].c_str());
   #endif
   
   if (result.size() > 0)
   {
      std::string scLabel = "The spacecraft \"";
      std::string desc = "\" is undefined.\n";
      if (result.size() > 1)
      {
         scLabel = "The spacecrafts \"";
         desc = "\" are undefined.\n";
      }
      
      std::string unknownSc = (ToWxString(ToWxArrayString(result))).c_str();
      std::string msg = scLabel + unknownSc + desc;
      MessageInterface::PopupMessage(Gmat::ERROR_, msg);
      canClose = false;
      return;
   }
   
   //-----------------------------------------------------------------
   // save values to base, base code should do the range checking
   //-----------------------------------------------------------------
   try
   {
      // save finite burn
      wxString burnString = mFiniteBurnComboBox->GetValue();
      theCommand->SetRefObjectName(Gmat::FINITE_BURN, burnString.c_str());
      
      // save spacecrafts
      int count = scList.size();
      theCommand->TakeAction("Clear");
      
      for (int i=0; i<count; i++)
         theCommand->SetRefObjectName(Gmat::SPACECRAFT, scList[i]);
      
      mSpacecraftList = ToWxArrayString(scList);
      
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
   
   #ifdef DEBUG_ENDFBPANEL_SAVE
   MessageInterface::ShowMessage("EndFiniteBurnPanel::SaveData() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// void OnButtonClicked(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::OnButtonClicked(wxCommandEvent& event)
{
   // Allow multiple selection on spacecraft
   ParameterSelectDialog paramDlg(this, mObjectTypeList,
                                  GuiItemManager::SHOW_WHOLE_OBJECT_ONLY, true,
                                  false, true, false, false, false, "Spacecraft");
   
   paramDlg.SetParamNameArray(mSpacecraftList);
   paramDlg.ShowModal();
   
   if (paramDlg.HasSelectionChanged())
   {
      EnableUpdate(true);
      wxArrayString satNames = paramDlg.GetParamNameArray();
      wxString value = ToWxString(satNames);
      mSatTextCtrl->SetValue(value);
   }
}


//------------------------------------------------------------------------------
// void OnComboBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::OnComboBoxChange(wxCommandEvent& event)
{
   EnableUpdate(true);
}

//------------------------------------------------------------------------------
// void OnTextUpdate(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EndFiniteBurnPanel::OnTextUpdate(wxCommandEvent& event)
{
   EnableUpdate(true);
}

