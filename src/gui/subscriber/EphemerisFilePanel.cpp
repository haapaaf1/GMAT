//$Id$
//------------------------------------------------------------------------------
//                           EphemerisFilePanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Tuan Dang Nguyen
// Created: 2010/02/02
/**
 * This class is a generic setup panel used by objects derived from GmatBase
 */
//------------------------------------------------------------------------------

#include "EphemerisFilePanel.hpp"
#include "MessageInterface.hpp"


/// wxWidget event mappings for the panel
BEGIN_EVENT_TABLE(EphemerisFilePanel, GmatPanel)
   EVT_COMBOBOX(ID_COMBOBOX, EphemerisFilePanel::OnComboBoxChange)
   EVT_TEXT(ID_COMBOBOX, EphemerisFilePanel::OnComboBoxTextChange)
   EVT_TEXT(ID_TEXTCTRL, EphemerisFilePanel::OnTextChange)
   EVT_BUTTON(ID_BUTTON_BROWSE, EphemerisFilePanel::OnBrowse)
   EVT_CHECKBOX(ID_CHECKBOX, EphemerisFilePanel::OnCheck)
END_EVENT_TABLE()


//-----------------------------------------
// public methods
//-----------------------------------------

//------------------------------------------------------------------------------
// EphemerisFilePanel(wxWindow *parent, const wxString &name)
//------------------------------------------------------------------------------
/**
 * Panel constructor
 * 
 * @param parent Owner for this panel
 * @param name Name of the object that is to be configured 
 */
//------------------------------------------------------------------------------
EphemerisFilePanel::EphemerisFilePanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent)
{
   fileDialog = NULL;
   
   mObject = theGuiInterpreter->GetConfiguredObject(name.c_str());
   
   if (mObject != NULL)
   {
      Create();
      Show();
   }
   else
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "The object named \"%s\" does not exist\n", name.c_str());      
   }
}


//------------------------------------------------------------------------------
// ~EphemerisFilePanel()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
EphemerisFilePanel::~EphemerisFilePanel()
{
   // Unregister automatically registered ComboBoxes
   std::map<wxString, wxComboBox *>::iterator iter;
   for (iter = managedComboBoxMap.begin(); iter != managedComboBoxMap.end(); ++iter)
      theGuiManager->UnregisterComboBox(iter->first, iter->second);
}


//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
/**
 * Inherited function that is called to create the panel.
 */
//------------------------------------------------------------------------------

void EphemerisFilePanel::Create()
{
   Integer bsize = 2;              // border size
   Integer id;  
   
   // 1. Create Options box:
   wxStaticBoxSizer *optionsStaticBoxSizer = new wxStaticBoxSizer(wxHORIZONTAL, this, "Options");
   
   wxFlexGridSizer *grid1 = new wxFlexGridSizer( 2, 0, 0 );
   grid1->AddGrowableCol(1);
   
   id = mObject->GetParameterID("Spacecraft");
   wxStaticText * spacecraftStaticText =
      new wxStaticText(this, ID_TEXT, wxT("Spacecraft"), wxDefaultPosition, wxDefaultSize, 0 );
   spacecraftComboBox = (wxComboBox*)BuildControl(this, id);
   grid1->Add(spacecraftStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid1->Add(spacecraftComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("StateType");
   wxStaticText * stateTypeStaticText =
      new wxStaticText(this, ID_TEXT, wxT("State Type"), wxDefaultPosition, wxDefaultSize, 0 );
   stateTypeComboBox = (wxComboBox*) BuildControl(this, id);
   grid1->Add(stateTypeStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid1->Add(stateTypeComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("CoordinateSystem");
   wxStaticText * coordinateSystemStaticText =
      new wxStaticText(this, ID_TEXT, wxT("Coordinate System"), wxDefaultPosition, wxDefaultSize, 0 );
   coordinateSystemComboBox = (wxComboBox*) BuildControl(this, id);
   grid1->Add(coordinateSystemStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid1->Add(coordinateSystemComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("WriteEphemeris");
   writeEphemerisCheckBox = (wxCheckBox*) BuildControl(this, id);
   grid1->Add(writeEphemerisCheckBox, 0, wxALIGN_LEFT|wxALL, bsize );
   grid1->Add(0, 0, wxALIGN_LEFT|wxALL, bsize );
   
   optionsStaticBoxSizer->Add( grid1, 0, wxALIGN_LEFT|wxALL, bsize );
   
   
   // 2. Create File Settings box:
   wxStaticBoxSizer *fileSettingsStaticBoxSizer =
      new wxStaticBoxSizer(wxHORIZONTAL, this, "File Settings");
   wxFlexGridSizer *grid2 = new wxFlexGridSizer( 3, 0, 0 );
   grid2->AddGrowableCol(1);
   
   id = mObject->GetParameterID("FileFormat");
   wxStaticText * fileFormatStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("File Format"), wxDefaultPosition, wxDefaultSize, 0 );
   fileFormatComboBox = (wxComboBox*) BuildControl(this, id);
   grid2->Add(fileFormatStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(fileFormatComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(0, 0, wxALIGN_CENTER|wxALL, bsize );
   
   id = mObject->GetParameterID("FileName");
   wxStaticText * fileNameStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("File Name"), wxDefaultPosition, wxDefaultSize, 0 );
   fileNameTextCtrl = (wxTextCtrl*) BuildControl(this, id);
   browseButton = new wxButton(this, ID_BUTTON_BROWSE, wxT("Browse"));
   grid2->Add(fileNameStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(fileNameTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(browseButton, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("Interpolator");
   wxStaticText * interpolatorStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Interpolator"), wxDefaultPosition, wxDefaultSize, 0 );
   interpolatorComboBox = (wxComboBox*) BuildControl(this, id);
   grid2->Add(interpolatorStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(interpolatorComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(0, 0, wxALIGN_CENTER|wxALL, bsize );
   
   id = mObject->GetParameterID("InterpolationOrder");
   wxStaticText * interpolationOrderStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Interpolation Order"), wxDefaultPosition, wxDefaultSize, 0 );
   interpolationOrderTextCtrl = (wxTextCtrl*) BuildControl(this, id);
   grid2->Add(interpolationOrderStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(interpolationOrderTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(0, 0, wxALIGN_CENTER|wxALL, bsize );
   
   id = mObject->GetParameterID("StepSize");
   wxStaticText * stepSizeStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Step Size"), wxDefaultPosition, wxDefaultSize, 0 );
   stepSizeComboBox = (wxComboBox*) BuildControl(this, id);
   grid2->Add(stepSizeStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(stepSizeComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   grid2->Add(0, 0, wxALIGN_CENTER|wxALL, bsize );
   
   fileSettingsStaticBoxSizer->Add( grid2, 0, wxALIGN_LEFT|wxALL, bsize );
   
   
   // 3. Create Epoch box:
   wxStaticBoxSizer *epochStaticBoxSizer = new wxStaticBoxSizer(wxHORIZONTAL, this, "Epoch");
   wxFlexGridSizer *grid3 = new wxFlexGridSizer( 2, 0, 0 );
   grid3->AddGrowableCol(1);
   
   id = mObject->GetParameterID("EpochFormat");
   wxStaticText * epochFormatStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Epoch Format"), wxDefaultPosition, wxDefaultSize, 0 );
   epochFormatComboBox = (wxComboBox*) BuildControl(this, id);
   grid3->Add(epochFormatStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid3->Add(epochFormatComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("InitialEpoch");
   wxStaticText * initialEpochStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Initial Epoch"), wxDefaultPosition, wxDefaultSize, 0 );
   initialEpochComboBox = (wxComboBox*) BuildControl(this, id);
   grid3->Add(initialEpochStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid3->Add(initialEpochComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   id = mObject->GetParameterID("FinalEpoch");
   wxStaticText * finalEpochStaticText =
      new  wxStaticText(this, ID_TEXT, wxT("Final Epoch"), wxDefaultPosition, wxDefaultSize, 0 );
   finalEpochComboBox = (wxComboBox*) BuildControl(this, id);
   grid3->Add(finalEpochStaticText, 0, wxALIGN_LEFT|wxALL, bsize );
   grid3->Add(finalEpochComboBox, 0, wxALIGN_LEFT|wxALL, bsize );
   
   epochStaticBoxSizer->Add( grid3, 0, wxALIGN_LEFT|wxALL, bsize );
   
   // 4. Create theMiddleSizer:
   theMiddleSizer->Add(optionsStaticBoxSizer, 0, wxGROW, bsize);
   theMiddleSizer->Add(fileSettingsStaticBoxSizer, 0, wxGROW, bsize);
   theMiddleSizer->Add(epochStaticBoxSizer, 0, wxGROW, bsize);
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
/**
 * Populates the panel with the configurable property data in the Solver 
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::LoadData()
{
   // load data from the core engine
   try
   {
      LoadControl("Spacecraft");
      LoadControl("StateType");
      LoadControl("CoordinateSystem");
      LoadControl("WriteEphemeris");
      LoadControl("FileFormat");
      LoadControl("FileName");
      LoadControl("Interpolator");
      LoadControl("InterpolationOrder");
      LoadControl("StepSize");
      LoadControl("EpochFormat");
      LoadControl("InitialEpoch");
      LoadControl("FinalEpoch");
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("EphemerisFilePanel:LoadData() error occurred!\n%s\n",
          e.GetFullMessage().c_str());
   }
   
   // explicitly disable apply button
   // it is turned on in each of the panels
   EnableUpdate(false);
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
/**
 * Passes configuration data from the panel to the Solver object
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::SaveData()
{
   canClose = true;
   
   try
   {
      SaveControl("Spacecraft");
      SaveControl("StateType");
      SaveControl("CoordinateSystem");
      SaveControl("WriteEphemeris");
      SaveControl("FileFormat");
      SaveControl("FileName");
      SaveControl("Interpolator");
      SaveControl("InterpolationOrder");
      SaveControl("StepSize");
      SaveControl("EpochFormat");
      SaveControl("InitialEpoch");
      SaveControl("FinalEpoch");
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
      canClose = false;
      return;
   }
}


//------------------------------------------------------------------------------
// wxControl* BuildControl(wxWindow *parent, Integer index)
//------------------------------------------------------------------------------
/**
 * Builds a wxWidget control for an object property
 * 
 * @param parent The window that owns the control
 * @param index The index for the property that the constructed control
 *              represents
 * 
 * @return The new control
 */
//------------------------------------------------------------------------------
wxControl *EphemerisFilePanel::BuildControl(wxWindow *parent, Integer index)
{
   wxControl *control = NULL;
   
   Gmat::ParameterType type = mObject->GetParameterType(index);
   
   switch (type)
   {
   case Gmat::BOOLEAN_TYPE:
      {
         wxCheckBox *cbControl;
         if (mObject->GetParameterText(index) == "WriteEphemeris")
            cbControl = new wxCheckBox(parent, ID_CHECKBOX, "Write Ephemeris");
         else
            cbControl = new wxCheckBox(parent, ID_CHECKBOX, (mObject->GetParameterText(index)).c_str());
         
         cbControl->SetValue(mObject->GetBooleanParameter(index));
         control = cbControl;
      }
      break;
   case Gmat::OBJECT_TYPE:
      {
         Gmat::ObjectType type = mObject->GetPropertyObjectType(index);
         if (type == Gmat::SPACE_POINT)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
               theGuiManager->GetSpacePointComboBox(this, ID_COMBOBOX,
                                                    wxSize(180,-1), false);
            managedComboBoxMap.insert(std::make_pair("SpacePoint", cbControl));
            control = cbControl;
         }
         else if (type == Gmat::CELESTIAL_BODY)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
            theGuiManager->GetCelestialBodyComboBox(this, ID_COMBOBOX,
                                                    wxSize(180,-1));
            managedComboBoxMap.insert(std::make_pair("CelestialBody", cbControl));
            control = cbControl;
         }
         else if (type == Gmat::SPACECRAFT)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
               theGuiManager->GetSpacecraftComboBox(this, ID_COMBOBOX,
                                                    wxSize(180,-1));
            managedComboBoxMap.insert(std::make_pair("Spacecraft", cbControl));
            control = cbControl;
         }
         else if (type == Gmat::COORDINATE_SYSTEM)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
               theGuiManager->GetCoordSysComboBox(this, ID_COMBOBOX,
                                                  wxSize(180,-1));
            managedComboBoxMap.insert(std::make_pair("CoordinateSystem", cbControl));
            control = cbControl;
         }
         else
         {
            // Check if enumeration strings available for owned object types
            wxArrayString enumList;
            StringArray enumStrings = mObject->GetPropertyEnumStrings(index);
            for (UnsignedInt i=0; i<enumStrings.size(); i++)
               enumList.Add(enumStrings[i].c_str());
            
            control = new wxComboBox(parent, ID_COMBOBOX, wxT(""),
                                     wxDefaultPosition, wxSize(180,-1), enumList,
                                     wxCB_READONLY);
         }
      }
      break;
   case Gmat::ENUMERATION_TYPE:
      {
         StringArray enumStrings = mObject->GetPropertyEnumStrings(index);
         wxArrayString enumList;
         for (UnsignedInt i=0; i<enumStrings.size(); i++)
            enumList.Add(enumStrings[i].c_str());
         
         wxComboBox *cbControl = NULL;
         if (enumStrings.size() == 1)
         {
            // Show the value even if value is not in the list
            cbControl =
               new wxComboBox(parent, ID_COMBOBOX, "", wxDefaultPosition,
                              wxSize(180,-1), enumList, 0);
         }
         else
         {
            // Do not show the value if value is not in the list
            cbControl =
               new wxComboBox(parent, ID_COMBOBOX, "", wxDefaultPosition,
                              wxSize(180,-1), enumList, wxCB_READONLY);
         }
         
         control = cbControl;
      }
      break;
      
   case Gmat::STRING_TYPE:
   default:
      control = new wxTextCtrl(parent, ID_TEXTCTRL, 
                               wxT(""), wxDefaultPosition, wxSize(180,-1));
      break;
   }
   
   return control;
}


//------------------------------------------------------------------------------
// void LoadControl(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Sets the data for a control
 * 
 * @param label The control's label
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::LoadControl(const std::string &label)
{
   wxString valueString;
   bool valueBool;
   Integer valueInteger;
   
   //Integer index = mObject->GetParameterID(label);
   //Gmat::ParameterType type = mObject->GetParameterType(index);
   
   if (label == "Spacecraft")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      spacecraftComboBox->SetValue(valueString);
   }
   else if (label == "StateType")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      stateTypeComboBox->SetValue(valueString);
   }
   else if (label == "CoordinateSystem")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      coordinateSystemComboBox->SetValue(valueString);
   }
   else if (label == "WriteEphemeris")
   {
      valueBool = mObject->GetBooleanParameter(mObject->GetParameterID(label));
      writeEphemerisCheckBox->SetValue(valueBool);
   }
   else if (label == "FileFormat")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      fileFormatComboBox->SetValue(valueString);
   }
   else if (label == "FileName")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      fileNameTextCtrl->SetValue(valueString);
   }
   else if (label == "Interpolator")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      interpolatorComboBox->SetValue(valueString);
   }
   else if (label == "InterpolationOrder")
   {
      valueInteger = mObject->GetIntegerParameter(label.c_str());
      interpolationOrderTextCtrl->SetValue(wxString::Format("%d",valueInteger));
   }
   else if (label == "StepSize")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      stepSizeComboBox->SetValue(valueString);
   }
   else if (label == "EpochFormat")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      epochFormatComboBox->SetValue(valueString);
   }
   else if (label == "InitialEpoch")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      initialEpochComboBox->SetValue(valueString);
   }
   else if (label == "FinalEpoch")
   {
      valueString = wxT(mObject->GetStringParameter(label).c_str());
      finalEpochComboBox->SetValue(valueString);
   }
}


//------------------------------------------------------------------------------
// void SaveControl(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Passes a control's data to the Solver
 * 
 * @param label The string associated with the control.
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::SaveControl(const std::string &label)
{
   std::string valueString;
   bool valueBool;
   Integer valueInteger;
   
   Integer index = mObject->GetParameterID(label);
   
   if (label == "Spacecraft")
   {
      valueString = spacecraftComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "StateType")
   {
      valueString = stateTypeComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "CoordinateSystem")
   {
      valueString = coordinateSystemComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "WriteEphemeris")
   {
      valueBool =  writeEphemerisCheckBox->GetValue();
      mObject->SetBooleanParameter(index, valueBool);
   }
   else if (label == "FileFormat")
   {
      valueString = fileFormatComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "FileName")
   {
      valueString = fileNameTextCtrl->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "Interpolator")
   {
      valueString = interpolatorComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "InterpolationOrder")
   {
      valueInteger = atoi(interpolationOrderTextCtrl->GetValue());
      mObject->SetIntegerParameter(index, valueInteger);
   }
   else if (label == "StepSize")
   {
      valueString = stepSizeComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "EpochFormat")
   {
      valueString = epochFormatComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "InitialEpoch")
   {
      valueString = initialEpochComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   else if (label == "FinalEpoch")
   {
      valueString = finalEpochComboBox->GetValue();
      mObject->SetStringParameter(index, valueString);
   }
   
}


//------------------------------------------------------------------------------
// void OnComboBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * Event handler for comboboxes
 * 
 * Activates the Apply button when selection is changed.
 * 
 * @param event The triggering event.
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::OnComboBoxChange(wxCommandEvent& event)
{
   if (theApplyButton != NULL)
      EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnComboBoxTextChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * Event handler for ComboBox text change
 * 
 * Activates the Apply button when selection is changed.
 * 
 * @param event The triggering event.
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::OnComboBoxTextChange(wxCommandEvent& event)
{
   if (theApplyButton != NULL)
      EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnTextChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * Event handler for text boxes
 * 
 * Activates the Apply button when text is changed.
 * 
 * @param event The triggering event.
 */
//------------------------------------------------------------------------------
void EphemerisFilePanel::OnTextChange(wxCommandEvent& event)
{
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnWriteEphemerisChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EphemerisFilePanel::OnCheck(wxCommandEvent& event)
{
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnBrowse(wxCommandEvent &event)
// This function is used to open file dialog.
//------------------------------------------------------------------------------
void EphemerisFilePanel::OnBrowse(wxCommandEvent &event)
{
   if (fileDialog == NULL)
   {
      // create fileDialog object when it does not exist
      wxString caption = wxT("Choose a File");
      wxString defaultDir = wxT("./output/");
      wxString defaultFile = wxEmptyString;
      wxString wildcard = wxT("*.*");
      
      fileDialog = new wxFileDialog(this, caption, 
                                    defaultDir, defaultFile, 
                                    wildcard, wxOPEN); 
   }
   else
   {
      // show fileDialog when it exists 
      fileDialog->Show();
   }
   
   if (fileDialog->ShowModal() == wxID_OK)
   {
      // change file name when a new file is chosen
      wxString filename = fileDialog->GetFilename();
      fileNameTextCtrl->SetValue(filename);
   }
   else
   {
      // only hide fileDialog when clik on Cancel button
      fileDialog->Hide();
   }
   
}