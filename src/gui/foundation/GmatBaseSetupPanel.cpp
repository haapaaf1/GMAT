//$Id$
//------------------------------------------------------------------------------
//                           GmatBaseSetupPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Linda Jun
// Created: 2008/11/19
/**
 * This class is a generic setup panel used by objects derived from GmatBase
 */
//------------------------------------------------------------------------------

#include "GmatBaseSetupPanel.hpp"
#include "MessageInterface.hpp"

//-----------------------------------------
// static members
//-----------------------------------------
const wxString GmatBaseSetupPanel::TF_SCHEMES[2] = 
   {
      wxT("false"),
      wxT("true")
   };

/// wxWidget event mappings for the panel
BEGIN_EVENT_TABLE(GmatBaseSetupPanel, GmatPanel)
   EVT_COMBOBOX(ID_COMBOBOX, GmatBaseSetupPanel::OnComboBoxChange)
   EVT_TEXT(ID_COMBOBOX, GmatBaseSetupPanel::OnComboBoxTextChange)
   EVT_TEXT(ID_TEXTCTRL, GmatBaseSetupPanel::OnTextChange)
END_EVENT_TABLE()


//-----------------------------------------
// public methods
//-----------------------------------------

//------------------------------------------------------------------------------
// GmatBaseSetupPanel(wxWindow *parent, const wxString &name)
//------------------------------------------------------------------------------
/**
 * Panel constructor
 * 
 * @param parent Owner for this panel
 * @param name Name of the object that is to be configured 
 */
//------------------------------------------------------------------------------
GmatBaseSetupPanel::GmatBaseSetupPanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent)
{
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
// ~GmatBaseSetupPanel()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
GmatBaseSetupPanel::~GmatBaseSetupPanel()
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
void GmatBaseSetupPanel::Create()
{
   Integer propertyCount = mObject->GetParameterCount();
   Integer j = 0;
   
   #ifdef DEBUG_BASEPANEL_CREATE
   MessageInterface::ShowMessage
      ("GmatBaseSetupPanel::Create() entered, name='%s', mObject=<%p><%s> '%s'\n",
       name.c_str(), mObject, mObject->GetTypeName().c_str(), mObject->GetName().c_str());
   MessageInterface::ShowMessage("   It has %d properties\n", propertyCount);
   #endif
   
   for (Integer i = 0; i < propertyCount; ++i)
   {
      #ifdef DEBUG_BASEPANEL_CREATE
      MessageInterface::ShowMessage
         ("   ParameterText(%d)='%s'\n", i, mObject->GetParameterText(i).c_str());
      #endif
      
      if (mObject->IsParameterReadOnly(i) == false)
      {
         propertyDescriptors.push_back(new wxStaticText(this, ID_TEXT, 
            wxT(mObject->GetParameterText(i).c_str())));
         
         controlMap[mObject->GetParameterText(i)] = j++;
         
         wxControl* control = BuildControl(this, i);
         propertyControls.push_back(control);
         
         propertyUnits.push_back(new wxStaticText(this, ID_TEXT, 
            wxT(mObject->GetParameterUnit(i).c_str())));
      }
   }
   
   // three columns: description, control, unit
   wxFlexGridSizer *itemSizer = new wxFlexGridSizer(3);
   wxGridSizer *mainSizer = new wxGridSizer(1);
   Integer border = 3;
   
   std::vector<wxStaticText*>::iterator item;
   
   for(item = propertyDescriptors.begin(), j = 0; 
       item != propertyDescriptors.end(); ++item, ++j) 
   {
      itemSizer->Add(*item, 0, wxALL|wxALIGN_RIGHT, border);
      itemSizer->Add(propertyControls[j], 0, wxALL|wxALIGN_CENTER, border);
      itemSizer->Add(propertyUnits[j], 0, wxALL|wxALIGN_LEFT, border);
   }
   
   mainSizer->Add(itemSizer, 0, wxALL|wxALIGN_CENTER, border);
   theMiddleSizer->Add(mainSizer, 0, wxALL|wxALIGN_CENTER, 5);
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
/**
 * Populates the panel with the configurable property data in the Solver 
 */
//------------------------------------------------------------------------------
void GmatBaseSetupPanel::LoadData()
{
   // load data from the core engine
   try
   {
      std::string label;
      Integer propertyCount = mObject->GetParameterCount();
      
      for (Integer i = 0; i < propertyCount; ++i)
      {
         if (mObject->IsParameterReadOnly(i) == false)
         {
            label = mObject->GetParameterText(i);
            LoadControl(label);
         }
      }
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("GmatBaseSetupPanel:LoadData() error occurred!\n%s\n",
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
void GmatBaseSetupPanel::SaveData()
{
   canClose = true;
   
   try
   {
      for (std::map<std::string, Integer>::iterator i = controlMap.begin();
            i != controlMap.end(); ++i)
      {
         SaveControl(i->first);
         if (!canClose)
            return;
      }
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
wxControl *GmatBaseSetupPanel::BuildControl(wxWindow *parent, Integer index)
{
   wxControl *control = NULL;
   
   Gmat::ParameterType type = mObject->GetParameterType(index);
   
   switch (type)
   {
   case Gmat::BOOLEAN_TYPE:
      {
         wxComboBox *cbControl =
            new wxComboBox(parent, ID_COMBOBOX, "true", 
                           wxDefaultPosition, wxDefaultSize, 2, TF_SCHEMES, 
                           wxCB_READONLY);
         
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
            wxArrayString emptyList;
            control = new wxComboBox(parent, ID_COMBOBOX, wxT(""),
                                     wxDefaultPosition, wxSize(180,-1), emptyList,
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
void GmatBaseSetupPanel::LoadControl(const std::string &label)
{
   #ifdef DEBUG_BASEPANEL_LOAD
   MessageInterface::ShowMessage
      ("GmatBaseSetupPanel::LoadControl() label='%s'\n", label.c_str());
   #endif
   
   Integer index = mObject->GetParameterID(label);
   Gmat::ParameterType type = mObject->GetParameterType(index);
   
   wxControl *theControl = propertyControls[controlMap[label]];
   wxString valueString;
   
   switch (type)
   {
      case Gmat::BOOLEAN_TYPE:
         if (mObject->GetBooleanParameter(mObject->GetParameterID(label)))
            ((wxComboBox*)(theControl))->SetValue(wxT("true"));
         else
            ((wxComboBox*)(theControl))->SetValue(wxT("false"));
         break;
         
      case Gmat::REAL_TYPE:
         {
            Real val = mObject->GetRealParameter(
                  mObject->GetParameterID(label));
            valueString << val;
            ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         }
         break;
         
      case Gmat::INTEGER_TYPE:
         {
            Integer val = mObject->GetIntegerParameter(
                  mObject->GetParameterID(label));
            valueString << val;
            ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         }
         break;
         
      case Gmat::STRING_TYPE:
         valueString = wxT(mObject->GetStringParameter(label).c_str());
         ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         break;
         
      case Gmat::OBJECT_TYPE:
         valueString = (mObject->GetStringParameter(mObject->GetParameterID(label))).c_str();
         ((wxComboBox*)theControl)->Append(valueString);
         
      case Gmat::ENUMERATION_TYPE:
         {
            valueString = (mObject->GetStringParameter(mObject->GetParameterID(label))).c_str();
            wxComboBox *cb = (wxComboBox*)theControl;
            cb->SetValue(valueString);
            
            // if ComboBox is not read only, add value to list
            if (cb->GetWindowStyleFlag() != wxCB_READONLY)
               cb->Append(valueString);
            break;
         }
      default:
         break;
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
void GmatBaseSetupPanel::SaveControl(const std::string &label)
{
   Integer index = mObject->GetParameterID(label);
   Gmat::ParameterType type = mObject->GetParameterType(index);

   wxControl *theControl = propertyControls[controlMap[label]];
   std::string valueString;
   
   switch (type)
   {
      case Gmat::BOOLEAN_TYPE:
         {
            bool val = true;
            if (((wxComboBox*)theControl)->GetValue() == "false")
               val = false;
            mObject->SetBooleanParameter(index, val);
         }
         break;
      
      case Gmat::REAL_TYPE:
         {
            Real val; 
            valueString = ((wxTextCtrl*)theControl)->GetValue();
            CheckReal(val, valueString, label, "Real Number");
            if (!canClose)
               return;
            mObject->SetRealParameter(index, val);
         }
         break;
         
      case Gmat::INTEGER_TYPE:
         {
            Integer val;
            valueString = ((wxTextCtrl*)theControl)->GetValue();
            CheckInteger(val, valueString, label, "Integer");
            if (!canClose)
               return;
            mObject->SetIntegerParameter(index, val);
         }
         break;
      
      case Gmat::STRING_TYPE:
         valueString = ((wxTextCtrl*)theControl)->GetValue();
         mObject->SetStringParameter(index, valueString.c_str());
         break;

      case Gmat::OBJECT_TYPE:
      case Gmat::ENUMERATION_TYPE:
         valueString = ((wxComboBox*)theControl)->GetValue();
         mObject->SetStringParameter(index, valueString);
         break;
         
      default:
         break;
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
void GmatBaseSetupPanel::OnComboBoxChange(wxCommandEvent& event)
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
void GmatBaseSetupPanel::OnComboBoxTextChange(wxCommandEvent& event)
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
void GmatBaseSetupPanel::OnTextChange(wxCommandEvent& event)
{
   EnableUpdate(true);
}
