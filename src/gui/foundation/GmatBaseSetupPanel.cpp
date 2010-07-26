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
#include "FileManager.hpp"         // for GetPathname()
#include <wx/confbase.h>
#include <wx/fileconf.h>
#include <wx/config.h>
#include "StringUtil.hpp"
#include "GmatStaticBoxSizer.hpp"
#include "FileUtil.hpp"
#include <map>
#include <algorithm>
#include <utility> // make_pair
#include "UtilityException.hpp"

//#define DEBUG_BASEPANEL_LOAD

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
   EVT_CHECKBOX(ID_CHECKBOX, GmatBaseSetupPanel::OnComboBoxChange)
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
   Integer i = 0;
   std::string labelText;
   std::vector<char> accelKeys;
   wxString groupProp;
   wxFlexGridSizer *itemSizer;
   wxSizer *sizer;
   SizerMapType *groups;
   std::vector<std::string> propertyNames;
   std::vector<wxString> propertyGroups;
   wxStaticText *aLabel;
   std::string configPath;
   
   #ifdef DEBUG_BASEPANEL_CREATE
   MessageInterface::ShowMessage
      ("GmatBaseSetupPanel::Create() entered, name='%s', mObject=<%p><%s> '%s'\n",
       name.c_str(), mObject, mObject->GetTypeName().c_str(), mObject->GetName().c_str());
   MessageInterface::ShowMessage("   It has %d properties\n", propertyCount);
   #endif

   wxFlexGridSizer *mainSizer = new wxFlexGridSizer(1);
   wxFlexGridSizer *mainItemSizer = new wxFlexGridSizer(3); // for properties that don't have parents
   mainSizer->Add(mainItemSizer, 0, wxALL|wxALIGN_CENTER, border);
   
   // get the GUI_CONFIG_PATH
   FileManager *fm = FileManager::Instance();
   try
   {
      configPath = fm->GetAbsPathname(FileManager::GUI_CONFIG_PATH);
   }
   catch (UtilityException &e)
   {
      MessageInterface::ShowMessage
         ("GmatBaseSetupPanel:Create() error occurred!\n%s\n",
          e.GetFullMessage().c_str());
      configPath = "";
   }
   bool configFileExists = GmatFileUtil::DoesFileExist(configPath+mObject->GetTypeName()+".ini");
   wxFileConfig *pConfig = new wxFileConfig(wxEmptyString, wxEmptyString,
                (configPath+mObject->GetTypeName()+".ini").c_str(),
	             wxEmptyString, wxCONFIG_USE_LOCAL_FILE );
   
   // create the groups
   groups = CreateGroups(mainSizer, pConfig);

   // get the properties
   for (Integer i = 0; i < propertyCount; ++i)
   {
      #ifdef DEBUG_BASEPANEL_CREATE
      MessageInterface::ShowMessage
         ("   ParameterText(%d)='%s'\n", i, mObject->GetParameterText(i).c_str());
      #endif
      if (mObject->IsParameterReadOnly(i) == false)
         propertyNames.push_back(mObject->GetParameterText(i));
   }

   // sort the properties per the config file
   if (configFileExists)
      SortProperties( &propertyNames, pConfig );

   // now go through the properties and create their controls
   std::vector<std::string>::iterator propertyItem;
   for(propertyItem = propertyNames.begin(), j=0;
       propertyItem != propertyNames.end(); ++propertyItem, ++j)
   {
      i = mObject->GetParameterID(*propertyItem);
      // set the path to the section that contains the parameter's items
      pConfig->SetPath(wxT(("/"+mObject->GetParameterText(i)).c_str()));
      labelText = GetParameterLabel(i, pConfig);
      labelText = AssignAcceleratorKey(labelText, &accelKeys);
      if (mObject->GetParameterType(i) != Gmat::BOOLEAN_TYPE)
         aLabel = new wxStaticText(this, ID_TEXT,
             labelText.c_str());
      else
         aLabel = new wxStaticText(this, ID_TEXT,
             "");
      propertyDescriptors.push_back(aLabel);
      controlMap[mObject->GetParameterText(i)] = j;

      wxControl* control = BuildControl(this, i, labelText, pConfig);
      propertyControls.push_back(control);

      aLabel = new wxStaticText(this, ID_TEXT, 
         wxT(GetParameterUnit(i, pConfig).c_str()));
      propertyUnits.push_back(aLabel);
   }
   
   // three columns: description, control, unit
   std::vector<wxStaticText*>::iterator item;

   // find the best minimum size for the description/unit for each sizer/group

   // Add the 3 columns to a sizer
   SizerMapType::iterator sizerItem;
   bool doDefaultAction;
   
   for(item = propertyDescriptors.begin(), j = 0; 
       item != propertyDescriptors.end(); ++item, ++j) 
   {
      // set the path to the section that contains the parameter's items
      pConfig->SetPath(wxT(("/"+propertyNames[j])).c_str());
      groupProp = "Parent";
      doDefaultAction = !(pConfig->Read(groupProp, &groupProp));
      if (!doDefaultAction)
      {
         // save the group for normalizing labels later
         propertyGroups.push_back(groupProp);
         // get the group and its itemsizer inside
         sizerItem = groups->find(groupProp.Lower());
         doDefaultAction = sizerItem == groups->end();
         if (!doDefaultAction)
         {
            sizer = ((wxSizer *) sizerItem->second);
            itemSizer = new wxFlexGridSizer(3);
            sizer->Add(itemSizer, 0, wxALL|wxALIGN_LEFT, 0);
            itemSizer->Add(*item, 0, wxALL|wxALIGN_RIGHT, border);
            itemSizer->Add(propertyControls[j], 0, wxALL|wxALIGN_LEFT, border);
            itemSizer->Add(propertyUnits[j], 0, wxALL|wxALIGN_LEFT, border);
         }
      }
      if (doDefaultAction)
      {
         // save the group for normalizing labels later
         propertyGroups.push_back("Main");
         mainItemSizer->Add(*item, 0, wxALL|wxALIGN_RIGHT, border);
         mainItemSizer->Add(propertyControls[j], 0, wxALL|wxALIGN_LEFT, border);
         mainItemSizer->Add(propertyUnits[j], 0, wxALL|wxALIGN_LEFT, border);
      }
   }

   NormalizeLabels( propertyNames, propertyGroups, propertyDescriptors, propertyControls, propertyUnits );
   FixTabOrder( NULL, mainSizer );
   delete pConfig;
   delete groups;
   theMiddleSizer->Add(mainSizer, 0, wxALL|wxALIGN_CENTER, 5);
}

SizerMapType *GmatBaseSetupPanel::CreateGroups(wxFlexGridSizer *mainSizer, wxFileConfig *config)
{
   SizerMapType *groups = new SizerMapType;
   wxString groupName;
   wxSizer *sizer;
   wxSizer *parentSizer;
   wxString groupProp;
   wxString parentName;
   long dummy;
   bool doDefaultAction;
   SizerMapType::iterator sizerItem, item;

   // first get all of the groups
   bool bCont = config->GetFirstGroup(groupName, dummy);
   while (bCont)
   {
      // if not Main group, then it is a group/sizer
      if ((groupName != "main") && (config->Read("/"+groupName+"/Type", &groupProp)))
      {
         // "Type" can be VERTICAL, HORIZONTAL, FLEX
         groupProp = groupProp.Lower();
         if (groupProp == "vertical")
           sizer = new GmatStaticBoxSizer( wxVERTICAL, this, config->Read("/"+groupName+"/Label") );
         else if (groupProp == "horizontal")
           sizer = new GmatStaticBoxSizer( wxHORIZONTAL, this, config->Read("/"+groupName+"/Label") );
         else if (groupProp == "flex")
         {
           sizer = new wxFlexGridSizer( config->Read("/"+groupName+"/Rows", (long) 0), 
                           config->Read("/"+groupName+"/Columns", (long) 0),
                           config->Read("/"+groupName+"/VerticalGap", (long) 0),
                           config->Read("/"+groupName+"/HorizontalGap", (long) 0));
         }
         else if (groupProp == "grid")
         {
           sizer = new wxGridSizer( config->Read("/"+groupName+"/Rows", (long) 0), 
                           config->Read("/"+groupName+"/Columns", (long) 0),
                           config->Read("/"+groupName+"/VerticalGap", (long) 0),
                           config->Read("/"+groupName+"/HorizontalGap", (long) 0));
         }
         else // property or unknown type
            continue;
         groups->insert(std::make_pair(groupName.Lower(), sizer));
      }
      // get next group
      bCont = config->GetNextGroup(groupName, dummy);
   }

   // create an ordered list of groups
   std::vector<std::string> groupNames;
   for (item = groups->begin(); item != groups->end(); ++item)
   {
	   groupName = item->first;
	   groupNames.push_back(groupName.Lower().c_str());
   }
   SortGroups(&groupNames, config);

   // now, for all the groups, add them to their parent
//   for (item = groups->begin(); item != groups->end(); ++item)
   for (unsigned int i = 0; i < groupNames.size(); i++)
   {
      item = groups->find(groupNames[i].c_str());
      groupName = item->first;
      sizer = (wxSizer *) item->second;
      // set the parent of the sizer, if doesn't exist, then it is the main sizer
      if (config->Read("/"+groupName+"/Parent", &parentName))
      {
         sizerItem = groups->find(parentName.Lower());
         doDefaultAction = sizerItem == groups->end();
         if (!doDefaultAction)
         {
            parentSizer = ((wxSizer *) sizerItem->second);
            parentSizer->Add(sizer, 0, wxALL|wxALIGN_CENTER|wxEXPAND, border);
         }
         else
            mainSizer->Add(sizer, 0, wxALL|wxALIGN_CENTER|wxEXPAND, border);
      }
      else
         mainSizer->Add(sizer, 0, wxALL|wxALIGN_CENTER|wxEXPAND, border);
   }
   return groups;
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
wxControl *GmatBaseSetupPanel::BuildControl(wxWindow *parent, Integer index, const std::string &label, wxFileConfig *config)
{
   wxControl *control = NULL;
   
   Gmat::ParameterType type = mObject->GetParameterType(index);
   
   switch (type)
   {
   case Gmat::BOOLEAN_TYPE:
      {
         wxCheckBox *cbControl = new wxCheckBox( this, ID_CHECKBOX, wxT(label.c_str()));
         cbControl->SetToolTip(config->Read(_T("Hint")));
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
         else if (type == Gmat::ANTENNA)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
               theGuiManager->GetAntennaComboBox(this, ID_COMBOBOX,
                                                  wxSize(180,-1));
            managedComboBoxMap.insert(std::make_pair("Antenna", cbControl));
            control = cbControl;
         }
         else if (type == Gmat::SENSOR)
         {
            // The GuiItemManager automatically registers wxComboBox in order to
            // listen for any SpacePoint updates, so need to unregister
            // in the destructor
            wxComboBox *cbControl =
               theGuiManager->GetSensorComboBox(this, ID_COMBOBOX,
                                                  wxSize(180,-1));
            managedComboBoxMap.insert(std::make_pair("Sensor", cbControl));
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
   case Gmat::REAL_TYPE:
   case Gmat::INTEGER_TYPE:
      {
	      control = new wxTextCtrl(parent, ID_TEXTCTRL,
	                               wxT(""), wxDefaultPosition, wxSize(180,-1), 0,
	                               wxTextValidator(wxGMAT_FILTER_NUMERIC));
      }
      break;
   case Gmat::STRING_TYPE:
   default:
      control = new wxTextCtrl(parent, ID_TEXTCTRL, 
                               wxT(""), wxDefaultPosition, wxSize(180,-1));
      break;
   }
   
   control->SetToolTip(config->Read(_T("Hint")));
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
    	  ((wxCheckBox*) theControl)->SetValue(mObject->GetBooleanParameter(mObject->GetParameterID(label)));
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
         ((wxComboBox*)theControl)->SetStringSelection(valueString);
//         ((wxComboBox*)theControl)->Append(valueString); // removed for Bug 1621 wcs 2009.11.10
         
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
        	  mObject->SetBooleanParameter(index, ((wxCheckBox*)theControl)->GetValue());
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

std::string GmatBaseSetupPanel::AssignAcceleratorKey(std::string text, std::vector<char> *accelKeys)
{
	// find best candidate
	// 1) Beginning of word
	// accelKeys contains list of already used accelerator keys (lower case)

	unsigned int accelIndex = 0;
	bool haveCandidate = false;
	bool findWord = true;  // true when flying over spaces in string
	unsigned int iText;
	std::string titleText = "";

	// first, check to see if accelerator has already been assigned (can happen if label
	// is loaded from INI file
	size_t found = text.find(GUI_ACCEL_KEY);
	if ( found != std::string::npos)
	{
		// add to accel keys
		accelKeys->push_back(tolower(text[found+1]));
		return text;
	}

	for (iText=0;iText<text.length();iText++)
	{
      // if character has not already been used
      if (find (accelKeys->begin(), accelKeys->end(), tolower(text[iText])) == accelKeys->end())
      {
		   // if it is the beginning of a word alphanumeric 
		   if ((findWord) && (isalnum(text[iText])))
		   {
			   accelIndex = iText;
			   accelKeys->push_back(tolower(text[iText]));
            if (accelIndex > 0)
			      titleText.append(text, 0, accelIndex);
			   titleText += GUI_ACCEL_KEY;
			   titleText.append(text, accelIndex, text.length());
			   return titleText;
		   }
         // if in the middle of a word and we don't already have a candidate
		   if ((!haveCandidate) && (isalnum(text[iText])))
		   {
			   accelIndex = iText;
			   haveCandidate = true;
		   }
      }
      if ((!findWord) && (!isalnum(text[iText]))) 
         findWord = true;
      if ((findWord) && (isalnum(text[iText])))
         findWord = false;
	}
	if (haveCandidate)
	{
		accelKeys->push_back(tolower(text[accelIndex]));
		titleText.append(text, 0, accelIndex);
		titleText += GUI_ACCEL_KEY;
		titleText.append(text, accelIndex, text.length());
	}
	return titleText;
}

//------------------------------------------------------------------------------
// wxString GetParameterLabel(Integer i, wxFileConfig *config)
//------------------------------------------------------------------------------
/**
 * Creates a label for a parameter text.  It uses INI (if available) or
 * just a TitleCase string representation of the parameter text
 *
 * @param text input text
 */
//------------------------------------------------------------------------------
std::string GmatBaseSetupPanel::GetParameterLabel(Integer i, wxFileConfig *config) const
{
	std::string text = mObject->GetParameterText(i);
	std::string titleText;
    wxString str;

	// first, see if the parameter is in the object's INI file
   // set the path to the section that contains the parameter's items
   config->SetPath(wxT(("/"+text).c_str()));
   if (config->Read(wxT("Label"), &str))
   {
   	titleText = str.c_str();
   	return titleText;
   }

	bool findword = false;
	if (text.length() > 0)
		titleText = text[0];
	for (unsigned int i=1;i<text.length();i++)
	{

		if (!findword)
		{
			// if not finding a word and is not uppercase, find the next word
			// e.g., NASATextFile -> NASAT (find word (next upper case char))
			//       Text File -> T (Find word (next upper case char))
			if (!isupper(text[i]))
				findword = true;
		}
		else
		{
			if (isupper(text[i]))
			{
				findword = false;
				titleText += " ";
			}
		}
		titleText += text[i];
	}
	return titleText;
}


//------------------------------------------------------------------------------
// wxString GetParameterUnit(Integer i, wxFileConfig *config)
//------------------------------------------------------------------------------
/**
 * Creates a label for a parameter text.  It uses INI (if available) or
 * just a TitleCase string representation of the parameter text
 *
 * @param text input text
 */
//------------------------------------------------------------------------------
std::string GmatBaseSetupPanel::GetParameterUnit(Integer i, wxFileConfig *config) const
{
	std::string text = mObject->GetParameterText(i);
    wxString str;

	// first, see if the parameter is in the object's INI file
   // set the path to the section that contains the parameter's items
   config->SetPath(wxT(("/"+text).c_str()));
   if (config->Read(wxT("Unit"), &str))
	   return str.c_str();
   else
	   return mObject->GetParameterUnit(i);
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


//------------------------------------------------------------------------------
// void SortProperties(std::vector<std::string> *propertyNames, wxFileConfig *config)
//------------------------------------------------------------------------------
/**
 * Sort properties based on INI PositionBefore statements
 *
 */
//------------------------------------------------------------------------------
void GmatBaseSetupPanel::SortProperties(std::vector<std::string> *propertyNames, wxFileConfig *config)
{
   // first, see if the INI file wants properties alphabetically sorted
   wxString alpha_sort;
   if (config->Read("/Main/Sort Properties", &alpha_sort))
   {
      if (alpha_sort.Lower() == "true")
         std::sort(propertyNames->begin(), propertyNames->end());
   }

   // Now, go through all the properties and order them according to "Position before"
   wxString position;
   std::vector<std::string>::iterator item;
   std::vector<std::string>::iterator beforeItem;
   std::vector<std::string> origOrder = *propertyNames;

   for (unsigned int i = 0; i < origOrder.size(); i++)
   {
      // see if position defined
      if (config->Read(("/"+origOrder[i]+"/Position Before").c_str(), &position))
      {
         // find the property to move before
         if (position != "")
         {
            beforeItem = find(propertyNames->begin(), propertyNames->end(), position.c_str());
            if (beforeItem == propertyNames->end()) continue;
         }
         // find the property to move
         item = find(propertyNames->begin(), propertyNames->end(), origOrder[i].c_str());
         propertyNames->erase(item);
         // if no position before specified, move to the end
         if (position == "")
            propertyNames->push_back(origOrder[i]);
         else
            propertyNames->insert(beforeItem, origOrder[i]);
      }
   }
}


//------------------------------------------------------------------------------
// void SortGroups(std::vector<std::string> *groupNames, wxFileConfig *config)
//------------------------------------------------------------------------------
/**
 * Sort groups based on INI PositionBefore statements
 *
 */
//------------------------------------------------------------------------------
void GmatBaseSetupPanel::SortGroups(std::vector<std::string> *groupNames, wxFileConfig *config)
{
   // go through all the groups and order them according to "Position before"
   wxString position;
   std::vector<std::string>::iterator item;
   std::vector<std::string>::iterator beforeItem;
   std::vector<std::string> origOrder = *groupNames;

   for (unsigned int i = 0; i < origOrder.size(); i++)
   {
      // see if position defined
      if (config->Read(("/"+origOrder[i]+"/Position Before").c_str(), &position))
      {
         // find the group to move before
         if (position != "")
         {
            beforeItem = find(groupNames->begin(), groupNames->end(), position.c_str());
            if (beforeItem == groupNames->end()) continue;
         }
         // find the group to move
         item = find(groupNames->begin(), groupNames->end(), origOrder[i].c_str());
         groupNames->erase(item);
         // if no position before specified, move to the end
         if (position == "")
            groupNames->push_back(origOrder[i]);
         else
            groupNames->insert(beforeItem, origOrder[i]);
      }
   }
}


void GmatBaseSetupPanel::NormalizeLabels( std::vector<std::string> propertyNames, 
                      std::vector<wxString> propertyGroups, 
                      std::vector<wxStaticText*> propertyDescriptors, 
                      std::vector<wxControl*> propertyControls, 
                      std::vector<wxStaticText*> propertyUnits )
{
   // initialize all groups with a max size
   SizerSizeType labelSizes, unitSizes;
   SizerSizeType::iterator sizeItem;
   std::vector<wxStaticText*>::iterator item;
   unsigned int j;

   // initialize the sizes first
   for (j = 0; j < propertyGroups.size(); j++)
   {
      if (labelSizes.find(propertyGroups[j].Lower()) == labelSizes.end())
      {
         labelSizes.insert( std::make_pair(propertyGroups[j].Lower(), 0) );
         unitSizes.insert( std::make_pair(propertyGroups[j].Lower(), 0) );
      }
   }

   // now find the minimums for the descriptions and units
   for(item = propertyDescriptors.begin(), j = 0; 
       item != propertyDescriptors.end(); ++item, ++j) 
   {
      // get the label size so far for the group
      sizeItem = labelSizes.find(propertyGroups[j].Lower());
      if (((int) sizeItem->second) < (*item)->GetBestSize().GetWidth())
         sizeItem->second = (*item)->GetBestSize().GetWidth();
      // get the unit size so far for the group
      sizeItem = unitSizes.find(propertyGroups[j].Lower());
      if (((int) sizeItem->second) < propertyUnits[j]->GetBestSize().GetWidth())
         sizeItem->second = propertyUnits[j]->GetBestSize().GetWidth();
   }

   // adjust grouplabels and groupunits
   for(item = propertyDescriptors.begin(), j = 0; 
       item != propertyDescriptors.end(); ++item, ++j) 
   {
      // set the label min size 
      sizeItem = labelSizes.find(propertyGroups[j].Lower());
      (*item)->SetMinSize(wxSize(((int) sizeItem->second), (*item)->GetMinHeight()));
      // set the unit min size 
      sizeItem = unitSizes.find(propertyGroups[j].Lower());
      propertyUnits[j]->SetMinSize(wxSize(((int) sizeItem->second), propertyUnits[j]->GetMinHeight()));
   }
}

wxWindow *GmatBaseSetupPanel::FixTabOrder( wxWindow *lastControl, wxSizer *sizer )
{
   wxSizerItemList list = sizer->GetChildren();
   wxSizerItemList::iterator iter;
   for (iter = list.begin(); iter != list.end(); ++iter)
   {
      if ((*iter)->IsSizer())
         lastControl = FixTabOrder( lastControl, (*iter)->GetSizer() );
      else 
      {
         if (lastControl != NULL)
            (*iter)->GetWindow()->MoveAfterInTabOrder(lastControl);
         lastControl = (*iter)->GetWindow();
      }
   }
   return lastControl;

}

