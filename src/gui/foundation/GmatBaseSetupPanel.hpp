//$Id$
//------------------------------------------------------------------------------
//                           GmatBaseSetupPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
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

#ifndef GmatBaseSetupPanel_hpp
#define GmatBaseSetupPanel_hpp

#include "GmatPanel.hpp"
#include <wx/confbase.h>
#include <wx/fileconf.h>
#include <wx/config.h>

typedef std::map<wxString, wxSizer*> SizerMapType;
typedef std::map<wxString, int> SizerSizeType;

/**
 * Generic configuration panel for GmatBase derived objects
 * 
 * This class defines a generic object configuration panel that is used when a
 * custom panel has not been coded.  It provides access to all of the object's
 * writable parameters using text controls and comboboxes.
 *
 * The GmatBaseSetupPanel uses the object types defined in the class's 
 * parameter list, accessed theough GmatBase methods, to configure itself,  Not
 * all of the parameter types have been mapped to autogenerated GUI elements
 * yet; those that are autogenerated are managed in the BuildControl() method.
 * If you need an unhandled control, add the handler for it to that method.
 *
 * New object types, when created in the base code, need to be added to the
 * selection in the GmatMainFrame code.  See that class documentation for
 * additional information.
 *
 * Help text, parameter units, and other controls are managed in an ini file
 * for the object.  These ini files are contained in the GUI configuration 
 * folder specified by the GUI_CONFIG_PATH parameter in the GMAT startup file.
 * The default location is \<root folder\>/data/gui_config.
 *
 * @todo Someone needs to fill in this section a bit more -- describe the ini
 * file contents better, and so on.
 */
class GmatBaseSetupPanel : public GmatPanel
{
public:
   GmatBaseSetupPanel(wxWindow *parent, const wxString &name,
							 bool reloadOnCBChange = false);
   virtual ~GmatBaseSetupPanel();
protected:
   
   virtual void         Create();
   virtual void         LoadData();
   virtual void         SaveData();
   
   wxControl *          BuildControl(wxWindow *parent, GmatBase *theObject,
                                     Integer index, const std::string &label,
                                     wxFileConfig *config);
   void                 CreateControls(wxFlexGridSizer *mainSizer, GmatBase *theObject);
   void                 CreateControls(GmatBase *theObject, Integer index,
                                       wxStaticText **aLabel, wxControl **aControl,
                                       wxControl **aUnit, wxFileConfig *config);
   void                 LoadControl(GmatBase *theObject, const std::string &label);
   bool                 SaveControl(GmatBase *theObject, const std::string &label,
                                    bool showErrorMsgs = false);
   virtual std::string  GetParameterLabel(GmatBase *theObject, Integer index, wxFileConfig *config) const;
   virtual std::string  GetParameterUnit(GmatBase *theObject, Integer index, wxFileConfig *config) const;
   std::string          AssignAcceleratorKey(std::string text);
   virtual SizerMapType* CreateGroups(wxFlexGridSizer *mainSizer, wxFileConfig *config);
   virtual void         CreateProperties(wxFlexGridSizer *mainSizer,
                                         GmatBase *theObject, StringArray *propertyNames,
                                         SizerMapType *groups, wxFileConfig *config);
   virtual void         SortGroups(StringArray *groupNames, wxFileConfig *config);
   virtual void         SortProperties(StringArray *propertyNames, wxFileConfig *config);
   virtual void         NormalizeLabels( StringArray propertyNames,
                                         std::vector<wxString> propertyGroups, 
                                         std::vector<wxStaticText*> propertyDescriptors, 
                                         std::vector<wxControl*> propertyControls, 
                                         std::vector<wxControl*> propertyUnits );
   wxWindow             *FixTabOrder( wxWindow *lastControl, wxSizer *sizer );
   virtual bool         GetLayoutConfig(GmatBase *theObject, wxFileConfig **config);
   virtual void         RefreshProperties(GmatBase *theObject, const std::string &ignoreControl = "",
                                          const std::string &onlyOneControl = "");
   virtual void         RefreshProperty(GmatBase *theObject, Integer index, wxControl *control,
                                        wxFileConfig *config);
   
   // Text control event method
   void OnTextUpdate(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   void OnBrowseButton(wxCommandEvent& event);
	void OnCheckBoxChange(wxCommandEvent& event);
   void OnComboBoxChange(wxCommandEvent& event);
   void OnComboBoxTextChange(wxCommandEvent& event);
   void OnTextChange(wxCommandEvent& event);
   void OnCheckListBoxChange(wxCommandEvent& event);
   
   /// List of used accelerator keys
   std::vector<char> accelKeys;
   /// Mapping between text strings and the associated control
   std::map<Integer, wxControl *> controlMap;
   /// Mapping between the associated control and text strings
   std::map<wxControl *, Integer> inverseControlMap;
   /// Managed wxComboBox map used by GuiItemManager
   std::map<wxString, wxComboBox*>  managedComboBoxMap;
   /// Managed wxCheckListBox map used by GuiItemManager
   std::map<wxString, wxCheckListBox*>  managedCheckListBoxMap;
   /// Number assigned to constructed wxCheckListBoxes
   Integer clbNumber;
   
   /// IDs used for event management
   enum
   {
      /// Text field ID
      ID_TEXT = 55000,           
      /// TextEdit box ID
      ID_TEXTCTRL,               
      /// ComboBox (dropdown list) ID
      ID_COMBOBOX,               
      /// ID for check boxes
      ID_CHECKBOX,               
      /// ID for a list containing check boxes
      ID_CHECKLISTBOX,
      /// wxCheckListBoxes require unique IDs; here is the last ID for them, so a panel can have up to 20
      ID_CHECKLISTBOX_LAST = ID_CHECKLISTBOX + 19,
      /// File browser button ID
      ID_BUTTON_BROWSE 
   };
   
   static const Integer border = 3;
   /// True-false strings (just a convenience here)
   static const wxString TF_SCHEMES[2];

   /// local copy of object for verifying changes before commit/apply
   GmatBase *localObject;
   bool reloadOnComboBoxChange;
	
};

#endif /* GmatBaseSetupPanel_hpp */
