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

#ifndef GmatBaseSetupPanel_hpp
#define GmatBaseSetupPanel_hpp

#include "GmatPanel.hpp"

/**
 * Generic configuration panel for GmatBase derived objects
 * 
 * This class defines a generic object configuration panel that is used when a
 * custom panel has not been coded.  It provides access to all of the object's
 * writable parameters using text controls and comboboxes.
 */
class GmatBaseSetupPanel : public GmatPanel
{
public:
   GmatBaseSetupPanel(wxWindow *parent, const wxString &name);
   virtual ~GmatBaseSetupPanel();
   
protected:
   
   virtual void         Create();
   virtual void         LoadData();
   virtual void         SaveData();
   wxControl *          BuildControl(wxWindow *parent, Integer index);
   void                 LoadControl(const std::string &label);
   void                 SaveControl(const std::string &label);
   
   // Text control event method
   void OnTextUpdate(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   void OnComboBoxChange(wxCommandEvent& event);
   void OnComboBoxTextChange(wxCommandEvent& event);
   void OnTextChange(wxCommandEvent& event);
   
   /// Labels used for the configurable properties
   std::vector<wxStaticText*>       propertyDescriptors;
   /// GUI controls that are used to configure the properties
   std::vector<wxControl*>          propertyControls;
   /// Units used for the configurable properties
   std::vector<wxStaticText*>       propertyUnits;
   /// Mapping between text strings and the index for the associated control
   std::map<std::string, Integer>   controlMap;
   /// Managed wxComboBox map used by GuiItemManager
   std::map<wxString, wxComboBox*>  managedComboBoxMap;
   /// IDs used for event management
   enum
   {
      ID_TEXT = 55000,
      ID_TEXTCTRL,
      ID_COMBOBOX
   };
   
   /// True-false strings (just a convenience here)
   static const wxString TF_SCHEMES[2];
   
};

#endif /* GmatBaseSetupPanel_hpp */
