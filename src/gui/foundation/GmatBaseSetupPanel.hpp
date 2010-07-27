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
#include <wx/confbase.h>
#include <wx/fileconf.h>
#include <wx/config.h>

/**
 * Generic configuration panel for GmatBase derived objects
 * 
 * This class defines a generic object configuration panel that is used when a
 * custom panel has not been coded.  It provides access to all of the object's
 * writable parameters using text controls and comboboxes.
 */
typedef std::map<wxString, wxSizer*> SizerMapType;
typedef std::map<wxString, int> SizerSizeType;
class GmatBaseSetupPanel : public GmatPanel
{
public:
   GmatBaseSetupPanel(wxWindow *parent, const wxString &name);
   virtual ~GmatBaseSetupPanel();
protected:
   
   virtual void         Create();
   virtual void         LoadData();
   virtual void         SaveData();
   wxControl *          BuildControl(wxWindow *parent, Integer index, const std::string &label, wxFileConfig *config);
   void          		CreateControls(Integer index, wxStaticText **aLabel, wxControl **aControl, wxStaticText **aUnit, std::vector<char> *accelKeys, wxFileConfig *config);
   void                 LoadControl(const std::string &label);
   void                 SaveControl(const std::string &label);
   virtual std::string  GetParameterLabel(Integer index, wxFileConfig *config) const;
   virtual std::string  GetParameterUnit(Integer index, wxFileConfig *config) const;
   std::string  		AssignAcceleratorKey(std::string text, std::vector<char> *accelKeys);
   SizerMapType *       CreateGroups(wxFlexGridSizer *mainSizer, wxFileConfig *config);
   virtual void         SortGroups(std::vector<std::string> *groupNames, wxFileConfig *config);
   virtual void         SortProperties(std::vector<std::string> *propertyNames, wxFileConfig *config);
   virtual void         NormalizeLabels( std::vector<std::string> propertyNames, 
                                         std::vector<wxString> propertyGroups, 
                                         std::vector<wxStaticText*> propertyDescriptors, 
                                         std::vector<wxControl*> propertyControls, 
                                         std::vector<wxStaticText*> propertyUnits );
   wxWindow             *FixTabOrder( wxWindow *lastControl, wxSizer *sizer );
   virtual bool         GetLayoutConfig(wxFileConfig **config);
   
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
      ID_COMBOBOX,
      ID_CHECKBOX
   };
   
   static const Integer border = 3;
   /// True-false strings (just a convenience here)
   static const wxString TF_SCHEMES[2];
   GmatBase *localObject;
   
};

#endif /* GmatBaseSetupPanel_hpp */
