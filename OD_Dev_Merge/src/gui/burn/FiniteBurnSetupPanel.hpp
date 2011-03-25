//$Id$
//------------------------------------------------------------------------------
//                              FiniteBurnSetupPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: LaMont Ruley
// Created: 2004/03/04
//
/**
 * This class contains the Finite Burn Setup window.
 */
//------------------------------------------------------------------------------

#ifndef FiniteBurnSetupPanel_hpp
#define FiniteBurnSetupPanel_hpp

#include "gmatwxdefs.hpp"
#include "GmatPanel.hpp"
#include "FiniteBurn.hpp"

class FiniteBurnSetupPanel : public GmatPanel
{
public:
   FiniteBurnSetupPanel( wxWindow *parent, const wxString &burnName);
   ~FiniteBurnSetupPanel();

private:
   static const int MAX_PROP_ROW = 5;
   
   // member data
   FiniteBurn *theBurn;
   
   wxComboBox *mThrusterComboBox;
   
   wxString thrusterSelected;
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // for event handling
   void OnComboBoxChange(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
       ID_TEXT = 81000,
       ID_COMBOBOX
   };
};

#endif // FiniteBurnSetupPanel_hpp
