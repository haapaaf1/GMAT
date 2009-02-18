//$Header$
//------------------------------------------------------------------------------
//                                  CelestialBodyOrbitPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan
// Created: 2009.01.26
//
/**
 * This is the panel for the Orbit tab on the notebook on the CelestialBody
 * Panel.
 *
 */
//------------------------------------------------------------------------------


#ifndef CelestialBodyOrbitPanel_hpp
#define CelestialBodyOrbitPanel_hpp

#include "gmatdefs.hpp"
#include "CelestialBody.hpp"
#include "SolarSystem.hpp"
#include "GuiInterpreter.hpp"   // <<<<<<<<<< ?
#include "gmatwxdefs.hpp"
#include "GuiItemManager.hpp"  
#include "GmatPanel.hpp"
#include "GmatStaticBoxSizer.hpp"

class CelestialBodyOrbitPanel : public wxPanel
{
public:
   CelestialBodyOrbitPanel(GmatPanel *cbPanel, wxWindow *parent, CelestialBody *body);
   ~CelestialBodyOrbitPanel();
   
   void SaveData();
   void LoadData();
   
   bool IsDataChanged() { return dataChanged;};
   bool CanClosePanel() { return canClose;};
   
private:
   
   bool           dataChanged;
   bool           canClose;
   
   CelestialBody  *theBody;
   GuiItemManager *guiManager;
   GuiInterpreter *guiInterpreter;
   SolarSystem    *ss;
   
   std::string    ephemSrc;
   std::string    ephemFile;
   Integer        naifID;
   std::string    centralBody;
   Real           initialEpoch;
   Real           SMA;
   Real           ECC;
   Real           INC;
   Real           RAAN;
   Real           AOP;
   Real           TA;
   
   bool           ephemSrcChanged;
   bool           ephemFileChanged;
   bool           naifIDChanged;
   bool           cBodyChanged;
   bool           epochChanged;
   bool           SMAChanged;
   bool           ECCChanged;
   bool           INCChanged;
   bool           RAANChanged;
   bool           AOPChanged;
   bool           TAChanged;
   
   bool           userDef;
   
   bool           isSun;
   
   GmatPanel      *theCBPanel;
   
  
   void     Create();
   void     ResetChangeFlags(bool discardMods = false);
   
   //Event Handling
   DECLARE_EVENT_TABLE();
   void     OnEphemSourceComboBoxChange(wxCommandEvent &event);
   void     OnEphemFileTextCtrlChange(wxCommandEvent &event);
   void     OnEphemFileBrowseButton(wxCommandEvent &event);
   void     OnNaifIdTextCtrlChange(wxCommandEvent &event);
   void     OnCentralBodyComboBoxChange(wxCommandEvent &event);
   void     OnEpochTextCtrlChange(wxCommandEvent &event);
   void     OnSMATextCtrlChange(wxCommandEvent &event);
   void     OnECCTextCtrlChange(wxCommandEvent &event);
   void     OnINCTextCtrlChange(wxCommandEvent &event);
   void     OnRAANTextCtrlChange(wxCommandEvent &event);
   void     OnAOPTextCtrlChange(wxCommandEvent &event);
   void     OnTATextCtrlChange(wxCommandEvent &event);
   
   // wx
   
   wxStaticText *ephemSourceStaticText;
   wxStaticText *ephemFileStaticText;
   wxStaticText *naifIDStaticText;
   wxStaticText *centralBodyStaticText;
   wxStaticText *initialEpochStaticText;
   wxStaticText *SMAStaticText;
   wxStaticText *ECCStaticText;
   wxStaticText *INCStaticText;
   wxStaticText *RAANStaticText;
   wxStaticText *AOPStaticText;
   wxStaticText *TAStaticText;

   wxStaticText *SMAUnitsStaticText;
   wxStaticText *ECCUnitsStaticText;
   wxStaticText *INCUnitsStaticText;
   wxStaticText *RAANUnitsStaticText;
   wxStaticText *AOPUnitsStaticText;
   wxStaticText *TAUnitsStaticText;

   wxTextCtrl   *ephemFileTextCtrl;
   wxTextCtrl   *naifIDTextCtrl;
   wxTextCtrl   *initialEpochTextCtrl;
   wxTextCtrl   *SMATextCtrl;
   wxTextCtrl   *ECCTextCtrl;
   wxTextCtrl   *INCTextCtrl;
   wxTextCtrl   *RAANTextCtrl;
   wxTextCtrl   *AOPTextCtrl;
   wxTextCtrl   *TATextCtrl;
   
   wxComboBox   *ephemSourceComboBox;
   wxComboBox   *centralBodyComboBox;
   wxButton     *ephemFileBrowseButton;
   
   // strings for the combo boxes
   StringArray  sourceArray;
//   StringArray  centralBodyArray;
   
   // wxString arrays for the combo boxes
   wxString     *sourceArrayWX;
//   wxString     *centralBodyArrayWX;
   
   /// string versions of current data
   wxString     ephemSourceStringWX;
   wxString     ephemFileStringWX;
   wxString     naifIDStringWX;
   wxString     centralBodyStringWX;
   wxString     initialEpochStringWX;
   wxString     SMAStringWX;
   wxString     ECCStringWX;
   wxString     INCStringWX;
   wxString     RAANStringWX;
   wxString     AOPStringWX;
   wxString     TAStringWX;
   
   
   GmatStaticBoxSizer *mainBoxSizer;
   
   /// IDs for the controls 
   enum
   {
      ID_TEXT = 7100,
      ID_COMBO_BOX_EPHEM_SOURCE,
      ID_TEXT_CTRL_EPHEM_FILE,
      ID_BROWSE_BUTTON_EPHEM_FILE,
      ID_TEXT_CTRL_NAIF_ID,
      ID_COMBO_BOX_CENTRAL_BODY,
      ID_TEXT_CTRL_INITIAL_EPOCH,
      ID_TEXT_CTRL_SMA,
      ID_TEXT_CTRL_ECC,
      ID_TEXT_CTRL_INC,
      ID_TEXT_CTRL_RAAN,
      ID_TEXT_CTRL_AOP,
      ID_TEXT_CTRL_TA,
   };
   
   
                                
};
#endif // CelestialBodyOrbitPanel_hpp
