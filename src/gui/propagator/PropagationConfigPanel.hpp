//$Id$
//------------------------------------------------------------------------------
//                           PropagationConfigPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2003/08/29
// Copyright: (c) 2003 NASA/GSFC. All rights reserved.
//
/**
 * This class contains the Propagation configuration window.
 */
//------------------------------------------------------------------------------
#ifndef PropagationConfigPanel_hpp
#define PropagationConfigPanel_hpp

// gui includes
#include "gmatwxdefs.hpp"

// base includes
#include "gmatdefs.hpp"
#include "Propagator.hpp"
#include "PropSetup.hpp"
#include "ODEModel.hpp"
#include "DragForce.hpp"
#include "GravityField.hpp"
#include "HarmonicField.hpp"
#include "PointMassForce.hpp"
#include "SolarRadiationPressure.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"
#include "AtmosphereModel.hpp"
#include "MessageInterface.hpp"
#include "GmatPanel.hpp"

class PropagationConfigPanel : public GmatPanel
{
public:

   PropagationConfigPanel(wxWindow *parent, const wxString &propName);
   ~PropagationConfigPanel();  

private:
   
   // Integrator types
   enum IntegratorType
   {
      RKV89 = 0,
      RKN68,
      RKF56,
      PD45,
      PD78,
      BS,
      ABM,
//      CW,
      IntegratorCount,
   };

   // Earth gravity field model
   enum EarthGravModelType
   {
      E_NONE_GM = 0,
      JGM2,
      JGM3,
      EGM96,
      E_OTHER,
      EarthGravModelCount,
   };   
   
   // Luna gravity field model
   enum LunaGravModelType
   {
      L_NONE_GM = 0,
      LP165,
      L_OTHER,
      LunaGravModelCount,
   };   
   
   // Venus gravity field model
   enum VenusGravModelType
   {
      V_NONE_GM = 0,
      MGNP180U,
      V_OTHER,
      VenusGravModelCount,
   };   
   
   // Mars gravity field model
   enum MarsGravModelType
   {
      M_NONE_GM = 0,
      MARS50C,
      M_OTHER,
      MarsGravModelCount,
   };   
   
   // Other gravity field model
   enum OthersGravModelType
   {
      O_NONE_GM = 0,
      O_OTHER,
      OthersGravModelCount,
   };
   
   // Earth drag model
   enum EarthDragModelType
   {
      NONE_DM = 0,
      EXPONENTIAL,
      MSISE90,
      JR,
      EarthDragModelCount,
   };
   
   // Magnetic force drag model
   enum MagfModelType
   {
      NONE_MM = 0,
      MagfModelCount,
   };

   // Error Control
   enum ErrorControlType
   {
      NONE_EC = 0,
      RSSSTEP,
      RSSSTATE,
      LARGESTSTEP,
      LARGESTSTATE,
      ErrorControlCount,
   };
   
   struct ForceType
   {
      wxString bodyName;
      wxString gravType;
      wxString dragType;
      wxString magfType;
      wxString gravDegree;
      wxString gravOrder;
      wxString magfDegree;
      wxString magfOrder;
      wxString potFilename;
      PointMassForce *pmf;
      GravityField *gravf;
      DragForce *dragf;
      SolarRadiationPressure *srpf;
      bool useSrp; // for future use (SRP on indivitual body is future implementation)
      
      ForceType(const wxString &body, const wxString &grav = "None",
                const wxString &drag = "None", const wxString &mag = "None",
                PointMassForce *pf = NULL, GravityField *gf = NULL,
                DragForce *df = NULL)
         {
            bodyName = body; gravType = grav; dragType = drag; magfType = mag;
            gravDegree = "4"; gravOrder = "4"; magfDegree = "0"; 
            magfOrder = "0"; potFilename = ""; pmf = pf; gravf = gf; 
            dragf = df; srpf = NULL; useSrp = false;
         }
      
      ForceType& operator= (const ForceType& right)
         {
            if (this == &right)
               return *this;
            
            bodyName = right.bodyName; gravType = right.gravType;
            dragType = right.dragType; magfType = right.magfType; 
            gravDegree = right.gravDegree; gravOrder = right.gravOrder;
            magfDegree = right.magfDegree; magfOrder = right.magfOrder;
            potFilename = right.potFilename; pmf = right.pmf;
            gravf = right.gravf; dragf = right.dragf; srpf = right.srpf;
            useSrp = right.useSrp;
            return *this;
         }
   };
   
   wxStaticText *minIntErrorStaticText;
   wxStaticText *nomIntErrorStaticText;
   wxStaticText *potFileStaticText;
   
   wxTextCtrl *initialStepSizeTextCtrl;
   wxTextCtrl *accuracyTextCtrl;
   wxTextCtrl *minStepTextCtrl;
   wxTextCtrl *maxStepTextCtrl;
   wxTextCtrl *maxStepAttemptTextCtrl;
   wxTextCtrl *minIntErrorTextCtrl;
   wxTextCtrl *nomIntErrorTextCtrl;
   wxTextCtrl *bodyTextCtrl;
   wxTextCtrl *gravityDegreeTextCtrl;
   wxTextCtrl *gravityOrderTextCtrl;
   wxTextCtrl *potFileTextCtrl;
   wxTextCtrl *magneticDegreeTextCtrl;
   wxTextCtrl *magneticOrderTextCtrl;
   wxTextCtrl *pmEditTextCtrl;
   
   wxComboBox *theIntegratorComboBox;
   wxComboBox *theOriginComboBox;
   wxComboBox *thePrimaryBodyComboBox;
   wxComboBox *theGravModelComboBox;
   wxComboBox *theAtmosModelComboBox;
   wxComboBox *theMagfModelComboBox;
   wxComboBox *theErrorComboBox;
   
   wxBitmapButton *theGravModelSearchButton;
   wxButton *theDragSetupButton;
   wxButton *theMagModelSearchButton;
   
   wxCheckBox *theSrpCheckBox;
   
   wxString integratorString;
   wxString primaryBodyString;
   wxString gravityFieldString;
   wxString atmosModelString;
   
   wxBoxSizer *leftBoxSizer;
   
   std::string propSetupName;
   std::string thePropagatorName;
   std::string mFmPrefaceComment;
   
   wxString currentBodyName;
   wxString gravTypeName;
   wxString dragTypeName;
   wxString propOriginName;
   wxString errorControlTypeName;
   
   wxArrayString integratorTypeArray;
   wxArrayString earthGravModelArray;
   wxArrayString lunaGravModelArray;
   wxArrayString venusGravModelArray;
   wxArrayString marsGravModelArray;
   wxArrayString othersGravModelArray;
   wxArrayString dragModelArray;
   wxArrayString magfModelArray;
   wxArrayString errorControlArray;
   
   std::map<wxString, wxString> theFileMap;
   
   wxArrayString primaryBodiesArray;
   wxArrayString secondaryBodiesArray;
   wxArrayString integratorArray;
   
   Integer numOfBodies;
   Integer numOfForces;
   Integer currentBodyId;
   
   /// normalized harmonic coefficients
   Real               Cbar[361][361];
   /// normalized harmonic coefficients
   Real               Sbar[361][361];
   /// coefficient drifts per year
   Real               dCbar[17][17];
   /// coefficient drifts per year
   Real               dSbar[17][17];
   
   bool useDragForce;
   bool usePropOriginForSrp;
   bool isForceModelChanged;
   bool isAtmosChanged;
   bool isDegOrderChanged;
   bool isPotFileChanged;
   bool isMagfTextChanged;
   bool isIntegratorChanged;
   bool isIntegratorDataChanged;
   bool isOriginChanged;
   bool isErrControlChanged;
   
   Propagator                     *thePropagator;
   PropSetup                      *thePropSetup;
   ODEModel                       *theForceModel;
   PointMassForce                 *thePMF;
   DragForce                      *theDragForce;
   GravityField                   *theGravForce;
   SolarRadiationPressure         *theSRP;
   SolarSystem                    *theSolarSystem;
   CelestialBody                  *theCelestialBody;
   AtmosphereModel                *theAtmosphereModel;
   std::vector<PointMassForce *>  thePMForces;
   std::vector<ForceType*> primaryBodyList;
   std::vector<ForceType*> pointMassBodyList;
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // Layout & data handling methods
   Integer FindPrimaryBody(const wxString& bodyName, bool create = true,
                           const wxString& gravType = "None",
                           const wxString& dragType = "None",
                           const wxString& magfType = "None");
   
   Integer FindPointMassBody(const wxString& bodyName);
   
   void Initialize();
   void DisplayIntegratorData(bool integratorChanged);
   void DisplayPrimaryBodyData();
   void DisplayForceData();
   void DisplayGravityFieldData(const wxString& bodyName);
   void DisplayAtmosphereModelData();
   void DisplayPointMassData();
   void DisplayMagneticFieldData();
   void DisplaySRPData();
   void DisplayErrorControlData();
   void EnablePrimaryBodyItems(bool enable = true, bool clear = false);
   void UpdatePrimaryBodyItems();
   
   // Saving data
   bool SaveIntegratorData();
   bool SaveDegOrder();
   bool SavePotFile();
   bool SaveAtmosModel();
   
   // Converting Data
   wxString ToString(Real rval);
   
   // Text control event method
   void OnIntegratorTextUpdate(wxCommandEvent &event);
   void OnGravityTextUpdate(wxCommandEvent& event);
   void OnMagneticTextUpdate(wxCommandEvent& event);
   
   // Checkbox event method
   void OnSRPCheckBoxChange(wxCommandEvent &event);
   
   // Combobox event method
   void OnIntegratorComboBox(wxCommandEvent &event);
   void OnPrimaryBodyComboBox(wxCommandEvent &event);
   void OnOriginComboBox(wxCommandEvent &event);
   void OnGravityModelComboBox(wxCommandEvent &event);
   void OnAtmosphereModelComboBox(wxCommandEvent &event);
   void OnErrorControlComboBox(wxCommandEvent &event);
   
   // Button event methods
   void OnAddBodyButton(wxCommandEvent &event);
   void OnGravSearchButton(wxCommandEvent &event);
   void OnSetupButton(wxCommandEvent &event);
   void OnMagSearchButton(wxCommandEvent &event);
   void OnPMEditButton(wxCommandEvent &event);
   void OnSRPEditButton(wxCommandEvent &event);
   
   // for Debug
   void ShowPropData(const std::string &header);
   void ShowForceList(const std::string &header);
   void ShowForceModel(const std::string &header);
   
   // for reading gravity files
   void ParseDATGravityFile(const wxString& fname);
   void ParseGRVGravityFile(const wxString& fname);
   void ParseCOFGravityFile(const wxString& fname);
   void PrepareGravityArrays();
   
   // Strictly for reading gravity files
   static const Integer GRAV_MAX_DRIFT_DEGREE = 2;
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 42000,
      ID_TEXTCTRL,
      ID_TEXTCTRL_PROP,
      ID_TEXTCTRL_GRAV,
      ID_TEXTCTRL_MAGF,
      ID_CHECKBOX,
      ID_CB_INTGR,
      ID_CB_BODY,
      ID_CB_ORIGIN,
      ID_CB_GRAV,
      ID_CB_ATMOS,
      ID_CB_MAG,
      ID_CB_ERROR,
      ID_BUTTON_ADD_BODY,
      ID_BUTTON_GRAV_SEARCH,
      ID_BUTTON_SETUP,
      ID_BUTTON_MAG_SEARCH,
      ID_BUTTON_PM_EDIT,
      ID_BUTTON_SRP_EDIT 
   };
};

#endif // PropagationConfigPanel_hpp
