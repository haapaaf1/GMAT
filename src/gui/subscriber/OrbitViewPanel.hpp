//$Id$
//------------------------------------------------------------------------------
//                              OrbitViewPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2010/06/18
/**
 * Declares OrbitViewPanel class. This class allows user to setup OrbitView.
 */
//------------------------------------------------------------------------------
#ifndef OrbitViewPanel_hpp
#define OrbitViewPanel_hpp

#include "gmatwxdefs.hpp"
#include "GmatPanel.hpp"
#include "GuiInterpreter.hpp"
#include "GuiItemManager.hpp"
#include "OrbitView.hpp"
#include "OrbitView.hpp"
#include "RgbColor.hpp"
#include "ColorTypes.hpp"

class OrbitViewPanel: public GmatPanel
{
public:
   OrbitViewPanel(wxWindow *parent, const wxString &subscriberName);
   ~OrbitViewPanel();
   
   virtual bool PrepareObjectNameChange();
   virtual void ObjectNameChanged(Gmat::ObjectType type,
                                  const wxString &oldName,
                                  const wxString &newName);
protected:
   OrbitView *mOrbitView;
   
   bool mHasIntegerDataChanged;
   bool mHasRealDataChanged;
   bool mHasDrawingOptionChanged;
   bool mHasSpChanged;
   bool mHasOrbitColorChanged;
   bool mHasTargetColorChanged;
   bool mHasShowObjectChanged;
   bool mHasCoordSysChanged;
   bool mHasViewInfoChanged;
   bool mHasViewUpInfoChanged;
   bool mHasStarOptionChanged;
   
   int  mScCount;
   int  mNonScCount;
   
   std::string mSelSpName;
   std::map<std::string, RgbColor> mOrbitColorMap;
   std::map<std::string, RgbColor> mTargetColorMap;
   std::map<std::string, bool> mDrawObjectMap;
   
   wxArrayString mExcludedScList;
   wxArrayString mExcludedCelesPointList;
   
   wxColour mOrbitColor;
   wxColour mTargetColor;
   
   wxStaticText *mTargetColorLabel;
   wxStaticText *mFovLabel;
   wxStaticText *mViewPointRefStaticText;
   wxStaticText *mViewPointVecStaticText;
   wxStaticText *mViewDirStaticText;
   wxStaticText *mStarCountStaticText;
   wxStaticText *mFovStaticText;
   wxStaticText *mFovMinStaticText;
   wxStaticText *mFovMaxStaticText;
   
   wxCheckBox *mShowPlotCheckBox;
   wxCheckBox *mWireFrameCheckBox;
   wxCheckBox *mEclipticPlaneCheckBox;
   wxCheckBox *mXYPlaneCheckBox;
   wxCheckBox *mUseInitialViewCheckBox;
   wxCheckBox *mPerspectiveModeCheckBox;
   wxCheckBox *mUseFixedFovCheckBox;
   wxCheckBox *mAxesCheckBox;
   wxCheckBox *mGridCheckBox;
   wxCheckBox *mOriginSunLineCheckBox;
   wxCheckBox *mDrawObjectCheckBox;
   wxCheckBox *mEnableStarsCheckBox;
   wxCheckBox *mEnableConstellationsCheckBox;
   
   wxTextCtrl *mDataCollectFreqTextCtrl;
   wxTextCtrl *mUpdatePlotFreqTextCtrl;
   wxTextCtrl *mNumPointsToRedrawTextCtrl;
   wxTextCtrl *mFixedFovTextCtrl;
   wxTextCtrl *mViewScaleFactorTextCtrl;
   wxTextCtrl *mViewPointRef1TextCtrl;
   wxTextCtrl *mViewPointRef2TextCtrl;
   wxTextCtrl *mViewPointRef3TextCtrl;
   wxTextCtrl *mViewPointVec1TextCtrl;
   wxTextCtrl *mViewPointVec2TextCtrl;
   wxTextCtrl *mViewPointVec3TextCtrl;
   wxTextCtrl *mViewDir1TextCtrl;
   wxTextCtrl *mViewDir2TextCtrl;
   wxTextCtrl *mViewDir3TextCtrl;
   wxTextCtrl *mStarCountTextCtrl;
   wxTextCtrl *mFovTextCtrl;
   wxTextCtrl *mFovMinTextCtrl;
   wxTextCtrl *mFovMaxTextCtrl;
   
   wxListBox *mSpacecraftListBox;
   wxListBox *mCelesPointListBox;
   wxListBox *mSelectedScListBox;
   wxListBox *mSelectedObjListBox;
   
   wxButton *addScButton;
   wxButton *removeScButton;
   wxButton *clearScButton;
   wxButton *mOrbitColorButton;
   wxButton *mTargetColorButton;
   
   wxComboBox *mSolverIterComboBox;
   wxComboBox *mCoordSysComboBox;
   wxComboBox *mViewPointRefComboBox;
   wxComboBox *mViewPointVectorComboBox;
   wxComboBox *mViewDirectionComboBox;
   wxComboBox *mViewUpCsComboBox;
   wxComboBox *mViewUpAxisComboBox;
   
   wxFlexGridSizer *mObjectSizer;
   wxFlexGridSizer *mViewDefSizer;
   wxBoxSizer *mScOptionSizer;
   wxBoxSizer *mViewPointRefSizer;
   wxBoxSizer *mViewPointVectorSizer;
   wxBoxSizer *mViewDirVectorSizer;
   
   // for initializing data
   void InitializeData();
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // event handler
   void OnAddSpacePoint(wxCommandEvent& event);
   void OnRemoveSpacePoint(wxCommandEvent& event);
   void OnClearSpacePoint(wxCommandEvent& event);
   void OnSelectAvailObject(wxCommandEvent& event);
   void OnSelectSpacecraft(wxCommandEvent& event);
   void OnSelectOtherObject(wxCommandEvent& event);
   void OnCheckBoxChange(wxCommandEvent& event);
   void OnOrbitColorClick(wxCommandEvent& event);
   void OnTargetColorClick(wxCommandEvent& event);
   void OnComboBoxChange(wxCommandEvent& event);
   void OnTextChange(wxCommandEvent& event);

        // Support Function
        void ValidateFovValues();
   
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXTCTRL = 93000,
      ID_COMBOBOX,
      ID_LISTBOX,
      SC_SEL_LISTBOX,
      OBJ_SEL_LISTBOX,
      CHECKBOX,
      ADD_SP_BUTTON,
      REMOVE_SP_BUTTON,
      CLEAR_SP_BUTTON,
      ORBIT_COLOR_BUTTON,
      TARGET_COLOR_BUTTON
   };
   
private:
   void ShowSpacePointOption(const wxString &name,
                             bool show = true, bool isSc = true,
                             UnsignedInt color = GmatColor::RED32);
   
};
#endif


