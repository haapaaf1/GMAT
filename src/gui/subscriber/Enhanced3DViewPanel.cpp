//$Id$
//------------------------------------------------------------------------------
//                              Enhanced3DViewPanel
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
 * Implements Enhanced3DViewPanel class.
 */
//------------------------------------------------------------------------------

#include "Enhanced3DViewPanel.hpp"
#include "GmatStaticBoxSizer.hpp"
#include "StringUtil.hpp"

#include "ColorTypes.hpp"           // for namespace GmatColor::
#include "MessageInterface.hpp"

#include "wx/colordlg.h"            // for wxColourDialog

// To enable perspective mode
//#define __ENABLE_GL_PERSPECTIVE__

//#define DEBUG_OPENGL_PANEL 1
//#define DEBUG_OPENGL_PANEL_CREATE 1
//#define DEBUG_OPENGL_PANEL_LOAD 1
//#define DEBUG_OPENGL_PANEL_SAVE 1
//#define DEBUG_OPENGL_PANEL_CHECKBOX 1
//#define DEBUG_OPENGL_PANEL_SHOW 1

//------------------------------
// event tables for wxWindows
//------------------------------
BEGIN_EVENT_TABLE(Enhanced3DViewPanel, GmatPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   EVT_BUTTON(ID_BUTTON_HELP, GmatPanel::OnHelp)
   
   EVT_BUTTON(ADD_SP_BUTTON, Enhanced3DViewPanel::OnAddSpacePoint)
   EVT_BUTTON(REMOVE_SP_BUTTON, Enhanced3DViewPanel::OnRemoveSpacePoint)
   EVT_BUTTON(CLEAR_SP_BUTTON, Enhanced3DViewPanel::OnClearSpacePoint)
   EVT_BUTTON(ORBIT_COLOR_BUTTON, Enhanced3DViewPanel::OnOrbitColorClick)
   EVT_BUTTON(TARGET_COLOR_BUTTON, Enhanced3DViewPanel::OnTargetColorClick)
   EVT_LISTBOX(ID_LISTBOX, Enhanced3DViewPanel::OnSelectAvailObject)
   EVT_LISTBOX(SC_SEL_LISTBOX, Enhanced3DViewPanel::OnSelectSpacecraft)
   EVT_LISTBOX(OBJ_SEL_LISTBOX, Enhanced3DViewPanel::OnSelectOtherObject)
   EVT_CHECKBOX(CHECKBOX, Enhanced3DViewPanel::OnCheckBoxChange)
   EVT_COMBOBOX(ID_COMBOBOX, Enhanced3DViewPanel::OnComboBoxChange)
   EVT_TEXT(ID_TEXTCTRL, Enhanced3DViewPanel::OnTextChange)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------
//------------------------------------------------------------------------------
// Enhanced3DViewPanel(wxWindow *parent, const wxString &subscriberName)
//------------------------------------------------------------------------------
/**
 * Constructs Enhanced3DViewPanel object.
 *
 * @param <parent> input parent.
 *
 * @note Creates the Enhanced3DViewPanel GUI
 */
//------------------------------------------------------------------------------
Enhanced3DViewPanel::Enhanced3DViewPanel(wxWindow *parent,
                                           const wxString &subscriberName)
   : GmatPanel(parent)
{
   #if DEBUG_OPENGL_PANEL
   MessageInterface::ShowMessage("Enhanced3DViewPanel() entering...\n");
   MessageInterface::ShowMessage("Enhanced3DViewPanel() subscriberName = " +
                                 std::string(subscriberName.c_str()) + "\n");
   #endif
   
   Subscriber *subscriber = (Subscriber*)
      theGuiInterpreter->GetConfiguredObject(subscriberName.c_str());
   
   mEnhanced3DView = (Enhanced3DView*)subscriber;
   
   // Set the pointer for the "Show Script" button
   mObject = mEnhanced3DView;

   InitializeData();
   
//    mHasIntegerDataChanged = false;
//    mHasRealDataChanged = false;
//    mHasDrawingOptionChanged = false;
//    mHasSpChanged = false;
//    mHasOrbitColorChanged = false;
//    mHasTargetColorChanged = false;
//    mHasShowObjectChanged = false;
//    mHasCoordSysChanged = false;
//    mHasViewInfoChanged = false;
//    mScCount = 0;
//    mNonScCount = 0;
   
//    mOrbitColorMap.clear();
//    mTargetColorMap.clear();
   
   Create();
   Show();
   
   // Listen for Spacecraft name change
   theGuiManager->AddToResourceUpdateListeners(this);
}


//------------------------------------------------------------------------------
// ~Enhanced3DViewPanel()
//------------------------------------------------------------------------------
Enhanced3DViewPanel::~Enhanced3DViewPanel()
{
   #if DEBUG_OPENGL_PANEL
   MessageInterface::ShowMessage
      ("Enhanced3DViewPanel::~Enhanced3DViewPanel() unregistering mSpacecraftListBox:%d\n",
       mSpacecraftListBox);
   #endif
   
   theGuiManager->UnregisterListBox("Spacecraft", mSpacecraftListBox, &mExcludedScList);
   
   theGuiManager->UnregisterComboBox("CoordinateSystem", mCoordSysComboBox);
   theGuiManager->UnregisterComboBox("CoordinateSystem", mViewUpCsComboBox);
   theGuiManager->UnregisterComboBox("SpacePoint", mViewPointRefComboBox);
   theGuiManager->UnregisterComboBox("SpacePoint", mViewPointVectorComboBox);
   theGuiManager->UnregisterComboBox("SpacePoint", mViewDirectionComboBox);
   
   theGuiManager->RemoveFromResourceUpdateListeners(this);
}


//------------------------------------------------------------------------------
// virtual bool PrepareObjectNameChange()
//------------------------------------------------------------------------------
bool Enhanced3DViewPanel::PrepareObjectNameChange()
{
   // Save GUI data
   wxCommandEvent event;
   OnApply(event);
   
   return GmatPanel::PrepareObjectNameChange();
}


//------------------------------------------------------------------------------
// virtual void ObjectNameChanged(Gmat::ObjectType type, const wxString &oldName,
//                                const wxString &newName)
//------------------------------------------------------------------------------
/*
 * Reflects resource name change to this panel.
 * By the time this method is called, the base code already changed reference
 * object name, so all we need to do is re-load the data.
 */
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::ObjectNameChanged(Gmat::ObjectType type,
                                             const wxString &oldName,
                                             const wxString &newName)
{
   #ifdef DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Enhanced3DViewPanel::ObjectNameChanged() type=%d, oldName=<%s>, "
       "newName=<%s>, mDataChanged=%d\n", type, oldName.c_str(), newName.c_str(),
       mDataChanged);
   #endif
   
   if (type != Gmat::SPACECRAFT)
      return;
   
   // Initialize GUI data and re-load from base
   InitializeData();
   LoadData();
   
   // We don't need to save data if object name changed from the resouce tree
   // while this panel is opened, since base code already has new name
   EnableUpdate(false);
}


//---------------------------------
// protected methods
//---------------------------------

//------------------------------------------------------------------------------
// void InitializeData()
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::InitializeData()
{
   mHasIntegerDataChanged = false;
   mHasRealDataChanged = false;
   mHasDrawingOptionChanged = false;
   mHasSpChanged = false;
   mHasOrbitColorChanged = false;
   mHasTargetColorChanged = false;
   mHasShowObjectChanged = false;
   mHasCoordSysChanged = false;
   mHasViewInfoChanged = false;
   mScCount = 0;
   mNonScCount = 0;
   
   mOrbitColorMap.clear();
   mTargetColorMap.clear();
}


//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::Create()
{
   #if DEBUG_OPENGL_PANEL_CREATE
      MessageInterface::ShowMessage("Enhanced3DViewPanel::Create() entered\n");
   #endif
   
   Integer bsize = 2; // border size
   
   // create axis array
   wxArrayString emptyList;
   wxString axisArray[] = {"X", "-X", "Y", "-Y", "Z", "-Z"};   
   wxArrayString empty;
   wxStaticText *emptyStaticText =
      new wxStaticText( this, -1, wxT("  "), wxDefaultPosition, wxDefaultSize, 0 );   
   
   
   //-----------------------------------------------------------------
   // platform dependent button size
   //-----------------------------------------------------------------
   int arrowW = 20;
   int colorW = 25;
   #ifdef __WXMAC__
   arrowW = 40;
   colorW = 10;
   #else
   arrowW = 20;
   colorW = 25;
   #endif
   
   //-----------------------------------------------------------------
   // Data collect and update frequency
   //-----------------------------------------------------------------
   wxStaticText *dataCollectFreqLabel1 =
      new wxStaticText(this, -1, wxT("Collect data every "),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   wxStaticText *dataCollectFreqLabel2 =
      new wxStaticText(this, -1, wxT("step"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   wxStaticText *updatePlotFreqLabel1 =
      new wxStaticText(this, -1, wxT("Update plot every "),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   wxStaticText *updatePlotFreqLabel2 =
      new wxStaticText(this, -1, wxT("cycle"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
	 
   mDataCollectFreqTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(35, 20), 0);
   
   mUpdatePlotFreqTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(35, 20), 0);

	mStarCountStaticText =
		new wxStaticText(this, -1, wxT("Number of stars "), 
								wxDefaultPosition, wxSize(-1,-1), 0);
	mStarCountTextCtrl = 
		new wxTextCtrl(this, ID_TEXTCTRL, wxT("7000"), wxDefaultPosition, wxSize(50, 20), 0);
	//mStarCountTextCtrl->Disable();

	mEnableStarsCheckBox = 
		new wxCheckBox(this, CHECKBOX, wxT("Enable Stars"), wxDefaultPosition, wxSize(100, 20), 0);
	mEnableConstellationsCheckBox = 
		new wxCheckBox(this, CHECKBOX, wxT("Enable Constellations"), wxDefaultPosition, wxSize(160, 20), 0);
	//mEnableConstellationsCheckBox->Disable();
   
   wxBoxSizer *colFreqSizer = new wxBoxSizer(wxHORIZONTAL);
   colFreqSizer->Add(dataCollectFreqLabel1, 0, wxALIGN_LEFT|wxALL, bsize);
   colFreqSizer->Add(mDataCollectFreqTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   colFreqSizer->Add(dataCollectFreqLabel2, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxBoxSizer *updFreqSizer = new wxBoxSizer(wxHORIZONTAL);
   updFreqSizer->Add(updatePlotFreqLabel1, 0, wxALIGN_LEFT|wxALL, bsize);
   updFreqSizer->Add(mUpdatePlotFreqTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   updFreqSizer->Add(updatePlotFreqLabel2, 0, wxALIGN_LEFT|wxALL, bsize);

	wxBoxSizer *starOptionSizer = new wxBoxSizer(wxHORIZONTAL);
	starOptionSizer->Add(mStarCountStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
	starOptionSizer->Add(mStarCountTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxBoxSizer *plotOptionSizer = new wxBoxSizer(wxVERTICAL);   
   plotOptionSizer->Add(colFreqSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   plotOptionSizer->Add(updFreqSizer, 0, wxALIGN_LEFT|wxALL, bsize);
	plotOptionSizer->Add(mEnableStarsCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
	plotOptionSizer->Add(mEnableConstellationsCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
	plotOptionSizer->Add(starOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxStaticText *numPointsToRedrawLabel1 =
      new wxStaticText(this, -1, wxT("Number of points to redraw\n"
                                     "(Enter 0 to redraw whole plot)"),
                       wxDefaultPosition, wxSize(-1, 30), 0);
   mNumPointsToRedrawTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(30, 20), 0);
   
   wxBoxSizer *numPointsSizer = new wxBoxSizer(wxHORIZONTAL);
   numPointsSizer->Add(numPointsToRedrawLabel1, 0, wxALIGN_LEFT|wxALL, 0);
   numPointsSizer->Add(mNumPointsToRedrawTextCtrl, 0, wxALIGN_LEFT|wxALL, 0);
   
   plotOptionSizer->Add(numPointsSizer, 0, wxALIGN_CENTER|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Show plot
   //-----------------------------------------------------------------
   mShowPlotCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Show Plot"),
                     wxDefaultPosition, wxSize(-1, -1), bsize);
   
   plotOptionSizer->AddSpacer(10);
   plotOptionSizer->Add(mShowPlotCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   
   GmatStaticBoxSizer *plotOptionStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "Plot Option");
   
   plotOptionStaticSizer->Add(plotOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // View option
   //-----------------------------------------------------------------
   mUseInitialViewCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Use Initial View Def."),
                     wxDefaultPosition, wxSize(-1, -1), bsize);
   
   wxBoxSizer *viewOptionSizer = new wxBoxSizer(wxVERTICAL);
   viewOptionSizer->Add(mUseInitialViewCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   
   #ifdef __ENABLE_GL_PERSPECTIVE__
   
   mPerspectiveModeCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Use Perspective Mode"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mUseFixedFovCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Use Fixed FOV Angle"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   
   viewOptionSizer->Add(mPerspectiveModeCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   viewOptionSizer->Add(mUseFixedFovCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   
   mFovLabel =
      new wxStaticText(this, -1, wxT("Field Of View (deg): "),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mFixedFovTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(35, -1), 0);
   
   wxBoxSizer *fovSizer = new wxBoxSizer(wxHORIZONTAL);
   fovSizer->Add(mFovLabel, 0, wxALIGN_LEFT|wxALL, 0);
   fovSizer->Add(mFixedFovTextCtrl, 0, wxALIGN_LEFT|wxALL, 0);
   viewOptionSizer->Add(fovSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   #endif
   
   GmatStaticBoxSizer *viewOptionStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "View Option");
   viewOptionStaticSizer->Add(viewOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Drawing option
   //-----------------------------------------------------------------
   mWireFrameCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw WireFrame"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mEclipticPlaneCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw Ecliptic Plane"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mXYPlaneCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw XY Plane"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mAxesCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw Axes"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mGridCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw Grid"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   mOriginSunLineCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw Sun Line"),
                     wxDefaultPosition, wxSize(-1, -1), 0);

	// Field of View Options
	/*wxBoxSizer *fovOptionSizer = new wxBoxSizer(wxHORIZONTAL);
	mFovStaticText = 
		new wxStaticText(this, -1, wxT("Starting FOV "),
								wxDefaultPosition, wxSize(-1, -1), 0);
	mFovTextCtrl = 
		new wxTextCtrl(this, ID_TEXTCTRL, wxT("45"), wxDefaultPosition, wxSize(50, -1), 0);
	fovOptionSizer->Add(mFovStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
	fovOptionSizer->Add(mFovTextCtrl, 0, wxALIGN_RIGHT|wxALL, bsize);

	wxBoxSizer *fovMinOptionSizer = new wxBoxSizer(wxHORIZONTAL);
	mFovMinStaticText =
		new wxStaticText(this, -1, wxT("Min FOV        "),
								wxDefaultPosition, wxSize(-1, -1), 0);
	mFovMinTextCtrl =
		new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(50, -1), 0);
	fovMinOptionSizer->Add(mFovMinStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
	fovMinOptionSizer->Add(mFovMinTextCtrl, 0, wxALIGN_RIGHT|wxALL, bsize);

	wxBoxSizer *fovMaxOptionSizer = new wxBoxSizer(wxHORIZONTAL);
	mFovMaxStaticText = 
		new wxStaticText(this, -1, wxT("Max FOV       "),
								wxDefaultPosition, wxSize(-1, -1), 0);
	mFovMaxTextCtrl =
		new wxTextCtrl(this, ID_TEXTCTRL, wxT("90"), wxDefaultPosition, wxSize(50, -1), 0);
	fovMaxOptionSizer->Add(mFovMaxStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
	fovMaxOptionSizer->Add(mFovMaxTextCtrl, 0, wxALIGN_RIGHT|wxALL, bsize);*/
   
   // Solver Iteration ComboBox
   wxStaticText *solverIterLabel =
      new wxStaticText(this, -1, wxT("Solver Iterations"),
                       wxDefaultPosition, wxSize(-1, -1), 0);
   
   mSolverIterComboBox =
      new wxComboBox(this, ID_COMBOBOX, wxT(""), wxDefaultPosition, wxSize(65, -1));
   
   // Get Solver Iteration option list from the Subscriber
   const std::string *solverIterList = Subscriber::GetSolverIterOptionList();
   int count = Subscriber::GetSolverIterOptionCount();
   for (int i=0; i<count; i++)
      mSolverIterComboBox->Append(solverIterList[i].c_str());
   wxBoxSizer *solverIterOptionSizer = new wxBoxSizer(wxHORIZONTAL);
   solverIterOptionSizer->Add(solverIterLabel, 0, wxALIGN_CENTER|wxALL, bsize);
   solverIterOptionSizer->Add(mSolverIterComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxBoxSizer *drawOptionSizer = new wxBoxSizer(wxVERTICAL);
   drawOptionSizer->Add(20, 2, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mWireFrameCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mEclipticPlaneCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mXYPlaneCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mAxesCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mGridCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(mOriginSunLineCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(solverIterOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
	//drawOptionSizer->Add(fovOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
	//drawOptionSizer->Add(fovMinOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
	//drawOptionSizer->Add(fovMaxOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   drawOptionSizer->Add(20, 2, 0, wxALIGN_LEFT|wxALL, bsize);
   
   GmatStaticBoxSizer *drawOptionStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "Drawing Option");
   drawOptionStaticSizer->Add(drawOptionSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Available spcecrafts and objects
   //-----------------------------------------------------------------
   wxStaticText *scAvailableLabel =
      new wxStaticText(this, -1, wxT("Spacecraft"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mSpacecraftListBox = theGuiManager->
      GetSpacecraftListBox(this, ID_LISTBOX, wxSize(150,65), &mExcludedScList);
   wxStaticText *coAvailableLabel =
      new wxStaticText(this, -1, wxT("Celestial Object"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mCelesObjectListBox = theGuiManager->
      GetCelestialPointListBox(this, ID_LISTBOX, wxSize(150,65), empty);
   
   wxBoxSizer *availObjSizer = new wxBoxSizer(wxVERTICAL);
   availObjSizer->Add(scAvailableLabel, 0, wxALIGN_CENTRE|wxALL, bsize);
   availObjSizer->Add(mSpacecraftListBox, 0, wxALIGN_CENTRE|wxALL, bsize);
   availObjSizer->Add(coAvailableLabel, 0, wxALIGN_CENTRE|wxALL, bsize);
   availObjSizer->Add(mCelesObjectListBox, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // add, remove, clear buttons
   //-----------------------------------------------------------------
   addScButton = new wxButton(this, ADD_SP_BUTTON, wxT("-->"),
                              wxDefaultPosition, wxSize(arrowW, 20), 0);
   removeScButton = new wxButton(this, REMOVE_SP_BUTTON, wxT("<--"),
                                 wxDefaultPosition, wxSize(arrowW ,20), 0);
   clearScButton = new wxButton(this, CLEAR_SP_BUTTON, wxT("<="),
                                wxDefaultPosition, wxSize(arrowW, 20), 0);
   
   wxBoxSizer *arrowButtonsSizer = new wxBoxSizer(wxVERTICAL);
   arrowButtonsSizer->Add(addScButton, 0, wxALIGN_CENTRE|wxALL, bsize);
   arrowButtonsSizer->Add(removeScButton, 0, wxALIGN_CENTRE|wxALL, bsize);
   arrowButtonsSizer->Add(clearScButton, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Selected spacecraft and objects
   //-----------------------------------------------------------------
   wxStaticText *titleSelectedSc =
      new wxStaticText(this, -1, wxT("Selected Spacecraft"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   wxStaticText *titleSelectedObj =
      new wxStaticText(this, -1, wxT("Selected Celestial Object"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mSelectedScListBox =
      new wxListBox(this, SC_SEL_LISTBOX, wxDefaultPosition, wxSize(150,65), //0,
                    emptyList, wxLB_SINGLE);
   mSelectedObjListBox =
      new wxListBox(this, OBJ_SEL_LISTBOX, wxDefaultPosition, wxSize(150,65), //0,
                    emptyList, wxLB_SINGLE);
   
   wxBoxSizer *mObjSelectedSizer = new wxBoxSizer(wxVERTICAL);
   mObjSelectedSizer->Add(titleSelectedSc, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjSelectedSizer->Add(mSelectedScListBox, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjSelectedSizer->Add(titleSelectedObj, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjSelectedSizer->Add(mSelectedObjListBox, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Draw object, orbit and target color
   //-----------------------------------------------------------------
   mDrawObjectCheckBox =
      new wxCheckBox(this, CHECKBOX, wxT("Draw Object"),
                     wxDefaultPosition, wxSize(-1, -1), 0);
   
   wxStaticText *orbitColorLabel =
      new wxStaticText(this, -1, wxT("Orbit Color"),
                       wxDefaultPosition, wxSize(-1,-1), wxALIGN_CENTRE);
   mTargetColorButton = new wxButton(this, TARGET_COLOR_BUTTON, "",
                                     wxDefaultPosition, wxSize(colorW, 20), 0);
   mTargetColorLabel =
      new wxStaticText(this, -1, wxT("Target Color"),
                       wxDefaultPosition, wxSize(-1,-1), wxALIGN_CENTRE);
   mOrbitColorButton = new wxButton(this, ORBIT_COLOR_BUTTON, "",
                                    wxDefaultPosition, wxSize(colorW, 20), 0);
   
   wxFlexGridSizer *scOptionSizer1 = new wxFlexGridSizer(1, 0, 0);
   scOptionSizer1->Add(mDrawObjectCheckBox, 0, wxALIGN_LEFT|wxALL, bsize);
   scOptionSizer1->Add(20, 10, 0, wxALIGN_LEFT|wxALL, bsize);
   scOptionSizer1->Add(orbitColorLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   scOptionSizer1->Add(mOrbitColorButton, 0, wxALIGN_LEFT|wxALL, bsize);
   scOptionSizer1->Add(mTargetColorLabel, 0, wxALIGN_LEFT|wxALL, bsize);
   scOptionSizer1->Add(mTargetColorButton, 0, wxALIGN_LEFT|wxALL, bsize);
   
   mScOptionSizer = new wxBoxSizer(wxVERTICAL);
   mScOptionSizer->Add(scOptionSizer1, 0, wxALIGN_LEFT|wxALL, bsize);
   
   mObjectSizer = new wxFlexGridSizer(5, 0, 0);
   mObjectSizer->Add(availObjSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjectSizer->Add(arrowButtonsSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjectSizer->Add(mObjSelectedSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   mObjectSizer->Add(mScOptionSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   GmatStaticBoxSizer *viewObjectStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "View Object");
   viewObjectStaticSizer->Add(mObjectSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // View point reference
   //-----------------------------------------------------------------
   mViewPointRef1TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewPointRef2TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewPointRef3TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewPointRefStaticText = new wxStaticText(this, -1, wxT("km"),
     wxDefaultPosition, wxSize(-1,-1), 0);
   
   mViewPointRefSizer = new wxBoxSizer(wxHORIZONTAL);
   mViewPointRefSizer->Add(mViewPointRef1TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointRefSizer->Add(mViewPointRef2TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointRefSizer->Add(mViewPointRef3TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointRefSizer->Add(mViewPointRefStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // View point vector
   //-----------------------------------------------------------------
   mViewPointVec1TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewPointVec2TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewPointVec3TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("30000"), wxDefaultPosition, wxSize(60,-1), 0);
   
   mViewPointVecStaticText = new wxStaticText(this, -1, wxT("km"),
     wxDefaultPosition, wxSize(-1,-1), 0);
   
   mViewPointVectorSizer = new wxBoxSizer(wxHORIZONTAL);
   mViewPointVectorSizer->Add(mViewPointVec1TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointVectorSizer->Add(mViewPointVec2TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointVectorSizer->Add(mViewPointVec3TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewPointVectorSizer->Add(mViewPointVecStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // View direction
   //-----------------------------------------------------------------
   mViewDirVectorSizer = new wxBoxSizer(wxHORIZONTAL);
   
   mViewDir1TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewDir2TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("0"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewDir3TextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT("-1"), wxDefaultPosition, wxSize(60,-1), 0);
   mViewDirStaticText = new wxStaticText(this, -1, wxT("km"),
     wxDefaultPosition, wxSize(-1,-1), 0);

   mViewDirVectorSizer->Add(mViewDir1TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDirVectorSizer->Add(mViewDir2TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDirVectorSizer->Add(mViewDir3TextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDirVectorSizer->Add(mViewDirStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxStaticText *coordSysLabel =
      new wxStaticText(this, -1, wxT("Coordinate System"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mCoordSysComboBox =
      theGuiManager->GetCoordSysComboBox(this, ID_COMBOBOX, wxSize(120,-1));
   
   wxStaticText *viewPointRefLabel =
      new wxStaticText(this, -1, wxT("View Point Reference"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewPointRefComboBox =
      theGuiManager->GetSpacePointComboBox(this, ID_COMBOBOX, wxSize(120,-1), true);
   
   wxStaticText *viewPointVectorLabel =
      new wxStaticText(this, -1, wxT("View Point Vector"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewPointVectorComboBox =
      theGuiManager->GetSpacePointComboBox(this, ID_COMBOBOX, wxSize(120,-1), true);
   
   //-----------------------------------------------------------------
   // Add to view definitin sizer
   //-----------------------------------------------------------------
   mViewDefSizer = new wxFlexGridSizer(3, 0, 0);
   mViewDefSizer->Add(coordSysLabel, 0, wxALIGN_RIGHT|wxALL, bsize);
   mViewDefSizer->Add(mCoordSysComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDefSizer->Add(emptyStaticText, 0, wxALIGN_LEFT|wxALL, bsize);

   mViewDefSizer->Add(viewPointRefLabel, 0, wxALIGN_RIGHT|wxALL, bsize);
   mViewDefSizer->Add(mViewPointRefComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDefSizer->Add(mViewPointRefSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   mViewDefSizer->Add(viewPointVectorLabel, 0, wxALIGN_RIGHT|wxALL, bsize);
   mViewDefSizer->Add(mViewPointVectorComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDefSizer->Add(mViewPointVectorSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxStaticText *viewScaleFactorLabel =
      new wxStaticText(this, -1, wxT("View Scale Factor"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewScaleFactorTextCtrl =
      new wxTextCtrl(this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, wxSize(120,-1), 0);

   mViewDefSizer->Add(viewScaleFactorLabel, 0, wxALIGN_RIGHT|wxALL, bsize);
   mViewDefSizer->Add(mViewScaleFactorTextCtrl, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDefSizer->Add(emptyStaticText, 0, wxALIGN_LEFT|wxALL, bsize);
   
   wxStaticText *viewDirectionLabel =
      new wxStaticText(this, -1, wxT("View Direction"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewDirectionComboBox =
      theGuiManager->GetSpacePointComboBox(this, ID_COMBOBOX, wxSize(120,-1), true);
   
   mViewDefSizer->Add(viewDirectionLabel, 0, wxALIGN_RIGHT|wxALL, bsize);
   mViewDefSizer->Add(mViewDirectionComboBox, 0, wxALIGN_LEFT|wxALL, bsize);
   mViewDefSizer->Add(mViewDirVectorSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   GmatStaticBoxSizer *viewDefStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "View Definition");
   viewDefStaticSizer->Add(mViewDefSizer, 0, wxALIGN_LEFT|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // View up definition
   //-----------------------------------------------------------------
   wxStaticText *upCsLabel =
      new wxStaticText(this, -1, wxT("Coordinate System"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewUpCsComboBox =
      theGuiManager->GetCoordSysComboBox(this, ID_COMBOBOX, wxSize(120,-1));
   wxStaticText *upAxisLabel =
      new wxStaticText(this, -1, wxT("Axis"),
                       wxDefaultPosition, wxSize(-1,-1), 0);
   mViewUpAxisComboBox =
      new wxComboBox(this, ID_COMBOBOX, wxT(""), wxDefaultPosition, 
                     wxSize(50,-1),6, axisArray, wxCB_READONLY);
   
   wxBoxSizer *viewUpSizer = new wxBoxSizer(wxHORIZONTAL);
   viewUpSizer->Add(upCsLabel, 0, wxALIGN_CENTER|wxALL, bsize);
   viewUpSizer->Add(mViewUpCsComboBox, 0, wxALIGN_CENTER|wxALL, bsize);
   viewUpSizer->AddSpacer(20);
   viewUpSizer->Add(upAxisLabel, 0, wxALIGN_CENTER|wxALL, bsize);
   viewUpSizer->Add(mViewUpAxisComboBox, 0, wxALIGN_CENTER|wxALL, bsize);
   
   GmatStaticBoxSizer *upDefStaticSizer =
      new GmatStaticBoxSizer(wxVERTICAL, this, "View Up Definition");
   
   upDefStaticSizer->Add(viewUpSizer, 0, wxALIGN_LEFT|wxALL, bsize);
      
   //-----------------------------------------------------------------
   // Add to page sizer
   //-----------------------------------------------------------------
   
   wxFlexGridSizer *pageSizer1 = new wxFlexGridSizer(3, 2, 0, 0);
   pageSizer1->Add(plotOptionStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);
   pageSizer1->Add(viewObjectStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);
   
   pageSizer1->Add(drawOptionStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);   
   pageSizer1->Add(viewDefStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);
   
   pageSizer1->Add(viewOptionStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);
   pageSizer1->Add(upDefStaticSizer, 0, wxALIGN_CENTRE|wxGROW|wxALL, bsize);
   
   //-----------------------------------------------------------------
   // Add to middle sizer
   //-----------------------------------------------------------------
   wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
   pageSizer->Add(pageSizer1, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   theMiddleSizer->Add(pageSizer, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   #if DEBUG_OPENGL_PANEL_CREATE
   MessageInterface::ShowMessage
      ("Enhanced3DViewPanel::Create() Exiting sizers for Windows\n");
   #endif
   
   #if DEBUG_OPENGL_PANEL_CREATE
   MessageInterface::ShowMessage("Enhanced3DViewPanel::Create() exiting...\n");
   #endif
}


//------------------------------------------------------------------------------
// virtual void LoadData()
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::LoadData()
{
   #if DEBUG_OPENGL_PANEL_LOAD
   MessageInterface::ShowMessage("Enhanced3DViewPanel::LoadData() entered.\n");
   #endif
   
   try
   {
      // load data from the core engine
      wxString str;
      Real rval;
      
      str.Printf("%d", mEnhanced3DView->GetIntegerParameter("DataCollectFrequency"));
      mDataCollectFreqTextCtrl->SetValue(str);
      str.Printf("%d", mEnhanced3DView->GetIntegerParameter("UpdatePlotFrequency"));
      mUpdatePlotFreqTextCtrl->SetValue(str);
      str.Printf("%d", mEnhanced3DView->GetIntegerParameter("NumPointsToRedraw"));
      mNumPointsToRedrawTextCtrl->SetValue(str);
      
      mShowPlotCheckBox->SetValue(mEnhanced3DView->IsActive());
      mXYPlaneCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("XYPlane") == "On");
      mEclipticPlaneCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("CelestialPlane") == "On");
      mWireFrameCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("WireFrame") == "On");
      mAxesCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("Axes") == "On");
      mGridCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("Grid") == "On");
      mOriginSunLineCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("SunLine") == "On");
      mUseInitialViewCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("UseInitialView") == "On");
      mSolverIterComboBox->
         SetValue(mEnhanced3DView->GetStringParameter("SolverIterations").c_str());
		mEnableStarsCheckBox->
			SetValue(mEnhanced3DView->GetOnOffParameter("EnableStars") == "On");
		mEnableConstellationsCheckBox->
			SetValue(mEnhanced3DView->GetOnOffParameter("EnableConstellations") == "On");
      
      #ifdef __ENABLE_GL_PERSPECTIVE__
      mPerspectiveModeCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("PerspectiveMode") == "On");
      mUseFixedFovCheckBox->
         SetValue(mEnhanced3DView->GetOnOffParameter("UseFixedFov") == "On");
      rval = mEnhanced3DView->GetRealParameter("FixedFovAngle");
      mFixedFovTextCtrl->SetValue(theGuiManager->ToWxString(rval));
      #endif
      
      mCoordSysComboBox->SetStringSelection
         (mEnhanced3DView->GetStringParameter("CoordinateSystem").c_str());
      
      //--------------------------------------------------------------
      // load view up direction info
      //--------------------------------------------------------------
      mViewUpAxisComboBox->
         SetStringSelection(mEnhanced3DView->GetStringParameter("ViewUpAxis").c_str());
      mViewUpCsComboBox->
         SetStringSelection(mEnhanced3DView->GetStringParameter("ViewUpCoordinateSystem").c_str());
      
      //--------------------------------------------------------------
      // load ViewPoint info
      //--------------------------------------------------------------
      wxString viewObj;
      viewObj = mEnhanced3DView->GetStringParameter("ViewPointRefType").c_str();
      if (viewObj != "Vector")
         viewObj = mEnhanced3DView->GetStringParameter("ViewPointReference").c_str();
      mViewPointRefComboBox->SetStringSelection(viewObj);
      
      viewObj = mEnhanced3DView->GetStringParameter("ViewPointVectorType").c_str();
      if (viewObj != "Vector")
         viewObj = mEnhanced3DView->GetStringParameter("ViewPointVector").c_str();
      mViewPointVectorComboBox->SetStringSelection(viewObj);
      
      viewObj = mEnhanced3DView->GetStringParameter("ViewDirectionType").c_str();
      
      if (viewObj != "Vector")
         viewObj = mEnhanced3DView->GetStringParameter("ViewDirection").c_str();
      mViewDirectionComboBox->SetStringSelection(viewObj);
      
      rval = mEnhanced3DView->GetRealParameter("ViewScaleFactor");
      mViewScaleFactorTextCtrl->SetValue(theGuiManager->ToWxString(rval));
      
      // show vector if viewpoint vector name is Vector
      if (mViewPointRefComboBox->GetStringSelection() == "Vector")
      {
         Rvector3 vec = mEnhanced3DView->GetVector("ViewPointReference");
         
         #if DEBUG_OPENGL_PANEL_LOAD
         MessageInterface::ShowMessage
            ("   ViewPointReference = %s\n", vec.ToString().c_str());
         #endif
         
         mViewPointRef1TextCtrl->SetValue(theGuiManager->ToWxString(vec[0]));
         mViewPointRef2TextCtrl->SetValue(theGuiManager->ToWxString(vec[1]));
         mViewPointRef3TextCtrl->SetValue(theGuiManager->ToWxString(vec[2]));

         mViewDefSizer->Show(mViewPointRefSizer, true);
      }
      else
      {
         mViewDefSizer->Show(mViewPointRefSizer, false);
      }
      
      // show vector if viewpoint vector name is Vector
      if (mViewPointVectorComboBox->GetStringSelection() == "Vector")
      {
         Rvector3 vec = mEnhanced3DView->GetVector("ViewPointVector");
         
         #if DEBUG_OPENGL_PANEL_LOAD
         MessageInterface::ShowMessage
            ("   ViewPointVector = %s\n", vec.ToString().c_str());
         #endif
         
         mViewPointVec1TextCtrl->SetValue(theGuiManager->ToWxString(vec[0]));
         mViewPointVec2TextCtrl->SetValue(theGuiManager->ToWxString(vec[1]));
         mViewPointVec3TextCtrl->SetValue(theGuiManager->ToWxString(vec[2]));
         
         mViewDefSizer->Show(mViewPointVectorSizer, true);
      }
      else
      {
         mViewDefSizer->Show(mViewPointVectorSizer, false);
      }
      
      // show vector if view direction name is Vector
      if (mViewDirectionComboBox->GetStringSelection() == "Vector")
      {
         Rvector3 vec = mEnhanced3DView->GetVector("ViewDirection");
         
         #if DEBUG_OPENGL_PANEL_LOAD
         MessageInterface::ShowMessage
            ("   ViewDirector = %s\n", vec.ToString().c_str());
         #endif
         
         mViewDir1TextCtrl->SetValue(theGuiManager->ToWxString(vec[0]));
         mViewDir2TextCtrl->SetValue(theGuiManager->ToWxString(vec[1]));
         mViewDir3TextCtrl->SetValue(theGuiManager->ToWxString(vec[2]));
         
         mViewDefSizer->Show(mViewDirVectorSizer, true);
      }
      else
      {
         mViewDefSizer->Show(mViewDirVectorSizer, false);
      }
      
      //--------------------------------------------------------------
      // get SpacePoint list to plot
      //--------------------------------------------------------------
      StringArray spNameList = mEnhanced3DView->GetSpacePointList();
      int spCount = spNameList.size();
      
      #if DEBUG_OPENGL_PANEL_LOAD
      MessageInterface::ShowMessage
         ("Enhanced3DViewPanel::LoadData() spCount=%d\n", spCount);
      
      for (int i=0; i<spCount; i++)
      {
         MessageInterface::ShowMessage
            ("Enhanced3DViewPanel::LoadData() spNameList[%d]=%s\n",
             i, spNameList[i].c_str());
      }
      #endif
      
      StringArray scNameArray;
      StringArray nonScNameArray;
      
      // get spacecraft and non-spacecraft list
      for (int i=0; i<spCount; i++)
      {
         if (mSpacecraftListBox->FindString(spNameList[i].c_str()) == wxNOT_FOUND)
            nonScNameArray.push_back(spNameList[i]);
         else
            scNameArray.push_back(spNameList[i]);
      }
      
      mScCount = scNameArray.size();
      mNonScCount = nonScNameArray.size();

      #if DEBUG_OPENGL_PANEL_LOAD
      MessageInterface::ShowMessage
         ("Enhanced3DViewPanel::LoadData() mScCount=%d, mNonScCount=%d\n",
          mScCount, mNonScCount);
      #endif
      
      //--------------------------------------------------------------
      // get object show, color
      //--------------------------------------------------------------
      if (mScCount > 0)
      {
         wxString *scNames = new wxString[mScCount];
         for (int i=0; i<mScCount; i++)
         {
            scNames[i] = scNameArray[i].c_str();
            
            mDrawObjectMap[scNameArray[i]] =
               mEnhanced3DView->GetShowObject(scNameArray[i]);
            mOrbitColorMap[scNameArray[i]]
               = RgbColor(mEnhanced3DView->GetColor("Orbit", scNameArray[i]));
            mTargetColorMap[scNameArray[i]]
               = RgbColor(mEnhanced3DView->GetColor("Target", scNameArray[i]));
            
            #if DEBUG_OPENGL_PANEL_LOAD > 1
            MessageInterface::ShowMessage
               ("Enhanced3DViewPanel::LoadData() scName=%s, orbColor=%u, "
                "targetColor=%u\n", scNameArray[i].c_str(),
                mOrbitColorMap[scNameArray[i]].GetIntColor(),
                mTargetColorMap[scNameArray[i]].GetIntColor());
            #endif
         }
         
         mSelectedScListBox->Set(mScCount, scNames);
         delete [] scNames;
      }
      
      if (mNonScCount > 0)
      {
         wxString *nonScNames = new wxString[mNonScCount];
         for (int i=0; i<mNonScCount; i++)
         {
            nonScNames[i] = nonScNameArray[i].c_str();
         
            mDrawObjectMap[nonScNameArray[i]] =
               mEnhanced3DView->GetShowObject(nonScNameArray[i]);
            mOrbitColorMap[nonScNameArray[i]]
               = RgbColor(mEnhanced3DView->GetColor("Orbit", nonScNameArray[i]));
            mTargetColorMap[nonScNameArray[i]]
               = RgbColor(mEnhanced3DView->GetColor("Target", nonScNameArray[i]));
         }
         
         mSelectedObjListBox->Set(mNonScCount, nonScNames);
         delete [] nonScNames;
      }
      
      // show spacecraft option
      mSelectedScListBox->SetSelection(0);
      ShowSpacePointOption(mSelectedScListBox->GetStringSelection(), true);
      
      
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage().c_str());
   }
   
   // deselect available object list
   mSpacecraftListBox->Deselect(mSpacecraftListBox->GetSelection());
   mCelesObjectListBox->Deselect(mCelesObjectListBox->GetSelection());

   #ifdef __ENABLE_GL_PERSPECTIVE__
   mPerspectiveModeCheckBox->Enable();
   #endif
   
   EnableUpdate(false);
   
   #ifdef __ENABLE_GL_PERSPECTIVE__
   if (!mUseFixedFovCheckBox->IsChecked())
   {
      mFovLabel->Disable();
      mFixedFovTextCtrl->Disable();
   }

   // if perspective mode, enalbe fov
   if (mPerspectiveModeCheckBox->IsChecked())
   {
      mUseFixedFovCheckBox->Enable();
      if (mUseFixedFovCheckBox->IsChecked())
      {
         mFovLabel->Enable();
         mFixedFovTextCtrl->Enable();
      }
      else
      {
         mFovLabel->Disable();
         mFixedFovTextCtrl->Disable();
      }
   }
   else
   {
      mUseFixedFovCheckBox->Disable();
      mFovLabel->Disable();
      mFixedFovTextCtrl->Disable();
   }
   #endif
   
   
   #if DEBUG_OPENGL_PANEL_LOAD
   MessageInterface::ShowMessage("Enhanced3DViewPanel::LoadData() exiting.\n");
   #endif
}


//------------------------------------------------------------------------------
// virtual void SaveData()
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::SaveData()
{

   #if DEBUG_OPENGL_PANEL_SAVE
   MessageInterface::ShowMessage("Enhanced3DViewPanel::SaveData() entered.\n");
   #endif

   canClose = true;
   std::string str1, str2;
   Integer collectFreq = 0, updateFreq = 0, pointsToRedraw = 0, starCount = 0,
		initialFOV = 0, minFOV = 0, maxFOV = 0;
   Real scaleFactor;
   Real viewRef[3], viewVec[3], viewDir[3];
   Rvector3 vec;
   bool setVector = false;
   
   //-----------------------------------------------------------------
   // check values from text field
   //-----------------------------------------------------------------

   if (mHasIntegerDataChanged)
   {
      CheckInteger(collectFreq, mDataCollectFreqTextCtrl->GetValue().c_str(),
                   "DataCollectFrequency", "Integer Number > 0", false, true, true);
      
      CheckInteger(updateFreq, mUpdatePlotFreqTextCtrl->GetValue().c_str(),
                   "UpdatePlotFrequency", "Integer Number > 0", false, true, true);
      
      CheckInteger(pointsToRedraw, mNumPointsToRedrawTextCtrl->GetValue().c_str(),
                   "NumPointsToRedraw", "Integer Number >= 0", false, true, true, true);

		CheckInteger(starCount, mStarCountTextCtrl->GetValue().c_str(),
						 "StarCount", "Integer Number >= 0", false, true);
		CheckInteger(initialFOV, mFovTextCtrl->GetValue().c_str(),
						 "InitialFOV", "");
		CheckInteger(minFOV, mFovMinTextCtrl->GetValue().c_str(),
						 "MinFOV", "");
		CheckInteger(maxFOV, mFovMaxTextCtrl->GetValue().c_str(),
						 "MaxFOV", "");
   }
   
   if ((mViewPointRefComboBox->GetStringSelection() == "Vector")||
       (mViewPointVectorComboBox->GetStringSelection() == "Vector") ||
       (mViewDirectionComboBox->GetStringSelection() == "Vector"))
      setVector = true;
   
   if (setVector || mHasRealDataChanged)
   {
      CheckReal(scaleFactor, mViewScaleFactorTextCtrl->GetValue().c_str(),
                "ViewScaleFactor", "Real Number > 0", false, true);
      
      if (mViewPointRefComboBox->GetStringSelection() == "Vector")
      {
         CheckReal(viewRef[0], mViewPointRef1TextCtrl->GetValue().c_str(),
                   "ViewPointReference[1]", "Real Number");
         CheckReal(viewRef[1], mViewPointRef2TextCtrl->GetValue().c_str(),
                   "ViewPointReference[2]", "Real Number");
         CheckReal(viewRef[2], mViewPointRef3TextCtrl->GetValue().c_str(),
                   "ViewPointReference[3]", "Real Number");
      }
      
      if (mViewPointVectorComboBox->GetStringSelection() == "Vector")
      {
         CheckReal(viewVec[0], mViewPointVec1TextCtrl->GetValue().c_str(),
                   "ViewPointVector[1]", "Real Number");
         CheckReal(viewVec[1], mViewPointVec2TextCtrl->GetValue().c_str(),
                   "ViewPointVector[2]", "Real Number");
         CheckReal(viewVec[2], mViewPointVec3TextCtrl->GetValue().c_str(),
                   "ViewPointVector[3]", "Real Number");
      }
      
      if (mViewDirectionComboBox->GetStringSelection() == "Vector")
      {
         CheckReal(viewDir[0], mViewDir1TextCtrl->GetValue().c_str(),
                   "ViewDirection[1]", "Real Number");
         CheckReal(viewDir[1], mViewDir2TextCtrl->GetValue().c_str(),
                   "ViewDirection[2]", "Real Number");
         CheckReal(viewDir[2], mViewDir3TextCtrl->GetValue().c_str(),
                   "ViewDirection[3]", "Real Number");
      }
   }
   
   if (!canClose)
      return;
   
   //-----------------------------------------------------------------
   // save values to base, base code should do the range checking
   //-----------------------------------------------------------------
   try
   {
      if (mHasIntegerDataChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving Integer data\n");
         #endif
         
         mHasIntegerDataChanged = false;
         mEnhanced3DView->SetIntegerParameter("DataCollectFrequency", collectFreq);
         mEnhanced3DView->SetIntegerParameter("UpdatePlotFrequency", updateFreq);
         mEnhanced3DView->SetIntegerParameter("NumPointsToRedraw", pointsToRedraw);
			mEnhanced3DView->SetIntegerParameter("StarCount", starCount);
			mEnhanced3DView->SetIntegerParameter("MinFOV", minFOV);
			mEnhanced3DView->SetIntegerParameter("MaxFOV", maxFOV);
			mEnhanced3DView->SetIntegerParameter("InitialFOV", initialFOV);
      }
      
      //--------------------------------------------------------------
      // save view definitions
      //--------------------------------------------------------------
      if (mHasViewInfoChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving View definition\n");
         #endif
         mHasViewInfoChanged = false;
         
         mEnhanced3DView->SetStringParameter
            ("ViewPointReference",
             std::string(mViewPointRefComboBox->GetStringSelection().c_str()));
         mEnhanced3DView->SetStringParameter
            ("ViewPointVector",
             std::string(mViewPointVectorComboBox->GetStringSelection().c_str()));
         mEnhanced3DView->SetStringParameter
            ("ViewDirection",
             std::string(mViewDirectionComboBox->GetStringSelection().c_str()));
      } // end if ( mHasViewInfoChanged)
      
      
      //--------------------------------------------------------------
      // save scale factor and view definition vectors
      //--------------------------------------------------------------
      if (setVector || mHasRealDataChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving Real data\n");
         #endif
         
         mHasViewInfoChanged = false;
         mHasRealDataChanged = false;
         mEnhanced3DView->SetRealParameter("ViewScaleFactor", scaleFactor);
         
         if (mViewPointRefComboBox->GetStringSelection() == "Vector")
         {
            vec.Set(viewRef[0], viewRef[1], viewRef[2]);
            mEnhanced3DView->SetStringParameter("ViewPointRefType", "Vector");
            mEnhanced3DView->SetVector("ViewPointReference", vec);
         }
         
         if (mViewPointVectorComboBox->GetStringSelection() == "Vector")
         {
            vec.Set(viewVec[0], viewVec[1], viewVec[2]);
            mEnhanced3DView->SetStringParameter("ViewPointVectorType", "Vector");
            mEnhanced3DView->SetVector("ViewPointVector", vec);
         }
         
         if (mViewDirectionComboBox->GetStringSelection() == "Vector")
         {
            vec.Set(viewDir[0], viewDir[1], viewDir[2]);
            mEnhanced3DView->SetStringParameter("ViewDirectionType", "Vector");
            mEnhanced3DView->SetVector("ViewDirection", vec);
         }
      }
      
      
      //--------------------------------------------------------------
      // save drawing options
      //--------------------------------------------------------------
      if (mHasDrawingOptionChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving drawing options\n");
         #endif
         mHasDrawingOptionChanged = false;
         
         mEnhanced3DView->Activate(mShowPlotCheckBox->IsChecked());
         
         if (mXYPlaneCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("XYPlane", "On");
         else
            mEnhanced3DView->SetOnOffParameter("XYPlane", "Off");
         
         if (mEclipticPlaneCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("CelestialPlane", "On");
         else
            mEnhanced3DView->SetOnOffParameter("CelestialPlane", "Off");
         
         if (mWireFrameCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("WireFrame", "On");
         else
            mEnhanced3DView->SetOnOffParameter("WireFrame", "Off");
         
         if (mAxesCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("Axes", "On");
         else
            mEnhanced3DView->SetOnOffParameter("Axes", "Off");
         
         if (mGridCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("Grid", "On");
         else
            mEnhanced3DView->SetOnOffParameter("Grid", "Off");
         
         if (mOriginSunLineCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("SunLine", "On");
         else
            mEnhanced3DView->SetOnOffParameter("SunLine", "Off");
         
         //if (mOverlapCheckBox->IsChecked())
         //   mEnhanced3DView->SetOnOffParameter("Overlap", "On");
         //else
         //   mEnhanced3DView->SetOnOffParameter("Overlap", "Off");
         
         if (mUseInitialViewCheckBox->IsChecked())
            mEnhanced3DView->SetOnOffParameter("UseInitialView", "On");
         else
            mEnhanced3DView->SetOnOffParameter("UseInitialView", "Off");
         
         mEnhanced3DView->SetStringParameter("SolverIterations",
                                         mSolverIterComboBox->GetValue().c_str());
      }

		//--------------------------------------------------------------
		// save star options
		//--------------------------------------------------------------
		if (mHasStarOptionChanged){
			mHasStarOptionChanged = false;
			if (mEnableStarsCheckBox->IsChecked())
				mEnhanced3DView->SetOnOffParameter("EnableStars", "On");
			else
				mEnhanced3DView->SetOnOffParameter("EnableStars", "Off");

			if (mEnableConstellationsCheckBox->IsChecked())
				mEnhanced3DView->SetOnOffParameter("EnableConstellations", "On");
			else
				mEnhanced3DView->SetOnOffParameter("EnableConstellations", "Off");
		}
      
      
      //--------------------------------------------------------------
      // save perspective info
      //--------------------------------------------------------------
      #ifdef __ENABLE_GL_PERSPECTIVE__
      #if DEBUG_OPENGL_PANEL_SAVE
      MessageInterface::ShowMessage("   Saving perspective mode data\n");
      #endif
      
      if (mPerspectiveModeCheckBox->IsChecked())
         mEnhanced3DView->SetOnOffParameter("PerspectiveMode", "On");
      else
         mEnhanced3DView->SetOnOffParameter("PerspectiveMode", "Off");
      
      if (mUseFixedFovCheckBox->IsChecked())
         mEnhanced3DView->SetOnOffParameter("UseFixedFov", "On");
      else
         mEnhanced3DView->SetOnOffParameter("UseFixedFov", "Off");
      
      Real fov;
      std::string fovStr = mFixedFovTextCtrl->GetValue();
      if (!GmatStringUtil::ToReal(fovStr, &fov) || fov < 1)
      {
         MessageInterface::PopupMessage(Gmat::ERROR_, msg.c_str(),
                 inputString[0].c_str(), "FixedFovAngle", "Real Number >= 1");
         return;
      }
      mEnhanced3DView->SetRealParameter("FixedFovAngle", fov);
      #endif
      
      
      //--------------------------------------------------------------
      // save spacecraft list
      //--------------------------------------------------------------
      if (mHasSpChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving spacecraft and body list\n");
         #endif
         
         mHasSpChanged = false;
         mHasOrbitColorChanged = true;
         mHasTargetColorChanged = true;
         
         mScCount = mSelectedScListBox->GetCount();
         mNonScCount = mSelectedObjListBox->GetCount();
         
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage
            ("   mScCount=%d, mNonScCount=%d\n", mScCount, mNonScCount);
         #endif
         
         // clear the list first
         mEnhanced3DView->TakeAction("Clear");
         
         // add spacecraft
         for (int i=0; i<mScCount; i++)
         {
            mSelSpName = std::string(mSelectedScListBox->GetString(i).c_str());
            
            #if DEBUG_OPENGL_PANEL_SAVE
            MessageInterface::ShowMessage("   Sc[%d] = %s\n", i, mSelSpName.c_str());
            #endif
            
            mEnhanced3DView->
               SetStringParameter("Add", mSelSpName, i);
         }         
         
         // add non-spacecraft
         for (int i=0; i<mNonScCount; i++)
         {
            mSelSpName = std::string(mSelectedObjListBox->GetString(i).c_str());
            
            #if DEBUG_OPENGL_PANEL_SAVE
            MessageInterface::ShowMessage
               ("Enhanced3DViewPanel::SaveData() NonSc[%d] = %s\n", i,
                mSelSpName.c_str());
            #endif
            
            
            mEnhanced3DView->
               SetStringParameter("Add", mSelSpName, mScCount+i);
         }
      }
      
      //--------------------------------------------------------------
      // save draw object
      //--------------------------------------------------------------
      if (mHasShowObjectChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving show object flags\n");
         #endif
         
         mHasShowObjectChanged = false;
         
         // change draw spacecraft
         for (int i=0; i<mScCount; i++)
         {
            mSelSpName = std::string(mSelectedScListBox->GetString(i).c_str());
         
            mEnhanced3DView->
               SetShowObject( mSelSpName, mDrawObjectMap[mSelSpName]);
         }
         
         // change draw non-spacecraft
         for (int i=0; i<mNonScCount; i++)
         {
            mSelSpName = std::string(mSelectedObjListBox->GetString(i).c_str());
         
            mEnhanced3DView->
               SetShowObject(mSelSpName, mDrawObjectMap[mSelSpName]);
         }
      }
      
      //--------------------------------------------------------------
      // save orbit color
      //--------------------------------------------------------------
      if (mHasOrbitColorChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving orbit colors\n");
         #endif
         
         mHasOrbitColorChanged = false;
         
         // change spacecraft orbit color
         for (int i=0; i<mScCount; i++)
         {
            mSelSpName = std::string(mSelectedScListBox->GetString(i).c_str());
            
            #if DEBUG_OPENGL_PANEL_SAVE
            MessageInterface::ShowMessage
               ("Enhanced3DViewPanel::SaveData() objName=%s, orbColor=%u\n",
                mSelSpName.c_str(), mOrbitColorMap[mSelSpName].GetIntColor());
            #endif
            
            mEnhanced3DView->
               SetColor("Orbit", mSelSpName,
                        mOrbitColorMap[mSelSpName].GetIntColor());
         }
         
         // change non-spacecraft orbit color
         for (int i=0; i<mNonScCount; i++)
         {
            mSelSpName = std::string(mSelectedObjListBox->GetString(i).c_str());
         
            mEnhanced3DView->
               SetColor("Orbit", mSelSpName,
                        mOrbitColorMap[mSelSpName].GetIntColor());
         }
      }
      
      //--------------------------------------------------------------
      // save target color
      //--------------------------------------------------------------
      if (mHasTargetColorChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving target colors\n");
         #endif
         
         mHasTargetColorChanged = false;
         
         // change spacecraft target color
         for (int i=0; i<mScCount; i++)
         {
            mSelSpName = std::string(mSelectedScListBox->GetString(i).c_str());
            
            #if DEBUG_OPENGL_PANEL_SAVE
            MessageInterface::ShowMessage
               ("Enhanced3DViewPanel::SaveData() objName=%s targetColor=%u\n",
                mSelSpName.c_str(), mTargetColorMap[mSelSpName].GetIntColor());
            #endif
            
            mEnhanced3DView->
               SetColor("Target", mSelSpName,
                        mTargetColorMap[mSelSpName].GetIntColor());
         }
      }
      
      //--------------------------------------------------------------
      // save coordinate system
      //--------------------------------------------------------------
      if (mHasCoordSysChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving view coordinate system\n");
         #endif
         
         mHasCoordSysChanged = false;
         mEnhanced3DView->SetStringParameter
            ("CoordinateSystem",
             std::string(mCoordSysComboBox->GetStringSelection().c_str()));
      }
      
      //--------------------------------------------------------------
      // save view up direction info
      //--------------------------------------------------------------
      if (mHasViewUpInfoChanged)
      {
         #if DEBUG_OPENGL_PANEL_SAVE
         MessageInterface::ShowMessage("   Saving view up coordinate system\n");
         #endif
         
         mHasViewUpInfoChanged = false;
         mEnhanced3DView->SetStringParameter
            ("ViewUpCoordinateSystem",
             std::string(mViewUpCsComboBox->GetStringSelection().c_str()));
         mEnhanced3DView->SetStringParameter
            ("ViewUpAxis",
             std::string(mViewUpAxisComboBox->GetStringSelection().c_str()));
      }
      
      //--------------------------------------------------------------
      // Just warning message
      //--------------------------------------------------------------
      // Check if Sun was added to draw Sun line
      if (mOriginSunLineCheckBox->IsChecked() &&
          mSelectedObjListBox->FindString("Sun") == wxNOT_FOUND)
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "\"Sun\" needs to be added to the view object list "
             "to draw Sun line");
      
      EnableUpdate(false);
      canClose = true;
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage().c_str());
   }
   
   #if DEBUG_OPENGL_PANEL_SAVE
   MessageInterface::ShowMessage("Enhanced3DViewPanel::SaveData() exiting.\n");
   #endif
}


//------------------------------------------------------------------------------
// void OnAddSpacePoint(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnAddSpacePoint(wxCommandEvent& event)
{
   if (mSpacecraftListBox->GetSelection() != -1)
   {
      // get string in first list and then search for it
      // in the second list
      wxString s = mSpacecraftListBox->GetStringSelection();
      int found = mSelectedScListBox->FindString(s);
    
      // if the string wasn't found in the second list, insert it
      if (found == wxNOT_FOUND)
      {
         mSelectedScListBox->Append(s);
         mSelectedScListBox->SetStringSelection(s);
      
         // select next available item
         mSpacecraftListBox->
            SetSelection(mSpacecraftListBox->GetSelection()+1);

         // deselect selected other object
         mSelectedObjListBox->Deselect(mSelectedObjListBox->GetSelection());
         
         mDrawObjectMap[s.c_str()] = true;
         ShowSpacePointOption(s, true, true, GmatColor::RED32);
         mHasSpChanged = true;
         EnableUpdate(true);
      }
   }
   else if (mCelesObjectListBox->GetSelection() != -1)
   {
      wxString s = mCelesObjectListBox->GetStringSelection();
      int found = mSelectedObjListBox->FindString(s);
    
      // if the string wasn't found in the second list, insert it
      if (found == wxNOT_FOUND)
      {
         mSelectedObjListBox->Append(s);
         mSelectedObjListBox->SetStringSelection(s);
         
         // select next available item
         mCelesObjectListBox->
            SetSelection(mCelesObjectListBox->GetSelection()+1);
         
         // deselect selected spacecraft
         mSelectedScListBox->Deselect(mSelectedScListBox->GetSelection());
         
         mDrawObjectMap[s.c_str()] = true;
         ShowSpacePointOption(s, true, false, GmatColor::L_BROWN32);
         mHasSpChanged = true;
         EnableUpdate(true);
      }
   }
}


//------------------------------------------------------------------------------
// void OnRemoveSpacePoint(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnRemoveSpacePoint(wxCommandEvent& event)
{
   if (mSelectedScListBox->GetSelection() != -1)
   {
      int sel = mSelectedScListBox->GetSelection();
      mSelectedScListBox->Delete(sel);
   
      if (sel-1 < 0)
      {
         mSelectedScListBox->SetSelection(0);
         if (mSelectedScListBox->GetCount() == 0)
            ShowSpacePointOption("", false); // hide spacecraft color, etc
         else
            ShowSpacePointOption(mSelectedScListBox->GetStringSelection(), true);
      }
      else
      {
         mSelectedScListBox->SetSelection(sel-1);
         ShowSpacePointOption(mSelectedScListBox->GetStringSelection(), true);
      }
   }
   else if (mSelectedObjListBox->GetSelection() != -1)
   {
      int sel = mSelectedObjListBox->GetSelection();
      mSelectedObjListBox->Delete(sel);
   
      if (sel-1 < 0)
      {
         mSelectedObjListBox->SetSelection(0);
         if (mSelectedObjListBox->GetCount() == 0)
            ShowSpacePointOption("", false); // hide spacecraft color, etc
         else
            ShowSpacePointOption(mSelectedObjListBox->GetStringSelection(), true);
      }
      else
      {
         mSelectedObjListBox->SetSelection(sel-1);
         ShowSpacePointOption(mSelectedObjListBox->GetStringSelection(), true);
      }
   }
   
   mHasSpChanged = true;
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnClearSpacePoint(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnClearSpacePoint(wxCommandEvent& event)
{
   if (mSelectedScListBox->GetSelection() != -1)
   {
      mSelectedScListBox->Clear();
   }
   else if (mSelectedObjListBox->GetSelection() != -1)
   {
      mSelectedObjListBox->Clear();
   }
   
   ShowSpacePointOption("", false);
   mHasSpChanged = true;
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnSelectAvailObject(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnSelectAvailObject(wxCommandEvent& event)
{
   if (event.GetEventObject() == mSpacecraftListBox)
      mCelesObjectListBox->Deselect(mCelesObjectListBox->GetSelection());
   else if (event.GetEventObject() == mCelesObjectListBox)
      mSpacecraftListBox->Deselect(mSpacecraftListBox->GetSelection());
}


//------------------------------------------------------------------------------
// void OnSelectSpacecraft(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnSelectSpacecraft(wxCommandEvent& event)
{
   ShowSpacePointOption(mSelectedScListBox->GetStringSelection(), true);
   mSelectedObjListBox->Deselect(mSelectedObjListBox->GetSelection());
}


//------------------------------------------------------------------------------
// void OnSelectOtherObject(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnSelectOtherObject(wxCommandEvent& event)
{
   ShowSpacePointOption(mSelectedObjListBox->GetStringSelection(), true, false);
   mSelectedScListBox->Deselect(mSelectedScListBox->GetSelection());
}


//------------------------------------------------------------------------------
// void OnCheckBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnCheckBoxChange(wxCommandEvent& event)
{
   #ifdef __ENABLE_GL_PERSPECTIVE__
   if (event.GetEventObject() == mPerspectiveModeCheckBox)
   {
      if (mPerspectiveModeCheckBox->IsChecked())
      {
         mUseFixedFovCheckBox->Enable();
         if (mUseFixedFovCheckBox->IsChecked())
         {
            mFovLabel->Enable();
            mFixedFovTextCtrl->Enable();
         }
         else
         {
            mFovLabel->Disable();
            mFixedFovTextCtrl->Disable();
         }
      }
      else
      {
          mUseFixedFovCheckBox->Disable();
          mFovLabel->Disable();
          mFixedFovTextCtrl->Disable();
      }
   }
   else if (event.GetEventObject() == mUseFixedFovCheckBox)
   {
      if (mUseFixedFovCheckBox->IsChecked())
      {
         mFovLabel->Enable();
         mFixedFovTextCtrl->Enable();
      }
      else
      {
         mFovLabel->Disable();
         mFixedFovTextCtrl->Disable();
      }
   }
   #endif
   
   
   if (event.GetEventObject() == mDrawObjectCheckBox)
   {
      if (mSelectedScListBox->GetSelection() != -1)
      {
         mSelSpName = std::string(mSelectedScListBox->GetStringSelection().c_str());
         mDrawObjectMap[mSelSpName] = mDrawObjectCheckBox->GetValue();
         mHasShowObjectChanged = true;
      }
      else if (mSelectedObjListBox->GetSelection() != -1)
      {
         mSelSpName = std::string(mSelectedObjListBox->GetStringSelection().c_str());
         mDrawObjectMap[mSelSpName] = mDrawObjectCheckBox->GetValue();
         mHasShowObjectChanged = true;
      }
      
      #if DEBUG_OPENGL_PANEL_CHECKBOX
      MessageInterface::ShowMessage
         ("Enhanced3DViewPanel::OnCheckBoxChange() mSelSpName=%s, show=%d\n",
          mSelSpName.c_str(), mDrawObjectMap[mSelSpName]);
      #endif
   }
	else if (event.GetEventObject() == mEnableStarsCheckBox)
	{
		if (mEnableStarsCheckBox->GetValue()){
			mEnableConstellationsCheckBox->Enable();
			mStarCountTextCtrl->Enable();
		}
		else {
			mEnableConstellationsCheckBox->Disable();
			mEnableConstellationsCheckBox->SetValue(false);
			mStarCountTextCtrl->Disable();
		}
		mHasStarOptionChanged = true;
	}
	else if (event.GetEventObject() == mEnableConstellationsCheckBox)
	{
		mHasStarOptionChanged = true;
	}
   else
   {
      mHasDrawingOptionChanged = true;
   }
   
   EnableUpdate(true);
}

//------------------------------------------------------------------------------
// void OnOrbitColorClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnOrbitColorClick(wxCommandEvent& event)
{
   wxColourData data;
   data.SetColour(mOrbitColor);

   wxColourDialog dialog(this, &data);
   //dialog.CenterOnParent();
   dialog.Center();
   
   if (dialog.ShowModal() == wxID_OK)
   {
      // if spacecraft is selected
      if (mSelectedScListBox->GetSelection() != -1)
      {
         mSelSpName = std::string(mSelectedScListBox->GetStringSelection().c_str());
      
         mOrbitColor = dialog.GetColourData().GetColour();
         mOrbitColorButton->SetBackgroundColour(mOrbitColor);
         mOrbitColorButton->Refresh();
         mOrbitColorMap[mSelSpName].Set(mOrbitColor.Red(),
                                        mOrbitColor.Green(),
                                        mOrbitColor.Blue());

         #if DEBUG_OPENGL_PANEL
         MessageInterface::ShowMessage("OnOrbitColorClick() r=%d g=%d b=%d\n",
                                       mOrbitColor.Red(), mOrbitColor.Green(),
                                       mOrbitColor.Blue());
         
         MessageInterface::ShowMessage
            ("OnOrbitColorClick() mOrbitColorMap[%s]=%u\n",
             mSelSpName.c_str(), mOrbitColorMap[mSelSpName].GetIntColor());
         #endif
      }
      else if (mSelectedObjListBox->GetSelection() != -1)
      {
         mSelSpName = std::string(mSelectedObjListBox->GetStringSelection().c_str());
      
         mOrbitColor = dialog.GetColourData().GetColour();
         mOrbitColorButton->SetBackgroundColour(mOrbitColor);
         mOrbitColorButton->Refresh();

         mOrbitColorMap[mSelSpName].Set(mOrbitColor.Red(),
                                        mOrbitColor.Green(),
                                        mOrbitColor.Blue());
         
         #if DEBUG_OPENGL_PANEL
         MessageInterface::ShowMessage
            ("OnOrbitColorClick() r=%d g=%d b=%d\n", mOrbitColor.Red(),
             mOrbitColor.Green(), mOrbitColor.Blue());

         MessageInterface::ShowMessage
            ("OnOrbitColorClick() mOrbitColorMap[%s]=%u\n",
             mSelSpName.c_str(), mOrbitColorMap[mSelSpName].GetIntColor());
         #endif
      }
      
      EnableUpdate(true);
      mHasOrbitColorChanged = true;
   }
}


//------------------------------------------------------------------------------
// void OnTargetColorClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnTargetColorClick(wxCommandEvent& event)
{
   wxColourData data;
   data.SetColour(mTargetColor);

   wxColourDialog dialog(this, &data);
   //dialog.CenterOnParent();
   dialog.Center();
   
   if (dialog.ShowModal() == wxID_OK)
   {
      mSelSpName = std::string(mSelectedScListBox->GetStringSelection().c_str());
      
      mTargetColor = dialog.GetColourData().GetColour();
      mTargetColorButton->SetBackgroundColour(mTargetColor);
      mTargetColorButton->Refresh();
      mTargetColorMap[mSelSpName].Set(mTargetColor.Red(),
                                      mTargetColor.Green(),
                                      mTargetColor.Blue());
      
      #if DEBUG_OPENGL_PANEL
      MessageInterface::ShowMessage
         ("OnTargetColorClick() r=%d g=%d b=%d\n", mTargetColor.Red(),
          mTargetColor.Green(), mTargetColor.Blue());
      
      MessageInterface::ShowMessage
         ("OnTargetColorClick() mTargetColorMap[%s]=%u\n",
          mSelSpName.c_str(), mTargetColorMap[mSelSpName].GetIntColor());
      #endif
      
      EnableUpdate(true);
      mHasTargetColorChanged = true;
   }
}


//------------------------------------------------------------------------------
// void OnComboBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnComboBoxChange(wxCommandEvent& event)
{    
   if (event.GetEventObject() == mCoordSysComboBox)
   {
      // if coordinate system is other than EarthMJ2000Eq,
      // uncheck and disable draw ecliptic plane CheckBox
      if (mCoordSysComboBox->GetValue() == "EarthMJ2000Eq")
      {
         mEclipticPlaneCheckBox->Enable();
      }
      else
      {
         mEclipticPlaneCheckBox->SetValue(false);
         mEclipticPlaneCheckBox->Disable();
      }
      
      mHasCoordSysChanged = true;
   }
   else if (event.GetEventObject() == mViewUpCsComboBox ||
            event.GetEventObject() == mViewUpAxisComboBox)
   {
      mHasViewUpInfoChanged = true;
   }
   else if (event.GetEventObject() == mViewPointRefComboBox)
   {
      mHasViewInfoChanged = true;
      
      if (mViewPointRefComboBox->GetStringSelection() == "Vector")
         mViewDefSizer->Show(mViewPointRefSizer, true);
      else
         mViewDefSizer->Show(mViewPointRefSizer, false);
   }
   else if (event.GetEventObject() == mViewPointVectorComboBox)
   {
      mHasViewInfoChanged = true;
      
      if (mViewPointVectorComboBox->GetStringSelection() == "Vector")
         mViewDefSizer->Show(mViewPointVectorSizer, true);
      else
         mViewDefSizer->Show(mViewPointVectorSizer, false);
   }
   else if (event.GetEventObject() == mViewDirectionComboBox)
   {
      mHasViewInfoChanged = true;
      
      if (mViewDirectionComboBox->GetStringSelection() == "Vector")
         mViewDefSizer->Show(mViewDirVectorSizer, true);
      else
         mViewDefSizer->Show(mViewDirVectorSizer, false);
   }
   else if (event.GetEventObject() == mSolverIterComboBox)
   {
      mHasDrawingOptionChanged = true;
   }
   
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnTextChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::OnTextChange(wxCommandEvent& event)
{
   wxObject *obj = event.GetEventObject();
   
   if (((wxTextCtrl*)obj)->IsModified())
   {
      if (obj == mDataCollectFreqTextCtrl ||
          obj == mUpdatePlotFreqTextCtrl ||
          obj == mNumPointsToRedrawTextCtrl ||
			 obj == mStarCountTextCtrl ||
			 obj == mFovTextCtrl ||
			 obj == mFovMinTextCtrl ||
			 obj == mFovMaxTextCtrl)
      {
         mHasIntegerDataChanged = true;
      }
      else if (obj == mViewPointRef1TextCtrl ||
               obj == mViewPointRef2TextCtrl ||
               obj == mViewPointRef3TextCtrl ||
               obj == mViewPointVec1TextCtrl ||
               obj == mViewPointVec2TextCtrl ||
               obj == mViewPointVec3TextCtrl ||
               obj == mViewDir1TextCtrl ||
               obj == mViewDir2TextCtrl ||
               obj == mViewDir3TextCtrl)
      {
         mHasRealDataChanged = true;
      }
      
      EnableUpdate(true);
   }
}


//---------------------------------
// private methods
//---------------------------------

//------------------------------------------------------------------------------
// void ShowSpacePointOption(const wxString &name, bool show = true,
//                           bool isSc = true,
//                           UnsignedInt color = GmatColor::RED32)
//------------------------------------------------------------------------------
void Enhanced3DViewPanel::ShowSpacePointOption(const wxString &name, bool show,
                                                bool isSc, UnsignedInt color)
{
   #if DEBUG_OPENGL_PANEL_SHOW
   MessageInterface::ShowMessage
      ("Enhanced3DViewPanel::ShowSpacePointOption() name=%s, show=%d, isSc=%d, "
       "color=%u\n", name.c_str(), show, isSc, color);
   #endif
   
   if (!name.IsSameAs(""))
   {
      mSelSpName = std::string(name.c_str());
      
      // if object name not found, insert
      if (mOrbitColorMap.find(mSelSpName) == mOrbitColorMap.end())
      {
         #if DEBUG_OPENGL_PANEL
         MessageInterface::ShowMessage
            ("ShowSpacePointOption() name not found, so adding it to color map\n");
         #endif
         
         mOrbitColorMap[mSelSpName] = RgbColor(color);
         mTargetColorMap[mSelSpName] = RgbColor(GmatColor::ORANGE32);
      }
      
      RgbColor orbColor = mOrbitColorMap[mSelSpName];
      RgbColor targColor = mTargetColorMap[mSelSpName];
      
      #if DEBUG_OPENGL_PANEL_SHOW
      MessageInterface::ShowMessage
         ("Enhanced3DViewPanel::ShowSpacePointOption() orbColor=%u, targColor=%u\n",
          orbColor.GetIntColor(), targColor.GetIntColor());
      #endif
      
      mDrawObjectCheckBox->SetValue(mDrawObjectMap[mSelSpName]);
      
      mOrbitColor.Set(orbColor.Red(), orbColor.Green(), orbColor.Blue());
      mTargetColor.Set(targColor.Red(), targColor.Green(), targColor.Blue());
      
      mOrbitColorButton->SetBackgroundColour(mOrbitColor);
      mTargetColorButton->SetBackgroundColour(mTargetColor);
      mOrbitColorButton->Refresh();
      mTargetColorButton->Refresh();
      
      if (isSc)
      {
         mTargetColorLabel->Enable();
         mTargetColorButton->Enable();
      }
      else
      {
         mTargetColorLabel->Disable();
         mTargetColorButton->Disable();
      }
      
      mObjectSizer->Show(mScOptionSizer, show);
   }
   else
   {
      mObjectSizer->Show(mScOptionSizer, false);
   }
}

//--------------------------------------------------------------
// void ValidateFovValues()
// Just a helper function to make sure the Initial, Minimum, and
// Maximum FOV values make sense
//--------------------------------------------------------------
void Enhanced3DViewPanel::ValidateFovValues(){
	wxString fov = mFovTextCtrl->GetValue(), 
				minFov = mFovMinTextCtrl->GetValue(),
				maxFov = mFovMaxTextCtrl->GetValue();
	double fovValue, minFovValue,	maxFovValue;
	fov.ToDouble(&fovValue); 
	minFov.ToDouble(&minFovValue);
	maxFov.ToDouble(&maxFovValue);
	if (minFovValue > maxFovValue)
		mFovMinTextCtrl->SetValue(maxFov);
	if (maxFovValue < minFovValue)
		mFovMaxTextCtrl->SetValue(minFov);
	if (minFovValue > fovValue)
		mFovMinTextCtrl->SetValue(fov);
	if (maxFovValue < fovValue)
		mFovMaxTextCtrl->SetValue(fov);
	if (fovValue < minFovValue)
		mFovTextCtrl->SetValue(minFov);
	if (fovValue > maxFovValue)
		mFovTextCtrl->SetValue(maxFov);
}

