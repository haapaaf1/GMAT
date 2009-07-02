//$Id$
//------------------------------------------------------------------------------
//                              GmatPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/02
//
/**
 * Implements GmatPanel class.
 */
//------------------------------------------------------------------------------

#include "GmatPanel.hpp"
#include "GmatAppData.hpp"
#include "MessageInterface.hpp"

#include "ShowScriptDialog.hpp"
#include "ShowSummaryDialog.hpp"

//#define DEBUG_GMATPANEL
//#define DEBUG_GMATPANEL_SAVE

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(GmatPanel, wxPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   EVT_BUTTON(ID_BUTTON_SUMMARY, GmatPanel::OnSummary)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// GmatPanel(wxWindow *parent, bool showBottomSizer, bool showScriptButton)
//------------------------------------------------------------------------------
/**
 * Constructs GmatPanel object.
 *
 * @param <parent> parent window.
 * @param <showBottomSizer> if true, shows bottom OK, Apply, Cancel buttons (true)
 * @param <showScriptButton> if true, shows Show Script button (true)
 *
 */
//------------------------------------------------------------------------------
GmatPanel::GmatPanel(wxWindow *parent, bool showBottomSizer, bool showScriptButton)
   : wxPanel(parent)
{
   theGuiInterpreter = GmatAppData::Instance()->GetGuiInterpreter();
   theGuiManager = GuiItemManager::GetInstance();
   UserInputValidator::SetGuiManager(theGuiManager);
   UserInputValidator::SetWindow(this);
   
   canClose = true;
   mDataChanged = false;
   
   mShowBottomSizer = showBottomSizer;
   mShowScriptButton = showScriptButton;
   
   theParent = parent;
   
   #ifdef DEBUG_GMATPANEL
   MessageInterface::ShowMessage
      ("GmatPanel::GmatPanel() entered. theGuiInterpreter=<%p>\n   "
       "showBottomSizer=%d, showScriptButton=%d\n", theGuiInterpreter,
       showBottomSizer, showScriptButton);
   #endif
   
   // create sizers
   thePanelSizer = new wxBoxSizer(wxVERTICAL);
   
   #ifdef __SHOW_TOP_SIZER__
   theTopSizer = new wxStaticBoxSizer(wxVERTICAL, this );
   #endif

   if (showBottomSizer)
      theMiddleSizer = (wxSizer*)(new wxStaticBoxSizer(wxVERTICAL, this));
   else
      theMiddleSizer = (wxSizer*)(new wxBoxSizer(wxVERTICAL));
   
   theBottomSizer = new wxStaticBoxSizer(wxVERTICAL, this );
   wxBoxSizer *theButtonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   #ifdef __SHOW_TOP_SIZER__
   wxBoxSizer *theTopButtonSizer = new wxBoxSizer(wxHORIZONTAL);
   #endif
   
   if (showBottomSizer)
   {
      // create bottom buttons
      theOkButton = new wxButton
         (this, ID_BUTTON_OK, "OK", wxDefaultPosition, wxDefaultSize, 0);
      theApplyButton = new wxButton
         (this, ID_BUTTON_APPLY, "Apply", wxDefaultPosition, wxDefaultSize, 0);
      theCancelButton = new wxButton
         (this, ID_BUTTON_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize, 0);
      
      #ifdef __SHOW_HELP_BUTTON__
      theHelpButton = new wxButton
         (this, ID_BUTTON_HELP, "Help", wxDefaultPosition, wxDefaultSize, 0);
      #endif
      
      theScriptButton = new wxButton
         (this, ID_BUTTON_SCRIPT, "Show Script", wxDefaultPosition, wxDefaultSize, 0);
      theSummaryButton = new wxButton
         (this, ID_BUTTON_SUMMARY, "Command Summary", wxDefaultPosition, wxDefaultSize, 0);
      
      // use different color for Show Script, and Command Summary for now
      theScriptButton->SetForegroundColour(*wxBLUE);
      theSummaryButton->SetForegroundColour(*wxBLUE);
   }
   
   int borderSize = 3;
   
   // add items to top sizer
   #ifdef __SHOW_TOP_SIZER__   
   theTopButtonSizer->Add(theScriptButton, 0, wxALIGN_RIGHT | wxALL, borderSize);
   theTopButtonSizer->Add(theSummaryButton, 0, wxALIGN_RIGHT | wxALL, borderSize);
   theTopSizer->Add(theTopButtonSizer, 0, wxALIGN_RIGHT | wxALL, borderSize);
   #endif
   
   // adds the buttons to button sizer
   if (showBottomSizer)
   {
      theButtonSizer->Add(theOkButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      theButtonSizer->Add(theApplyButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      theButtonSizer->Add(theCancelButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      
      #ifdef __SHOW_HELP_BUTTON__
      theButtonSizer->Add(theHelpButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      #endif
      
      theButtonSizer->Add(100, 20, 0, wxALIGN_CENTER | wxALL, borderSize);   
      theButtonSizer->Add(theScriptButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      theButtonSizer->Add(theSummaryButton, 0, wxALIGN_CENTER | wxALL, borderSize);
      
      theBottomSizer->Add(theButtonSizer, 0, wxALIGN_CENTER | wxALL, borderSize);
   }
   
   #ifdef __SHOW_TOP_SIZER__
   topStaticBox->Show(mShowScriptButton);
   #endif
   
   mObject = NULL;
   
   #ifdef DEBUG_GMATPANEL
   MessageInterface::ShowMessage("GmatPanel::GmatPanel() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// virtual void EnableUpdate(bool enable = true)
//------------------------------------------------------------------------------
void GmatPanel::EnableUpdate(bool enable)
{
   #ifdef DEBUG_GMATPANEL_SAVE
   MessageInterface::ShowMessage
      ("GmatPanel::EnableUpdate() enable=%d\n", enable);
   #endif
   
   GmatMdiChildFrame* mdichild = (GmatMdiChildFrame*)theParent->GetParent();
   
   if (enable)
   {
      mDataChanged = true;
      mdichild->SetDirty(true);
   }
   else
   {
      mDataChanged = false;
      mdichild->SetDirty(false);
   }
}


//------------------------------------------------------------------------------
// virtual bool PrepareObjectNameChange()
//------------------------------------------------------------------------------
bool GmatPanel::PrepareObjectNameChange()
{
   return canClose;
}


//------------------------------------------------------------------------------
// virtual void ObjectNameChanged(Gmat::ObjectType type, const wxString &oldName,
//                                const wxString &newName)
//------------------------------------------------------------------------------
void GmatPanel::ObjectNameChanged(Gmat::ObjectType type, const wxString &oldName,
                                  const wxString &newName)
{
   // Do we need anything here?
}


//------------------------------------------------------------------------------
// void SetCanClose(bool flag)
//------------------------------------------------------------------------------
void GmatPanel::SetCanClose(bool flag)
{
   canClose = flag;
}


//------------------------------------------------------------------------------
// virtual void OnApply()
//------------------------------------------------------------------------------
/**
 * Saves the data and remain unclosed.
 */
//------------------------------------------------------------------------------
void GmatPanel::OnApply(wxCommandEvent &event)
{
   #ifdef DEBUG_GMATPANEL_SAVE
   MessageInterface::ShowMessage
      ("GmatPanel::OnApply() mDataChanged=%d\n", mDataChanged);
   #endif
   
   if (mDataChanged)
   {
      SaveData();
      GmatMdiChildFrame* mdichild = (GmatMdiChildFrame*)theParent->GetParent();
      if (canClose)
      {
         mdichild->SetDirty(false);
         theGuiInterpreter->ConfigurationChanged(mObject, true);
         EnableUpdate(false);
      }
   }
}


//------------------------------------------------------------------------------
// virtual void OnOk()
//------------------------------------------------------------------------------
/**
 * Saves the data and closes the page
 */
//------------------------------------------------------------------------------
void GmatPanel::OnOK(wxCommandEvent &event)
{
   #ifdef DEBUG_GMATPANEL_SAVE
   MessageInterface::ShowMessage
      ("GmatPanel::OnOK() mDataChanged=%d\n", mDataChanged);
   #endif
   
   if (mDataChanged)
   {
      SaveData();
      GmatMdiChildFrame* mdichild = (GmatMdiChildFrame*)theParent->GetParent();

      if (canClose)
      {
         mdichild->SetDirty(false);
         theGuiInterpreter->ConfigurationChanged(mObject, true);
      }
   }
   
   if (canClose)
      GmatAppData::Instance()->GetMainFrame()->CloseActiveChild();
}


//------------------------------------------------------------------------------
// virtual void OnCancel()
//------------------------------------------------------------------------------
/**
 * Close page.
 */
//------------------------------------------------------------------------------
void GmatPanel::OnCancel(wxCommandEvent &event)
{
   GmatMdiChildFrame* mdichild = (GmatMdiChildFrame*)theParent->GetParent();
   mdichild->SetDirty(false);
   GmatAppData::Instance()->GetMainFrame()->CloseActiveChild();
}


//------------------------------------------------------------------------------
// virtual void OnHelp()
//------------------------------------------------------------------------------
/**
 * Shows Helps
 */
//------------------------------------------------------------------------------
void GmatPanel::OnHelp(wxCommandEvent &event)
{
   // open separate window to show help?
}


//------------------------------------------------------------------------------
// virtual void OnScript()
//------------------------------------------------------------------------------
/**
 * Shows Scripts
 */
//------------------------------------------------------------------------------
void GmatPanel::OnScript(wxCommandEvent &event)
{
   wxString title = "Object Script";
   // open separate window to show scripts?
   if (mObject != NULL) {
      title = "Scripting for ";
      title += mObject->GetName().c_str();
   }
   ShowScriptDialog ssd(this, -1, title, mObject);
   ssd.ShowModal();
}


//------------------------------------------------------------------------------
// virtual void OnSummary()
//------------------------------------------------------------------------------
/**
 * Shows command summary
 */
//------------------------------------------------------------------------------
void GmatPanel::OnSummary(wxCommandEvent &event)
{
   wxString title = "Object Script";
   // open separate window to show scripts?
   if (mObject != NULL) {
      title = "Command Summary for ";
      title += mObject->GetName().c_str();
   }
   ShowSummaryDialog ssd(this, -1, title, (GmatCommand*)mObject);
   ssd.ShowModal();
}



//-------------------------------
// protected methods
//-------------------------------

//------------------------------------------------------------------------------
// bool SetObject(GmatBase *obj)
//------------------------------------------------------------------------------
bool GmatPanel::SetObject(GmatBase *obj)
{
   if (obj == NULL)
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "The panel cannot be populated, the object named \"" +
          mObjectName + "\" is NULL\n");
      return false;
   }
   else
   {
      mObject = obj;
      UserInputValidator::SetObject(obj);
      return true;
   }
}


//------------------------------------------------------------------------------
// void Show()
//------------------------------------------------------------------------------
/**
 * Shows the panel.
 */
//------------------------------------------------------------------------------
void GmatPanel::Show()
{
   // add items to panel sizer
   
   #ifdef __SHOW_TOP_SIZER__
   thePanelSizer->Add(theTopSizer, 0, wxGROW | wxALL, 1);
   #endif
   
   thePanelSizer->Add(theMiddleSizer, 1, wxGROW | wxALL, 1);
   
   if (mShowBottomSizer)
      thePanelSizer->Add(theBottomSizer, 0, wxGROW | wxALL, 1);
   
   // displays the script button   
   #ifdef __SHOW_TOP_SIZER__
   thePanelSizer->Show(theTopSizer, mShowScriptButton);
   #endif
   
   if (mShowBottomSizer)
      theScriptButton->Show(mShowScriptButton);
   
   // tells the enclosing window to adjust to the size of the sizer
   SetAutoLayout( TRUE );
   SetSizer(thePanelSizer); //use the sizer for layout
   thePanelSizer->Fit(this);
   thePanelSizer->SetSizeHints(this); //set size hints to honour minimum size
   
   LoadData();
      
   EnableUpdate(false);
   
   if (mShowBottomSizer)
      if ((mObject == NULL) || (!mObject->IsOfType(Gmat::COMMAND)))
         theSummaryButton->Hide();
   
   // call Layout() to force layout of the children anew
   thePanelSizer->Layout();
}


