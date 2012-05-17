//$Id$
//------------------------------------------------------------------------------
//                              GmatSavePanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/02/02
//
/**
 * Implements GmatSavePanel class.
 */
//------------------------------------------------------------------------------

#include "GmatSavePanel.hpp"
#include "GmatAppData.hpp"
#include "GmatMainFrame.hpp"
#include "ShowScriptDialog.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_SAVE
//#define DEBUG_ACTIVE_SCRIPT

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(GmatSavePanel, GmatPanel)
   EVT_BUTTON(ID_BUTTON_SAVE, GmatSavePanel::OnSave)
   EVT_BUTTON(ID_BUTTON_SAVE_AS, GmatSavePanel::OnSaveAs)
   EVT_BUTTON(ID_BUTTON_CLOSE, GmatSavePanel::OnClosePanel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatSavePanel::OnScript)
END_EVENT_TABLE()

//------------------------------
// public methods
//------------------------------

//------------------------------------------------------------------------------
// GmatSavePanel(wxWindow *parent,  bool showScriptButton, ...)
//------------------------------------------------------------------------------
/**
 * Constructs GmatSavePanel object.
 *
 * @param <parent> parent window.
 *
 */
//------------------------------------------------------------------------------
GmatSavePanel::GmatSavePanel(wxWindow *parent, bool showScriptButton,
                             wxString filename, bool showScriptActiveStatus,
                             bool isScriptActive)
   : GmatPanel(parent, false, showScriptButton)
{
   theGuiInterpreter = GmatAppData::Instance()->GetGuiInterpreter();
   theGuiManager = GuiItemManager::GetInstance();
   canClose = true;
   mEditorModified = false;
   hasFileLoaded = false;
   mShowScriptButton = showScriptButton;
   mFilename = filename;
   if (mFilename == "")
      mFilename = GmatAppData::Instance()->GetTempScriptName().c_str();
   mShowScriptActiveStatus = showScriptActiveStatus;
   mIsScriptActive = isScriptActive;
   mSyncGui = false;
   theParent = parent;
   SetName(filename);
   
   int borderSize = 2;
   wxStaticBox *topStaticBox = new wxStaticBox( this, -1, wxT("") );
   wxStaticBox *middleStaticBox = new wxStaticBox( this, -1, wxT("") );
   wxStaticBox *bottomStaticBox = new wxStaticBox( this, -1, wxT("") );
   
   // create sizers
   thePanelSizer = new wxBoxSizer(wxVERTICAL);
   theTopSizer = new wxStaticBoxSizer( topStaticBox, wxVERTICAL );
   theMiddleSizer = new wxStaticBoxSizer( middleStaticBox, wxVERTICAL );
   theBottomSizer = new wxStaticBoxSizer( bottomStaticBox, wxVERTICAL );
   theButtonSizer = new wxBoxSizer(wxHORIZONTAL);
   
   // create script button
   theScriptButton = new wxButton(this, ID_BUTTON_SCRIPT, "Show Script",
                                  wxDefaultPosition, wxDefaultSize, 0);
   
   // create bottom buttons
   theSaveButton =
      new wxButton(this, ID_BUTTON_SAVE, "Save", wxDefaultPosition, wxDefaultSize, 0);
   theSaveAsButton =
      new wxButton(this, ID_BUTTON_SAVE_AS, "Save As", wxDefaultPosition, wxDefaultSize, 0);
   theCloseButton =
      new wxButton(this, ID_BUTTON_CLOSE, "Close", wxDefaultPosition, wxDefaultSize, 0);
   
   // add items to top sizer
   theTopSizer->Add(theScriptButton, 0, wxALIGN_RIGHT | wxALL, borderSize);
   
   //------------------------------------------------------
   // for active or inactive script indicator
   //------------------------------------------------------
   wxFlexGridSizer *bottomGridSizer = new wxFlexGridSizer(5);
   
   if (mShowScriptActiveStatus)
   {
      mScriptActiveLabel = new wxStaticText
         (this, -1, wxT(""), wxDefaultPosition, wxSize(110, 18), wxALIGN_CENTRE);
      mScriptActiveLabel->Centre();
      // Make font bold face
      wxFont font = mScriptActiveLabel->GetFont();
      font.SetWeight(wxFONTWEIGHT_BOLD);
      // Make font size little bigger
      int fontSize = font.GetPointSize();
      font.SetPointSize(fontSize + 1);
      mScriptActiveLabel->SetFont(font);
      
      RefreshScriptActiveStatus(mIsScriptActive);
      bottomGridSizer->Add(mScriptActiveLabel, 0, wxALIGN_LEFT | wxALL, borderSize*2);
      bottomGridSizer->Add(30, 20);
   }
   
   // Add script status label
   mScriptDirtyLabel = new wxStaticText
      (this, -1, wxT(""), wxDefaultPosition, wxSize(80, 20), wxALIGN_CENTRE);
   bottomGridSizer->Add(mScriptDirtyLabel, 0, wxALIGN_LEFT | wxALL, borderSize*2);
   bottomGridSizer->Add(50, 20);
   
   // adds the buttons to button sizer
   theButtonSizer->Add(theSaveButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   theButtonSizer->Add(theSaveAsButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   theButtonSizer->Add(theCloseButton, 0, wxALIGN_CENTER | wxALL, borderSize);
   ////if (mIsScriptActive)
   ////   theButtonSizer->Hide(theSaveButton);
   
   bottomGridSizer->Add(theButtonSizer, 0, wxALIGN_RIGHT | wxALL, borderSize);
   theBottomSizer->Add(bottomGridSizer, 0, wxALIGN_LEFT | wxALL, borderSize);
   
   topStaticBox->Show(mShowScriptButton);
   
   mObject = NULL;
}

//-------------------------------
// protected methods
//-------------------------------

//------------------------------------------------------------------------------
// void Show()
//------------------------------------------------------------------------------
/**
 * Shows the panel.
 */
//------------------------------------------------------------------------------
void GmatSavePanel::Show()
{
   // add items to middle sizer
   thePanelSizer->Add(theTopSizer, 0, wxGROW | wxALL, 1);
   thePanelSizer->Add(theMiddleSizer, 1, wxGROW | wxALL, 1);
   thePanelSizer->Add(theBottomSizer, 0, wxGROW | wxALL, 1);
   
   // displays the script button
   thePanelSizer->Show(theTopSizer, mShowScriptButton);
   theScriptButton->Show(mShowScriptButton);
   thePanelSizer->Layout();
   
   // tells the enclosing window to adjust to the size of the sizer
   SetAutoLayout( TRUE );
   SetSizer(thePanelSizer); //use the sizer for layout
   thePanelSizer->Fit(this); //loj: if theParent is used it doesn't show the scroll bar
   thePanelSizer->SetSizeHints(this); //set size hints to honour minimum size
   
   LoadData();
   mScriptDirtyLabel->SetLabel("");

}


//------------------------------------------------------------------------------
// void OnSave()
//------------------------------------------------------------------------------
/**
 * Saves the data to disk. No syntax checking is performed and no GUI
 * synchronization is performed.
 */
//------------------------------------------------------------------------------
void GmatSavePanel::OnSave(wxCommandEvent &event)
{
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::OnSave() entered, mSyncGui=%d, mFilename='%s', tempFileName='%s'\n",
       mSyncGui, mFilename.c_str(), GmatAppData::Instance()->GetTempScriptName().c_str());
   #endif
   
   if (theGuiManager->GetActiveScriptStatus() == 1)
      return;
   
   // if it is temp script file, call OnSaveAs() to bring up file dialog to save
   if (mFilename == GmatAppData::Instance()->GetTempScriptName())
      OnSaveAs(event);
   else if (!mSyncGui)
   {
      SaveScript();
      mSyncGui = false;
      #ifdef DEBUG_SAVE
      MessageInterface::ShowMessage
         ("GmatSavePanel::OnSave() leaving, mFilename='%s'\n", mFilename.c_str());
      #endif
      return;
   }
   
   // Confirm user if user wants to refresh GUI with script
   bool saveScript = false;
   if (theGuiManager->GetGuiStatus() == 1)
   {
      saveScript = true;
   }
   // If GUI is dirty, prompt user to select an action
   else if (theGuiManager->GetGuiStatus() == 2)
   {
      #ifdef DEBUG_SAVE
      MessageInterface::ShowMessage("   ==> Showing GUI ovewrite confirm message\n");
      #endif
      wxMessageDialog *msgDlg = new wxMessageDialog
         (this, "You will lose changes made in the GUI if the script is saved.  "
          "Do you want to save the script, discard the GUI changes, and reload "
          "the GUI with the saved script?", "Save script...",
          wxYES_NO | wxCANCEL |wxICON_QUESTION, wxDefaultPosition);
      
      int result = msgDlg->ShowModal();
      if (result == wxID_YES)
         saveScript = true;
      else
         mSyncGui = false;
      
      delete msgDlg;
   }
   
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage("   saveScript=%d, mSyncGui=%d\n", saveScript, mSyncGui);
   #endif
   
   if (saveScript)
      SaveAndBuildScript(event);
   
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::OnSave() leaving, mFilename='%s'\n", mFilename.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void OnSaveAs()
//------------------------------------------------------------------------------
/**
 * Saves the data and remain unclosed.
 */
//------------------------------------------------------------------------------
void GmatSavePanel::OnSaveAs(wxCommandEvent &event)
{
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::OnSaveAs() entered, script = '%s', mIsScriptActive = %d\n",
       mFilename.c_str(), mIsScriptActive);
   #endif
   
   wxFileDialog dialog(this, _T("Choose a file"), _T(""), _T(""),
         _T("Script files (*.script, *.m)|*.script;*.m|"\
            "Text files (*.txt, *.text)|*.txt;*.text|"\
            "All files (*.*)|*.*"), wxSAVE);

   bool saveScript = false;
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxString path = dialog.GetPath().c_str();
      
      if(DoesFileExist(path.c_str()))
      {
         if (wxMessageBox(_T("File already exists.\nDo you want to overwrite?"), 
                          _T("Please confirm"), wxICON_QUESTION | wxYES_NO) == wxYES)
         {
            mFilename = path;
            saveScript = true;
         }
      }
      else
      {
         mFilename = path;
         saveScript = true;
      }
   }
   
   if (saveScript)
      SaveScript();
      
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::OnSaveAs() leaving, script = '%s'\n", mFilename.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void OnClosePanel()
//------------------------------------------------------------------------------
/**
 * Closes panel.
 */
//------------------------------------------------------------------------------
void GmatSavePanel::OnClosePanel(wxCommandEvent &event)
{
   #ifdef DEBUG_CLOSE_PANEL
   MessageInterface::ShowMessage
      ("GmatSavePanel::OnClosePanel() '%s' entered, mEditorModified=%d\n",
       mFilename.c_str(), mEditorModified);
   #endif
   
   // We don't want to show duplicate save message when GmatMdiChildFrame is closing,
   // so set override dirty flag to false
   ((GmatMdiChildFrame*)
    (GmatAppData::Instance()->GetMainFrame()->GetActiveChild()))->OverrideDirty(false);
   
   if (mEditorModified)
      ((GmatMdiChildFrame*)
       (GmatAppData::Instance()->GetMainFrame()->GetActiveChild()))->SetDirty(true);
   else
      ((GmatMdiChildFrame*)
       (GmatAppData::Instance()->GetMainFrame()->GetActiveChild()))->SetDirty(false);
   
   GmatAppData::Instance()->GetMainFrame()->CloseActiveChild();
}


//------------------------------------------------------------------------------
// void OnScript()
//------------------------------------------------------------------------------
/**
 * Shows Scripts
 */
//------------------------------------------------------------------------------
void GmatSavePanel::OnScript(wxCommandEvent &event)
{
   wxString title = "Object Script";
   // open separate window to show scripts?
   if (mObject != NULL)
   {
      title = "Scripting for ";
      title += mObject->GetName().c_str();
   }
   ShowScriptDialog ssd(this, -1, title, mObject);
   ssd.ShowModal();
}


//------------------------------------------------------------------------------
// void UpdateScriptActiveStatus(bool isActive)
//------------------------------------------------------------------------------
void GmatSavePanel::UpdateScriptActiveStatus(bool isActive)
{
   #ifdef DEBUG_UPDATE_ACTIVE_STATUS
   MessageInterface::ShowMessage
      ("GmatSavePanel::UpdateScriptActiveStatus() entered, isActive=%d, "
       "mFilename='%s'\n", isActive, mFilename.c_str());
   #endif
   
   mIsScriptActive = isActive;
   
   if (mShowScriptActiveStatus)
      RefreshScriptActiveStatus(mIsScriptActive);
}


//------------------------------------------------------------------------------
// void ReloadFile()
//------------------------------------------------------------------------------
/**
 * Reloads file contents.
 */
//------------------------------------------------------------------------------
void GmatSavePanel::ReloadFile()
{
   LoadData();
   mScriptDirtyLabel->SetLabel("");
}


//------------------------------------------------------------------------------
// void SetEditorModified(bool modified, updateSyncStatus)
//------------------------------------------------------------------------------
/**
 * Updates script modified status and GUI/Script sync status if updateSyncStatus
 * is true
 */
//------------------------------------------------------------------------------
void GmatSavePanel::SetEditorModified(bool modified, bool updateSyncStatus)
{
   #ifdef DEBUG_TEXT_MODIFIED
   MessageInterface::ShowMessage
      ("GmatSavePanel::SetEditorModified() entered, modified=%d, mEditorModified=%d, "
       "hasFileLoaded=%d, mIsScriptActive=%d\n", modified, mEditorModified, hasFileLoaded,
       mIsScriptActive);
   #endif
   
   // Update script modified status
   if (hasFileLoaded)
   {
      int scriptStatus = modified ? 2 : 1; // 1 = clean, 2 = dirty
      if (mEditorModified != modified)
      {
         if (scriptStatus == 1)
            mScriptDirtyLabel->SetLabel("");
         else
            mScriptDirtyLabel->SetLabel("**modified**");
      }
      
      // For active script, update sync status
      if (mIsScriptActive && updateSyncStatus)
      {
         theGuiManager->SetActiveScriptStatus(scriptStatus);
         GmatAppData::Instance()->GetMainFrame()->
            UpdateGuiScriptSyncStatus(0, scriptStatus);
      }
      
      mEditorModified = modified;
   }
}


//------------------------------------------------------------------------------
// bool DoesFileExist(std::string scriptFilename)
//------------------------------------------------------------------------------
bool GmatSavePanel::DoesFileExist(std::string scriptFilename)
{
   FILE * pFile;
   pFile = fopen (scriptFilename.c_str(),"rt+");
   if (pFile!=NULL)
   {
      fclose (pFile);
      return true;
   }
   else
      return false;
   
}


//------------------------------------------------------------------------------
// void MakeScriptActive(wxCommandEvent &event, bool isScriptModified)
//------------------------------------------------------------------------------
void GmatSavePanel::MakeScriptActive(wxCommandEvent &event, bool isScriptModified)
{
   #ifdef DEBUG_ACTIVE_SCRIPT
   MessageInterface::ShowMessage
      ("GmatSavePanel::MakeScriptActive() entered, isScriptModified=%d\n",
       isScriptModified);
   #endif
   
   bool saveScript = true;
   mSyncGui = false;
   
   if (event.GetEventObject() == mSaveSyncButton ||
       event.GetEventObject() == mSaveSyncRunButton)
      mSyncGui = true;
   
   GmatAppData *gmatAppData = GmatAppData::Instance();
   
   // If this is not an active script, prompt the user for setting active
   if (!mIsScriptActive)
   {
      #ifdef DEBUG_ACTIVE_SCRIPT
      MessageInterface::ShowMessage("   ==> Showing active script confirm message\n");
      #endif
      
      wxMessageDialog *msgDlg = new wxMessageDialog
         (this,"The script file \"" + mFilename + "\" is not the active script.  "
          "Would you like to save, make it the active script, and load it into the GUI?",
          "Save active...", wxYES_NO | wxICON_QUESTION, wxDefaultPosition);
      int result = msgDlg->ShowModal();
      
      if (result == wxID_YES)
      {
         mSyncGui = true;
         saveScript = true;
      }
      else
      {
         mSyncGui = false;
         saveScript = false;
      }
   }
   
   // No action is performed if user said No.
   // See GMAT Software Specification, Script Editor section (LOJ: 2012.02.22)
   //if (isScriptModified)
   if (saveScript)
      OnSave(event);
   
   // If continue building, set script file name and build script
   if (mSyncGui)
   {
      if (event.GetEventObject() == mSaveSyncButton)
      {
         if (gmatAppData->GetMainFrame()->SetScriptFileName(mScriptFilename.c_str()))
            gmatAppData->GetMainFrame()->OnScriptBuildObject(event);
      }
      else if (event.GetEventObject() == mSaveSyncRunButton)
      {
         if (gmatAppData->GetMainFrame()->SetScriptFileName(mScriptFilename.c_str()))
            gmatAppData->GetMainFrame()->OnScriptBuildAndRun(event);
      }
      
      // Make current script active script (Fix for GMT-206, LOJ: 2012.02.09)
      UpdateScriptActiveStatus(true);
      mSyncGui = false;
   }
   
   #ifdef DEBUG_ACTIVE_SCRIPT
   MessageInterface::ShowMessage("GmatSavePanel::MakeScriptActive() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void RefreshScriptActiveStatus(bool isActive)
//------------------------------------------------------------------------------
void GmatSavePanel::RefreshScriptActiveStatus(bool isActive)
{
   if (mIsScriptActive)
   {
      ////theButtonSizer->Hide(theSaveButton);
      theButtonSizer->Layout();
      
      mScriptActiveLabel->SetLabel(" Active Script ");
      wxColour bgcolor = wxTheColourDatabase->Find("DIM GREY");
      mScriptActiveLabel->SetBackgroundColour(bgcolor);
      mScriptActiveLabel->SetForegroundColour(*wxGREEN);
   }
   else
   {
      ////theButtonSizer->Show(theSaveButton);
      theButtonSizer->Layout();
      
      mScriptActiveLabel->SetLabel(" Inactive Script ");
      wxColour bgcolor = wxTheColourDatabase->Find("LIGHT GREY");
      mScriptActiveLabel->SetBackgroundColour(bgcolor);
      mScriptActiveLabel->SetForegroundColour(*wxRED);
   }
}


//------------------------------------------------------------------------------
// void SaveScript()
//------------------------------------------------------------------------------
void GmatSavePanel::SaveScript()
{
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::SaveScript() entered\n   mFilename = '%s'\n",
       mFilename.c_str());
   #endif
   
   SaveData();
   mScriptDirtyLabel->SetLabel("");
   SetEditorModified(false);
   
   if (mIsScriptActive)
   {
      GmatMainFrame *mainFrame = GmatAppData::Instance()->GetMainFrame();
      mainFrame->SetActiveChildDirty(false);
      mainFrame->SetScriptFileName(mFilename.c_str());
      mainFrame->RefreshActiveScript(mFilename.c_str(), false);
      mainFrame->UpdateGuiScriptSyncStatus(2, 2);
   }
   
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::SaveScript() leaving\n   mFilename = '%s'\n",
       mFilename.c_str());
   #endif
}


//------------------------------------------------------------------------------
// void SaveAndBuildScript(wxCommandEvent &event)
//------------------------------------------------------------------------------
void GmatSavePanel::SaveAndBuildScript(wxCommandEvent &event)
{
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::SaveAndBuildScript() entered\n   mFilename = '%s'\n",
       mFilename.c_str());
   #endif
   
   SaveData();
   mScriptDirtyLabel->SetLabel("");
   
   GmatAppData *gmatAppData = GmatAppData::Instance();
   SetEditorModified(false);
   gmatAppData->GetMainFrame()->SetActiveChildDirty(false);
   
   // If script is active, build the script 
   if (mIsScriptActive)
   {
      if (gmatAppData->GetMainFrame()->SetScriptFileName(mFilename.c_str()))
         gmatAppData->GetMainFrame()->OnScriptBuildObject(event);
   }
   
   #ifdef DEBUG_SAVE
   MessageInterface::ShowMessage
      ("GmatSavePanel::SaveAndBuildScript() leaving\n   mFilename = '%s'\n",
       mFilename.c_str());
   #endif
}



