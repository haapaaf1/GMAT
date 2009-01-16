//$Id$
//------------------------------------------------------------------------------
//                              EditorPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2009/01/05
//
/**
 * Implements EditorPanel class.
 */
//------------------------------------------------------------------------------

#include "EditorPanel.hpp"
#include "MessageInterface.hpp"   // for Instance()
#include "GmatAppData.hpp"
#include <wx/file.h>              // for wxFile
#include <wx/gdicmn.h>            // for wxColourDatabase

//#define DEBUG_EDITOR_PANEL

// to add build and build&run at top of the panel
#define __ADD_BUILD_TO_TOP__

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EditorPanel, GmatSavePanel)
   EVT_BUTTON(ID_BUTTON_SAVE, GmatSavePanel::OnSave)
   EVT_BUTTON(ID_BUTTON_SAVE_AS, GmatSavePanel::OnSaveAs)
   EVT_BUTTON(ID_BUTTON_CLOSE, EditorPanel::OnClosePanel)
   EVT_BUTTON(ID_BUTTON, EditorPanel::OnButton)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// EditorPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
EditorPanel::EditorPanel(wxWindow *parent, const wxString &name)
   : GmatSavePanel(parent, false, name)
{
   #ifdef DEBUG_EDITORPANEL
   MessageInterface::ShowMessage
      ("EditorPanel::EditorPanel() entered, name='%s'\n", name.c_str());
   #endif
   
   mScriptFilename = name;
   
   Create();
   Show();
}


//------------------------------------------------------------------------------
// ~EditorPanel()
//------------------------------------------------------------------------------
/**
 * A destructor.
 */
//------------------------------------------------------------------------------
EditorPanel::~EditorPanel()
{
   if (mEditor)
      delete mEditor;
}


//------------------------------------------------------------------------------
// void OnClosePanel(wxCommandEvent &event)
//------------------------------------------------------------------------------
/**
 * Close page.
 */
//------------------------------------------------------------------------------
void EditorPanel::OnClosePanel(wxCommandEvent &event)
{
   if (mEditor->IsModified())
   {
      wxMessageDialog *msgDlg =
         new wxMessageDialog(this, "Would you like to save changes?", "Save...",
                             wxYES_NO | wxICON_QUESTION, wxDefaultPosition);
      int result = msgDlg->ShowModal();
      
      if (result == wxID_YES)
         OnSave(event);
      else
         SetModified(false);
   }
   
   GmatSavePanel::OnClosePanel(event);
}


//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void EditorPanel::Create()
{
   //------------------------------------------------------
   // for editor
   //------------------------------------------------------
   mEditor = new Editor(this);
   mEditor->SetFocus();

   #ifdef DEBUG_EDITORPANEL_CREATE
   MessageInterface::ShowMessage
      ("EditorPanel::Create() new Editor <%p> created\n", mEditor);
   #endif
   
   //------------------------------------------------------
   // for build and build & run
   //------------------------------------------------------
   mBuildButton =
      new wxButton(this, ID_BUTTON, "Build", wxDefaultPosition, wxDefaultSize, 0);
   mBuildRunButton =
      new wxButton(this, ID_BUTTON, "Build and Run", wxDefaultPosition, wxDefaultSize, 0);   
   
   //------------------------------------------------------
   // add to sizer
   //------------------------------------------------------
   int bsize = 3; // border size
   
   #ifdef __ADD_BUILD_TO_TOP__
   wxBoxSizer *topSizer = new wxBoxSizer( wxHORIZONTAL);
   topSizer->Add(mBuildButton, 0, wxALIGN_CENTER | wxALL, bsize);
   topSizer->Add(mBuildRunButton, 0, wxALIGN_CENTER | wxALL, bsize);
   
   wxGridSizer *bottomSizer = new wxGridSizer( 1, 0, 0 );
   bottomSizer->Add(mEditor, 0, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   #else
   theButtonSizer->Insert(0, mBuildButton, 0, wxALIGN_LEFT | wxALL, bsize);
   theButtonSizer->Insert(1, mBuildRunButton, 0, wxALIGN_LEFT | wxALL, bsize);
   theButtonSizer->Insert(2, 100, 20);
   #endif
   
   //------------------------------------------------------
   // add to parent sizer
   //------------------------------------------------------
   wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
   
   #ifdef __ADD_BUILD_TO_TOP__
   pageSizer->Add(topSizer, 0, wxALIGN_CENTER | wxALL, bsize);
   pageSizer->Add(bottomSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   #else
   pageSizer->Add(mEditor, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   #endif
   
   theMiddleSizer->Add(pageSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void EditorPanel::LoadData()
{
   #ifdef DEBUG_EDITOR_PANEL
   MessageInterface::ShowMessage("EditorPanel::LoadData() entered\n");
   #endif
   
   wxFile *file = new wxFile();
   bool mFileExists = file->Exists(mScriptFilename);
   
   if (mFileExists)
      mEditor->LoadFile(mScriptFilename);
   
   theSaveAsButton->Enable(true);
   theSaveButton->Enable(true);
   GmatAppData::Instance()->GetMainFrame()->SetActiveChildDirty(false);
   
   #ifdef DEBUG_EDITOR_PANEL
   MessageInterface::ShowMessage("EditorPanel::LoadData() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void EditorPanel::SaveData()
{
   GmatAppData *gmatAppData = GmatAppData::Instance();
   
   if (mScriptFilename != mFilename)
   {
      // add new script to tree
      gmatAppData->GetResourceTree()->AddScriptItem(mFilename);
      
      // rename this child window
      gmatAppData->GetMainFrame()->RenameActiveChild(mFilename);
      mScriptFilename = mFilename;
   }
   
   mEditor->SaveFile(mScriptFilename);
   gmatAppData->GetMainFrame()->SetActiveChildDirty(false);
}


//------------------------------------------------------------------------------
// void OnTextOverMaxLen(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EditorPanel::OnTextOverMaxLen(wxCommandEvent& event)
{
   wxMessageBox(wxT("Text control is already filled up to the maximum length.\n"
                    "The extra input will be discarded."),
                wxT("GMAT Warning"));
}


//------------------------------------------------------------------------------
// void OnButton(wxCommandEvent& event)
//------------------------------------------------------------------------------
void EditorPanel::OnButton(wxCommandEvent& event)
{
   if (mEditor->GetText() == "")
   {
      wxMessageDialog *msgDlg = new wxMessageDialog
         (this, "Can not build an empty file ", "Can not build...",
          wxOK | wxICON_INFORMATION, wxDefaultPosition);
      msgDlg->ShowModal();
      return;
   }
   
   GmatAppData *gmatAppData = GmatAppData::Instance();
   
   if (event.GetEventObject() == mBuildButton ||
       event.GetEventObject() == mBuildRunButton)
   {
      if (mEditor->IsModified())
      {
         // prompt user to save
         wxMessageDialog *msgDlg = new wxMessageDialog(this,
            "Would you like to save changes?", "Save...", wxYES_NO | wxICON_QUESTION ,
            wxDefaultPosition);
         int result = msgDlg->ShowModal();
         
         if (result == wxID_YES)
            OnSave(event);
      }
   }
   
   // Set script file name and build script
   if (event.GetEventObject() == mBuildButton)
   {
      if (gmatAppData->GetMainFrame()->SetScriptFileName(mScriptFilename.c_str()))
         gmatAppData->GetMainFrame()->OnScriptBuildObject(event);
   }
   else if (event.GetEventObject() == mBuildRunButton)
   {
      if (gmatAppData->GetMainFrame()->SetScriptFileName(mScriptFilename.c_str()))
         gmatAppData->GetMainFrame()->OnScriptBuildAndRun(event);
   }
}

