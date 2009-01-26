//$Id$
//------------------------------------------------------------------------------
//                              FunctionSetupPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Allison Greene
// Created: 2004/12/15
//
/**
 * Implements FunctionSetupPanel class.
 */
//------------------------------------------------------------------------------

#include "FunctionSetupPanel.hpp"
#include "MessageInterface.hpp"
#include <wx/filename.h>          // for wxFileName::

//#define DEBUG_FUNCTIONPANEL_LOAD 1

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(FunctionSetupPanel, GmatPanel)
   EVT_BUTTON(ID_BUTTON_OK, GmatPanel::OnOK)
   EVT_BUTTON(ID_BUTTON_APPLY, GmatPanel::OnApply)
   EVT_BUTTON(ID_BUTTON_CANCEL, GmatPanel::OnCancel)
   EVT_BUTTON(ID_BUTTON_SCRIPT, GmatPanel::OnScript)
   
   EVT_TEXT(ID_TEXTCTRL, FunctionSetupPanel::OnTextUpdate)
   EVT_BUTTON(ID_BUTTON, FunctionSetupPanel::OnButton)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// FunctionSetupPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
FunctionSetupPanel::FunctionSetupPanel(wxWindow *parent, const wxString &name)
   : GmatPanel(parent, true)
{
   mEnableLoad = false;
   mEnableSave = false;
   mFileContentsTextCtrl = NULL;
   
   #ifdef __USE_STC_EDITOR__
   mEditor = NULL;
   #endif
   
   theGmatFunction = (GmatFunction *)
            theGuiInterpreter->GetConfiguredObject(std::string(name.c_str()));
   
   Create();
   Show();
}


//------------------------------------------------------------------------------
// ~FunctionSetupPanel()
//------------------------------------------------------------------------------
/**
 * A destructor.
 */
//------------------------------------------------------------------------------
FunctionSetupPanel::~FunctionSetupPanel()
{
   #ifdef __USE_STC_EDITOR__
   if (mEditor)
   {
      delete mEditor;
      mEditor = NULL;
   }
   #endif
}


//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void FunctionSetupPanel::Create()
{
   int bsize = 3; // border size
   
   //------------------------------------------------------
   // Create file contents
   //------------------------------------------------------
   
#ifdef __USE_STC_EDITOR__
   mEditor = new Editor(this, -1, wxDefaultPosition, wxSize(700,400));
#else
   mFileContentsTextCtrl = 
      new wxTextCtrl( this, ID_TEXTCTRL, wxT(""), wxDefaultPosition, 
         wxSize(700,400), wxTE_MULTILINE | wxGROW | wxTE_DONTWRAP);
   mFileContentsTextCtrl->SetFont( GmatAppData::Instance()->GetFont() );
#endif
   
   //------------------------------------------------------
   // Add to sizer
   //------------------------------------------------------
   wxGridSizer *textSizer = new wxGridSizer( 1, 0, 0 );
#ifdef __USE_STC_EDITOR__
   textSizer->Add(mEditor, 0, wxGROW | wxALIGN_CENTER | wxALL, bsize);
#else
   textSizer->Add(mFileContentsTextCtrl, 0, wxGROW | wxALIGN_CENTER | wxALL, 
                  bsize);
#endif
   
   wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
   pageSizer->Add(textSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   theMiddleSizer->Add(pageSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   
   // Change the label of OK, Apply and Cancel button
   theOkButton->SetLabel("Save");
   theApplyButton->SetLabel("Save As");
   theCancelButton->SetLabel("Close");
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void FunctionSetupPanel::LoadData()
{
   // Set the pointer for the "Show Script" button
   mObject = theGmatFunction;
   mFullFunctionPath = theGmatFunction->GetStringParameter("FunctionPath");
   
   #ifdef DEBUG_FUNCTIONPANEL_LOAD
   MessageInterface::ShowMessage
      ("===> FunctionSetupPanel::LoadData() mFullFunctionPath='%s'\n",
       mFullFunctionPath.c_str());
   #endif
   
   if (wxFileName::FileExists(mFullFunctionPath))
   {
      #ifdef __USE_STC_EDITOR__
      mEditor->LoadFile(mFullFunctionPath);
      #else
      mFileContentsTextCtrl->LoadFile(mFullFunctionPath);
      #endif
      mEnableSave = false;
   }
   
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void FunctionSetupPanel::SaveData()
{
   std::string pathname = theGmatFunction->GetStringParameter("FunctionPath");
   
   MessageInterface::ShowMessage
      ("===> FunctionSetupPanel::SaveData() path='%s'\n", pathname.c_str());
   
   if (pathname == "")
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "FunctionSetupPanel::SaveData()\n"
         "A function path was not specified.");
      return;
   }

//    // save file path to base
//    int pathId = theGmatFunction->GetParameterID("FunctionPath");
//    theGmatFunction->SetStringParameter(pathId,
//          mFileNameTextCtrl->GetValue().c_str());

   // was file edited and not saved?
   if (mEnableSave)
   {
      wxMessageDialog *msgDlg = new wxMessageDialog(this,
         "Save function file?", "Save...", wxYES_NO | wxICON_QUESTION ,
         wxDefaultPosition);
      int result = msgDlg->ShowModal();

      if (result == wxID_YES)
         mFileContentsTextCtrl->SaveFile(pathname);
   }
}

//------------------------------------------------------------------------------
// void OnTextUpdate(wxCommandEvent& event)
//------------------------------------------------------------------------------
void FunctionSetupPanel::OnTextUpdate(wxCommandEvent& event)
{
//    if (event.GetEventObject() == mFileNameTextCtrl)
//    {
//       mEnableLoad = true;
//       EnableUpdate(true);

//       if (mFileNameTextCtrl->GetValue() == "")
//       {
//          mEnableSave = false;
//          mEnableLoad = false;
//       }
//    }
   if (event.GetEventObject() == mFileContentsTextCtrl)
   {
      mEnableSave = true;
      EnableUpdate(true);
   }
}

//---------------------------------
// private methods
//---------------------------------
void FunctionSetupPanel::OnButton(wxCommandEvent& event)
{
//    if (event.GetEventObject() == mSaveButton)
//    {
//       wxString filename = mFileNameTextCtrl->GetValue();

//       if (filename != "")
//       {
//          mFileContentsTextCtrl->SaveFile(filename);
//          mEnableSave = false;
//       }
//    }
//    else
//    {
//       //Error - unknown object
//    }

//    mSaveButton->Enable(mEnableSave);
}



