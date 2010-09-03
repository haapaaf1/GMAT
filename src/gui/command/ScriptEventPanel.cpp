//$Id$
//------------------------------------------------------------------------------
//                              ScriptEventPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: Allison Greene
// Created: 2005/1/12
//
/**
 * Implements ScriptEventPanel class.
 */
//------------------------------------------------------------------------------

#include "ScriptEventPanel.hpp"
#include "GmatAppData.hpp"
#include "MessageInterface.hpp"
#include "CommandUtil.hpp"       // for GetCommandSeqString()
#include "NoOp.hpp"
#include <algorithm>             // for sort(), set_difference()
#include <sstream>               // for std::stringstream

//#define DBGLVL_SCRIPTEVENTPANEL_LOAD 1
//#define DBGLVL_SCRIPTEVENTPANEL_SAVE 1
//#define DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS 1

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ScriptEventPanel, GmatPanel)
   EVT_TEXT(ID_COMMENT_CTRL, ScriptEventPanel::OnCommentChange)
   EVT_TEXT(ID_SCRIPT_CTRL, ScriptEventPanel::OnCommentChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// ScriptEventPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
//ScriptEventPanel::ScriptEventPanel(wxWindow *parent, GmatCommand *cmd)
ScriptEventPanel::ScriptEventPanel(wxWindow *parent, MissionTreeItemData *item)
   : GmatPanel(parent)
{
   theItem = item;
   theCommand = item->GetCommand();
   
   #if DBGLVL_SCRIPTEVENTPANEL
   ShowCommand("ScriptEventPanel() theCommand=", theCommand);
   #endif
   
   mPrevCommand = NULL;
   mNextCommand = NULL;
   mNewCommand = NULL;
   
   #ifdef __USE_STC_EDITOR__
   mEditor = NULL;
   #endif
   
   if (theCommand != NULL)
   {
     Create();
     Show();
   }
}


//------------------------------------------------------------------------------
// ~ScriptEventPanel()
//------------------------------------------------------------------------------
/**
 * A destructor.
 *
 * If new command sequence was created, it replaces the old sequence of commands
 * with new sequence via command->ForceSetNext() and command->ForceSetPrevious().
 */
//------------------------------------------------------------------------------
ScriptEventPanel::~ScriptEventPanel()
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
void ScriptEventPanel::Create()
{
   int bsize = 3; // border size
   
   // Comment label
   wxStaticText *commentText =
      new wxStaticText(this, ID_TEXT,
                       wxT("Comments"), wxDefaultPosition, wxDefaultSize, 0);
   // Comment text
   mCommentTextCtrl =
      new wxTextCtrl(this, ID_COMMENT_CTRL, wxT(""), wxDefaultPosition,
                     wxSize(-1, 50), wxTE_MULTILINE | wxTE_DONTWRAP);
   
   // BeginScript label
   wxStaticText *beginScriptText =
      new wxStaticText(this, ID_TEXT,
                       wxT("BeginScript;"), wxDefaultPosition, wxDefaultSize, 0);
   // EndScript label
   wxStaticText *endScriptText =
      new wxStaticText(this, ID_TEXT,
                       wxT("EndScript;"), wxDefaultPosition, wxDefaultSize, 0);
   // Change font color to blue
   beginScriptText->SetForegroundColour(*wxBLUE);
   endScriptText->SetForegroundColour(*wxBLUE);
   
#ifdef __USE_STC_EDITOR__
   #ifdef DEBUG_CREATE
   MessageInterface::ShowMessage
      ("ScriptEventPanel::Create(), creating Editor from parent = <%p>\n", this);
   #endif
   mEditor = new Editor(this, true, ID_STC);
#else
   // We don't want TextCtrl to wrap text, so add wxTE_DONTWRAP to style
   mFileContentsTextCtrl =
      new wxTextCtrl(this, ID_SCRIPT_CTRL, wxT(""), wxDefaultPosition,
                     wxDefaultSize, wxTE_MULTILINE | wxTE_DONTWRAP);
   
   mFileContentsTextCtrl->SetFont( GmatAppData::Instance()->GetFont() );
#endif
   
   //------------------------------------------------------
   // add to sizer
   //------------------------------------------------------
   
   mBottomSizer = new wxGridSizer( 1, 0, 0 );
   
#ifdef __USE_STC_EDITOR__
   mBottomSizer->Add(mEditor, 0, wxGROW | wxALIGN_CENTER | wxALL, bsize);
#else
   mBottomSizer->Add(mFileContentsTextCtrl, 0, wxGROW | wxALIGN_CENTER | wxALL, 
                     bsize);
#endif
   
   //------------------------------------------------------
   // add to parent sizer
   //------------------------------------------------------
   mPageSizer = new wxBoxSizer(wxVERTICAL);
   mPageSizer->Add(commentText, 0, wxALIGN_LEFT | wxALL, bsize);
   mPageSizer->Add(mCommentTextCtrl, 0, wxALIGN_LEFT | wxGROW | wxALL, bsize);
   mPageSizer->Add(beginScriptText, 0, wxALIGN_LEFT | wxALL, bsize);
   mPageSizer->Add(mBottomSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
   mPageSizer->Add(endScriptText, 0, wxALIGN_LEFT | wxALL, bsize);
   theMiddleSizer->Add(mPageSizer, 1, wxGROW | wxALIGN_CENTER | wxALL, bsize);
}


//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void ScriptEventPanel::LoadData()
{
   
   // Set the pointer for the "Show Script" button
   mObject = theCommand;
   
   #if DBGLVL_SCRIPTEVENTPANEL_LOAD
   MessageInterface::ShowMessage("ScriptEventPanel::LoadData()\n");
   MessageInterface::ShowMessage("     theCommand=%s\n", theCommand->GetTypeName().c_str());
   std::string cmdString = GmatCommandUtil::GetCommandSeqString(theCommand);
   MessageInterface::ShowMessage("     theCommand seuqence=%s\n", cmdString.c_str());
   
   GmatCommand *firstCmd = theGuiInterpreter->GetFirstCommand();
   cmdString = GmatCommandUtil::GetCommandSeqString(firstCmd);
   MessageInterface::ShowMessage("===> mission seuqence=%s\n", cmdString.c_str());
   
   mPrevCommand = theCommand->GetPrevious();
   ShowCommand("     mPrevCommand = ", mPrevCommand);
   #endif
   
   std::stringstream text;
   
   // We don't want to include Begin/EndScript,
   // so pass Gmat::GUI_EDITOR to GetGeneratingString()
   text << theCommand->GetGeneratingString(Gmat::GUI_EDITOR);
   mCommentTextCtrl->AppendText(theCommand->GetCommentLine().c_str());
   mCommentTextCtrl->SetModified(false);
   
   wxString scriptText = text.str().c_str();
   
   #ifdef __USE_STC_EDITOR__
      mEditor->AppendText(scriptText);
      mEditor->EmptyUndoBuffer();
      mEditor->SetSavePoint();
   #else
      mFileContentsTextCtrl->AppendText(scriptText);
      mFileContentsTextCtrl->SetModified(false);
   #endif
   
   EnableUpdate(false);
}


//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
/*
 * In order for ScriptEvent to save new scripts, it creates new sequence of
 * commands and replace the old sequence of commands via command->ForceSetNext()
 * and command->ForceSetPrevious().
 */
//------------------------------------------------------------------------------
void ScriptEventPanel::SaveData()
{
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   MessageInterface::ShowMessage("ScriptEventPanel::SaveData() Entered\n");
   #endif
   
   canClose = false;
   std::stringstream scriptText1;
   
   // If only comment changed, just set comment and return
   #ifdef __USE_STC_EDITOR__
   if (!mEditor->IsModified() && mCommentTextCtrl->IsModified())
   #else
   if (!mFileContentsTextCtrl->IsModified() && mCommentTextCtrl->IsModified())
   #endif
   {
      theCommand->SetCommentLine(mCommentTextCtrl->GetValue().c_str());
      mCommentTextCtrl->SetModified(false);
      EnableUpdate(false);
      canClose = true;
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      MessageInterface::ShowMessage
         ("ScriptEventPanel::SaveData() only comment saved, just returning\n");
      #endif
      return;
   }
   
   
   //-----------------------------------------------------------------
   // Add lines to stringstream
   //-----------------------------------------------------------------
   scriptText1 << "BeginScript;" << "\n";
   //=======================================================
   #ifdef __USE_STC_EDITOR__
   //=======================================================
   
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   int numLines = mEditor->GetLineCount();
   MessageInterface::ShowMessage("   number of lines=%d\n", numLines);
   #endif
   
   wxString text = mEditor->GetText();
   
   // Add EOL if not there.
   size_t length = text.Len();
   wxChar lastChar = text[length-1];
   
   if (lastChar != '\n' && lastChar != '\r' && lastChar != '\0')
      text = text + "\n";
   
   scriptText1 << text;
   
   //=======================================================
   #else
   //=======================================================
   
   int numLines = mFileContentsTextCtrl->GetNumberOfLines();
   wxString line;
   for (int i=0; i<numLines; i++)
   {      
      line = mFileContentsTextCtrl->GetLineText(i);
      scriptText1 << line << "\n";
      // Since GmatMdiChildFrame uses this TextCtrl for checking
      // modified flag, set the flag to false
      mFileContentsTextCtrl->SetModified(false);
   }
   
   //=======================================================
   #endif
   //=======================================================
   
   scriptText1 << "EndScript;" << "\n";
   
   
   //-----------------------------------------------------------------
   // Get previous command
   //-----------------------------------------------------------------
   std::string cmdString = GmatCommandUtil::GetCommandSeqString(theCommand);
   
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   MessageInterface::ShowMessage
      ("   ==> Current BeginScript sequence=%s\n", cmdString.c_str());
   #endif
   
   // Get previous command
   mPrevCommand = theCommand->GetPrevious();
   
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   ShowCommand("   mPrevCommand = ", mPrevCommand);
   ShowCommand("   theCommand->GetPrevious() = ", theCommand->GetPrevious());   
   #endif
   
   //-----------------------------------------------------------------
   // If previous command is NULL, just return
   //-----------------------------------------------------------------
   if (mPrevCommand == NULL)
   {      
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      GmatCommand *firstCmd = theGuiInterpreter->GetFirstCommand();
      ShowCommand("   firstCmd = ", firstCmd);
      #endif
      
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "ScriptEventPanal::SaveData() *** Internal Error "
          "Occurred ***\nthe previous command is empty. Cannot continue.\n");
      
      return;
   }
   
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   ShowCommand("   mPrevCommand = ", mPrevCommand);
   #endif
   
   //-----------------------------------------------------------------
   // Create temporary NoOp command, so that commands can be appended
   //-----------------------------------------------------------------
   GmatCommand *tempNoOp = new NoOp;
   GmatCommand *inCommand = tempNoOp;
   
   // Save old function list, so that new functions can be delete if there
   // is script errors
   StringArray oldFunctions = theGuiInterpreter->GetListOfObjects(Gmat::FUNCTION);
   for (UnsignedInt i=0; i<oldFunctions.size(); i++)
      MessageInterface::ShowMessage
         ("   ==> oldFunctions[%d] = '%s'\n", i, oldFunctions[i].c_str());
   
   try
   {
      //--------------------------------------------------------------
      // Set text lines to istringstream and interpret
      //--------------------------------------------------------------
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      MessageInterface::ShowMessage
         ("   scriptText1 =\n%s\n", scriptText1.str().c_str());
      #endif
      
      std::istringstream *inStringStream = new std::istringstream;
      inStringStream->str(scriptText1.str());
      
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      MessageInterface::ShowMessage
         ("====================> ScriptEvent calling "
          "theGuiInterpreter->Interpret(%p)\n", inCommand);
      #endif
      
      canClose = theGuiInterpreter->Interpret(inCommand, inStringStream);
      
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      ShowCommand("   inCommand = ", inCommand);
      cmdString = GmatCommandUtil::GetCommandSeqString(inCommand);
      MessageInterface::ShowMessage
         ("   ==> temp noOp seuqence=%s\n", cmdString.c_str());
      MessageInterface::ShowMessage("   canClose = %d\n", canClose);
      #endif
      
      //--------------------------------------------------------------
      // If error occurred during interpretation, handle and return
      //--------------------------------------------------------------
      bool ignoreErrors = true;
      if (!canClose)
      {         
         wxMessageDialog *msgDlg = new wxMessageDialog
            (this, "Errors were found in the ScriptEvent; Do you want to save it anyway?", "",
             wxYES_NO | wxICON_QUESTION, wxDefaultPosition);
         int result = msgDlg->ShowModal();
         
         ignoreErrors = false;
         if (result == wxID_YES)
         {
            ignoreErrors = true;
            canClose = true;
         }
      }
      
      if (!ignoreErrors)
      {
         #if DBGLVL_SCRIPTEVENTPANEL_SAVE
         MessageInterface::ShowMessage
            ("   Deleting tempNoOp->GetNext()=%p\n", tempNoOp->GetNext());
         #endif
         
         //--------------------------------------------
         // Delete newly created GmatFunctions first
         // since any non-command text will create as
         // GmatFunctions, such as aaaa; or bbbb;
         //--------------------------------------------
         StringArray allFunctions = theGuiInterpreter->GetListOfObjects(Gmat::FUNCTION);
         #if DBGLVL_SCRIPTEVENTPANEL_SAVE
         for (UnsignedInt i=0; i<allFunctions.size(); i++)
            MessageInterface::ShowMessage
               ("   ==> allFunctions[%d] = '%s'\n", i, allFunctions[i].c_str());
         #endif
         
         sort(oldFunctions.begin(), oldFunctions.end());
         sort(allFunctions.begin(), allFunctions.end());
         
         StringArray newFunctions;
         // Make list of new functions
         set_difference(allFunctions.begin(), allFunctions.end(), oldFunctions.begin(),
                        oldFunctions.end(), back_inserter(newFunctions));
         
         for (UnsignedInt i=0; i<newFunctions.size(); i++)
         {
            #if DBGLVL_SCRIPTEVENTPANEL_SAVE
            MessageInterface::ShowMessage
               ("   ==> removing newFunctions[%d] = '%s'\n", i, newFunctions[i].c_str());
            #endif
            theGuiInterpreter->RemoveObjectIfNotUsed(Gmat::FUNCTION, newFunctions[i]);
         }
         
         //--------------------------------------------
         // Delete commands appended to BeginScript
         //--------------------------------------------
         GmatCommand *temp =
            theGuiInterpreter->DeleteCommand(tempNoOp->GetNext());
         
         if (temp != NULL)
         {
            temp->ForceSetNext(NULL);
            delete temp;
            temp = NULL;
         }
         
         #if DBGLVL_SCRIPTEVENTPANEL_SAVE
         MessageInterface::ShowMessage
            ("   New Script not saved. Deleting tempNoOp=%p\n", tempNoOp);
         #endif
         
         if (tempNoOp != NULL)
         {
            tempNoOp->ForceSetNext(NULL);
            delete tempNoOp;
            tempNoOp = NULL;
         }
         
         return;
      }
      
      //--------------------------------------------------------------
      // Everthing is good to go, delete temporary NoOp
      //--------------------------------------------------------------
      mNewCommand = tempNoOp->GetNext();
      
      #if DBGLVL_SCRIPTEVENTPANEL_SAVE
      ShowCommand("   saving  mNewCommand = ", mNewCommand);
      ShowCommand("   deleting tempNoOp   = ", tempNoOp);
      #endif
      
      tempNoOp->ForceSetNext(NULL);
      delete tempNoOp;
      tempNoOp = NULL;
      
      //--------------------------------------------------------------
      // Replace current ScrptEvent with new one
      //--------------------------------------------------------------
      ReplaceScriptEvent();
      mObject = mNewCommand;
      theCommand = mNewCommand;
      
      // Save comment if modified
      if (mCommentTextCtrl->IsModified())
      {
         theCommand->SetCommentLine(mCommentTextCtrl->GetValue().c_str());
         #if DBGLVL_SCRIPTEVENTPANEL_SAVE
         MessageInterface::ShowMessage("   saving new comments\n");
         #endif
      }
      
      EnableUpdate(false);
   }
   catch (BaseException &ex)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_,
         "Error parsing the ScriptEvent; please correct the text");
      canClose = false;
   }
   
   #if DBGLVL_SCRIPTEVENTPANEL_SAVE
   MessageInterface::ShowMessage("ScriptEventPanel::SaveData() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// void OnCommentChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ScriptEventPanel::OnCommentChange(wxCommandEvent& event)
{
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnScriptChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ScriptEventPanel::OnScriptChange(wxCommandEvent& event)
{
   EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void ReplaceScriptEvent()
//------------------------------------------------------------------------------
/**
 * If new command sequence was created, it replaces the old sequence of commands
 * with new sequence via command->ForceSetNext() and command->ForceSetPrevious().
 */
//------------------------------------------------------------------------------
void ScriptEventPanel::ReplaceScriptEvent()
{
   #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
   MessageInterface::ShowMessage("ScriptEventPanel::ReplaceScriptEvent() Entered\n");
   ShowCommand("   theCommand   = ", theCommand);
   ShowCommand("   mNewCommand  = ", mNewCommand);
   ShowCommand("   mPrevCommand = ", mPrevCommand);
   #endif
   
   if (theCommand != NULL && mPrevCommand != NULL && mNewCommand != NULL)
   {
      // Get first command from the mission sequence
      GmatCommand *first = theGuiInterpreter->GetFirstCommand();
      // Get parent of current ScriptEvent
      GmatCommand *parent = GmatCommandUtil::GetParentCommand(first, theCommand);
      // Find matching EndScript for new ScriptEvent
      GmatCommand *endScript = GmatCommandUtil::GetMatchingEnd(mNewCommand);
      
      if (parent == NULL)
      {
         MessageInterface::PopupMessage
            (Gmat::ERROR_, "ScriptEventPanel::ReplaceScriptEvent() "
             "*** INTERNAL ERROR ***  parent is NULL\n");
         return;
      }
      
      #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
      ShowCommand("   first     = ", first);
      ShowCommand("   parent    = ", parent);
      ShowCommand("   endScript = ", endScript);
      #endif
      
      // If endScript found continue, otherwise internal error found      
      if (endScript)
      {
         //-----------------------------------------------------------
         // Get command after EndScript from current ScriptEvent
         //-----------------------------------------------------------
         mNextCommand = GmatCommandUtil::GetNextCommand(theCommand);
         
         #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
         ShowCommand("   mNextCommand = ", mNextCommand);
         #endif
         
         //-----------------------------------------------------------
         // Delete old ScriptEvent
         //-----------------------------------------------------------
         #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
         ShowCommand("   ==> calling gui->DeleteCommand() ", theCommand);
         #endif
         
         //Note: DeleteCommand() does not delete theCommand
         GmatCommand *temp = theGuiInterpreter->DeleteCommand(theCommand);
         
         if (temp != NULL)
         {
            temp->ForceSetNext(NULL);
            delete temp;
            temp = NULL;
         }
         
         #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
         MessageInterface::ShowMessage
            ("   ==> after Calling gui->DeleteCommand()\n");
         #endif
         
         //-----------------------------------------------------------
         // If this ScriptEvent is within BranchCommand, Insert Command
         //-----------------------------------------------------------
         if (parent->IsOfType("BranchCommand"))
         {
            #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
            ShowCommand("   insert commands to BranchCommand = ", parent);
            #endif
            
            GmatCommand *prevCmd = mPrevCommand;
            GmatCommand *current = mNewCommand;
            GmatCommand *next = NULL;
            
            // if previous command is BranchCommand or BeginScript,
            // find matching end command, and set it to previous command
            GmatCommand *realPrevCmd = prevCmd;
            if (prevCmd->GetTypeName() == "BeginScript")
            {
               realPrevCmd = GmatCommandUtil::GetMatchingEnd(prevCmd);
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               ShowCommand("   realPrevCmd = ", realPrevCmd);
               #endif
               
               prevCmd = realPrevCmd;
            }
            
            // insert commands in the new script event
            while (current != mNextCommand && current != NULL && prevCmd != NULL)
            {
               next = current->GetNext();
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               ShowCommand("   Inserting current = ", current);
               ShowCommand("   after     prevCmd = ", prevCmd);
               ShowCommand("             next    = ", next);
               #endif
               
               current->ForceSetNext(NULL);
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               MessageInterface::ShowMessage("   ==> calling gui->InsertCommand()\n");
               #endif
               
               realPrevCmd = prevCmd;
               
               //----------------------------------------------------------
               // Handle nested BranchCommand inside Begin/EndScript.
               // Since Insert BranchCommand inserts whole branch to command
               // sequence, if previous command is BranchCommand, and current
               // command is not BeginScript, find matching End BranchCommand
               // and insert after it.
               //----------------------------------------------------------
               if ((prevCmd->IsOfType("BranchCommand") &&
                    current->GetTypeName() != "BeginScript"))
               {
                  realPrevCmd = GmatCommandUtil::GetMatchingEnd(prevCmd);
                  
                  #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
                  ShowCommand("   realPrevCmd = ", realPrevCmd);
                  #endif

                  prevCmd = realPrevCmd;
               }
               
               theGuiInterpreter->InsertCommand(current, prevCmd);               
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               MessageInterface::ShowMessage
                  ("   ==> after calling gui->InsertCommand()\n");
               ShowCommand("   prevCmd->next = ", prevCmd->GetNext());
               
               std::string cmdString = GmatCommandUtil::GetCommandSeqString(first);
               MessageInterface::ShowMessage
                  ("===> new sequence=%s\n", cmdString.c_str());
               #endif
               
               prevCmd = current;
               current = next;
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               ShowCommand("   Inserting current = ", current);
               ShowCommand("   after     prevCmd = ", prevCmd);
               #endif
            } // end while
            
            mNewCommand->ForceSetPrevious(mPrevCommand);
         }
         //-----------------------------------------------------------
         // Else, just set previous/next pointer
         //-----------------------------------------------------------
         else
         {
            #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
            MessageInterface::ShowMessage
               ("   Set previous/next command of BeginScript\n");
            #endif
            // Set previous/next command of BeginScript
            if (mPrevCommand->IsOfType("BranchEnd"))
            {
               // If previous command is BranchEnd, find matching parent of
               // BranchEnd and set BranchEnd's next command to matching parent, then
               // set next command of matching parent to new command
               GmatCommand *subParent =
                  GmatCommandUtil::GetParentCommand(first, mPrevCommand);
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               ShowCommand("   Setting ", subParent, " as next of ", mPrevCommand);
               #endif
               mPrevCommand->ForceSetNext(subParent);
               
               #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
               ShowCommand("   Setting ", mNewCommand, " as next of ", subParent);
               #endif
               subParent->ForceSetNext(mNewCommand);
            }
            else
            {
               mPrevCommand->ForceSetNext(mNewCommand);
               mNewCommand->ForceSetPrevious(mPrevCommand);
            }
            
            // Set previous/next command of EndScript
            if (mNextCommand != NULL)
               mNextCommand->ForceSetPrevious(endScript);
            
            endScript->ForceSetNext(mNextCommand);
         }
         
         //-----------------------------------------------------------
         // Set new ScriptEvent to item command
         //-----------------------------------------------------------
         theItem->SetCommand(mNewCommand);
         
         //-----------------------------------------------------------
         // Some sanity check
         //-----------------------------------------------------------
         if (mNewCommand->GetPrevious() != mPrevCommand)
         {
            if (!mPrevCommand->IsOfType("BranchEnd"))
            {
               MessageInterface::PopupMessage
                  (Gmat::ERROR_, "ScriptEventPanel::ReplaceScriptEvent() "
                   "*** INTERNAL ERROR ***  \nmNewCommand->GetPrevious() != "
                   "mPrevCommand\n");
               //ShowCommand("mPrevCommand = ", mPrevCommand);
            }
         }
         
         if (mNewCommand->GetNext() == NULL)
         {
            MessageInterface::PopupMessage
               (Gmat::ERROR_, "ScriptEventPanel::ReplaceScriptEvent() "
                "*** INTERNAL ERROR ***  mNewCommand->GetNext() == NULL\n");
         }
         else
         {
            #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
            std::string cmdString = GmatCommandUtil::GetCommandSeqString(first);    
            MessageInterface::ShowMessage
               ("===> final new sequence=%s\n", cmdString.c_str());
            #endif
         }
      }
      else
      {
         MessageInterface::PopupMessage
            (Gmat::ERROR_, "ScriptEventPanel::ReplaceScriptEvent() "
             "*** INTERNAL ERROR ***  last command should not be NULL\n");
      }
   }
   
   #if DBGLVL_SCRIPTEVENTPANEL_REPLACE_CMDS
   MessageInterface::ShowMessage
      ("ScriptEventPanel::ReplaceScriptEvent() Leaving\n\n");
   #endif
}


//------------------------------------------------------------------------------
// void ShowCommand(const std::string &title1, GmatCommand *cmd1,
//                   const std::string &title2, GmatCommand *cmd2)
//------------------------------------------------------------------------------
void ScriptEventPanel::ShowCommand(const std::string &title1, GmatCommand *cmd1,
                                   const std::string &title2, GmatCommand *cmd2)
{
   if (title2 == "")
   {
      if (cmd1 == NULL)
         MessageInterface::ShowMessage("%s(%p)NULL\n", title1.c_str(), cmd1);
      else
         MessageInterface::ShowMessage
            ("%s(%p)%s\n", title1.c_str(), cmd1, cmd1->GetTypeName().c_str());
   }
   else
   {
      if (cmd2 == NULL)
         MessageInterface::ShowMessage
            ("%s(%p)NULL%s(%p)NULL\n", title1.c_str(), cmd1, title2.c_str(), cmd2);
      else
         MessageInterface::ShowMessage
            ("%s(%p)%s%s(%p)%s\n", title1.c_str(), cmd1, cmd1->GetTypeName().c_str(),
             title2.c_str(), cmd2, cmd2->GetTypeName().c_str());
   }
}

