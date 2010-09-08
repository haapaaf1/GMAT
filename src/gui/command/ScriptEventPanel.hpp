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
 * Declares ScriptEventPanel class.
 */
//------------------------------------------------------------------------------

#ifndef ScriptEventPanel_hpp
#define ScriptEventPanel_hpp

#include "GmatPanel.hpp"
#include "Parameter.hpp"
#include "MissionTreeItemData.hpp"
#include <wx/laywin.h>

#ifdef __USE_STC_EDITOR__
#include "Editor.hpp"
#endif

class ScriptEventPanel: public GmatPanel
{
public:
   // constructors
   ScriptEventPanel(wxWindow *parent, MissionTreeItemData *item);
   ~ScriptEventPanel();
   
   wxTextCtrl *mFileContentsTextCtrl;
   
#ifdef __USE_STC_EDITOR__
   Editor* GetEditor() { return mEditor; };
#endif
   
private:
   // member data
#ifdef __USE_STC_EDITOR__
   Editor *mEditor;
#endif
   
   MissionTreeItemData *theItem;
   GmatCommand *theCommand;
   GmatCommand *mPrevCommand;
   GmatCommand *mNextCommand;
   GmatCommand *mNewCommand;
   
   wxSashLayoutWindow* theCommentsWin;
   wxSashLayoutWindow* theScriptsWin;
   
   wxTextCtrl  *mCommentTextCtrl;
   wxGridSizer *mBottomSizer;
   wxBoxSizer  *mPageSizer;
   
   void ReplaceScriptEvent();
   
   // for Debug
   void ShowCommand(const std::string &title1, GmatCommand *cmd1,
                    const std::string &title2 = "", GmatCommand *cmd2 = NULL);
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // event handling
   void OnCommentChange(wxCommandEvent& event);
   void OnScriptChange(wxCommandEvent& event);
   void OnSashDrag(wxSashEvent &event);
   void OnSize(wxSizeEvent &event);
   
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {
      ID_SASH_WINDOW = 9000,
      ID_TEXT,
      ID_COMMENT_CTRL,
      ID_SCRIPT_CTRL,
      ID_STC,
   };
};

#endif
