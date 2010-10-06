//$Id$
//------------------------------------------------------------------------------
//                              GmatMainFrame
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Linda Jun
// Created: 2003/08/05
//
// Modified: 2003/08/28 - Allison Greene
//         : Updated to include full menubar and splitter window.
//
//         : 2003/09/18 - Allison Greene
//         : Updated to include tool bar.
//
//         : 2003/10/03 - Allison Greene
//         : Updated to include tabs for right side of window and left side.
//
/**
 * This class provides the main frame for GMAT.
 */
//------------------------------------------------------------------------------
#ifndef GmatMainFrame_hpp
#define GmatMainFrame_hpp

#include "gmatwxdefs.hpp"

#include <stdio.h>

#include "ViewTextFrame.hpp"
#include "GuiInterpreter.hpp"
#include "GmatTreeItemData.hpp"
#include "GmatServer.hpp"
#include "MdiChildTrajFrame.hpp"
#include "MdiChildTsFrame.hpp"
#include "WelcomePanel.hpp"

#include <wx/notebook.h>
#include <wx/toolbar.h>
#include <wx/docview.h>
#include <wx/laywin.h>
#include <wx/textctrl.h>
#include <wx/hyperlink.h>

namespace GmatGui
{
   enum ScriptOption
   {
      OPEN_SCRIPT_ON_ERROR,
      ALWAYS_OPEN_SCRIPT,
      DO_NOT_OPEN_SCRIPT
   };
};

class GmatMainFrame : public wxMDIParentFrame
{
public:
   // constructors
   GmatMainFrame(wxWindow *parent, const wxWindowID id, const wxString& title,
                 const wxPoint& pos, const wxSize& size, const long style);
   ~GmatMainFrame();
   
   GmatMdiChildFrame* CreateChild(GmatTreeItemData *item, bool restore = true);
   GmatMdiChildFrame* GetChild(const wxString &name);
   Integer GetNumberOfChildOpen(bool incPlots = false, bool incScripts = false);
   bool IsChildOpen(GmatTreeItemData *item, bool restore = true);
   bool RenameChild(GmatTreeItemData *item, wxString newName);
   bool RenameChild(const wxString &oldName, const wxString &newName);
   bool RenameActiveChild(const wxString &newName);
   bool RemoveChild(const wxString &name, GmatTree::ItemType itemType,
                    bool deleteChild = true);
   void CloseChild(const wxString &name, GmatTree::ItemType itemType);
   void CloseChild(GmatMdiChildFrame *child);
   void CloseWelcomePanel();
   void CloseActiveChild();
   bool CloseAllChildren(bool closeScriptWindow = true, bool closePlots = true,
                         bool closeReports = true);
   void MinimizeChildren();
   void SetActiveChildDirty(bool dirty);
   void OverrideActiveChildDirty(bool override);
   void CloseCurrentProject();
   bool InterpretScript(const wxString &filename,
                        Integer scriptOpenOpt = GmatGui::OPEN_SCRIPT_ON_ERROR,
                        bool closeScript = false, bool readBack = false,
                        const wxString &savePath = "", bool multScripts = false);
   void BuildAndRunScript(const wxString &filename);
   Integer RunCurrentMission();
   void StopRunningMission();
   void NotifyRunCompleted();
   void ProcessPendingEvent();
   void StartMatlabServer();
   void StopMatlabServer();
   void UpdateRecentMenu(wxArrayString files);
   wxToolBar* GetMainFrameToolBar();
   wxStatusBar* GetMainFrameStatusBar();
   
   // event handling
   void OnClose(wxCloseEvent& event);
   void OnProjectNew(wxCommandEvent &event);
   void OnClearCurrentMission(wxCommandEvent &event);
   void OnLoadDefaultMission(wxCommandEvent &event);
   void OnSaveScriptAs(wxCommandEvent &event);
   void OnSaveScript(wxCommandEvent &event);
   void OnPrintSetup(wxCommandEvent &event);
   void OnPrint(wxCommandEvent &event);
   void OnProjectExit(wxCommandEvent &event);
   void OnRun(wxCommandEvent &event);
   void OnPause(wxCommandEvent &event);
   void OnStop(wxCommandEvent &event);
   void OnHelpAbout(wxCommandEvent &event);
   void OnHelpWelcome(wxCommandEvent &event);
   void OnHelpOnline(wxCommandEvent &event);
   void OnHyperLinkClick(wxHyperlinkEvent &event);
   
   void OnNewScript(wxCommandEvent &event);
   void OnOpenScript(wxCommandEvent &event);
   void OpenRecentScript(size_t index, wxCommandEvent &event);
   void OpenRecentScript(wxString filename, wxCommandEvent &event);
   void OnOpenRecentScript1(wxCommandEvent &event);  // I know this is retarded but the event returns the MDI frame
   void OnOpenRecentScript2(wxCommandEvent &event);  // as its event object.  There doesn't seem to be a way
   void OnOpenRecentScript3(wxCommandEvent &event);  // to figure out which menu item called an event handler
   void OnOpenRecentScript4(wxCommandEvent &event);
   void OnOpenRecentScript5(wxCommandEvent &event);
   void OnSetPath(wxCommandEvent &event);
   
   void OnScreenshot(wxCommandEvent &event);
  
   void OnUndo(wxCommandEvent& event);
   void OnRedo(wxCommandEvent& event);
   void OnCut(wxCommandEvent& event);
   void OnCopy(wxCommandEvent& event);
   void OnPaste(wxCommandEvent& event);
   void OnComment(wxCommandEvent& event);
   void OnUncomment(wxCommandEvent& event);
   void OnSelectAll(wxCommandEvent& event);
   
   void OnFind(wxCommandEvent& event);
   void OnFindNext(wxCommandEvent& event);
   void OnReplace(wxCommandEvent& event);
   void OnReplaceNext(wxCommandEvent& event);
   void OnGoToLine(wxCommandEvent& event);
   void OnLineNumber(wxCommandEvent& event);
   void OnIndentMore(wxCommandEvent& event);
   void OnIndentLess(wxCommandEvent& event);
   
   void OnFont(wxCommandEvent& event);
   
   void OnOpenMatlab(wxCommandEvent& event);
   void OnCloseMatlab(wxCommandEvent& event);
   
   void OnMatlabServerStart(wxCommandEvent& event);
   void OnMatlabServerStop(wxCommandEvent& event);
   
   void OnFileCompareNumeric(wxCommandEvent& event);
   void OnFileCompareText(wxCommandEvent& event);
   void OnGenerateTextEphemFile(wxCommandEvent& event);
   
   void OnSashDrag(wxSashEvent &event);
   void OnMsgSashDrag(wxSashEvent &event);
   void OnMainFrameSize(wxSizeEvent &event);
   void OnSetFocus(wxFocusEvent &event);
   void OnKeyDown(wxKeyEvent &event);
   
   void OnAnimation(wxCommandEvent& event);
   
   void UpdateMenus(bool openOn);
   void EnableMenuAndToolBar(bool enable, bool missionRunning = false,
                             bool forAnimation = false);
   
   void OnScriptBuildObject(wxCommandEvent& WXUNUSED(event));
   void OnScriptBuildAndRun(wxCommandEvent& event);
   void OnScriptRun(wxCommandEvent& WXUNUSED(event));
   void OnCloseAll(wxCommandEvent &event);
   void OnCloseActive(wxCommandEvent &event);
   
   bool SetScriptFileName(const std::string &filename);
   
   virtual bool Show(bool show = true);

   MdiChildTrajFrame *trajSubframe;
   MdiChildTsFrame *tsSubframe;
   wxList *theMdiChildren;

protected:

private:
   int  mAnimationFrameInc;
   bool mRunPaused;
   bool mRunCompleted;
   bool mInterpretFailed;
   bool mExitWithoutConfirm;
   Integer mRunStatus;
   
   GmatServer *mMatlabServer;
   std::string mScriptFilename;
   std::string mTempScriptName;
   GuiInterpreter *theGuiInterpreter;
   
   wxSashLayoutWindow* theMainWin;
   wxSashLayoutWindow* theMessageWin;
   
   ViewTextFrame *mTextFrame;
   WelcomePanel *mWelcomePanel;
   wxStatusBar *theStatusBar;
   wxMenuBar *theMenuBar;
   wxToolBar *theToolBar;
   
   GmatMdiChildFrame* CreateNewResource(const wxString &title,
                                        const wxString &name,
                                        GmatTree::ItemType itemType);
   GmatMdiChildFrame* CreateNewCommand(GmatTree::ItemType itemType,
                                       GmatTreeItemData *item);
   GmatMdiChildFrame* CreateNewControl(const wxString &title,
                                       const wxString &name,
                                       GmatTree::ItemType itemType,
                                       GmatCommand *cmd);
   GmatMdiChildFrame* CreateNewOutput(const wxString &title,
                                      const wxString &name,
                                      GmatTree::ItemType itemType);
   
   bool ShowSaveMessage();
   bool SaveScriptAs();
   void OpenScript(bool restore = true);
   void UpdateTitle(const wxString &filename = "");
   
   // IDs for the controls
   enum
   {
      ID_SASH_WINDOW = 100,
      ID_MSG_SASH_WINDOW,
   };
   
   // event handling
   DECLARE_EVENT_TABLE();
   
};

#endif // GmatMainFrame_hpp
