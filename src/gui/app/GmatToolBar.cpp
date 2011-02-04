//$Id$
//------------------------------------------------------------------------------
//                             GmatToolBar
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// ** Legal **
//
// Author: Linda Jun
// Created: 2008/11/13
/**
 * This class provides the tool bar for the main frame.
 */
//------------------------------------------------------------------------------

#include "GmatToolBar.hpp"
#include "GmatMenuBar.hpp"        // for namespace GmatMenu
#include "GuiItemManager.hpp"
#include "MessageInterface.hpp"

#include "bitmaps/NewScript.xpm"
#include "bitmaps/OpenScript.xpm"
#include "bitmaps/NewMission.xpm"
#include "bitmaps/SaveMission.xpm"
#include "bitmaps/copy.xpm"
#include "bitmaps/cut.xpm"
#include "bitmaps/paste.xpm"
#include "bitmaps/print.xpm"
#include "bitmaps/Help.xpm"
#include "bitmaps/WebHelp.xpm"
#include "bitmaps/RunMission.xpm"
#include "bitmaps/PauseMission.xpm"
#include "bitmaps/StopMission.xpm"
#include "bitmaps/CloseAll.xpm"
#include "bitmaps/CloseOne.xpm"
#include "bitmaps/build.xpm"
#include "bitmaps/RunAnimation.xpm"
#include "bitmaps/StopAnimation.xpm"
#include "bitmaps/FasterAnimation.xpm"
#include "bitmaps/SlowerAnimation.xpm"
#include "bitmaps/screenshot.xpm"

//#define DEBUG_TOOLBAR
//#define DEBUG_SYNC_STATUS

using namespace GmatMenu;

//------------------------------------------------------------------------------
// GmatToolBar(wxWindow* parent, long style = wxTB_HORIZONTAL | wxNO_BORDER,
//             wxWindowID id, const wxPoint& pos = wxDefaultPosition,
//             const wxSize& size = wxDefaultSize,
//             const wxString& name = wxPanelNameStr)
//------------------------------------------------------------------------------
GmatToolBar::GmatToolBar(wxWindow* parent, long style, wxWindowID id,
                         const wxPoint& pos, const wxSize& size,
                         const wxString& name)
   : wxToolBar(parent, id, pos, size, style, name)
{
   CreateToolBar(this);
   AddAnimationTools(this);
   AddGuiScriptSyncStatus(this);
}


//------------------------------------------------------------------------------
// void CreateToolBar(wxToolBar* toolBar)
//------------------------------------------------------------------------------
void GmatToolBar::CreateToolBar(wxToolBar* toolBar)
{
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("GmatToolBar::CreateToolBar() entered\n");
   #endif
   
   const int NUM_ICONS = 17;
   wxBitmap* bitmaps[NUM_ICONS];
   
   bitmaps[0] = new wxBitmap(NewScript_xpm);
   bitmaps[1] = new wxBitmap(OpenScript_xpm);
   bitmaps[2] = new wxBitmap(SaveMission_xpm);
   bitmaps[3] = new wxBitmap(copy_xpm);
   bitmaps[4] = new wxBitmap(cut_xpm);
   bitmaps[5] = new wxBitmap(paste_xpm);
   bitmaps[6] = new wxBitmap(print_xpm);
   bitmaps[7] = new wxBitmap(Help_xpm);
   bitmaps[8] = new wxBitmap(RunMission_xpm);
   bitmaps[9] = new wxBitmap(PauseMission_xpm);
   bitmaps[10] = new wxBitmap(StopMission_xpm);
   bitmaps[11] = new wxBitmap(CloseAll_xpm);
   bitmaps[12] = new wxBitmap(CloseOne_xpm);
   bitmaps[13] = new wxBitmap(NewMission_xpm);
   bitmaps[14] = new wxBitmap(build_xpm);
   bitmaps[15] = new wxBitmap(WebHelp_xpm);
   bitmaps[16] = new wxBitmap(screenshot_xpm);
   
   // Changed from wxSize(18, 15) (LOJ: 2011.02.04)
   toolBar->SetToolBitmapSize(wxSize(16, 16));
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage
      ("   Rescaling to default size of 16x15 (Mac), 16x16 (Other)\n");
   #endif
   
   // Do we need to rescale? Commented out for now(LOJ: 2011.02.04)
   // recale to default size of 16x16
//    for (int i=0; i<NUM_ICONS; i++)
//    {
//       wxImage image = bitmaps[i]->ConvertToImage();
//       #ifdef __WXMAC__
//          image = image.Rescale(16, 15);
//       #else
//          image = image.Rescale(16, 16);
//       #endif
//       *bitmaps[i] = wxBitmap(image);
//    }
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("   Adding mission tools\n");
   #endif
   
   // add file tools
   toolBar->AddSeparator();
   toolBar->AddTool(MENU_FILE_NEW_SCRIPT, _T("New Script"), *bitmaps[0], _T("New Script"));
   toolBar->AddTool(MENU_FILE_OPEN_SCRIPT, _T("Open"), *bitmaps[1], _T("Open"));
   toolBar->AddTool(MENU_FILE_SAVE_SCRIPT, _T("Save"), *bitmaps[2], _T("Save"));
   toolBar->AddSeparator();
   
   toolBar->AddTool(MENU_LOAD_DEFAULT_MISSION, _T("New Mission"), *bitmaps[13], 
                    _T("New Mission"));
   toolBar->AddSeparator();
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("   Adding edit tools\n");
   #endif
   
   // add edit tools
   toolBar->AddTool(MENU_EDIT_COPY, _T("Copy"), *bitmaps[3], _T("Copy"));
   toolBar->AddTool(MENU_EDIT_CUT, _T("Cut"), *bitmaps[4], _T("Cut"));
   toolBar->AddTool(MENU_EDIT_PASTE, _T("Paste"), *bitmaps[5], _T("Paste"));
   toolBar->AddSeparator();
   
   #ifdef __ADD_PRINT_TO_TOOLBAR__
   // add print tool
   toolBar->AddTool(MENU_FILE_PRINT, _T("Print"), *bitmaps[6], _T("Print"));
   toolBar->AddSeparator();
   #endif
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("   Adding run tools\n");
   #endif
   
   // add run tools
   toolBar->AddTool(TOOL_RUN, _T("Run"), *bitmaps[8], _T("Run"));
   toolBar->AddTool(TOOL_PAUSE, _T("Pause"), *bitmaps[9], _T("Pause"));
   toolBar->AddTool(TOOL_STOP, _T("Stop"), *bitmaps[10], _T("Stop"));
   toolBar->AddTool(TOOL_SCREENSHOT, _T("Screenshot"), *bitmaps[16], _T("Screenshot"));
   toolBar->AddSeparator();
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("   Adding window tools\n");
   #endif
   
   // add close window tool
   toolBar->AddTool(TOOL_CLOSE_CHILDREN, _T("Close All"), *bitmaps[11], _T("Close All"));
   toolBar->AddTool(TOOL_CLOSE_CURRENT, _T("Close"), *bitmaps[12],
                    _T("Close"));
   toolBar->AddSeparator();
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("   Adding help tools\n");
   #endif
   
   // add help tool
   toolBar->AddTool(MENU_HELP_ABOUT, _T("About GMAT"), *bitmaps[7], _T("About GMAT"));
   toolBar->AddTool(MENU_HELP_ONLINE, _T("Online Help"), *bitmaps[15], _T("Online Help"));
   
   // now realize to make tools appear
   toolBar->Realize();
   
   // disable tools
   toolBar->EnableTool(MENU_EDIT_COPY, FALSE);
   toolBar->EnableTool(MENU_EDIT_CUT, FALSE);
   toolBar->EnableTool(MENU_EDIT_PASTE, FALSE);
   
   #ifdef __ADD_PRINT_TO_TOOLBAR__   
   toolBar->EnableTool(MENU_FILE_PRINT, FALSE);
   #endif
   
   toolBar->EnableTool(TOOL_PAUSE, FALSE);
   toolBar->EnableTool(TOOL_STOP, FALSE);
   toolBar->EnableTool(TOOL_SCREENSHOT, FALSE);
   
   for (int i = 0; i < NUM_ICONS; i++)
      delete bitmaps[i];
   
   #ifdef DEBUG_TOOLBAR
   MessageInterface::ShowMessage("GmatToolBar::CreateToolBar() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// void AddAnimationTools(wxToolBar* toolBar)
//------------------------------------------------------------------------------
/**
 * Adds animation tool icons to tool bar.
 *
 * @param <toolBar> input tool bar.
 */
//------------------------------------------------------------------------------
void GmatToolBar::AddAnimationTools(wxToolBar* toolBar)
{
   #ifdef DEBUG_MAINFRAME
   MessageInterface::ShowMessage("GmatToolBar::AddAnimationTools() entered\n");
   #endif
   
   #ifdef __SHOW_GL_OPTION_DIALOG__
   const int NUM_ICONS = 5;
   #else
   const int NUM_ICONS = 4;
   #endif
   
   wxBitmap* bitmaps[NUM_ICONS];
   
   bitmaps[0] = new wxBitmap(RunAnimation_xpm);
   bitmaps[1] = new wxBitmap(StopAnimation_xpm);
   bitmaps[2] = new wxBitmap(FasterAnimation_xpm);
   bitmaps[3] = new wxBitmap(SlowerAnimation_xpm);
   
   #ifdef __SHOW_GL_OPTION_DIALOG__
   bitmaps[4] = new wxBitmap(animation_options_xpm);
   #endif
   
   // recale to default size of 16x15
   for (int i=0; i<NUM_ICONS; i++)
   {
      wxImage image = bitmaps[i]->ConvertToImage();
      image = image.Rescale(16, 15);
      *bitmaps[i] = wxBitmap(image);
   }
   
   toolBar->AddSeparator();
   
   #ifndef __WXMAC__
   // Add Animation text
   wxStaticText *aniLabel = new wxStaticText(this, -1, wxT("Animation"));
   toolBar->AddControl(aniLabel);
   #endif
   
   toolBar->AddTool(TOOL_ANIMATION_PLAY, _T("AnimationPlay"), *bitmaps[0],
                    _T("Start Animation"), wxITEM_CHECK);
   toolBar->AddTool(TOOL_ANIMATION_STOP, _T("AnimationStop"), *bitmaps[1],
                    _T("Stop Animation"));
   toolBar->AddTool(TOOL_ANIMATION_FAST, _T("AnimationFast"), *bitmaps[2],
                    _T("Faster Animation"));
   toolBar->AddTool(TOOL_ANIMATION_SLOW, _T("AnimationSlow"), *bitmaps[3],
                    _T("Slower Animation"));
   
   #ifdef __SHOW_GL_OPTION_DIALOG__
   toolBar->AddTool(TOOL_ANIMATION_OPTIONS, _T("AnimationOptions"), *bitmaps[4],
                    _T("Show Animation Options"));
   #endif
   
   // now realize to make tools appear
   toolBar->Realize();
   
   // disable tools
   toolBar->EnableTool(TOOL_ANIMATION_PLAY, FALSE);
   toolBar->EnableTool(TOOL_ANIMATION_STOP, FALSE);
   toolBar->EnableTool(TOOL_ANIMATION_FAST, FALSE);
   toolBar->EnableTool(TOOL_ANIMATION_SLOW, FALSE);
   
   for (int i = 0; i < NUM_ICONS; i++)
      delete bitmaps[i];
}


//------------------------------------------------------------------------------
// void AddGuiScriptSyncStatus(wxToolBar* toolBar)
//------------------------------------------------------------------------------
/**
 * Adds GUI and Script file synchronization status to tool bar.
 *
 * @param <toolBar> input tool bar.
 */
//------------------------------------------------------------------------------
void GmatToolBar::AddGuiScriptSyncStatus(wxToolBar* toolBar)
{
#ifndef __WXMAC__
   // Add GUI/Script status text
   wxStaticText *syncLabel = new wxStaticText(this, -1, wxT("GUI/Script Sync Status: "));
   theSyncStatus = new wxStaticText
      (this, -1, wxT(" Synchronized "), wxDefaultPosition, wxSize(120, 20), wxALIGN_CENTRE);
#else
   theSyncStatus = new wxStaticText
      (this, -1, wxT("S"), wxDefaultPosition, wxSize(20, 20), wxALIGN_CENTRE);
#endif

   // Make font bold face
   wxFont font = theSyncStatus->GetFont();
   font.SetWeight(wxFONTWEIGHT_BOLD);
   // Make font size little bigger
   int fontSize = font.GetPointSize();
   font.SetPointSize(fontSize + 1);
   theSyncStatus->SetFont(font);
   
   // Set color
   wxColour bgcolor = wxTheColourDatabase->Find("DIM GREY");
   theSyncStatus->SetBackgroundColour(bgcolor);
   theSyncStatus->SetForegroundColour(*wxGREEN);
   
   toolBar->AddSeparator();
   
#ifndef __WXMAC__
   toolBar->AddControl(syncLabel);
#endif
   toolBar->AddControl(theSyncStatus);
   
   // now realize to make tools appear
   toolBar->Realize();
}


//------------------------------------------------------------------------------
// void UpdateGuiScriptSyncStatus(wxToolBar* toolBar, int guiStat, int scriptStat)
//------------------------------------------------------------------------------
/**
 * Updates GUI and Script file synchronization status in the tool bar.
 *
 * @param <toolBar> input tool bar.
 * @param <guiStat> status of GUI,
 *                       0 = no change, 1 = clean, 2 = dirty, 3 = error
 * @param <scriptStat>  status of active script,
 *                       0 = no change, 1 = clean, 2 = dirty, 3 = error
 */
//------------------------------------------------------------------------------
void GmatToolBar::UpdateGuiScriptSyncStatus(wxToolBar* toolBar, int guiStat,
                                            int scriptStat)
{
   #ifdef DEBUG_SYNC_STATUS
   MessageInterface::ShowMessage
      ("GmatToolBar::UpdateGuiScriptSyncStatus() entered, guiStat=%d, scriptStat = %d\n",
       guiStat, scriptStat);
   #endif
   
   int guiStatus = guiStat;
   int scriptStatus = scriptStat;
   
   GuiItemManager *guiManager = GuiItemManager::GetInstance();
   
   if (guiStatus == 0)
      guiStatus = guiManager->GetGuiStatus();
   if (scriptStatus == 0)
      scriptStatus = guiManager->GetActiveScriptStatus();
   
   #ifdef DEBUG_SYNC_STATUS
   MessageInterface::ShowMessage
      ("   guiStatus=%d, scriptStatus = %d\n", guiStatus, scriptStatus);
   #endif
   
   wxString synchronized;
   wxString guiModified;
   wxString scriptModified;
   wxString unsynchronized;
   wxString guiScriptError;
   wxString guiError;
   wxString scriptError;
#ifndef __WXMAC__
   synchronized    = " Synchronized ";
   guiModified     = " GUI Modified ";
   scriptModified  = " Script Modified ";
   unsynchronized  = " Unsynchronized ";
   guiScriptError  = " Both GUI and Script Error ";
   guiError        = " GUI Error ";
   scriptError     = " Script Error ";
#else
   synchronized    = "S";
   guiModified     = "g";
   scriptModified  = "s";
   unsynchronized  = "U";
   guiScriptError  = "E";
   guiError        = "g";
   scriptError     = "s";
#endif

   wxColour bgcolor = wxTheColourDatabase->Find("DIM GREY");
   if (guiStatus == 1 && scriptStatus == 1)
   {
      theSyncStatus->SetLabel(synchronized);
      theSyncStatus->SetBackgroundColour(bgcolor);
      theSyncStatus->SetForegroundColour(*wxGREEN);
   }
   else if (guiStatus == 2 && scriptStatus == 1)
   {
      theSyncStatus->SetLabel(guiModified);
      wxColour yellow = wxTheColourDatabase->Find("Yellow");
      theSyncStatus->SetBackgroundColour(bgcolor);
      theSyncStatus->SetForegroundColour(yellow);
   }
   else if (guiStatus == 1 && scriptStatus == 2)
   {
      theSyncStatus->SetLabel(scriptModified);
      wxColour yellow = wxTheColourDatabase->Find("Yellow");
      theSyncStatus->SetBackgroundColour(bgcolor);
      theSyncStatus->SetForegroundColour(yellow);
   }
   else if (guiStatus == 2 && scriptStatus == 2)
   {
      theSyncStatus->SetLabel(unsynchronized);
      wxColour bgcolor = wxTheColourDatabase->Find("LIGHT GREY");
      theSyncStatus->SetBackgroundColour(bgcolor);
      theSyncStatus->SetForegroundColour(*wxRED);
   }
   else if (guiStatus == 3 && scriptStatus == 3)
   {
      // This will never happen, but handle just in case
      theSyncStatus->SetLabel(guiScriptError);
      theSyncStatus->SetBackgroundColour(*wxRED);
      theSyncStatus->SetForegroundColour(*wxWHITE);
   }
   else if (guiStatus == 3)
   {
      theSyncStatus->SetLabel(guiError);
      theSyncStatus->SetBackgroundColour(*wxRED);
      theSyncStatus->SetForegroundColour(*wxWHITE);
   }
   else if (scriptStatus == 3)
   {
      theSyncStatus->SetLabel(scriptError);
      theSyncStatus->SetBackgroundColour(*wxRED);
      theSyncStatus->SetForegroundColour(*wxWHITE);
   }
   
   #ifdef DEBUG_SYNC_STATUS
   MessageInterface::ShowMessage
      ("GmatToolBar::UpdateGuiScriptSyncStatus() leaving\n");
   #endif
}



