//$Id$
//------------------------------------------------------------------------------
//                             GmatMdiChildFrame
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Allison Greene
// Created: 2003/08/28
/**
 * This class provides the layout of a mdi child frame
 */
//------------------------------------------------------------------------------

#include "GmatMdiChildFrame.hpp"
#include "GmatAppData.hpp"
#include "GuiInterpreter.hpp"
#include "FileManager.hpp"
#include "MessageInterface.hpp"
#include "GuiItemManager.hpp"
#include "GmatMenuBar.hpp"       // for namespace GmatMenu

// @todo We cannot create own MenuBar yet.
// Double Window menu appears when more than one child is open and cannot
// delete theMenuBar in the destructor.
//#define __CREATE_CHILD_MENU_BAR__

//#define DEBUG_MDI_CHILD_FRAME

//------------------------------
// event tables for wxWindows
//------------------------------

//------------------------------------------------------------------------------
// EVENT_TABLE(GmatMdiChildFrame, wxMDIChildFrame)
//------------------------------------------------------------------------------
/**
 * Events Table for the menu and tool bar
 *
 * @note Indexes event handler functions.
 */
//------------------------------------------------------------------------------
BEGIN_EVENT_TABLE(GmatMdiChildFrame, wxMDIChildFrame)
   EVT_CLOSE(GmatMdiChildFrame::OnClose) 
   EVT_ACTIVATE(GmatMdiChildFrame::OnActivate)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// GmatMdiChildFrame::GmatMdiChildFrame(...)
//------------------------------------------------------------------------------
GmatMdiChildFrame::GmatMdiChildFrame(wxMDIParentFrame *parent, 
                                     const wxString &title, 
                                     const wxString &name,
                                     const GmatTree::ItemType type,
                                     wxWindowID id, 
                                     const wxPoint &pos, 
                                     const wxSize &size, 
                                     long style)
   : wxMDIChildFrame(parent, id, title, pos, size, style, name)
{
   #ifdef DEBUG_MDI_CHILD_FRAME
   MessageInterface::ShowMessage
      ("GmatMdiChildFrame::GmatMdiChildFrame() entered, title='%s', name='%s', "
       "type=%d\n", title.c_str(), name.c_str(), type);
   #endif
   
   theParent = parent;
   mDirty = false;
   mItemType = type;
   mCanClose = true;
   
   #ifdef __WXMAC__
   childTitle = title;
   #endif
   
   #ifdef __CREATE_CHILD_MENU_BAR__
      // create a menu bar, pass Window menu if Windows
      #ifdef __WXMSW__
         theMenuBar = new GmatMenuBar(mItemType, parent->GetWindowMenu());
      #else
         theMenuBar = new GmatMenuBar(mItemType, NULL);
      #endif
   
      #ifdef DEBUG_MENUBAR
      MessageInterface::ShowMessage
         ("GmatMdiChildFrame::GmatMdiChildFrame() theMenuBarCreated %p\n", theMenuBar);
      #endif
      
      // Commented out so that Window menu works for MdiChildFrame (loj: 2008.02.08)
      // Double Window menu appears when more than one child is open and cannot
      // delete theMenuBar in the destructor.
      //SetMenuBar(theMenuBar);
   #else
      theMenuBar = (GmatMenuBar*)(parent->GetMenuBar());      
   #endif
   
   // Enalbe Edit menu if ScriptFile
   int editIndex = theMenuBar->FindMenu("Edit");
   if (mItemType == GmatTree::SCRIPT_FILE)
      theMenuBar->EnableTop(editIndex, true);
   else
      theMenuBar->EnableTop(editIndex, false);
   
   // Set icon if icon file is in the start up file
   FileManager *fm = FileManager::Instance();
   try
   {
      wxString iconfile = fm->GetFullPathname("MAIN_ICON_FILE").c_str();
      #if defined __WXMSW__
         SetIcon(wxIcon(iconfile, wxBITMAP_TYPE_ICO));
      #elif defined __WXGTK__
         SetIcon(wxIcon(iconfile, wxBITMAP_TYPE_XPM));
      #elif defined __WXMAC__
         SetIcon(wxIcon(iconfile, wxBITMAP_TYPE_PICT_RESOURCE));
      #endif
   }
   catch (GmatBaseException &e)
   {
      //MessageInterface::ShowMessage(e.GetMessage());
   }
}


//------------------------------------------------------------------------------
// ~GmatMdiChildFrame()
//------------------------------------------------------------------------------
GmatMdiChildFrame::~GmatMdiChildFrame()
{
   #ifdef __CREATE_CHILD_MENU_BAR__
      delete theMenuBar;
   #else
      // There should be only one MenuBar associated with GmatMainFrame,
      // so we cannot delete it here.
      int editIndex = theMenuBar->FindMenu("Edit");
      theMenuBar->EnableTop(editIndex, false);
   #endif
}


//------------------------------------------------------------------------------
//void OnActivate(wxActivateEvent &event)
//------------------------------------------------------------------------------
void GmatMdiChildFrame::OnActivate(wxActivateEvent &event)
{
   #ifdef DEBUG_ACTIVATE
   MessageInterface::ShowMessage
      ("GmatMdiChildFrame::OnActivate() entered, title='%s', mItemType=%d\n",
       GetTitle().c_str(), mItemType);
   #endif
   
   //------------------------------------------------------------
   // update edit from menubar and toolbar
   //------------------------------------------------------------
   // Update MenuBar for this child
   int editIndex = theMenuBar->FindMenu("Edit");
   if (mItemType == GmatTree::SCRIPT_FILE)
      theMenuBar->EnableTop(editIndex, true);
   else
      theMenuBar->EnableTop(editIndex, false);
   
   // Update ToolBar for this child
   wxToolBar *toolBar = theParent->GetToolBar();
   if (mItemType == GmatTree::SCRIPT_FILE)
   {
      toolBar->EnableTool(GmatMenu::MENU_EDIT_CUT, TRUE);
      toolBar->EnableTool(GmatMenu::MENU_EDIT_COPY, TRUE);
      toolBar->EnableTool(GmatMenu::MENU_EDIT_PASTE, TRUE);
   }
   else
   {
      toolBar->EnableTool(GmatMenu::MENU_EDIT_CUT, FALSE);
      toolBar->EnableTool(GmatMenu::MENU_EDIT_COPY, FALSE);
      toolBar->EnableTool(GmatMenu::MENU_EDIT_PASTE, FALSE);
   }
   
   //------------------------------------------------------------
   // update animation icons from toolbar
   //------------------------------------------------------------
   if (mItemType == GmatTree::OUTPUT_OPENGL_PLOT)
   {
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_PLAY, true);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_STOP, true);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_FAST, true);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_SLOW, true);
   }
   else
   {
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_PLAY, false);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_STOP, false);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_FAST, false);
      toolBar->EnableTool(GmatMenu::TOOL_ANIMATION_SLOW, false);
   }
   
   event.Skip();
}


//------------------------------------------------------------------------------
// void OnClose(wxCloseEvent &event)
//------------------------------------------------------------------------------
void GmatMdiChildFrame::OnClose(wxCloseEvent &event)
{
   #ifdef DEBUG_MDI_CHILD_FRAME
   MessageInterface::ShowMessage("GmatMdiChildFrame::OnClose() entered\n");
   #endif
   
   mCanClose = true;
   
   // check if window is dirty?
   if (mDirty)
   {
      #ifdef DEBUG_MDI_CHILD_FRAME
      MessageInterface::ShowMessage
         ("GmatMdiChildFrame::OnClose() show exit confirm message\n");
      #endif
      
      if ( wxMessageBox(_T("There were changes made to \"" + GetTitle() + "\" panel"
                           " which will be lost on Close. \nDo you want to close anyway?"),
                        _T("Please Confirm Close"),
                        wxICON_QUESTION | wxYES_NO) != wxYES )
      {
         event.Veto();
         mCanClose = false;
         return;
      }
   }
   
   // remove from list of frames
   GmatAppData::Instance()->GetMainFrame()->RemoveChild(GetTitle(), mItemType);   
   wxSafeYield();
   
   #ifdef DEBUG_MDI_CHILD_FRAME
   MessageInterface::ShowMessage("GmatMdiChildFrame::OnClose() exiting\n");
   #endif
}


#ifdef __WXMAC__
//------------------------------------------------------------------------------
// void SetTitle(wxString newTitle)
//------------------------------------------------------------------------------
void GmatMdiChildFrame::SetTitle(wxString newTitle)
{
   childTitle = newTitle;
}


//------------------------------------------------------------------------------
// wxString GetTitle()
//------------------------------------------------------------------------------
wxString GmatMdiChildFrame::GetTitle()
{
   if (childTitle.IsNull())
      return "";
   else
      return childTitle;
}
#endif


