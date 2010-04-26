//$Id$
//------------------------------------------------------------------------------
//                             GmatMenuBar
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Allison Greene
// Created: 2006/05/09
/**
 * This class provides the menu bar for the main frame.
 */
//------------------------------------------------------------------------------
#include "GmatMenuBar.hpp"
#include "GmatTreeItemData.hpp"
#include "GmatGlobal.hpp"        // for GetRunMode()
#include "MessageInterface.hpp"

#define __ADD_CLOSE_TO_WINDOW_MENU__
//#define __SHOW_EPHEM_FILE__

//#define DEBUG_MENUBAR 1

using namespace GmatMenu;
//------------------------------------------------------------------------------
// EVENT_TABLE(GmatMenuBar, wxMenuBar)
//------------------------------------------------------------------------------
/**
 * Events Table for the menu and tool bar
 *
 * @note Indexes event handler functions.
 */
//------------------------------------------------------------------------------
BEGIN_EVENT_TABLE(GmatMenuBar, wxMenuBar)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// GmatMenuBar(GmatTree::ItemType itemType, wxMenu *windowMenu,
//             wxMenu *newWinMenu, long style)
//------------------------------------------------------------------------------
GmatMenuBar::GmatMenuBar(GmatTree::ItemType itemType, wxMenu *windowMenu,
                         long style)
   : wxMenuBar(style)
{
   CreateMenu(itemType, windowMenu);
}


//------------------------------------------------------------------------------
// void CreateMenu(GmatTree::ItemType itemType, wxMenu *windowMenu)
//------------------------------------------------------------------------------
/**
 * Adds items to the menu.
 *
 * @param <itemType> input item type from GmatTree::ItemType
 * @param <windowMenu> input Window menu pointer. This is available on Windows only
 *
 */
//------------------------------------------------------------------------------
void GmatMenuBar::CreateMenu(GmatTree::ItemType itemType, wxMenu *windowMenu)
{
   Integer runMode = GmatGlobal::Instance()->GetRunMode();
   
   #ifdef DEBUG_MENUBAR
   MessageInterface::ShowMessage
      ("GmatMenuBar::CreateMenu() itemType=%d, windowMenu=%p, runMode=%d\n",
       itemType, windowMenu, runMode);
   #endif
   
   //-----------------------------------------------------------------
   // File menu
   //-----------------------------------------------------------------   
   wxMenu *fileMenu = new wxMenu;
   wxMenu *newMenu = new wxMenu;
   newMenu->Append(MENU_FILE_NEW_SCRIPT, wxT("Script"));
   newMenu->Append(MENU_LOAD_DEFAULT_MISSION, wxT("Mission"));
   fileMenu->Append(MENU_FILE_NEW, wxT("New"), newMenu, wxT(""));
   fileMenu->Append(MENU_FILE_OPEN_SCRIPT, wxT("&Open"), wxT(""));   
   fileMenu->Append(MENU_FILE_SAVE_SCRIPT, wxT("&Save"), wxT(""));
   fileMenu->Append(MENU_FILE_SAVE_SCRIPT_AS, wxT("Save As"), wxT(""));
   
   if (runMode == GmatGlobal::TESTING)
   {
      fileMenu->AppendSeparator();
      fileMenu->Append(MENU_EMPTY_PROJECT, wxT("Empty Project"), wxT(""));   
      fileMenu->AppendSeparator();
   }
   
   fileMenu->Append(MENU_SET_PATH_AND_LOG, wxT("Set File Paths"), wxT(""));
   
   #ifdef __ENABLE_PRINT__
   fileMenu->AppendSeparator();
   fileMenu->Append(MENU_FILE_PRINT_SETUP, wxT("Print Setup..."), wxT(""));
   fileMenu->Append(MENU_FILE_PRINT, wxT("Print...\tCtrl+P"), wxT(""));
   #endif //__ENABLE_PRINT__
   
   fileMenu->AppendSeparator();
   fileMenu->Append(MENU_PROJECT_EXIT, wxT("Exit"), wxT(""));
   #ifdef __WXMAC__
       wxApp::s_macExitMenuItemId = MENU_PROJECT_EXIT;
   #endif
   
   this->Append(fileMenu, wxT("&File"));
   
   //-----------------------------------------------------------------
   // Edit menu
   //-----------------------------------------------------------------
   wxMenu *editMenu = new wxMenu;
   editMenu->Append(MENU_EDIT_UNDO, wxT("Undo\tCtrl+Z"), wxT(""));
   editMenu->Append(MENU_EDIT_REDO, wxT("Redo\tCtrl+Y"), wxT(""));
   editMenu->AppendSeparator();
   editMenu->Append(MENU_EDIT_CUT, wxT("Cut\tCtrl+X"), wxT(""));
   editMenu->Append(MENU_EDIT_COPY, wxT("Copy\tCtrl+C"), wxT(""));
   editMenu->Append(MENU_EDIT_PASTE, wxT("Paste\tCtrl+V"), wxT(""));
   editMenu->AppendSeparator();
   editMenu->Append(MENU_EDIT_COMMENT, wxT("Comment\tCtrl+R"), wxT(""));
   editMenu->Append(MENU_EDIT_UNCOMMENT, wxT("Uncomment\tCtrl+T"), wxT(""));
   editMenu->Append(MENU_EDIT_SELECT_ALL, wxT("Select All\tCtrl+A"), wxT(""));
   
   #ifdef __USE_STC_EDITOR__
   editMenu->AppendSeparator();
   editMenu->Append(MENU_EDIT_FIND, _("&Find...\tCtrl+F"));
   editMenu->Append(MENU_EDIT_FIND_NEXT, _("Find &next\tF3"));
   editMenu->Append(MENU_EDIT_REPLACE, _("&Replace...\tCtrl+F"));
   editMenu->Append(MENU_EDIT_REPLACE_NEXT, _("Find and &Replace\tCtrl+H"));
   editMenu->AppendSeparator();
   editMenu->AppendCheckItem(MENU_EDIT_LINE_NUMBER, _("Show line &numbers"));
   editMenu->Append(MENU_EDIT_GOTO_LINE, _("&Goto\tCtrl+G"));
   editMenu->AppendSeparator();
   editMenu->Append(MENU_EDIT_INDENT_MORE, _("&Indent more\tCtrl+I"));
   editMenu->Append(MENU_EDIT_INDENT_LESS, _("I&ndent less\tCtrl+Shift+I"));
   
   // default is to show line numbers, so put check mark
   editMenu->Check(MENU_EDIT_LINE_NUMBER, true);
   
   #endif // __USE_STC_EDITOR__
   
   this->Append(editMenu, wxT("&Edit"));
   
   //-----------------------------------------------------------------
   // Tools menu
   //-----------------------------------------------------------------
   if (runMode == GmatGlobal::TESTING)
   {
      wxMenu *toolsMenu = new wxMenu;
      toolsMenu->Append(MENU_TOOLS_FILE_COMPARE_NUMERIC, wxT("Compare Numeric Values"), wxT(""));
      toolsMenu->Append(MENU_TOOLS_FILE_COMPARE_TEXT, wxT("Compare Text Lines"), wxT(""));

      #ifdef __SHOW_EPHEM_FILE__
      toolsMenu->Append(MENU_TOOLS_GEN_TEXT_EPHEM_FILE, wxT("Generate Text Ephemeris File"), wxT(""));
      #endif
      
      this->Append(toolsMenu, wxT("Tools"));
   }
   
   //-----------------------------------------------------------------
   // Help menu
   //-----------------------------------------------------------------
   
   wxMenu *helpMenu = new wxMenu;
   helpMenu->Append(MENU_HELP_ONLINE, wxT("Online Help"), wxT(""));
   helpMenu->Append(MENU_HELP_ABOUT, wxT("About GMAT"), wxT(""));
   this->Append(helpMenu, wxT("Help"));
   
   //-----------------------------------------------------------------
   // Window menu
   //-----------------------------------------------------------------
   // @note: In order for system Window menu to work, do not call
   // SetMenuBar() from GmatMdiChildFrame after theMenuBar is created.
   #ifdef __ADD_CLOSE_TO_WINDOW_MENU__
   //-------------------------------------------------------
   // If on Windows, use Window menu from MdiParenentFrame
   // otherwise, create Window menu
   //-------------------------------------------------------
   
   wxMenu *winMenu =(wxMenu*)NULL;
   bool createWindowMenu = true;
   bool prependClose = false;
   
   #ifdef __WXMSW__
   if (windowMenu != NULL)
   {
      createWindowMenu = false;
      winMenu = windowMenu;
      if (winMenu->FindItem("Close All") == wxNOT_FOUND)
         prependClose = true;
   }
   #endif
   
   #ifdef DEBUG_MENUBAR
   MessageInterface::ShowMessage
     ("GmatMenuBar::CreateMenu() All menus created. Now creating WindowMenu...\n"
       "   this=%p, winMenu=%p, createWindowMenu=%d, prependClose=%d\n", this,
       winMenu, createWindowMenu, prependClose);
   #endif
   
   if (createWindowMenu)
   {
      #ifdef DEBUG_MENUBAR
      MessageInterface::ShowMessage("===> creating Window menu\n");
      #endif
      winMenu = new wxMenu;
      winMenu->Append(TOOL_CLOSE_CHILDREN, wxT("Close All"), wxT(""));
      winMenu->Append(TOOL_CLOSE_CURRENT, wxT("Close"), wxT(""));
      
      // Insert before Help menu
      int helpPos = FindMenu("Help");
      this->Insert(helpPos, winMenu, wxT("Window"));
   }
   else if (prependClose)
   {
      #ifdef DEBUG_MENUBAR
      MessageInterface::ShowMessage("===> prepending Close menu item\n");
      #endif
      winMenu->PrependSeparator();
      winMenu->Prepend(TOOL_CLOSE_CURRENT, wxT("Close"), wxT(""));      
      winMenu->Prepend(TOOL_CLOSE_CHILDREN, wxT("Close All"), wxT(""));
   }
   #endif
   
   #ifdef DEBUG_MENUBAR
   MessageInterface::ShowMessage("GmatMenuBar::CreateMenu() exiting\n");
   #endif
}
