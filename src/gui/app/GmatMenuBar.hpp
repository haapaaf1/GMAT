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
#ifndef GmatMenuBar_hpp
#define GmatMenuBar_hpp

#include "gmatwxdefs.hpp"
#include "GmatTreeItemData.hpp"  // for GmatTree::

class GmatMenuBar : public wxMenuBar
{
public:
   // constructors
   GmatMenuBar(GmatTree::ItemType itemType, wxMenu *windowMenu, long style = 0);
   
protected:
private:
   void CreateMenu(GmatTree::ItemType itemType, wxMenu *windowMenu);
   
   DECLARE_EVENT_TABLE();

};


namespace GmatMenu
{
   // IDs for the controls and the menu commands
   enum
   {
      MENU_FILE_NEW = 10000,
      MENU_FILE_NEW_SCRIPT,
      MENU_FILE_OPEN_SCRIPT,
      MENU_FILE_SAVE_SCRIPT,
      MENU_FILE_SAVE_SCRIPT_AS,
      MENU_FILE_PRINT,
      MENU_LOAD_DEFAULT_MISSION,
      MENU_EMPTY_PROJECT,
      MENU_PROJECT_EXIT,
      
      MENU_PREFERENCES_FONT,
      MENU_SET_PATH_AND_LOG,
      
      MENU_EDIT_UNDO = 11000,
      MENU_EDIT_REDO,
      MENU_EDIT_CUT,
      MENU_EDIT_COPY,
      MENU_EDIT_PASTE,
      MENU_EDIT_COMMENT,
      MENU_EDIT_UNCOMMENT,
      MENU_EDIT_SELECT_ALL,
      
      MENU_SCRIPT_OPEN_EDITOR = 12000,
      MENU_SCRIPT_BUILD_OBJECT,
      MENU_SCRIPT_RUN,
      MENU_SCRIPT_BUILD_AND_RUN,
      
      MENU_TOOLS_FILE_COMPARE_NUMERIC = 13000,
      MENU_TOOLS_FILE_COMPARE_TEXT,
      MENU_TOOLS_GEN_TEXT_EPHEM_FILE,
      
      TOOL_RUN = 14000,
      TOOL_PAUSE,
      TOOL_RESUME,
      TOOL_STOP,
      TOOL_BUILD,
      
      TOOL_CLOSE_CHILDREN,
      TOOL_CLOSE_CURRENT,
      TOOL_SCRIPT,
      
      TOOL_ANIMATION_PLAY = 15000,
      TOOL_ANIMATION_STOP,
      TOOL_ANIMATION_FAST,
      TOOL_ANIMATION_SLOW,
      TOOL_ANIMATION_OPTIONS,
      
      MENU_MATLAB_OPEN = 16000,
      MENU_MATLAB_CLOSE,
      MENU_START_SERVER,
      MENU_STOP_SERVER,
      
      MENU_HELP_TOPICS,
      MENU_HELP_ONLINE,
      
      // it is important for the id corresponding to the "About" command to have
      // this standard value as otherwise it won't be handled properly under Mac
      // (where it is special and put into the "Apple" menu)
      MENU_HELP_ABOUT = wxID_ABOUT,
   };
};
#endif // GmatMenuBar_hpp
