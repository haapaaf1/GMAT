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
#ifndef GmatToolBar_hpp
#define GmatToolBar_hpp

#include "gmatwxdefs.hpp"
#include "GmatTreeItemData.hpp"  // for GmatTree::

class GmatToolBar : public wxToolBar
{
public:
   GmatToolBar(wxWindow* parent, long style = wxTB_HORIZONTAL | wxNO_BORDER,
               wxWindowID id = -1,
               const wxPoint& pos = wxDefaultPosition,
               const wxSize& size = wxDefaultSize,
               const wxString& name = wxPanelNameStr);
   
   void CreateToolBar(wxToolBar *toolBar);
   void AddAnimationTools(wxToolBar* toolBar);
   void AddGuiScriptSyncStatus(wxToolBar* toolBar);
   void UpdateGuiScriptSyncStatus(wxToolBar* toolBar, int guiStat, int scriptStat);

protected:
   wxStaticText *theSyncStatus;
};

#endif
