//$Header$
//------------------------------------------------------------------------------
//                              GmatMdiChildFrame
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/02
//
/**
 * Declares GmatMdiChildFrame class.
 */
//------------------------------------------------------------------------------

#ifndef GmatMdiChildFrame_hpp
#define GmatMdiChildFrame_hpp

#include "gmatwxdefs.hpp"
#include "GmatMenuBar.hpp"
#include "GmatTreeItemData.hpp"  // for namespace GmatTree::

#ifdef __USE_STC_EDITOR__
#include "Editor.hpp"
#endif

class GmatMdiChildFrame : public wxMDIChildFrame
{
public:
   // constructors
   GmatMdiChildFrame(wxMDIParentFrame *parent, 
                     const wxString &title = "", 
                     const wxString &name = "",
                     const GmatTree::ItemType type = GmatTree::UNKNOWN_ITEM,
                     wxWindowID id = -1, 
                     const wxPoint &pos = wxDefaultPosition, 
                     const wxSize &size = wxDefaultSize, 
                     long style = wxDEFAULT_FRAME_STYLE);
   ~GmatMdiChildFrame();
   
#ifdef __WXMAC__
   wxString GetTitle();
   void SetTitle(wxString newTitle);
#endif

   wxMenuBar* GetMenuBar();
   GmatTree::ItemType GetItemType();
   void SetDataType(GmatTree::ItemType type);
   wxTextCtrl* GetScriptTextCtrl();
   void SetScriptTextCtrl(wxTextCtrl *textCtrl);
   
#ifdef __USE_STC_EDITOR__
   Editor* GetEditor();
   void SetEditor(Editor *editor);
#endif
   
   void SetDirty(bool dirty);
   void OverrideDirty(bool flag);
   bool IsDirty();
   bool CanClose();
   
   virtual void OnActivate(wxActivateEvent &event);
   virtual void OnClose(wxCloseEvent &event);
   
protected:
   
#ifdef __WXMAC__
   wxString childTitle;
#endif
   
   bool mDirty;
   bool mOverrideDirty;
   bool mCanClose;
   GmatTree::ItemType mItemType;
   wxTextCtrl *theScriptTextCtrl;
   GmatMenuBar *theMenuBar;
   wxMDIParentFrame *theParent;
   
#ifdef __USE_STC_EDITOR__
   Editor *theEditor;
#endif
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
      
};


#endif // GmatMdiChildFrame_hpp
