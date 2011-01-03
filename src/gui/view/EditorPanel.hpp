//$Id$
//------------------------------------------------------------------------------
//                              EditorPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2009/01/15
//
/**
 * Declares EditorPanel class.
 */
//------------------------------------------------------------------------------

#ifndef EditorPanel_hpp
#define EditorPanel_hpp

#include "GmatSavePanel.hpp"
#include "Editor.hpp"
#include <wx/fontdlg.h>

class EditorPanel: public GmatSavePanel
{
public:
   EditorPanel(wxWindow *parent, const wxString &name, bool activeScript = false);
   ~EditorPanel();

   virtual void OnClosePanel(wxCommandEvent &event);

   Editor* GetEditor() { return mEditor; };

private:
   Editor   *mEditor;
   wxString mScriptFilename;
   
   wxButton *mBuildButton;
   wxButton *mBuildRunButton;

   // methods inherited from GmatSavePanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();

   // event handling
   void OnTextOverMaxLen(wxCommandEvent& event);
   void OnButton(wxCommandEvent& event);

   DECLARE_EVENT_TABLE();

   // IDs for the controls and the menu commands
   enum
   {
      ID_TEXT = 9000,
      ID_BUTTON,
   };
};

#endif
