//$Id$
//------------------------------------------------------------------------------
//                              FunctionSetupPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Allison Greene
// Created: 2004/12/15
//
/**
 * Declares FunctionSetupPanel class.
 */
//------------------------------------------------------------------------------

#ifndef FunctionSetupPanel_hpp
#define FunctionSetupPanel_hpp

#include "GmatPanel.hpp"
#include "GmatFunction.hpp"

#ifdef __USE_STC_EDITOR__
#include "Editor.hpp"
#endif

class FunctionSetupPanel: public GmatPanel
{
public:
   // constructors
   FunctionSetupPanel(wxWindow *parent, const wxString &name); 
   ~FunctionSetupPanel();
   
   wxTextCtrl *mFileContentsTextCtrl;
   
#ifdef __USE_STC_EDITOR__
   Editor* GetEditor() { return mEditor; };
#endif
   
private:
   GmatFunction *theGmatFunction;
   wxString mFullFunctionPath;
   
#ifdef __USE_STC_EDITOR__
   Editor *mEditor;
#endif
   
   bool mEnableLoad;
   bool mEnableSave;
      
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // event handling
   void OnTextUpdate(wxCommandEvent& event);
   void OnButton(wxCommandEvent& event);
   
   DECLARE_EVENT_TABLE();

   // IDs for the controls and the menu commands
   enum
   {     
      ID_TEXT = 9000,
      ID_LISTBOX,
      ID_BUTTON,
      ID_COLOR_BUTTON,
      ID_COMBO,
      ID_TEXTCTRL
   };
};

#endif
