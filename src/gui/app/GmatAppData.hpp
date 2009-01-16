//$Id$
//------------------------------------------------------------------------------
//                              GmatAppData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/10/29
//
/**
 * Declares GuiInterpreter and other GUI compoment pointers.
 */
//------------------------------------------------------------------------------
#ifndef GmatAppData_hpp
#define GmatAppData_hpp

#if !defined __CONSOLE_APP__
#include "ViewTextFrame.hpp"
#include "ResourceTree.hpp"
#include "MissionTree.hpp"
#include "OutputTree.hpp"
#include "GmatMainFrame.hpp"
#include <wx/print.h>         // for wxPrintData
#include <wx/printdlg.h>      // for wxPageSetupDialogData

#ifdef __USE_STC_EDITOR__
#include <wx/cmndata.h>
#endif

#endif

#include "GuiInterpreter.hpp"

class GmatAppData
{
public:
   
   static GmatAppData* Instance();
   
   void SetGuiInterpreter(GuiInterpreter *guiInterp);
   GuiInterpreter* GetGuiInterpreter();
   
#if !defined __CONSOLE_APP__
   
   void SetMainFrame(GmatMainFrame *mainFrame);
   GmatMainFrame* GetMainFrame();
   
   void SetResourceTree(ResourceTree *resourceTree);
   ResourceTree* GetResourceTree();
   
   void SetMissionTree(MissionTree *missionTree);
   MissionTree* GetMissionTree();
   
   void SetOutputTree(OutputTree *outputTree);
   OutputTree* GetOutputTree();
   
   void SetMessageWindow(ViewTextFrame *frame);
   ViewTextFrame* GetMessageWindow();
   
   void SetCompareWindow(ViewTextFrame *frame);
   ViewTextFrame* GetCompareWindow();
   
   void SetMessageTextCtrl(wxTextCtrl *msgTextCtrl);
   wxTextCtrl* GetMessageTextCtrl();
   
   void SetFont(wxFont font);
   wxFont GetFont();
      
#endif
    
private:
   
   GuiInterpreter *theGuiInterpreter;
   
   /// The singleton instance
   static GmatAppData *theGmatAppData;
   
   GmatAppData();
   ~GmatAppData();
   
#if !defined __CONSOLE_APP__
   GmatMainFrame *theMainFrame;
   ResourceTree  *theResourceTree;
   MissionTree   *theMissionTree;
   OutputTree    *theOutputTree;
   ViewTextFrame *theMessageWindow;
   ViewTextFrame *theCompareWindow;
   wxTextCtrl    *theMessageTextCtrl;
   wxFont        theFont;
   
   #ifdef __USE_STC_EDITOR__
   wxPageSetupDialogData *thePageSetupDialogData;
   #endif
#endif
   
};

#if !defined __CONSOLE_APP__
   #if wxUSE_PRINTING_ARCHITECTURE
   // global print data, to remember settings during the session
   extern wxPrintData *globalPrintData;
   extern wxPageSetupData *globalPageSetupData;
   #endif // wxUSE_PRINTING_ARCHITECTURE
#endif

#endif // GmatAppData_hpp
