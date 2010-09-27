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
// Modified: 
//    2010.03.01 Thomas Grubb 
//       - Open configuration file (GMAT.ini)
//
/**
 * defines application data.
 */
//------------------------------------------------------------------------------
#include "GmatAppData.hpp"
#include "GuiInterpreter.hpp"
#include "FileManager.hpp"
#include "MessageInterface.hpp"
#include <wx/confbase.h>
#include <wx/fileconf.h>

GmatAppData* GmatAppData::theGmatAppData = NULL;

//------------------------------------------------------------------------------
// GmatAppData* Instance()
//------------------------------------------------------------------------------
/**
 * Accessor method used to obtain the singleton.
 *
 * @return the singleton instance of the GmatAppData class.
 */
//------------------------------------------------------------------------------
GmatAppData* GmatAppData::Instance()
{
   if (!theGmatAppData)
      theGmatAppData = new GmatAppData;
   
   return theGmatAppData;
}


//------------------------------------------------------------------------------
// GuiInterpreter* GetGuiInterpreter()
//------------------------------------------------------------------------------
GuiInterpreter* GmatAppData::GetGuiInterpreter()
{
   return theGuiInterpreter;
}


//------------------------------------------------------------------------------
// void SetGuiInterpreter(GuiInterpreter *guiInterp)
//------------------------------------------------------------------------------
void GmatAppData::SetGuiInterpreter(GuiInterpreter *guiInterp)
{
   theGuiInterpreter = guiInterp;
}


#if !defined __CONSOLE_APP__

//------------------------------------------------------------------------------
// void SetMainFrame(GmatMainFrame *mainFrame)
//------------------------------------------------------------------------------
void GmatAppData::SetMainFrame(GmatMainFrame *mainFrame)
{
   theMainFrame = mainFrame;
}


//------------------------------------------------------------------------------
// GmatMainFrame* GetMainFrame()
//------------------------------------------------------------------------------
GmatMainFrame* GmatAppData::GetMainFrame()
{
   return theMainFrame;
}


//------------------------------------------------------------------------------
// void SetResourceTree(ResourceTree *resourceTree)
//------------------------------------------------------------------------------
void GmatAppData::SetResourceTree(ResourceTree *resourceTree)
{
   theResourceTree = resourceTree;
}


//------------------------------------------------------------------------------
// ResourceTree* GetResourceTree()
//------------------------------------------------------------------------------
ResourceTree* GmatAppData::GetResourceTree()
{
   return theResourceTree;
}


//------------------------------------------------------------------------------
// wxConfigBase* GetPersonalizationConfig()
//------------------------------------------------------------------------------
wxConfigBase* GmatAppData::GetPersonalizationConfig()
{
   if (thePersonalizationConfig == NULL)
   {
//      MessageInterface::PopupMessage(Gmat::INFO_, "Setting up Personalization file: %s\n",FileManager::Instance()->GetFilename(FileManager::PERSONALIZATION_FILE).c_str());
      thePersonalizationConfig = new wxFileConfig(wxEmptyString, wxEmptyString, FileManager::Instance()->GetFullPathname(FileManager::PERSONALIZATION_FILE),
              wxEmptyString, wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_RELATIVE_PATH);
   }
   return thePersonalizationConfig;
}


//------------------------------------------------------------------------------
// void SetMissionTree(MissionTree *missionTree)
//------------------------------------------------------------------------------
void GmatAppData::SetMissionTree(MissionTree *missionTree)
{
   theMissionTree = missionTree;
}


//------------------------------------------------------------------------------
// MissionTree* GetMissionTree()
//------------------------------------------------------------------------------
MissionTree* GmatAppData::GetMissionTree()
{
   return theMissionTree;
}


//------------------------------------------------------------------------------
// void SetOutputTree(OutputTree *outputTree)
//------------------------------------------------------------------------------
void GmatAppData::SetOutputTree(OutputTree *outputTree)
{
   theOutputTree = outputTree;
}


//------------------------------------------------------------------------------
// OutputTree* GetOutputTree()
//------------------------------------------------------------------------------
OutputTree* GmatAppData::GetOutputTree()
{
   return theOutputTree;
}


//------------------------------------------------------------------------------
// void SetMessageWindow(ViewTextFrame *msgFrame)
//------------------------------------------------------------------------------
void GmatAppData::SetMessageWindow(ViewTextFrame *frame)
{
   theMessageWindow = frame;
}


//------------------------------------------------------------------------------
// ViewTextFrame* GetMessageWindow()
//------------------------------------------------------------------------------
ViewTextFrame* GmatAppData::GetMessageWindow()
{
   return theMessageWindow;
}


//------------------------------------------------------------------------------
// void SetCompareWindow(ViewTextFrame *frame)
//------------------------------------------------------------------------------
void GmatAppData::SetCompareWindow(ViewTextFrame *frame)
{
   theCompareWindow = frame;
}


//------------------------------------------------------------------------------
// ViewTextFrame* GetCompareWindow()
//------------------------------------------------------------------------------
ViewTextFrame* GmatAppData::GetCompareWindow()
{
   return theCompareWindow;
}


//------------------------------------------------------------------------------
// void SetMessageTextCtrl(wxTextCtrl *msgTextCtrl)
//------------------------------------------------------------------------------
void GmatAppData::SetMessageTextCtrl(wxTextCtrl *msgTextCtrl)
{
   theMessageTextCtrl = msgTextCtrl;
}


//------------------------------------------------------------------------------
// wxTextCtrl* GetMessageTextCtrl()
//------------------------------------------------------------------------------
wxTextCtrl* GmatAppData::GetMessageTextCtrl()
{
   return theMessageTextCtrl;
}


//------------------------------------------------------------------------------
// void SetFont(wxFont font)
//------------------------------------------------------------------------------
void GmatAppData::SetFont(wxFont font)
{
   theFont = font;
}


//------------------------------------------------------------------------------
// wxFont GetFont()
//------------------------------------------------------------------------------
wxFont GmatAppData::GetFont()
{
   return theFont;
}


//------------------------------------------------------------------------------
// void SetTempScriptName(const wxString &tempName)
//------------------------------------------------------------------------------
void GmatAppData::SetTempScriptName(const wxString &tempName)
{
   theTempScriptName = tempName;
}


//------------------------------------------------------------------------------
// wxString GetTempScriptName()
//------------------------------------------------------------------------------
wxString GmatAppData::GetTempScriptName()
{
   return theTempScriptName;
}



//------------------------------------------------------------------------------
// GmatAppData()
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
GmatAppData::GmatAppData()
{
#if !defined __CONSOLE_APP__
   theMainFrame = NULL;
   theResourceTree = NULL;
   theMissionTree = NULL;
   theOutputTree = NULL;
   theMessageWindow = NULL;
   theCompareWindow = NULL;
   theMessageTextCtrl = NULL;
   theTempScriptName = "$gmattempscript$.script";
   thePersonalizationConfig = NULL;

   
   #ifdef __USE_EDITOR__
   thePageSetupDialogData = NULL;
   #endif
   
   theFont = wxFont(10, wxMODERN, wxNORMAL, wxNORMAL);
   // set the global wx config, read from local directory (GMAT.ini)
   wxFileConfig *pConfig = new wxFileConfig(wxEmptyString, wxEmptyString, "GMAT.ini", 
           wxEmptyString, wxCONFIG_USE_LOCAL_FILE | wxCONFIG_USE_RELATIVE_PATH);
   wxConfigBase::Set(pConfig);
#endif
}


//------------------------------------------------------------------------------
// ~GmatAppData()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
GmatAppData::~GmatAppData()
{
}

#endif

