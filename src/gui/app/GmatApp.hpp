//$Id$
//------------------------------------------------------------------------------
//                              GmatApp
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/08/05
//
/**
 * This class contains GMAT main application. Program starts here.
 */
//------------------------------------------------------------------------------
#ifndef GmatApp_hpp
#define GmatApp_hpp

#include "gmatwxdefs.hpp"
#include "Moderator.hpp"
#include "GmatMainFrame.hpp"
#include "GmatAppData.hpp"

#if wxUSE_PRINTING_ARCHITECTURE
// global print data, to remember settings during the session
wxPrintData *globalPrintData = (wxPrintData*) NULL;
wxPageSetupData *globalPageSetupData = (wxPageSetupData*) NULL;
#endif // wxUSE_PRINTING_ARCHITECTURE

class GmatApp : public wxApp
{
public:
   GmatApp();
   // override base class virtuals
   // ----------------------------
   
   // This one is called on application startup and is a good place for the app
   // initialization (doing it here and not in the constructor allows to have
   // an error return: if OnInit() returns false, the application terminates)
   virtual bool OnInit();
   
   int OnExit(void);
   int FilterEvent(wxEvent& event);
   
protected:
   
private:
   
   Moderator *theModerator;
   GmatMainFrame *theMainFrame;
   
   void ProcessCommandLineOptions();
};

DECLARE_APP(GmatApp)

#endif // GmatApp_hpp