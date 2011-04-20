/*
 * GmatInterfaceGui.hpp
 *
 *  Created on: Mar 22, 2011
 *      Author: Tuan Nguyen
 */

#ifndef GmatInterfaceGui_hpp
#define GmatInterfaceGui_hpp
#include <sstream>       // for stringstream, istringstream
#include "gmatdefs.hpp"
#include "GmatCommand.hpp"

#include <wx/event.h>

class GmatInterfaceGui
{
public:
   static GmatInterfaceGui* Instance();
   ~GmatInterfaceGui();

   void OpenScript();
   void ClearScript();
   void PutScript(char *str);
   void BuildObject();
   void UpdateObject();
   void RunScript(wxEvtHandler* evtHandler);

   // methods to manage execution of GMAT callback
   bool  ExecuteCallback();
   bool  RegisterCallbackServer(GmatBase *callbackObject);
   char* GetCallbackStatus();
   void  PutCallbackData(std::string &data);
   char* GetCallbackResults();

   char* GetRunState();
   char* GetGmatObject(const std::string &name);
   char* GetParameter(const std::string &name);

   void CheckUserInterrupt();

private:
   static const int MAX_PARAM_VAL_STRING = 512;
   static const int MAX_OBJECT_VAL_STRING = 4096;
   static const int MAX_CALLBACK_DATA_VAL_STRING = 1024;

   void RedirectBuffer(std::ios *stream, std::streambuf* newBuff)
      { stream->rdbuf(newBuff); }

   GmatInterfaceGui();

   std::stringstream mStringStream;
   std::istringstream *mInStringStream;
   static GmatInterfaceGui *instance;
   static bool mPassedInterpreter;

   /// class containing callback function
   GmatBase *callbackObj;
};

#endif /* GmatInterfaceGui_hpp */
