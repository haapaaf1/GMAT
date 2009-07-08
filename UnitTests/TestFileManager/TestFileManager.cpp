//$Id$
//------------------------------------------------------------------------------
//                                  TestFileManager
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2005/05/27
//
// Modifications:
//
/**
 * Test driver for FileManager.
 */
//------------------------------------------------------------------------------

#if !defined __TEST_NON_REAL__
#define __TEST_NON_REAL__
#endif

#include "gmatdefs.hpp"
#include "TimeTypes.hpp"
#include "FileManager.hpp"
#include "TestOutput.hpp"
#include "BaseException.hpp"
#include "MessageInterface.hpp"
#include "ConsoleMessageReceiver.hpp"

#include <iostream>

using namespace std;

//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{
   std::string startupFile = "gmat_startup_file.txt";
   std::string result;
   
   FileManager *fm = FileManager::Instance();
   
   //---------------------------------------------------------------------------
   out.Put("======================================== test FileManager\n");
   //---------------------------------------------------------------------------
   
   out.Put("------------------------- test ReadStartupFile()");
   fm->ReadStartupFile(startupFile);
   
   // Set log file to current test directory
   MessageInterface::SetLogPath("../../TestFileManager/", true);
   
   out.Put("\n------------------------- test GetRootPath()");
   result = fm->GetRootPath();
   out.Put(result);
   
   out.Put("\n------------------------- test GetPathname(LOG_FILE)");
   result = fm->GetPathname("LOG_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetPathname(REPORT_FILE)");
   result = fm->GetPathname("REPORT_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetPathname(JGM2_FILE)");
   result = fm->GetPathname("JGM2_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetFilename(JGM2_FILE)");
   result = fm->GetFilename("JGM2_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetFullPathname(JGM2_FILE)");
   result = fm->GetFullPathname("JGM2_FILE");
   out.Put(result);
      
   out.Put("\n------------------------- test GetPathname(EARTH_TEXTURE_FILE)");
   result = fm->GetPathname("EARTH_TEXTURE_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetFilename(EARTH_TEXTURE_FILE)");
   result = fm->GetFilename("EARTH_TEXTURE_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetFullPathname(EARTH_TEXTURE_FILE)");
   result = fm->GetFullPathname("EARTH_TEXTURE_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test GetPathname(enum DE200_FILE)");
   result = fm->GetPathname(FileManager::DE200_FILE);
   out.Put(result);
   
   out.Put("\n------------------------- test GetFilename(enum DE200_FILE)");
   result = fm->GetFilename(FileManager::DE200_FILE);
   out.Put(result);
   
   out.Put("\n------------------------- test GetFullPathname(enum DE200_FILE)");
   result = fm->GetFullPathname(FileManager::DE200_FILE);
   out.Put(result);
   
   out.Put("\n------------------------- test GetAllMatlabFunctionPaths()");
   StringArray pathNames = fm->GetAllMatlabFunctionPaths();
   int numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
   
   out.Put("\n------------------------- test AddMatlabFunctionPath(d:/projects/gmat/MatlabFunctionsXXX/)");
   std::string pathname = "d:/projects/gmat/MatlabFunctionsXXX/";
   fm->AddMatlabFunctionPath(pathname);
   pathNames = fm->GetAllMatlabFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
   
   out.Put("\n------------------------- test AddMatlabFunctionPath(d:/projects/gmat/MatlabFunctions/)");
   pathname = "d:/projects/gmat/MatlabFunctions/";
   fm->AddMatlabFunctionPath(pathname);
   pathNames = fm->GetAllMatlabFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
   
   out.Put("\n------------------------- test GetMatlabFunctionPath(atan3.m)");
   std::string funcName = "atan3.m";
   std::string pathToUse = fm->GetMatlabFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetMatlabFunctionPath(DoNothing.m)");
   funcName = "DoNothing.m";
   pathToUse = fm->GetMatlabFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test AddMatlabFunctionPath(d:/projects/gmat/files/MatlabFunctions/)");
   pathname = "d:/projects/gmat/files/MatlabFunctions/";
   fm->AddMatlabFunctionPath(pathname);
   pathNames = fm->GetAllMatlabFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
   
   out.Put("\n------------------------- test GetMatlabFunctionPath(DoNothing.m)");
   funcName = "DoNothing.m";
   pathToUse = fm->GetMatlabFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetAllGmatFunctionPaths()");
   pathNames = fm->GetAllGmatFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);

   out.Put("\n------------------------- test AddGmatFunctionPath(d:/projects/gmat/GmatFunctionsXXX/)");
   pathname = "d:/projects/gmat/GmatFunctionsXXX/";
   fm->AddGmatFunctionPath(pathname);
   pathNames = fm->GetAllGmatFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
   
   
   out.Put("\n------------------------- test AddGmatFunctionPath(d:/projects/gmat/GmatFunctionsCVS/)");
   pathname = "d:/projects/gmat/GmatFunctionsCVS/";
   fm->AddGmatFunctionPath(pathname);
   pathNames = fm->GetAllGmatFunctionPaths();
   numPaths = pathNames.size();
   out.Put("Number of paths = ", numPaths);
   for (int i=0; i<numPaths; i++)
      out.Put(pathNames[i]);
      
   out.Put("\n------------------------- test GetGmatFunctionPath(Func_allPropagates.gmf)");
   funcName = "Func_AAA.gmf";
   pathToUse = fm->GetGmatFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetGmatFunctionPath(Func_AAA.gmf)");
   funcName = "Func_AAA.gmf";
   pathToUse = fm->GetGmatFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetGmatFunctionPath(cross.gmf)");
   funcName = "cross.gmf";
   pathToUse = fm->GetGmatFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetGmatFunctionPath(Atan3.gmf)");
   funcName = "Atan3.gmf";
   pathToUse = fm->GetGmatFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test GetGmatFunctionPath(times2.gmf)");
   funcName = "times2.gmf";
   pathToUse = fm->GetGmatFunctionPath(funcName);
   out.Put("path to use = " + pathToUse);
   
   out.Put("\n------------------------- test WriteStartupFile()");
   std::string startupFile1 = "gmat_startup_file.new.txt";
   out.Put("new startup file = " + startupFile1);
   fm->WriteStartupFile(startupFile1);
   
   out.Put("\n------------------------- Now read it back");
   fm->ReadStartupFile(startupFile1);
   
   out.Put("\n------------------------- test GetFullPathname(JGM2_FILE)");
   result = fm->GetFullPathname(FileManager::JGM2_FILE);
   out.Put(result);
   
   out.Put("\n------------------------- test GetFullPathname(DE200_FILE)");
   result = fm->GetFullPathname(FileManager::DE200_FILE);
   out.Put(result);
   
   out.Put("\n------------------------- test GetFullPathname(SPLASH_FILE)");
   result = fm->GetFullPathname("SPLASH_FILE");
   out.Put(result);
   
   out.Put("\n------------------------- test OutOfBounds exception");
   try
   {
      result = fm->GetPathname((FileManager::FileType)20);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   try
   {
      result = fm->GetFilename((FileManager::FileType)20);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   try
   {
      result = fm->GetFullPathname((FileManager::FileType)20);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   delete fm;
   return 1;
}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   ConsoleMessageReceiver *consoleMsg = ConsoleMessageReceiver::Instance();
   MessageInterface::SetMessageReceiver(consoleMsg);
   MessageInterface::SetLogFile("../../TestFileManager/GmatLog.txt");
   
   TestOutput out("../../TestFileManager/TestFileManagerOut.txt");
   out.Put(GmatTimeUtil::GetCurrentTime());
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of FileManager!!");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   catch (...)
   {
      out.Put("Unknown error occurred\n");
   }
   
   out.Close();
   
   cout << endl;
   cout << "Hit enter to end" << endl;
   cin.get();
}
