//$Id$
//------------------------------------------------------------------------------
//                            FileManager
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun, NASA/GSFC
// Created: 2004/04/02
/**
 * Implements FileManager class. This is singleton class which manages
 * list of file paths and names.
 */
//------------------------------------------------------------------------------

#include "FileManager.hpp"
#include "MessageInterface.hpp"
#include "UtilityException.hpp"
#include "StringUtil.hpp"
#include "FileTypes.hpp"          // for GmatFile::MAX_PATH_LEN
#include "FileUtil.hpp"           // for GmatFileUtil::
#include <fstream>
#include <sstream>
#include <iomanip>

#ifndef _MSC_VER  // if not Microsoft Visual C++
#include <dirent.h>
#endif

// If we want to create default input file names turn this on
//#define FM_CREATE_DEFAULT_INPUT

//#define DEBUG_FILE_MANAGER
//#define DEBUG_FUNCTION_PATH
//#define DEBUG_FILE_PATH
//#define DEBUG_SET_PATH
//#define DEBUG_WRITE_STARTUP_FILE
//#define DEBUG_PLUGIN_DETECTION


//---------------------------------
// static data
//---------------------------------
const std::string
FileManager::FILE_TYPE_STRING[FileTypeCount] =
{
   // file path
   "SPLASH_PATH",
   "OUTPUT_PATH",
   "SLP_PATH",
   "DE_PATH",
   "EARTH_POT_PATH",
   "LUNA_POT_PATH",
   "VENUS_POT_PATH",
   "MARS_POT_PATH",
   "PLANETARY_COEFF_PATH",
   "TIME_PATH",
   "TEXTURE_PATH",  
   // file name
   "LOG_FILE",
   "REPORT_FILE",
   "SPLASH_FILE",
   "TIME_COEFF_FILE",
   // specific file name
   "SLP_FILE",
   "DE200_FILE",
   "DE202_FILE",
   "DE405_FILE",
   "JGM2_FILE",
   "JGM3_FILE",
   "EGM96_FILE",
   "LP165P_FILE",
   "MGNP180U_FILE",
   "MARS50C_FILE",
   "EOP_FILE",
   "PLANETARY_COEFF_FILE",
   "NUTATION_COEFF_FILE",
   "LEAP_SECS_FILE",
};

FileManager* FileManager::theInstance = NULL;
const std::string FileManager::VERSION_DATE = "2005-07-13";

using namespace std;

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// FileManager* Instance()
//------------------------------------------------------------------------------
FileManager* FileManager::Instance()
{
   if (theInstance == NULL)
      theInstance = new FileManager;
   return theInstance;
}


//------------------------------------------------------------------------------
// ~FileManager()
//------------------------------------------------------------------------------
FileManager::~FileManager()
{
   for (std::map<std::string, FileInfo*>::iterator pos = mFileMap.begin();
        pos != mFileMap.end(); ++pos)
   {
      if (pos->second)
      {
         #ifdef DEBUG_FILE_MANAGER
         MessageInterface::ShowMessage
            ("FileManager::~FileManager deleting %s\n", pos->first.c_str());
         #endif
         
         delete pos->second;
      }
   }
}


//------------------------------------------------------------------------------
// std::string GetPathSeparator()
//------------------------------------------------------------------------------
/**
 * @return path separator; "/" or "\\" dependends on the platform
 */
//------------------------------------------------------------------------------
std::string FileManager::GetPathSeparator()
{
   std::string sep = "/";
   
   char *buffer;
   buffer = getenv("OS");
   if (buffer != NULL)
   {
      //MessageInterface::ShowMessage("Current OS is %s\n", buffer);
      std::string osStr(buffer);
      
      if (osStr.find("Windows") != osStr.npos)
         sep = "\\";
   }
   
   return sep;
}


//------------------------------------------------------------------------------
// std::string GetCurrentPath()
//------------------------------------------------------------------------------
/*
 * Note: This function calls getcwd() which is defiend in <dirent>. There is a
 *       problem compling with VC++ compiler, so until it is resolved, it will
 *       always return blank if it is compiled with VC++ compiler.
 *
 * @return  The current working directory, generally the application path.
 *
 */
//------------------------------------------------------------------------------
std::string FileManager::GetCurrentPath()
{
   std::string currPath;
   
#ifndef _MSC_VER  // if not Microsoft Visual C++
   char buffer[GmatFile::MAX_PATH_LEN];
   getcwd(buffer, GmatFile::MAX_PATH_LEN);
   currPath = buffer;
#endif
   
   return currPath;
   
}


//------------------------------------------------------------------------------
// bool DoesDirectoryExist(const std::string &dirPath)
//------------------------------------------------------------------------------
/*
 * Note: This function calls opendir() which is defiend in <dirent>. There is a
 *       problem compling with VC++ compiler, so until it is resolved, it will
 *       always return false if it is compiled with VC++ compiler.
 *
 * @return  true  If directory exist, false otherwise
 */
//------------------------------------------------------------------------------
bool FileManager::DoesDirectoryExist(const std::string &dirPath)
{
   if (dirPath == "")
      return false;
   
   bool dirExist = false;
   
#ifndef _MSC_VER  // if not Microsoft Visual C++
   DIR *dir = NULL;
   dir = opendir(dirPath.c_str());
   
   if (dir != NULL)
   {
      dirExist = true; 
      closedir(dir);
   }
#endif
   
   return dirExist;
}


//------------------------------------------------------------------------------
// bool DoesFileExist(const std::string &filename)
//------------------------------------------------------------------------------
bool FileManager::DoesFileExist(const std::string &filename)
{
   FILE * pFile;
   pFile = fopen (filename.c_str(), "rt+");
  
   if (pFile!=NULL)
   {
      fclose (pFile);
      return true;
   }
   else
   {
      return false;
   }
}


//------------------------------------------------------------------------------
// void ReadStartupFile(const std::string &fileName = "")
//------------------------------------------------------------------------------
/**
 * Reads GMAT startup file.
 *
 * @param <fileName> startup file name.
 *
 * @exception thrown if file not found, or VERSION date on the startup up file
 *    does not exist or does not match with VERSION_DATE.
 */
//------------------------------------------------------------------------------
void FileManager::ReadStartupFile(const std::string &fileName)
{
   std::string line;
   bool correctVersionFound = false;
   mSavedComments.clear();
   
   if (fileName != "")
      mStartupFileName = fileName;
   
   #ifdef DEBUG_FILE_MANAGER
   MessageInterface::ShowMessage("FileManager::ReadStartupFile() reading:%s\n",
                                 mStartupFileName.c_str());
   #endif
   
   std::ifstream mInStream(mStartupFileName.c_str());
   
   if (!mInStream)
      throw UtilityException
         ("FileManager::ReadStartupFile() cannot open:" + fileName);
   
   while (!mInStream.eof())
   {
      // Use cross-platform GetLine
      GmatFileUtil::GetLine(&mInStream, line);      
      
      #ifdef DEBUG_FILE_MANAGER
      MessageInterface::ShowMessage("line=%s\n", line.c_str());
      #endif
      
      // Skip empty line or comment line
      if (line[0] == '\0' || line[0] == '#')
      {
         if (line.size() > 1 && line[1] == '#')
            mSavedComments.push_back(line);
         continue;
      }
      
      std::string type, equal, name;
      std::stringstream ss("");
      
      ss << line;
      ss >> type >> equal;
      
      if (equal != "=")
      {
         mInStream.close();
         throw UtilityException
            ("FileManager::ReadStartupFile() expecting '=' at line:\n" +
             std::string(line) + "\n");
      }
      
      ss >> name;
      
      #ifdef DEBUG_FILE_MANAGER
      MessageInterface::ShowMessage("type=%s, name=%s\n", type.c_str(), name.c_str());
      #endif
      
      if (!correctVersionFound)
      {
         if (type == "VERSION")
         {
            // check for version date
            if (name == VERSION_DATE)
            {
               correctVersionFound = true;
               continue;
            }
            else
            {
               throw UtilityException
                  ("FileManager::ReadStartupFile() the VERSION is incorrect.\n"
                   "The version date it can handle is " + VERSION_DATE + "\n");
            }
         }
      }
      
      if (correctVersionFound)
         AddFileType(type, name);
      else
         throw UtilityException
            ("FileManager::ReadStartupFile() the VERSION not found.\n"
             "It no longer can read old startup file.\n");
   } // end While()
   
   // add potential files by type names
   AddAvailablePotentialFiles();
   
   // now use log file from the startup file
   MessageInterface::SetLogFile(GetAbsPathname("LOG_FILE"));
   MessageInterface::SetLogEnable(true);
   mInStream.close();
}


//------------------------------------------------------------------------------
// void WriteStartupFile(const std::string &fileName = "")
//------------------------------------------------------------------------------
/**
 * Reads GMAT startup file.
 *
 * @param <fileName> startup file name.
 */
//------------------------------------------------------------------------------
void FileManager::WriteStartupFile(const std::string &fileName)
{
   std::string outFileName = "gmat_startup_file.new.txt";
   
   if (fileName != "")
      outFileName = fileName;
   
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage
      ("FileManager::WriteStartupFile() entered, outFileName = %s\n",
       outFileName.c_str());
   #endif
   
   std::ofstream outStream(outFileName.c_str());
   
   if (!outStream)
      throw UtilityException
         ("FileManager::WriteStartupFile() cannot open:" + fileName);
   
   //---------------------------------------------
   // write header
   //---------------------------------------------
   WriteHeader(outStream);

   // set left justified
   outStream.setf(std::ios::left);
   
   //---------------------------------------------
   // write ROOT_PATH first
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing ROOT_PATH path\n");
   #endif
   outStream << setw(20) << "ROOT_PATH" << " = " << mPathMap["ROOT_PATH"] << "\n";
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write OUTPUT_PATH and LOG file next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing OUTPUT_PATH paths\n");
   #endif
   outStream << setw(20) << "OUTPUT_PATH" << " = " << mPathMap["OUTPUT_PATH"] << "\n";
   WriteFiles(outStream, "LOG");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write GMAT_FUNCTION_PATH next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing GMAT_FUNCTION_PATH paths\n");
   #endif
   for (std::map<std::string, std::string>::iterator pos = mPathMap.begin();
        pos != mPathMap.end(); ++pos)
   {
      if (pos->first == "GMAT_FUNCTION_PATH")
      {
         // Write all GmatFunction paths
         std::list<std::string>::iterator listpos = mGmatFunctionPaths.begin();
         while (listpos != mGmatFunctionPaths.end())
         {
            outStream << setw(20) << pos->first << " = " << *listpos << "\n";
            ++listpos;
         }
         break;
      }
   }
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write MATLAB_FUNCTION_PATH next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing MATLAB_FUNCTION_PATH paths\n");
   #endif
   for (std::map<std::string, std::string>::iterator pos = mPathMap.begin();
        pos != mPathMap.end(); ++pos)
   {
      if (pos->first == "MATLAB_FUNCTION_PATH")
      {
         // Write all GmatFunction paths
         std::list<std::string>::iterator listpos = mMatlabFunctionPaths.begin();
         while (listpos != mMatlabFunctionPaths.end())
         {
            outStream << setw(20) << pos->first << " = " << *listpos << "\n";
            ++listpos;
         }
         break;
      }
   }   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the DE_PATH and DE file next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing DE path\n");
   #endif
   outStream << setw(20) << "DE_PATH" << " = " << mPathMap["DE_PATH"] << "\n";
   WriteFiles(outStream, "DE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the SLP_PATH and SLP file next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing SLP path\n");
   #endif
   outStream << setw(20) << "SLP_PATH" << " = " << mPathMap["SLP_PATH"] << "\n";
   WriteFiles(outStream, "SLP");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the PLANETARY_COEFF_PATH and files next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing PLANETARY_COEFF_PATH path\n");
   #endif
   outStream << setw(20) << "PLANETARY_COEFF_PATH" << " = "
             << mPathMap["PLANETARY_COEFF_PATH"] << "\n";
   WriteFiles(outStream, "EOP_FILE_");   
   WriteFiles(outStream, "PLANETARY_COEFF_FILE");   
   WriteFiles(outStream, "NUTATION_COEFF_FILE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the TIME_PATH and TIME file next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing TIME path\n");
   #endif
   outStream << setw(20) << "TIME_PATH" << " = " << mPathMap["TIME_PATH"] << "\n";
   WriteFiles(outStream, "LEAP_");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write *_POT_PATH and files next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing *_POT_PATH paths\n");
   #endif
   for (std::map<std::string, std::string>::iterator pos = mPathMap.begin();
        pos != mPathMap.end(); ++pos)
   {
      if (pos->first.find("_POT_") != std::string::npos)
      {
         outStream << setw(20) << pos->first << " = " << pos->second << "\n";
      }
   }
   outStream << "#-----------------------------------------------------------\n";
   WriteFiles(outStream, "POT_FILE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the TEXTURE_PATH and files next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing TEXTURE_PATH path\n");
   #endif
   outStream << setw(20) << "TEXTURE_PATH" << " = " << mPathMap["TEXTURE_PATH"] << "\n";
   WriteFiles(outStream, "TEXTURE_FILE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the SPLASH_PATH and files next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing SPLASH_PATH path\n");
   #endif
   outStream << setw(20) << "SPLASH_PATH" << " = " << mPathMap["SPLASH_PATH"] << "\n";
   WriteFiles(outStream, "SPLASH_FILE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write the ICON_PATH and files next
   //---------------------------------------------
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing ICON_PATH path\n");
   #endif
   outStream << setw(20) << "ICON_PATH" << " = " << mPathMap["ICON_PATH"] << "\n";
   WriteFiles(outStream, "ICON_FILE");   
   outStream << "#-----------------------------------------------------------\n";
   
   //---------------------------------------------
   // write saved comments
   //---------------------------------------------
   if (!mSavedComments.empty())
   {
      #ifdef DEBUG_WRITE_STARTUP_FILE
      MessageInterface::ShowMessage("   .....Writing saved comments\n");
      #endif
      outStream << "# Saved Comments\n";
      outStream << "#-----------------------------------------------------------\n";
      for (UnsignedInt i=0; i<mSavedComments.size(); i++)
         outStream << mSavedComments[i] << "\n";
      outStream << "#-----------------------------------------------------------\n";
   }
   
   outStream << "\n";
   outStream.close();
   
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("FileManager::WriteStartupFile() exiting\n");
   #endif
}


//------------------------------------------------------------------------------
// std::string GetRootPath()
//------------------------------------------------------------------------------
/**
 * Retrives root pathname.
 *
 * @return file pathname if path type found.
 */
//------------------------------------------------------------------------------
std::string FileManager::GetRootPath()
{
   return mPathMap["ROOT_PATH"];
}


//------------------------------------------------------------------------------
// std::string GetPathname(const FileType type)
//------------------------------------------------------------------------------
/**
 * Retrives file pathname for the type. 
 *
 * @param <type> enum file type of which pathname to be returned.
 *
 * @return file pathname if path type found.
 * @exception thrown if enum type is out of bounds.
 */
//------------------------------------------------------------------------------
std::string FileManager::GetPathname(const FileType type)
{
   if (type >=0 && type < FileTypeCount)
      return GetPathname(FILE_TYPE_STRING[type]);
   
   std::stringstream ss("");
   ss << "FileManager::GetPathname() enum type: " << type
      << " is out of bounds\n";
   
   throw UtilityException(ss.str());
}


//------------------------------------------------------------------------------
// std::string GetPathname(const std::string &typeName)
//------------------------------------------------------------------------------
/**
 * Retrives file pathname for the type name.
 *
 * @param <typeName> file type name of which pathname to be returned.
 *
 * @return pathname if type found.
 * @exception thrown if type cannot be found.
 */
//------------------------------------------------------------------------------
std::string FileManager::GetPathname(const std::string &typeName)
{
   std::string fileType = GmatStringUtil::ToUpper(typeName);
   
   // typeName contains _PATH
   if (fileType.find("_PATH") != fileType.npos)
   {
      if (mPathMap.find(fileType) != mPathMap.end())
         return mPathMap[fileType];
   }
   else
   {
      if (mFileMap.find(fileType) != mFileMap.end())
      {
         // Replace ROOT_PATH with abs path
         std::string pathname = mPathMap[mFileMap[fileType]->mPath];
         if (pathname.find("ROOT_PATH") != pathname.npos)
         {
            std::string pathname2 = mPathMap["ROOT_PATH"] +
               mPathMap[mFileMap[fileType]->mPath];
            std::string::size_type pos1 = pathname2.find("ROOT_PATH");
            pathname2.replace(pos1, 10, "");
            return pathname2;
         }
         else
         {
            return mPathMap[mFileMap[fileType]->mPath];
         }
      }
   }
   
   throw UtilityException("FileManager::GetPathname() file type: " + typeName +
                           " is unknown\n");
}


//------------------------------------------------------------------------------
// std::string GetFilename(const FileType type)
//------------------------------------------------------------------------------
/**
 * Retrives filename for the type. 
 *
 * @param <type> enum file type of which filename to be returned.
 *
 * @return file filename if file type found
 * @exception thrown if enum type is out of bounds
 */
//------------------------------------------------------------------------------
std::string FileManager::GetFilename(const FileType type)
{
   if (type >=0 && type < FileTypeCount)
      return GetFilename(FILE_TYPE_STRING[type]);
   
   std::stringstream ss("");
   ss << "FileManager::GetFilename() enum type: " << type
      << " is out of bounds\n";
   
   throw UtilityException(ss.str());
}


//------------------------------------------------------------------------------
// std::string GetFilename(const std::string &typeName)
//------------------------------------------------------------------------------
/**
 * Retrives filename for the type name.
 *
 * @param <type> file type name of which filename to be returned.
 *
 * @return file filename if file type found
 * @exception thrown if type cannot be found.
 */
//------------------------------------------------------------------------------
std::string FileManager::GetFilename(const std::string &typeName)
{
   if (mFileMap.find(typeName) != mFileMap.end())
      return mFileMap[typeName]->mFile;

   //MessageInterface::ShowMessage
   //   ("FileManager::GetFilename() file type: %s is unknown\n", typeName.c_str());
   
   //return "UNKNOWN_FILE_TYPE";
   
   throw UtilityException("FileManager::GetFilename() file type: " + typeName +
                           " is unknown\n");
}


//------------------------------------------------------------------------------
// std::string GetFullPathname(const FileType type)
//------------------------------------------------------------------------------
std::string FileManager::GetFullPathname(const FileType type)
{
   return GetAbsPathname(type);
}


//------------------------------------------------------------------------------
// std::string GetFullPathname(const std::string &typeName)
//------------------------------------------------------------------------------
std::string FileManager::GetFullPathname(const std::string &typeName)
{
   return GetAbsPathname(typeName);
}


//------------------------------------------------------------------------------
// std::string GetAbsPathname(const FileType type)
//------------------------------------------------------------------------------
/**
 * Retrieves full pathname for the type.
 *
 * @param <type> file type of which filename to be returned.
 *
 * @return file pathname if file type found
 * @exception thrown if enum type is out of bounds
 */
//------------------------------------------------------------------------------
std::string FileManager::GetAbsPathname(const FileType type)
{
   
   if (type >=0 && type < FileTypeCount)
      return GetAbsPathname(FILE_TYPE_STRING[type]);
   
   std::stringstream ss("");
   ss << "FileManager::GetAbsPathname() enum type: " << type <<
      " is out of bounds\n";
   
   throw UtilityException(ss.str());
}


//------------------------------------------------------------------------------
// std::string GetAbsPathname(const std::string &typeName)
//------------------------------------------------------------------------------
/**
 * Retrives full pathname for the type name.
 *
 * @param <type> file type name of which filename to be returned.
 *
 * @return file pathname if file type name found
 * @exception thrown if type cannot be found.
 */
//------------------------------------------------------------------------------
std::string FileManager::GetAbsPathname(const std::string &typeName)
{
   std::string fileType = GmatStringUtil::ToUpper(typeName);
   std::string absPath;
   
   #ifdef DEBUG_FILE_MANAGER
   MessageInterface::ShowMessage
      ("FileManager::GetAbsPathname() typeName=%s\n", typeName.c_str());
   #endif
   
   // typeName contains _PATH
   if (fileType.find("_PATH") != fileType.npos)
   {
      if (mPathMap.find(fileType) != mPathMap.end())
      {
         // Replace ROOT_PATH with abs path
         std::string pathname = mPathMap[fileType];
         if (pathname.find("ROOT_PATH") != pathname.npos)
         {
            std::string pathname2 = mPathMap["ROOT_PATH"] + pathname;
            std::string::size_type pos1 = pathname2.find("ROOT_PATH");
            pathname2.replace(pos1, 10, "");
            absPath = pathname2;
         }
         else
         {
            absPath = pathname;
         }
         
         #ifdef DEBUG_FILE_MANAGER
         MessageInterface::ShowMessage
            ("FileManager::GetAbsPathname() with _PATH returning '%s'\n", absPath.c_str());
         #endif
         return absPath;
      }
   }
   else
   {
      if (mFileMap.find(fileType) != mFileMap.end())
      {
         std::string path = GetPathname(fileType);
         absPath = path + mFileMap[fileType]->mFile;
      }
      else if (mFileMap.find(fileType + "_ABS") != mFileMap.end())
      {
         absPath = mFileMap[typeName]->mFile;
      }
      
      #ifdef DEBUG_FILE_MANAGER
      MessageInterface::ShowMessage
         ("FileManager::GetAbsPathname() without _PATH returning '%s'\n", absPath.c_str());
      #endif
      return absPath;
   }
   
   throw UtilityException
      (GmatStringUtil::ToUpper(typeName) + " not in the gmat_startup_file\n");
   
}


//------------------------------------------------------------------------------
// std::string ConvertToAbsPath(const std::string &relPath)
//------------------------------------------------------------------------------
std::string FileManager::ConvertToAbsPath(const std::string &relPath)
{
   #ifdef DEBUG_FILE_PATH
   MessageInterface::ShowMessage
      ("FileManager::ConvertToAbsPath() relPath='%s'\n", relPath.c_str());
   #endif
   
   std::string absPath = relPath;
   
   // relPath contains _PATH
   std::string::size_type index = absPath.find("_PATH");
   if (index != absPath.npos)
   {
      std::string pathSymbol = absPath.substr(0, index+5);
      std::string remPath = absPath.substr(index+6);
      
      #ifdef DEBUG_FILE_PATH
      MessageInterface::ShowMessage("   pathSymbol='%s'\n", pathSymbol.c_str());
      MessageInterface::ShowMessage("   remPath='%s'\n", remPath.c_str());
      #endif
      
      if (mPathMap.find(pathSymbol) != mPathMap.end())
      {
         // Replace *_PATH with abs path
         std::string pathname = mPathMap[pathSymbol];
         absPath = pathname + remPath;
         absPath = ConvertToAbsPath(absPath);
      }
   }
   
   #ifdef DEBUG_FILE_PATH
   MessageInterface::ShowMessage
      ("FileManager::ConvertToAbsPath() returning '%s'\n", absPath.c_str());
   #endif
   
   return absPath;
}


//------------------------------------------------------------------------------
// void SetAbsPathname(const FileType type, const std::string &newpath)
//------------------------------------------------------------------------------
/**
 * Sets absoulute pathname for the type.
 *
 * @param <type> file type of which path to be set.
 * @param <newpath> new pathname.
 *
 * @exception thrown if enum type is out of bounds
 */
//------------------------------------------------------------------------------
void FileManager::SetAbsPathname(const FileType type, const std::string &newpath)
{
   if (type >=0 && type <= TEXTURE_PATH)
   {
      SetAbsPathname(FILE_TYPE_STRING[type], newpath);
   }
   else
   {
      std::stringstream ss("");
      ss << "FileManager::GetAbsPathname() enum type: " << type <<
         " is out of bounds of file path\n";
   
      throw UtilityException(ss.str());
   }
}


//------------------------------------------------------------------------------
// void SetAbsPathname(const std::string &type, const std::string &newpath)
//------------------------------------------------------------------------------
/**
 * Sets absolute pathname for the type.
 *
 * @param <type> type name of which path to be set.
 * @param <newpath> new pathname.
 *
 * @exception thrown if enum type is out of bounds
 */
//------------------------------------------------------------------------------
void FileManager::SetAbsPathname(const std::string &type, const std::string &newpath)
{
   if (mPathMap.find(type) != mPathMap.end())
   {
      if (type.find("_PATH") != type.npos)
      {
         std::string str2 = newpath;
         
         // append '/' if not there
         std::string::size_type index = str2.find_last_of("/\\");
         if (index != str2.length() - 1)
         {
            str2 = str2 + "/";
         }
         else
         {
            index = str2.find_last_not_of("/\\");            
            str2 = str2.substr(0, index+1) + "/";
         }
         
         mPathMap[type] = str2;
         
         #ifdef DEBUG_SET_PATH
         MessageInterface::ShowMessage
            ("FileManager::SetAbsPathname() %s set to %s\n", type.c_str(),
             str2.c_str());
         #endif
      }
      else
      {
         throw UtilityException
            ("FileManager::SetAbsPathname() type doesn't contain _PATH");
      }
   }
}


//------------------------------------------------------------------------------
// void ClearGmatFunctionPath()
//------------------------------------------------------------------------------
void FileManager::ClearGmatFunctionPath()
{
   mGmatFunctionPaths.clear();
}


//------------------------------------------------------------------------------
// void  AddGmatFunctionPath(const std::string &path, bool addFront, bool addFront)
//------------------------------------------------------------------------------
/*
 * If new path it adds to the GmatFunction path list.
 * If path already exist, it moves to the front or back of the list, depends on
 * addFront flag.
 *
 * @param  path  path name to be added
 * @param  addFront  if set to true, it adds to the front, else adds to the back (true)
 */
//------------------------------------------------------------------------------
void FileManager::AddGmatFunctionPath(const std::string &path, bool addFront)
{
   #ifdef DEBUG_FUNCTION_PATH
   MessageInterface::ShowMessage
      ("FileManager::AddGmatFunctionPath() Adding %s to GmatFunctionPath\n   "
       "addFront=%d\n", path.c_str(), addFront);
   #endif
   
   std::list<std::string>::iterator pos =
      find(mGmatFunctionPaths.begin(), mGmatFunctionPaths.end(), path);
   
   if (pos == mGmatFunctionPaths.end())
   {
      #ifdef DEBUG_FUNCTION_PATH
      MessageInterface::ShowMessage
         ("   the path <%s> is new, so adding to %s\n", path.c_str(),
          addFront ? "front" : "back");
      #endif
      
      // if new path, add to front or back of the list
      if (addFront)
         mGmatFunctionPaths.push_front(path);
      else
         mGmatFunctionPaths.push_back(path);
   }
   else
   {
      // if existing path remove and add front or back of the list
      #ifdef DEBUG_FUNCTION_PATH
      MessageInterface::ShowMessage
         ("   the path <%s> already exist, so moving to %s\n", path.c_str(),
          addFront ? "front" : "back");
      #endif
      
      std::string oldPath = *pos;
      mGmatFunctionPaths.erase(pos);
      if (addFront)
         mGmatFunctionPaths.push_front(oldPath);
      else
         mGmatFunctionPaths.push_back(oldPath);
   }
   
   #ifdef DEBUG_FUNCTION_PATH
   pos = mGmatFunctionPaths.begin();
   while (pos != mGmatFunctionPaths.end())
   {
      MessageInterface::ShowMessage
         ("   mGmatFunctionPaths = %s\n", (*pos).c_str());
      ++pos;
   }
   #endif
}


//------------------------------------------------------------------------------
// std::string GetGmatFunctionPath(const std::string &funcName)
//------------------------------------------------------------------------------
/*
 * Returns the absolute path that has GmatFunction name.
 * It searches in the most recently added path first which is at the top of
 * the list.
 *
 * @param   funcName  Name of the GmatFunction to be located
 * @return  Path that has GmatFunction name
 */
//------------------------------------------------------------------------------
std::string FileManager::GetGmatFunctionPath(const std::string &funcName)
{
   return GetFunctionPath(GMAT_FUNCTION, mGmatFunctionPaths, funcName);
   
//    #ifdef DEBUG_FUNCTION_PATH
//    MessageInterface::ShowMessage
//       ("FileManager::GetGmatFunctionPath() funcName='%s'\n", funcName.c_str());
//    #endif
   
//    // Search through mGmatFunctionPaths
//    // The most recent path added to the last, so search backwards
//    std::string pathName, fullPath;
//    bool fileFound = false;
   
//    // add .gmf if not found
//    std::string funcName1 = funcName;
//    if (funcName.find(".gmf") == funcName.npos)
//       funcName1 = funcName1 + ".gmf";
   
//    // Search from the top of the list, which is the most recently added path
//    // The search order goes from top to bottom. (loj: 2008.10.02)
//    std::list<std::string>::iterator pos = mGmatFunctionPaths.begin();
//    while (pos != mGmatFunctionPaths.end())
//    {
//       pathName = *pos;
//       fullPath = ConvertToAbsPath(pathName) + funcName1;
      
//       #ifdef DEBUG_FUNCTION_PATH
//       MessageInterface::ShowMessage("   fullPath='%s'\n", fullPath.c_str());
//       #endif
      
//       if (GmatFileUtil::DoesFileExist(fullPath))
//       {
//          fileFound = true;
//          break;
//       }
      
//       pos++;
//    }
   
//    if (fileFound)
//       fullPath = GmatFileUtil::ParsePathName(fullPath);
//    else
//       fullPath = "";
   
//    #ifdef DEBUG_FUNCTION_PATH
//    MessageInterface::ShowMessage
//       ("FileManager::GetGmatFunctionPath(%s) returning '%s'\n", funcName.c_str(),
//        fullPath.c_str());
//    #endif
   
//    return fullPath;
}


//------------------------------------------------------------------------------
// const StringArray& GetAllGmatFunctionPaths()
//------------------------------------------------------------------------------
const StringArray& FileManager::GetAllGmatFunctionPaths()
{
   mGmatFunctionFullPaths.clear();
   
   std::list<std::string>::iterator listpos = mGmatFunctionPaths.begin();
   while (listpos != mGmatFunctionPaths.end())
   {
      mGmatFunctionFullPaths.push_back(ConvertToAbsPath(*listpos));
      ++listpos;
   }
   
   return mGmatFunctionFullPaths;
}


//------------------------------------------------------------------------------
// void ClearMatlabFunctionPath()
//------------------------------------------------------------------------------
void FileManager::ClearMatlabFunctionPath()
{
   mMatlabFunctionPaths.clear();
}


//------------------------------------------------------------------------------
// void  AddMatlabFunctionPath(const std::string &pat, bool addFront)
//------------------------------------------------------------------------------
/*
 * If new path it adds to the MatlabFunction path list.
 * If path already exist, it moves to the front or back of the list, depends on
 * addFront flag.
 *
 * @param  path  path name to be added
 * @param  addFront  if set to true, it adds to the front, else adds to the back (true)
 */
//------------------------------------------------------------------------------
void FileManager::AddMatlabFunctionPath(const std::string &path, bool addFront)
{
   #ifdef DEBUG_FUNCTION_PATH
   MessageInterface::ShowMessage
      ("FileManager::AddMatlabFunctionPath() Adding %s to MatlabFunctionPath\n",
       path.c_str());
   #endif
   
   std::list<std::string>::iterator pos =
      find(mMatlabFunctionPaths.begin(), mMatlabFunctionPaths.end(), path);
   
   if (pos == mMatlabFunctionPaths.end())
   {
      // if new path, add to front or back of the list
      if (addFront)
         mMatlabFunctionPaths.push_front(path);
      else
         mMatlabFunctionPaths.push_back(path);
   }
   else
   {
      // if existing path remove and add front or back of the list
      std::string oldPath = *pos;
      mMatlabFunctionPaths.erase(pos);
      if (addFront)
         mMatlabFunctionPaths.push_front(oldPath);
      else
         mMatlabFunctionPaths.push_back(oldPath);
   }
   
   #ifdef DEBUG_FUNCTION_PATH
   pos = mMatlabFunctionPaths.begin();
   while (pos != mMatlabFunctionPaths.end())
   {
      MessageInterface::ShowMessage
         ("   mMatlabFunctionPaths=%s\n",(*pos).c_str());
      ++pos;
   }
   #endif
}


//------------------------------------------------------------------------------
// std::string GetMatlabFunctionPath(const std::string &name)
//------------------------------------------------------------------------------
/*
 * Returns the absolute path that has MatlabFunction name.
 * It searches in the most recently added path first which is at the top of
 * the list.
 *
 * @param   funcName  Name of the MatlabFunction to be located
 * @return  Path that has MatlabFunction name
 */
//------------------------------------------------------------------------------
std::string FileManager::GetMatlabFunctionPath(const std::string &name)
{
   return GetFunctionPath(MATLAB_FUNCTION, mMatlabFunctionPaths, name);
}


//------------------------------------------------------------------------------
// const StringArray& GetAllMatlabFunctionPaths()
//------------------------------------------------------------------------------
const StringArray& FileManager::GetAllMatlabFunctionPaths()
{
   mMatlabFunctionFullPaths.clear();
   
   std::list<std::string>::iterator listpos = mMatlabFunctionPaths.begin();
   while (listpos != mMatlabFunctionPaths.end())
   {
      mMatlabFunctionFullPaths.push_back(ConvertToAbsPath(*listpos));
      ++listpos;
   }
   
   return mMatlabFunctionFullPaths;
}


//------------------------------------------------------------------------------
// const StringArray& GetPluginList()
//------------------------------------------------------------------------------
/**
 * Accesses the list of plug-in libraries parsed from the startup file.
 * 
 * @return The list of plug-in libraries
 */
//------------------------------------------------------------------------------
const StringArray& FileManager::GetPluginList()
{
   return mPluginList;
}

//---------------------------------
// private methods
//---------------------------------

//------------------------------------------------------------------------------
// std::string GetFunctionPath(FunctionType type, const std::list<std::string> &pathList
//                             const std::string &funcName)
//------------------------------------------------------------------------------
/*
 * Searches proper function path list from the top and return first path found.
 *
 * @param  type  type of function (MATLAB_FUNCTION, GMAT_FUNCTION)
 * @param  pathList  function path list to use in search
 * @param  funcName  name of the function to search
 */
//------------------------------------------------------------------------------
std::string FileManager::GetFunctionPath(FunctionType type,
                                         std::list<std::string> &pathList,
                                         const std::string &funcName)
{
   #ifdef DEBUG_FUNCTION_PATH
   MessageInterface::ShowMessage
      ("FileManager::GetFunctionPath(%s) with type %d entered\n",
       funcName.c_str(), type);
   #endif
   
   std::string funcName1 = funcName;
   if (type == GMAT_FUNCTION)
   {
      if (funcName.find(".gmf") == funcName.npos)
         funcName1 = funcName1 + ".gmf";
   }
   else
   {
      if (funcName.find(".m") == funcName.npos)
         funcName1 = funcName1 + ".m";
   }
   
   // Search through pathList
   // The most recent path added to the last, so search backwards
   std::string pathName, fullPath;
   bool fileFound = false;
   
   // Search from the top of the list, which is the most recently added path
   // The search order goes from top to bottom. (loj: 2008.10.02)
   std::list<std::string>::iterator pos = pathList.begin();
   while (pos != pathList.end())
   {
      pathName = *pos;
      fullPath = ConvertToAbsPath(pathName) + funcName1;
      
      #ifdef DEBUG_FUNCTION_PATH
      MessageInterface::ShowMessage("   fullPath='%s'\n", fullPath.c_str());
      #endif
      
      if (GmatFileUtil::DoesFileExist(fullPath))
      {
         fileFound = true;
         break;
      }
      
      pos++;
   }
   
   if (fileFound)
      fullPath = GmatFileUtil::ParsePathName(fullPath);
   else
      fullPath = "";
   
   #ifdef DEBUG_FUNCTION_PATH
   MessageInterface::ShowMessage
      ("FileManager::GetFunctionPath(%s) returning '%s'\n", funcName.c_str(),
       fullPath.c_str());
   #endif
   
   return fullPath;
   
}


//------------------------------------------------------------------------------
// void AddFileType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Adds file type, path, name to the list. If typeName ends with _PATH, it is
 * added to path map. If typeName ends with _FILE, it is added to file map, an
 * exception is throw otherwise.
 *
 * @param <type> file type
 * @param <name> file path or name
 *
 * @excepton thrown if typeName does not end with _PATH or _FILE
 */
//------------------------------------------------------------------------------
void FileManager::AddFileType(const std::string &type, const std::string &name)
{  
   #ifdef DEBUG_FILE_MANAGER
   MessageInterface::ShowMessage
      ("FileManager::AddFileType() type=%s, name=%s\n", type.c_str(), name.c_str());
   #endif
   
   if (type.find("_PATH") != type.npos)
   {
      std::string str2 = name;
      
      // append '/' if not there
      if (str2.find_last_of('/') != str2.length()-1)
         str2 = str2 + "/";
      
      mPathMap[type] = str2;
      
      // Handle Gmat and Matlab Function path
      if (type == "GMAT_FUNCTION_PATH")
         AddGmatFunctionPath(str2, false);
      else if (type == "MATLAB_FUNCTION_PATH")
         AddMatlabFunctionPath(str2, false);
      
   }
   else if (type.find("_FILE_ABS") != type.npos)
   {
      mFileMap[type] = new FileInfo("", name);
   }
   else if (type.find("_FILE") != type.npos)
   {
      std::string pathName;
      std::string fileName;
      
      // file name
      std::string::size_type pos = name.find("/");
      if (pos != name.npos)
      {
         std::string pathName = name.substr(0, pos);
         std::string fileName = name.substr(pos+1, name.npos);
         mFileMap[type] = new FileInfo(pathName, fileName);
      }
      else
      {
         //loj: Should we add current path?
         std::string pathName = "CURRENT_PATH";
         mPathMap[pathName] = "./";
         std::string fileName = name;
         mFileMap[type] = new FileInfo(pathName, fileName);
         
         MessageInterface::ShowMessage
            ("FileManager::AddFileType() 'PATH/' not found in line:\n% = % \n"
             "So adding CURRENT_PATH = ./\n", type.c_str(), name.c_str());
         
         //loj: Should we just throw an exception?
         //mInStream.close();
         //throw UtilityException
         //   ("FileManager::AddFileType() expecting 'PATH/' in line:\n" +
         //    type + " = " + name);
      }
   }
   else if (type == "PLUGIN")
   {
      #ifdef DEBUG_PLUGIN_DETECTION
         MessageInterface::ShowMessage("Adding plug-in %s to plugin list\n", 
               name.c_str());
      #endif
      mPluginList.push_back(name);
   }
   else
   {
      throw UtilityException
         ("FileManager::AddFileType() file type should have '_PATH' or '_FILE'"
          " in:\n" + type);
   }
}


//------------------------------------------------------------------------------
// void AddAvailablePotentialFiles()
//------------------------------------------------------------------------------
void FileManager::AddAvailablePotentialFiles()
{
   // add available potential files
   
   // earth gravity files
   if (mFileMap.find("JGM2_FILE") == mFileMap.end())
      AddFileType("JGM2_FILE", "EARTH_POT_PATH/JGM2.cof");
   
   if (mFileMap.find("JGM3_FILE") == mFileMap.end())
      AddFileType("JGM3_FILE", "EARTH_POT_PATH/JGM3.cof");
   
   if (mFileMap.find("EGM96_FILE") == mFileMap.end())
      AddFileType("EGM96_FILE", "EARTH_POT_PATH/EGM96low.cof");
   
   // luna gravity files
   if (mFileMap.find("LP165P_FILE") == mFileMap.end())
      AddFileType("LP165P_FILE", "LUNA_POT_PATH/LP165P.cof");
   
   // venus gravity files
   if (mFileMap.find("MGNP180U_FILE") == mFileMap.end())
      AddFileType("MGNP180U_FILE", "VENUS_POT_PATH/MGNP180U.cof");
   
   // mars gravity files
   if (mFileMap.find("MARS50C_FILE") == mFileMap.end())
      AddFileType("MARS50C_FILE", "MARS_POT_PATH/Mars50c.cof");
   
}


//------------------------------------------------------------------------------
// void WriteHeader(std::ofstream &outStream)
//------------------------------------------------------------------------------
void FileManager::WriteHeader(std::ofstream &outStream)
{
   outStream << "VERSION = " << VERSION_DATE << "\n";
   outStream << "#============================================================="
      "==================\n";
   outStream << "# ! Do not remove or change VERSION date, "
      "it won't work otherwise!!\n";
   outStream << "# Only the new FileManager, version after " << VERSION_DATE <<
      " reconizes this new format.\n";
   outStream << "#-------------------------------------------------------------"
      "------------------\n";
   outStream << "# Path/File naming convention:\n";
   outStream << "#   - Path name should end with _PATH\n";
   outStream << "#   - File name should end with _FILE\n";
   outStream << "#   - Path/File names are case sensative\n";
   outStream << "#\n";
   outStream << "# You can add potential file and texture file by following the naming\n";
   outStream << "# convention.\n";
   outStream << "#   - Potential file should begin with planet name and end with _POT_FILE\n";
   outStream << "#   - Texture file should begin with planet name and end with _TEXTURE_FILE\n";
   outStream << "#\n";
   outStream << "# If same _FILE is specified multiple times, it will use the last one.\n";
   outStream << "#\n";
   outStream << "# All comment lines starting ## will be written last when saving this file.\n";
   outStream << "#\n";
   outStream << "#============================================================="
      "==================\n";
}

//------------------------------------------------------------------------------
// void WriteFiles(std::ofstream &outStream, const std::string &type)
//------------------------------------------------------------------------------
void FileManager::WriteFiles(std::ofstream &outStream, const std::string &type)
{
   #ifdef DEBUG_WRITE_STARTUP_FILE
   MessageInterface::ShowMessage("   .....Writing %s file\n", type.c_str());
   #endif
   
   for (std::map<std::string, FileInfo*>::iterator pos = mFileMap.begin();
        pos != mFileMap.end(); ++pos)
   {
      if (pos->first.find(type) != std::string::npos)
      {
         if (pos->second)
         {
            outStream << setw(20) << pos->first << " = " << pos->second->mPath << "/"
                      << pos->second->mFile << "\n";
         }
      }
   }
}


//------------------------------------------------------------------------------
// FileManager()
//------------------------------------------------------------------------------
/*
 * Constructor
 */
//------------------------------------------------------------------------------
FileManager::FileManager()
{  
   MessageInterface::SetLogEnable(false); // so that debug can be written from here
   
   #ifdef DEBUG_FILE_MANAGER
   MessageInterface::ShowMessage("FileManager::FileManager() entered\n");
   #endif
   
   AddFileType("ROOT_PATH", "./");
   mStartupFileName = "gmat_startup_file.txt";
   
   //-------------------------------------------------------
   // create default output paths and files
   //-------------------------------------------------------
   // output file path
   AddFileType("OUTPUT_PATH", "./files/output/");
   AddFileType("LOG_FILE", "OUTPUT_PATH/GmatLog.txt");
   AddFileType("REPORT_FILE", "OUTPUT_PATH/ReportFile.txt");

   //loj: Should we create default input files?
#ifdef FM_CREATE_DEFAULT_INPUT
   
   //-------------------------------------------------------
   // create default input paths and files
   //-------------------------------------------------------
   // output file path
   AddFileType("OUTPUT_PATH", "./files/output/");
   AddFileType("LOG_FILE", "OUTPUT_PATH/GmatLog.txt");
   AddFileType("REPORT_FILE", "OUTPUT_PATH/ReportFile.txt");
   
   // texture file path
   AddFileType("TEXTURE_PATH", "./files/plot/texture/");
   
   // slp files
   AddFileType("SLP_PATH", "./files/planetary_ephem/slp/");
   AddFileType("SLP_FILE", "SLP_PATH/mn2000.pc");
   AddFileType("SLP_TIME_COEFF_FILE", "SLP_PATH/timecof.pc");
   
   // de files
   AddFileType("DE_PATH", "./files/planetary_ephem/de/");
   AddFileType("DE200_FILE", "DE_PATH/winp1941.200");
   AddFileType("DE202_FILE", "DE_PATH/winp1950.202");
   AddFileType("DE405_FILE", "DE_PATH/winp1941.405");
   
   // earth gravity files
   AddFileType("EARTH_POT_PATH", "./files/gravity/earth/");
   AddFileType("JGM2_FILE", "EARTH_POT_PATH/JGM2.cof");
   AddFileType("JGM3_FILE", "EARTH_POT_PATH/JGM3.cof");
   AddFileType("EGM96_FILE", "EARTH_POT_PATH/EGM96.cof");
   
   // luna gravity files
   AddFileType("LUNA_POT_PATH", "./files/gravity/luna/");
   AddFileType("LP165P_FILE", "LUNA_POT_PATH/lp165p.cof");
   
   // venus gravity files
   AddFileType("VENUS_POT_PATH", "./files/gravity/venus/");
   AddFileType("MGNP180U_FILE", "VENUS_POT_PATH/MGNP180U.cof");
   
   // mars gravity files
   AddFileType("MARS_POT_PATH", "./files/gravity/mars/");
   AddFileType("MARS50C_FILE", "MARS_POT_PATH/Mars50c.cof");
   
   // planetary coeff. fiels
   AddFileType("PLANETARY_COEFF_PATH", "./files/planetary_coeff/");
   AddFileType("EOP_FILE", "PLANETARY_COEFF_PATH/eopc04.62-now");
   AddFileType("PLANETARY_COEFF_FILE", "PLANETARY_COEFF_PATH/NUT85.DAT");
   AddFileType("NUTATION_COEFF_FILE", "PLANETARY_COEFF_PATH/NUTATION.DAT");
   
   // time files
   AddFileType("TIME_PATH", "./files/time/");
   AddFileType("LEAP_SECS_FILE", "TIME_PATH/tai-utc.dat");
   
#endif  
}


