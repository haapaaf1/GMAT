//$Header$
//------------------------------------------------------------------------------
//                               ScriptInterpreter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Waka Waktola
// Created: 2006/08/25
// Copyright: (c) 2006 NASA/GSFC. All rights reserved.
//
//------------------------------------------------------------------------------
/**
 * Class definition for the ScriptInterpreter
 */
//------------------------------------------------------------------------------

#ifndef SCRIPTINTERPRETER_HPP
#define SCRIPTINTERPRETER_HPP

#include "Interpreter.hpp"
#include "InterpreterException.hpp"
#include "ScriptReadWriter.hpp"

/**
 * The ScriptInterpreter class manages the script reading and writing process.
 */
class ScriptInterpreter : public Interpreter
{
public:        
   static ScriptInterpreter*   Instance();

   virtual bool                Interpret();
   virtual bool                Interpret(const std::string &scriptfile); 
   virtual bool                Build(Gmat::WriteMode mode);
        
   bool                        Build(const std::string &scriptfile,
                                     Gmat::WriteMode mode = Gmat::SCRIPTING);
   
   bool SetInStream(std::istream *str);
   bool SetOutStream(std::ostream *str);
   
protected:
   
   /// The script interpreter singleton
   static ScriptInterpreter *instance;
   
   std::istream *inStream;
   std::ostream *outStream;
   
   bool                        ReadScript();
   std::string                 ReadLogicalBlock();
   bool                        Parse(const std::string &logicBlock);
   bool                        WriteScript(Gmat::WriteMode mode = Gmat::SCRIPTING);
   
private:
   ScriptInterpreter();
   virtual ~ScriptInterpreter();
   
   /// The logical block
   //std::string logicalBlock; replaced by currentBlock
   /// A counter that counts the logical blocks of script as they are read.
   Integer logicalBlockCount; 
   /// The stream used for script reading and writing.
   //std::iostream scriptStream;
   /// A pointer to the ScriptReadWriter used when reading or writing script.
   ScriptReadWriter* theReadWriter;
   /// Name of the current script file
   std::string scriptFilename;
   /// Section delimiter comment
   StringArray sectionDelimiterString;
   
};

#endif // SCRIPTINTERPRETER_HPP

