//$Header: /cygdrive/p/dev/cvs/test/TestTextParser/TestTextParser.cpp,v 1.2 2008/08/22 14:47:49 lojun Exp $
//------------------------------------------------------------------------------
//                                  TestTextParser
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2006/08/21
//
/**
 * Test driver for TextParser.
 */
//------------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "gmatdefs.hpp"
#include "StringUtil.hpp"
#include "GmatBaseException.hpp"
#include "TestOutput.hpp"
#include "TextParser.hpp"
#include "MessageInterface.hpp"
#include "ConsoleMessageReceiver.hpp"

using namespace std;
using namespace GmatStringUtil;

//------------------------------------------------------------------------------
// void WriteStringArray(TestOutput &out, const StringArray &parts,
//                       const std::string &exp1, const std::string &exp2,
//                       const std::string &exp3, bool addNewLine)
//------------------------------------------------------------------------------
void WriteStringArray(TestOutput &out, const StringArray &parts,
                      const std::string &exp1, const std::string &exp2,
                      const std::string &exp3, bool addNewLine)
{
   out.SetAddNewLine(addNewLine);
   int count = parts.size();
   out.Put("count = ", count);
   for (unsigned int i=0; i<parts.size(); i++)
      out.Put(parts[i]);
   
   if (count == 0)
      throw GmatBaseException
         (">>>>> The count of parts must be greater than 0\n");
   
   out.Put("");
   
   if (exp1 != "")
      out.Validate(parts[0], exp1);
   
   if (exp2 != "")
      out.Validate(parts[1], exp2);
   
   if (exp3 != "")
      out.Validate(parts[2], exp3);
}


//------------------------------------------------------------------------------
// void WriteParts(TestOutput &out, TextParser &tp, const std::string &exp)
//------------------------------------------------------------------------------
void WriteParts(TestOutput &out, TextParser &tp, const std::string &exp)
{
   out.SetAddNewLine(false);
   out.Put("prefaceComment = ", tp.GetPrefaceComment());
   out.SetAddNewLine(false);
   out.Put("inlineComment  = ", tp.GetInlineComment());
   out.Put("theInstruction = ", tp.GetInstruction());
   out.Put("");
   
   out.Validate(tp.GetInstruction(), exp);
}


//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{
   ConsoleMessageReceiver *consoleMsg = ConsoleMessageReceiver::Instance();
   MessageInterface::SetMessageReceiver(consoleMsg);
   MessageInterface::SetLogFile("../../TestTextParser/GmatLog.txt");
   MessageInterface::ShowMessage
      ("================================================== TestTextParser\n");
   
   TextParser tp;
   
   Integer blockType, count;
   const std::string GMAT = "GMAT ";
   std::string str, str1, inst;
   std::string cmt1, cmt2;
   std::string cmdStr, typStr, objStr, braceStr, parenStr, cmdExp;
   std::string lhs, rhs;
   StringArray chunks;
   StringArray parts;
   StringArray theCommandList;
   
   theCommandList.push_back("Propagate");
   theCommandList.push_back("Maneuver");
   theCommandList.push_back("BeginFiniteBurn");
   theCommandList.push_back("EndFiniteBurn");
   theCommandList.push_back("BeginScript");
   theCommandList.push_back("EndScript");
   theCommandList.push_back("Target");
   theCommandList.push_back("Achieve");
   theCommandList.push_back("Vary");
   theCommandList.push_back("If");
   theCommandList.push_back("EndIf");
   theCommandList.push_back("While");
   theCommandList.push_back("EndWhile");
   theCommandList.push_back("For");
   theCommandList.push_back("EndFor");
   theCommandList.push_back("Save");
   theCommandList.push_back("Report");
   
   tp.Initialize(theCommandList);
   
   
   out.Put("======================================== test SeparateBrackets()\n");
   out.Put("----------------------------------------");
   str = "{sat.EarthMJ2000Eq.X}";
   out.Put("===== SeparateBrackets('{}')");
   out.Put(str);
   parts = tp.SeparateBrackets(str, "{}", " ,", false, true);
   WriteStringArray(out, parts, "{sat.EarthMJ2000Eq.X}", "", "", true);
   
   out.Put("----------------------------------------");
   str = "sat.EarthMJ2000Eq.X";
   out.Put("===== SeparateBrackets('{}')");
   out.Put(str);
   parts = tp.SeparateBrackets(str, "{}", " ,", false, true);
   WriteStringArray(out, parts, "sat.EarthMJ2000Eq.X", "", "", true);
   
   out.Put("");
   out.Put("======================================== test SeparateSpaces()\n");
   
   out.Put("----------------------------------------");
   str = "Sat1 , Sat2 , Sat3";
   out.Put(str);
   
   out.Put("===== SeparateSpaces()");
   parts = tp.SeparateSpaces(str);
   WriteStringArray(out, parts, "Sat1", "Sat2", "Sat3", true);
   
   out.Put("----------------------------------------");
   str = "A[1, 3], B[3, 3],  C[6, 6]";
   out.Put(str);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(str, "()");
   WriteStringArray(out, parts, "A[1, 3],", "B[3, 3],", "C[6, 6]", true);
   
   out.Put("===== SeparateSpaces()");
   parts = tp.SeparateSpaces(str);
   WriteStringArray(out, parts, "A[1", "3]", "B[3", true);
   
   out.Put("");
   out.Put("======================================== test SeparateDots()\n");
   
   out.Put("----------------------------------------");
   str = "Sat1.Earth.RMAG";
   out.Put(str);
   
   out.Put("===== SeparateDots()");
   parts = tp.SeparateDots(str);
   WriteStringArray(out, parts, "Sat1", "Earth", "RMAG", true);
   
   out.Put("----------------------------------------");
   str = "Sat1. Earth. RMAG";
   out.Put(str);
   
   out.Put("===== SeparateDots()");
   parts = tp.SeparateDots(str);
   WriteStringArray(out, parts, "Sat1", " Earth", " RMAG", true);
   
   out.Put("");
   out.Put("======================================== test DecomposeBlock()\n");
   
   out.Put("----------------------------------------");
   cmt1 = "% Build first spacecraft\n";
   cmdStr = "Create";
   typStr = "Spacecraft";
   inst = cmdStr + " " + typStr + " sat1    % my first spacecraft\n";
   str = cmt1 + inst;
   out.Put(str);
   
   out.Put("===== DecomposeBlock()");
   chunks = tp.DecomposeBlock(str);
   WriteStringArray(out, chunks, cmt1, inst, "", false);
   
   out.Put("----------------------------------------");
   inst = "Create " + typStr + " sat2    % my second pacecraft\n";
   cmt1 = "% Build second spacecraft\n";
   cmt2 = "% Second line comment\r";
   str = cmt1 + cmt2 + inst;
   out.Put(str);
   
   out.Put("===== DecomposeBlock()");
   chunks = tp.DecomposeBlock(str);
   WriteStringArray(out, chunks, cmt1, cmt2, inst, false);
   
   out.Put("----------------------------------------");
   cmt1 = "     % Build third spacecraft\n";
   cmt2 = "     % Second line comment\r";
   inst = "     Create " + typStr + " sat3    % my third spacecraft\n";
   str = cmt1 + cmt2 + inst;
   out.Put(str);
   
   out.Put("===== DecomposeBlock()");
   chunks = tp.DecomposeBlock(str);
   WriteStringArray(out, chunks, cmt1, cmt2, inst, false);
   
   
   out.Put("");
   out.Put("======================================== test Decompose()\n");
   
   out.Put("----------------------------------------");
   str = "Vec[3] Mat1[3,1], Mat2[6,1]";
   out.Put(str);
   
   out.Put("===== Decompose()");
   chunks = tp.Decompose(str, "()");
   WriteStringArray(out, chunks, "Vec[3]", "Mat1[3,1],", "Mat2[6,1]", false);
   
   out.Put("");
   out.Put("======================================== test EvaluateBlock()\n");
   
   out.Put("----------------------------------------");
   str =
      "% comment line 1\n"
      "% comment line 2\n";
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 0);
   WriteParts(out, tp, "");
   
   out.Put("----------------------------------------");
   inst = "Create Spacecraft sat1";
   str =
      "% Build first spacecraft\n" + inst + "    % my first spacecraft\n";
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 1);
   WriteParts(out, tp, inst);
   
   out.Put("----------------------------------------");
   str = "Save sat1 sat2 sat3";
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("----------------------------------------");
   inst = "Propagate prop(Sat1, Sat2, {Sat1.ElapsedDays = 10})";
   str =
      "%This is propagate command\n"
      "% Propagate Sat1 and Sat2\n" + inst + ";  % propagate for 10 days\n";
   out.Put(str);
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, inst);
   
   
   out.Put("----------------------------------------");
   inst = "a=b";
   str =
      "%This is assignment command\n" + GMAT + inst + "  % assign b to a\n";
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, inst);
   
   
   out.Put("======================================== test EvaluateBlock()\n");
   out.Put("======================================== test ChunkLine()\n");
   out.Put("============================== DEFINITION_BLOCK");
   
   out.Put("----------------------------------------");
   cmdStr = "Create";
   typStr = "Spacecraft";
   objStr = "Sat1 Sat2,Sat3";
   str = cmdStr + " " + typStr + " " + objStr;
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 1);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, typStr, objStr, true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[2], "()");
   WriteStringArray(out, parts, "Sat1", "Sat2", "Sat3", true);
   
   out.Put("----------------------------------------");
   typStr = "Spacecraft";
   objStr = "Sat1 Sat2,Sat3";
   str1 = cmdStr + "      " + typStr + "    " + objStr;
   str = "     " + str1 + "   ";
   out.Put(str);
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 1);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, typStr, objStr, true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[2], "()");
   WriteStringArray(out, parts, "Sat1", "Sat2", "Sat3", true);
   
   out.Put("----------------------------------------");
   typStr = "Array";
   objStr = "Mat1[3,3], Mat2[6,3]";
   str = cmdStr + " " + typStr + " " + objStr;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 1);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, typStr, objStr, true);
   
//    out.Put("===== Decompose('()')");
//    parts = tp.Decompose(chunks[2], "()");
//    WriteStringArray(out, parts, "Mat1[3,3],", "Mat2[6,3]", "", true);
   
//    out.Put("----------------------------------------");
//    typStr = "Array";
//    objStr = "Vec1[3],Mat1[3,3], Mat2[6,3]";
//    str = cmdStr + " " + typStr + " " + objStr;
//    out.Put(str);
//    chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 1);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, typStr, objStr, true);
   
//    out.Put("===== Decompose('()')");
//    parts = tp.Decompose(chunks[2], "()");
//    WriteStringArray(out, parts, "Vec1[3],", "Mat1[3,3],", "Mat2[6,3]", true);
   
   out.Put("======================================== COMMAND_BLOCK");
   out.Put("---------------------------------------- Report");
   cmdStr = "Report";
   typStr = "reportObject";
   objStr = " Mat3(2,1), Vec(1) Vec(2)";
   cmdExp = typStr + objStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, "Mat3(2,1)", "Vec(1)", true);
   
   out.Put("----------------------------------------");
   objStr = " Vec1(5),Mat1,Mat2(1,1) Mat3(2,1)";
   cmdExp = typStr + objStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, "Vec1(5)", "Mat1", true);
   
   out.Put("---------------------------------------- Save");
   cmdStr = "Save";
   cmdExp = "Sat1 Sat2,Sat3 Sat4";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, "Sat1", "Sat2", "Sat3", true);
   
   out.Put("---------------------------------------- EndIf");
   cmdStr = "EndIf";
   cmdExp = "";
   str = cmdStr;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks.clear();
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, "", "", true);
   
   out.Put("---------------------------------------- BeginFiniteBurn");
   cmdStr = "BeginFiniteBurn";
   typStr = "burn1";
   cmdExp = typStr + "(Sat1 Sat2)";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", false);
   WriteStringArray(out, parts, typStr, "(Sat1 Sat2)", "", true);
   
   out.Put("===== SeparateBrackets('()')");
   parts = tp.SeparateBrackets(parts[1], "()", " ,");
   WriteStringArray(out, parts, "Sat1", "Sat2", "", true);
   
   out.Put("---------------------------------------- For");
   cmdStr = "For";
   typStr = "step";
   cmdExp = typStr + " = 1 : 10 : 2";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   out.Validate(blockType, 2);
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, "=", "1", true);
   
   out.Put("----------------------------------------");
   cmdStr = "For";
   typStr = "step";
   cmdExp = typStr + "=1:10:2";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   out.Validate(blockType, 2);
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, "step=1:10:2", "", "", true);
   
   out.Put("---------------------------------------- While");
   cmdStr = "While";
   cmdExp = "var1 < Sat1.X";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, "var1", "<", "Sat1.X", true);
   
   out.Put("----------------------------------------");
   cmdExp = "var1 == Sat1.X";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, "var1", "==", "Sat1.X", true);
   
   out.Put("---------------------------------------- If");
   cmdStr = "If";
   cmdExp = "var1 ~= var2";
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, "var1", "~=", "var2", true);
   
   out.Put("---------------------------------------- Propagate");
   cmdStr = "Propagate";
   typStr = "prop";
   braceStr = "{Sat1.ElapsedDays=10}";
   parenStr = "(Sat1,Sat2," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== SeparateBrackets('()')");
   parts = tp.SeparateBrackets(parts[1], "()", ",");
   WriteStringArray(out, parts, "Sat1", "Sat2", braceStr, true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", "=");
   WriteStringArray(out, parts, "Sat1.ElapsedDays", "10", "", true);
   
   out.Put("---------------------------------------- Propagate");
   cmdStr = "Propagate";
   typStr = "-prop";
   braceStr = "{Sat1.Periapsis}";
   parenStr = "(Sat1," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== SeparateBrackets('()')");
   parts = tp.SeparateBrackets(parts[1], "()", ",");
   WriteStringArray(out, parts, "Sat1", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", " ");
   WriteStringArray(out, parts, "Sat1.Periapsis", "", "", true);
   
   out.Put("---------------------------------------- Achieve");
   cmdStr = "Achieve";
   typStr = "myDC";
   braceStr = "{Tolerance=0.1}";
   parenStr = "(Sat1.SMA=21545.0," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);

   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== SeparateBrackets('()')");
   parts = tp.SeparateBrackets(parts[1], "()", ",");
   WriteStringArray(out, parts, "Sat1.SMA=21545.0", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", "=");
   WriteStringArray(out, parts, "Tolerance", "0.1", "", true);
   
   out.Put("---------------------------------------- Achieve");
   cmdStr = "Achieve";
   typStr = "myDC";
   braceStr = "{Tolerance=0.1}";
   parenStr = "(Sat1.SMA=Mat1(1,1)," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);

   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()");
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== SeparateBrackets('()')");
   parts = tp.SeparateBrackets(parts[1], "()", ",");
   WriteStringArray(out, parts, "Sat1.SMA=Mat1(1,1)", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", "=");
   WriteStringArray(out, parts, "Tolerance", "0.1", "", true);
   
   out.Put("---------------------------------------- Vary");
   cmdStr = "Vary";
   typStr = "DC";
   braceStr = "{Pert=1,MaxStep=1000,Lower=6000,Upper=100000}";
   parenStr = "(DefaultSC.SMA=Vec(3)," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", true, true);
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(parts[1], "()", true, true);
   WriteStringArray(out, parts, "DefaultSC.SMA=Vec(3)", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", ",");
   WriteStringArray(out, parts, "Pert=1", "MaxStep=1000", "Lower=6000", true);
   
   
   out.Put("---------------------------------------- Vary");
   cmdStr = "Vary";
   typStr = "DC";
   braceStr = "{Pert=1,MaxStep=1000,Lower=6000,Upper=100000}";
   parenStr = "(DefaultSC.SMA=6500," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", true, true);
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(parts[1], "()", true, true);
   WriteStringArray(out, parts, "DefaultSC.SMA=6500", braceStr, "", true); //<<<<<
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", ",");
   WriteStringArray(out, parts, "Pert=1", "MaxStep=1000", "Lower=6000", true);
   
   out.Put("---------------------------------------- Vary");
   cmdStr = "Vary";
   typStr = "DC";
   braceStr = "{Pert=Pert(1,1),MaxStep=MaxStep(1,1),Lower=Lower(1,1),Upper=Upper(1,1)}";
   parenStr = "(DefaultSC.SMA=GOAL(1,1)," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
      
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", true, true);
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(parts[1], "()", true, true);
   WriteStringArray(out, parts, "DefaultSC.SMA=GOAL(1,1)", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", ",");
   WriteStringArray(out, parts, "Pert=Pert(1,1)", "MaxStep=MaxStep(1,1)", "Lower=Lower(1,1)", true);
   
   out.Put("---------------------------------------- Vary");
   cmdStr = "Vary";
   typStr = "DC";
   braceStr = "{Pert=Pert(1,1),MaxStep=MaxStep(1,1),Lower=Lower(1,1),Upper=Upper(1,1)}";
   parenStr = "(array(1,1)=GOAL(1,1)," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
      
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", true, true);
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(parts[1], "()", true, true);
   WriteStringArray(out, parts, "array(1,1)=GOAL(1,1)", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", ",");
   WriteStringArray(out, parts, "Pert=Pert(1,1)", "MaxStep=MaxStep(1,1)", "Lower=Lower(1,1)", true);


   out.Put("---------------------------------------- Vary");
   cmdStr = "Vary";
   typStr = "DC";
   braceStr = "{Perturbation=array1(array2(1,1),array2(1,1)),MaxStep=var2,"
      "Lower=0,Upper=array1(array2(2,2),array2(2,2))}";
   parenStr = "(TOI.Element1=var1," + braceStr + ")";
   cmdExp = typStr + parenStr;
   str = cmdStr + " " + cmdExp;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 2);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, cmdStr, cmdExp, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(chunks[1], "()", true, true);
   WriteStringArray(out, parts, typStr, parenStr, "", true);
   
   out.Put("===== Decompose('()')");
   parts = tp.Decompose(parts[1], "()", true, true);
   WriteStringArray(out, parts, "TOI.Element1=var1", braceStr, "", true);
   
   count = parts.size();
   out.Put("===== SeparateBrackets('{}')");
   parts = tp.SeparateBrackets(parts[count-1], "{}", ",");
   WriteStringArray(out, parts, "Perturbation=array1(array2(1,1),array2(1,1))",
                    "MaxStep=var2", "Lower=0", true);
   
   out.Put("----------------------------------------");
   str = "State1(1,1) State2(1,1) State(2,2)";
   out.Put("===== SeparateBrackets('{}')");
   out.Put(str);
   parts = tp.SeparateBrackets(str, "{}", " ,", false);
   WriteStringArray(out, parts, "State1(1,1)", "State2(1,1)", "State(2,2)", true);
      
   out.Put("======================================== ASSIGNMENT_BLOCK");
   out.Put("----------------------------------------");
   lhs = "Sat1.X";
   rhs = "7000";
   str1 = lhs + "=" + rhs;
   str = GMAT + str1 + ";";
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   out.Put("----------------------------------------");
   lhs = "Sat1.VZ";
   rhs = "Mat1(1,3)";
   str = lhs + " = " + rhs;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   out.Put("----------------------------------------");
   lhs = "Sat1.Z";
   rhs = "vec(2)";
   str = lhs + " = " + rhs;
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, str);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   out.Put("----------------------------------------");
   lhs = "Mat1(1,1)";
   rhs = "Sqrt(a+b+c+mat1(1,1)^2)";
   str1 = lhs + " = " + rhs;
   str = GMAT + str1 + ";";
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);

   out.Put("----------------------------------------");
   lhs = "";
   rhs = "StoreData(State1)";
   str1 = rhs;
   str = GMAT + str1 + ";";
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   // block type should be COMMAND_BLOCK because Function call without output
   out.Validate(blockType, 2);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   out.Put("----------------------------------------");
   lhs = "var2";
   rhs = "MyFunction(a, b, c)";
   str1 = lhs + " = " + rhs;
   str = GMAT + str1 + ";";
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   out.Validate(blockType, 3);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   out.Put("----------------------------------------");
   lhs = "[S1,S2,S3,S1dot,S2dot,S3dot]";
   rhs = "GetLISAData(x,y,z,v(1),vv(1,1),vz)";
   str1 = lhs + "=" + rhs;
   str = GMAT + str1 + ";";
   out.Put(str);
   chunks.clear();
   
   out.Put("===== EvaluateBlock()");
   blockType = tp.EvaluateBlock(str);
   // block type should be COMMAND_BLOCK because LHS has []
   out.Validate(blockType, 2);
   WriteParts(out, tp, str1);
   
   out.Put("===== ChunkLine()");
   chunks = tp.ChunkLine();
   WriteStringArray(out, chunks, lhs, rhs, "", true);
   
   //-----------------------------------------------------------------
   // Error condition
   //-----------------------------------------------------------------
   out.Put("----------------------------------------");
   cmdStr = "Create";
   typStr = "Spacecraft";
   str = cmdStr + " " + typStr;
   out.Put(str);
   chunks.clear();

   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 1);
      WriteParts(out, tp, str);
      
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   typStr = "Propagator";
   str = cmdStr + " " + typStr;
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 1);
      WriteParts(out, tp, str);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   cmdStr = "Propagate";
   typStr = "";
   str = cmdStr + " " + typStr;
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 2);
      WriteParts(out, tp, str);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   cmdStr = "While";
   typStr = "";
   str = cmdStr + " " + typStr;
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 2);
      WriteParts(out, tp, str);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   str = "   A1 = ";
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 3);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   str = "= Sat.X   ";
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 3);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   str = "   = Sat.X";
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 3);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   out.Put("----------------------------------------");
   str = "   = ";
   out.Put(str);
   chunks.clear();
   
   try
   {
      out.Put("===== EvaluateBlock()");
      blockType = tp.EvaluateBlock(str);
      out.Validate(blockType, 3);
   
      out.Put("===== ChunkLine()");
      chunks = tp.ChunkLine();
      WriteStringArray(out, chunks, "Error", "Error", "", true);
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   
   return 0;
}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   std::string outPath = "../../TestTextParser/";
   std::string outFile = outPath + "TestTextParserOut.txt";   
   TestOutput out(outFile);
   
//    char *buffer;
//    buffer = getenv("OS");
//    if (buffer  != NULL)
//       printf("Current OS is %s\n", buffer);
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of TextParser!!");
   }
   catch (BaseException &e)
   {
      out.Put(e.GetFullMessage());
   }
   catch (...)
   {
      out.Put("Unknown error occurred\n");
   }
   
   cout << endl;
   cout << "Hit enter to end" << endl;
   cin.get();
}
