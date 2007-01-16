//$Header$
//-------------------------------------------------------------------------------
//                              TextParser
//-------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2006/08/10
//
/**
 * Implements the methods to parse text into parts.
 */
//-------------------------------------------------------------------------------

#include "TextParser.hpp"
#include "StringTokenizer.hpp"
#include "StringUtil.hpp"
#include "InterpreterException.hpp"
#include "Parameter.hpp"
#include "FileManager.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_TP 1
//#define DEBUG_TP_EVAL_BLOCK 2
//#define DEBUG_TP_CHUNK_LINE 1
//#define DEBUG_TP_DECOMPOSE 1
//#define DEBUG_TP_SEP_BRACKETS 1
//#define DEBUG_TP_DECOMPOSE_BLOCK 1

using namespace GmatStringUtil;

//----------------------------------
// public
//----------------------------------

//-------------------------------------------------------------------------------
// TextParser()
//-------------------------------------------------------------------------------
TextParser::TextParser()
{
   whiteSpace = " \t";
}


//-------------------------------------------------------------------------------
// ~TextParser()
//-------------------------------------------------------------------------------
TextParser::~TextParser()
{
}


//-------------------------------------------------------------------------------
// void Initialize(const StringArray &commandList)
//-------------------------------------------------------------------------------
void TextParser::Initialize(const StringArray &commandList)
{
   theCommandList = commandList;
   Reset();
}


//-------------------------------------------------------------------------------
// void Reset()
//-------------------------------------------------------------------------------
/**
 * Resets internal comments and instruction.
 */
//-------------------------------------------------------------------------------
void TextParser::Reset()
{
   prefaceComment = "";
   inlineComment  = "";
   theInstruction = "";
}


//-------------------------------------------------------------------------------
// StringArray DecomposeBlock(const std::string &logicalBlock)
//-------------------------------------------------------------------------------
/*
 * Decomposes logical block into string array of lines.
 * This method expects to end-of-line character for each line since it looks
 * for \n or \r.
 */
//-------------------------------------------------------------------------------
StringArray TextParser::DecomposeBlock(const std::string &logicalBlock)
{
   Integer length = logicalBlock.size();
   
   #if DEBUG_TP_DECOMPOSE_BLOCK
   MessageInterface::ShowMessage
      ("TextParser::DecomposeBlock() length=%d\n", length);
   #endif
   
   std::string str = logicalBlock;
   std::string block;
   Integer lastPos = 0;
   Integer lineCounter = 0;
   StringArray lines;
   
   // put logicalBlock into StringArray, ending LF or CR
   for (int i=0; i<length; i++)
   {
      #if DEBUG_TP_DECOMPOSE_BLOCK > 1
      MessageInterface::ShowMessage
         ("   ===> TextParser::DecomposeBlock() str[%d]=%c, %d\n", i, str[i], str[i]);
      #endif
      
      if (str[i] == '\n' || str[i] == '\r')
      {
         // Remove end-of-line character
         block = str.substr(lastPos, i-lastPos+1);
         lines.push_back(block);
         
         #if DEBUG_TP_DECOMPOSE_BLOCK > 1
         MessageInterface::ShowMessage
            ("   ===> TextParser::DecomposeBlock() i=%d, lastPos=%d, block=%s\n",
             i, lastPos, block.c_str());
         #endif
         
         lastPos = i+1;
         lineCounter++;
      }
   }
   
   
   if (lineCounter == 0)
      lines.push_back(str);
   else if (lastPos < length)
      lines.push_back(str.substr(lastPos));
   
   return lines;
}


//-------------------------------------------------------------------------------
// Gmat::BlockType EvaluateBlock(const std::string &logicalBlock)
//-------------------------------------------------------------------------------
/*
 * Breaks the logical block into three pieces: preface comments, instruction,
 * inline comments. These pieces are stored internally.
 *
 * @note It removes the keyword GMAT and ending semicolon ';'.
 *       It removes leading and and trailing spaces.
 *
 * @param <logicalBlock> logical block to be evaluated
 *
 * @return BlockType of the logical block.
 *
 * The instruction of object definition block is in the following format:
 *       <"Create"  ObjectType  Name1 [Name2 ...]>
 *
 * The instruction of command definition block is in the following format:
 *       <Command CommandExpression>
 *
 * The instruction of assignment block is in the following format:
 *       <Left = Right>
 *       <"" = Right>         <-- for call function returning no output
 *
 */
//-------------------------------------------------------------------------------
Gmat::BlockType TextParser::EvaluateBlock(const std::string &logicalBlock)
{
   Integer length = logicalBlock.size();
   
   #if DEBUG_TP_EVAL_BLOCK
   MessageInterface::ShowMessage
      ("TextParser::EvaluateBlock() length=%d\n", length);
   #endif
   
   #if DEBUG_TP_EVAL_BLOCK > 1
   MessageInterface::ShowMessage
      ("TextParser::EvaluateBlock() logicalBlock=\n   %s\n", logicalBlock.c_str());
   #endif
   
   // first break into string array
   StringArray lines = DecomposeBlock(logicalBlock);
   
   Integer count = lines.size();
   
   #if DEBUG_TP_EVAL_BLOCK
   MessageInterface::ShowMessage
      ("TextParser::EvaluateBlock() count=%d\n", count);
   #endif
   
   Integer commentCounter = 0;
   Integer noCommentLine = -1;
   std::string str, keyword, substr;
   UnsignedInt index1, index2, index3, index4;
   
   Reset();
   
   #if DEBUG_TP_EVAL_BLOCK > 1
   for (int i=0; i<count; i++)
      MessageInterface::ShowMessage("   lines[%d]=<%s>\n", i, lines[i].c_str());
   #endif
   
   // first find block type
   for (int i=0; i<count; i++)
   {
      str = lines[i];
      length = str.size();
      
      // Remove leading and trailing blank spaces and ending semi-colon
      str = GmatStringUtil::Trim(str, GmatStringUtil::BOTH, true);
      
      length = str.size();
      
      // Remove GMAT keyword
      index1 = str.find("GMAT");
      if (index1 == 0)
      {
         index2 = str.find_first_of(whiteSpace, index1);
         index3 = str.find_first_not_of(whiteSpace, index2);
         str = str.substr(index3, length-index3+1);
      }
      
      index1 = str.find_first_not_of(whiteSpace);
      if (index1 != str.npos)
      {
         if (str[index1] == '%' || str[index1] == '\n' || str[index1] == '\r')
         {
            prefaceComment = prefaceComment + str;
            commentCounter++;
            continue;
         }
         
         // find keyword, and remove semicolon
         index2 = str.find_first_of(whiteSpace, index1);
         if (str[index2-1] == ';')
            keyword = str.substr(index1, index2-index1-1);
         else
            keyword = str.substr(index1, index2-index1);
         
         // check for "Create" or Commands
         if (keyword == "Create")
         {
            theBlockType = Gmat::DEFINITION_BLOCK;
            noCommentLine = i;
         }
         else if (IsCommand(keyword))
         {
            theBlockType = Gmat::COMMAND_BLOCK;
            noCommentLine = i;
         }
         else
         {
            theBlockType = Gmat::ASSIGNMENT_BLOCK;
            noCommentLine = i;

            // check for CallFunction
            // ex) [a b c] = function(d, e, f);
            
            //if (str.find("[") != str.npos) // Is this checking enough?
            
            // check for RVECTOR_TYPE or UNSIGNED_INTARRAY_TYPE setting
            // ex) opengl.OrbitColor = [100 200 300 ];
            //     opengl.ViewPointVectorVector = [0, 0, 50000];
            
            UnsignedInt index1 = str.find("[");
            if (index1 != str.npos)
            {
               UnsignedInt index2 = str.find("=");
               if (index2 == str.npos || index2 > index1)
                  theBlockType = Gmat::COMMAND_BLOCK;
            }
            
            /// @TODO: This is a work around for a call function with
            /// without any return parameters.  It should be updated in
            /// the design to handle this situation.
            if (str.find("=") == str.npos) // Is this checking enough?
            {
               theBlockType = Gmat::COMMAND_BLOCK;
            }
            
         }
         
         if (noCommentLine >= 0)
         {
            // if % found in the no-comment line, it is inline comment
            index3 = str.find("%", index2);
            
            #if DEBUG_TP_EVAL_BLOCK
            MessageInterface::ShowMessage
            ("   index1=%d, index2=%u, index3=%u\n", index1, index2, index3);
            #endif
            
            // if inline comment
            if (index3 != str.npos)
            {
               // find last non-blank
               index4 = str.find_last_not_of(whiteSpace, index3-1);
               inlineComment = str.substr(index4+1);
               theInstruction = str.substr(index1, index4-index1+1);
            }
            else
            {
               theInstruction = str.substr(index1);
            }
         }
      }
   }

   //MessageInterface::ShowMessage("===> theInstruction=%s\n", theInstruction.c_str());
   
   if (commentCounter == count)
      theBlockType = Gmat::COMMENT_BLOCK;
   
   // remove ending ; from the instruction
   length = theInstruction.size();   
   if (theInstruction[length-1] == ';')
      theInstruction = theInstruction.substr(0, length-1);
   
   theChunks.clear();
   theChunks.push_back(prefaceComment);
   theChunks.push_back(inlineComment);
   theChunks.push_back(theInstruction);
   
    #if DEBUG_TP_EVAL_BLOCK
   MessageInterface::ShowMessage
      ("   keyword=<%s>, blockType=%d\n", keyword.c_str(), theBlockType);
   MessageInterface::ShowMessage
      ("   prefaceComment=<%s>\n   inlineComment=<%s>\n   theInstruction=<%s>\n",
       prefaceComment.c_str(), inlineComment.c_str(), theInstruction.c_str());
   #endif
   
   return theBlockType;
}


//-------------------------------------------------------------------------------
// StringArray ChunkLine()
//-------------------------------------------------------------------------------
/*
 * Breaks the internal instruction string into logical groups.  The instruction
 * line is broken at white space and comma characters. Blocks marked with the
 * grouping delimiters (), {}, and [] are kept together.
 *
 * @note It removes the keyword GMAT and ending semicolon ';'.
 *       It removes leading and and trailing spaces.
 *
 * @return array of constituent parts.
 *    The object definition instructions returns in the following format:
 *       <"Create"> <ObjectType> <Name1> [<Name2> ...]
 
 *    The command definition instructions returns in the following format:
 *       <Command> <CommandExpression>   <-- command, EndFiniteBurn
 *       <Command>                       <-- End* command
 *
 *    The assignment definition instructions returns in the following format:
 *       <Left> <Right>
 *
 *    The function definition instructions returns in the following format:
 *       <"CallFunction"> <out> <FunctionName> <in>
 *
 * Examples of object definition line are:
 *   Create Spacecraft Sat1 Sat2, Sat3
 *      <Create> <Spacecraft> <Sat1 Sat2, Sat3>
 *   Create Array Mat1[3,3], Mat2[6,3];
 *      <Create> <Array> <Mat1[3,3], Mat2[6,3]>
 *
 * Examples of command definition line are:
 *   Save Sat1 Sat2,Sat3
 *      <Save> <Sat1 Sat2,Sat3>
 *   Report reportObject Mat1, Mat2,Mat2(1,1);
 *      <Report> <reportObject Mat1, Mat2,Mat2(1,1)>
 *   Report reportObject Mat2(1,1), Mat2(1,2) Mat2(1,3);
 *      <Report> <reportObject Mat2(1,1), Mat2(1,2) Mat2(1,3)>
 *   BeginFiniteBurn burn1(Sat1);
 *      <BeginFiniteBurn> <burn1(Sat1)>
 *   Propagate prop(Sat1, Sat2, {Sat1.ElapsedDays = 10})
 *      <Propagate> <prop(Sat1, Sat2, {Sat1.ElapsedDays = 10})>
 *   Propagate -prop(Sat1, Sat2, {Sat1.Periapsis})
 *      <Propagate> <-prop(Sat1, Sat2, {Sat1.Periapsis})>
 *   Achieve myDC(Sat1.SMA = 21545.0, {Tolerance = 0.1});
 *      <Achieve> <myDC(Sat1.SMA = 21545.0, {Tolerance = 0.1})>
 *   Achieve myDC(Sat1.SMA = Arr1(I,J), {Tolerance = 0.1});
 *      <Achieve> <myDC(Sat1.SMA = Arr1(I,J), {Tolerance = 0.1})>
 *   Vary DC(DefaultSC.SMA = 6500, {Pert = 1, MaxStep = 1000, Lower = 6000, Upper = 100000});
 *      <Vary> <DC(DefaultSC.SMA = 6500, {Pert = 1, MaxStep = 1000, Lower = 6000, Upper = 100000})>
 *   For step = 1 : 10 : 2
 *      <For> <step = 1 : 10 : 2>
 *   While var1 < Sat1.X
 *      <While> <var1 < Sat1.X>
 *   If var1 ~= var2
 *      <If> <var1 ~= var2>
 *   EndIf
 *      <EndIf>
 * Examples of assignment definition line are:
 *   Sat1.X = 7000;
 *      <Sat1.X> <7000>
 *   Sat1.Y = Sat2.Y;
 *      <Sat1.Y> = <Sat2.Y>
 *   Sat1.Z = Mat1(1,3);
 *      <Sat1.Z> = <Mat1(1,3)>
 *   Sat1.VX = var1;
 *      <Sat1.VX> <var1>
 *   Sat1 = Sat2;
 *      <Sat1> <Sat2>
 *   Mat1(1,1) = Sqrt(a + b + c + mat1(1,1)^2)
 *      <Mat1(1,1)> <Sqrt(a + b + c + mat1(1,1)^2)>
 *   var2 = MyFunction();
 *      <var2> <MyFunction()>
 *   StoreData( FormState );
 *      <> <StoreData( FormState )>
 *   GMAT [S1,S2,S3,S1dot,S2dot,S3dot] = GetLISAData(x, y, z, v(1), vv(1,1), vz );
 *      <[S1,S2,S3,S1dot,S2dot,S3dot]> <GetLISAData(x, y, z, v(1), vv(1,1), vz )>
 *   
 */
//-------------------------------------------------------------------------------
StringArray TextParser::ChunkLine()
{
   #if DEBUG_TP_CHUNK_LINE
   MessageInterface::ShowMessage
      ("TextParser::ChunkLine() theInstruction= %s\n", theInstruction.c_str());
   #endif
   
   std::string str = theInstruction;
   std::string space;
   UnsignedInt index1, index2, index3 = 123456;
   
   Integer length = str.size();
   StringArray chunks;
   
   //------------------------------------------------------------
   // object definition block
   //------------------------------------------------------------
   if (theBlockType == Gmat::DEFINITION_BLOCK)
   {
      // find keyword Create
      index1 = str.find("Create");
      if (index1 == str.npos)
      {
         sprintf(errorMsg, "TextParser::ChunkLine() keyword \"Create\" not "
                 "found in the definition block\n   \"%s\"\n", str.c_str());
         throw InterpreterException(errorMsg);
      }
      else
      {
         // find object type
         index2 = str.find_first_of(whiteSpace, index1);
         chunks.push_back(str.substr(index1, index2-index1));
         
         index1 = str.find_first_not_of(whiteSpace, index2);
         
         if (index1 == str.npos)
         {
            sprintf(errorMsg, "TextParser::ChunkLine() object type not "
                    "found in the definition block\n   \"%s\"\n", str.c_str());
            throw InterpreterException(errorMsg);
         }
         else
         {
            index2 = str.find_first_of(whiteSpace, index1);
            chunks.push_back(str.substr(index1, index2-index1));
            
            index1 = str.find_first_not_of(whiteSpace, index2);
            
            if (index1 == str.npos)
            {
               sprintf(errorMsg, "TextParser::ChunkLine() object name not "
                       "found in the definition block\n   \"%s\"\n", str.c_str());
               throw InterpreterException(errorMsg);
            }
            else
            {
               chunks.push_back(str.substr(index1, length-index1+1));
            }
         }
      }
   }
   //------------------------------------------------------------
   // command block
   //------------------------------------------------------------
   else if (theBlockType == Gmat::COMMAND_BLOCK)
   {
      index1 = str.find_first_not_of(whiteSpace);
      if (index1 == str.npos)
      {
         sprintf(errorMsg, "TextParser::ChunkLine() command name not found "
                 "in the command block\n   \"%s\"\n", str.c_str());
         throw InterpreterException(errorMsg);
      }
      else
      {
         index2 = str.find_first_of(whiteSpace, index1);
         
         if (index2 == str.npos)
         {
            chunks.push_back(str);
            
//             sprintf(errorMsg, "TextParser::ChunkLine() command description "
//                     "not found in the command block\n   \"%s\"\n", str.c_str());
//             throw InterpreterException(errorMsg);
         }
         else
         {
            chunks.push_back(str.substr(index1, index2-index1));
            
            index1 = str.find_first_not_of(whiteSpace, index2);
            if (index1 == str.npos)
            {
               // push back empty string
               //chunks.push_back("");
               
//                sprintf(errorMsg, "TextParser::ChunkLine() command description "
//                        "not found in the command block\n   \"%s\"\n", str.c_str());
//                throw InterpreterException(errorMsg);
            }
            else
            {
               chunks.push_back(str.substr(index1, length-index1));
            }
         }
      }
   }
   //------------------------------------------------------------
   // assignment block
   //------------------------------------------------------------
   else if (theBlockType == Gmat::ASSIGNMENT_BLOCK)
   {
      // find equal sign
      index1 = str.find_first_not_of(whiteSpace);
      
      if (index1 == str.npos)
      {
         sprintf(errorMsg, "TextParser::ChunkLine() no assignment expression "
                 "found in the assignment block\n   \"%s\"\n", str.c_str());
         throw InterpreterException(errorMsg);
      }
      else
      {
         index2 = str.find_first_of("=", index1);
         
         if (index2 == str.npos)
         {
            // set "" to lhs
            chunks.push_back("");
            chunks.push_back(str);
         }
         else
         {
            if (index2 != 0)
               index3 = str.find_last_not_of(whiteSpace, index2-1);
            
            //MessageInterface::ShowMessage("index3=%u, index2=%u\n", index3, index2);
            
            if (index3 == str.npos || index2 == 0)
            {
               sprintf(errorMsg, "TextParser::ChunkLine() LHS of = not found in "
                       "the assignment block\n   \"%s\"\n", str.c_str());
               throw InterpreterException(errorMsg);
            }
            else
            {
               // found lhs
               chunks.push_back(str.substr(index1, index3-index1+1));               
               index1 = str.find_first_not_of(whiteSpace, index2+1);
               
               if (index1 == str.npos)
               {
                  sprintf(errorMsg, "TextParser::ChunkLine() RHS of = not found in "
                          "the assignment block\n   \"%s\"\n", str.c_str());
                  throw InterpreterException(errorMsg);
               }
               else
               {
                  chunks.push_back(str.substr(index1, length-index1));
               }
            }
         }
      }
   }
   
   #if DEBUG_TP_CHUNK_LINE
   MessageInterface::ShowMessage("   Returning:\n");
   for (unsigned int i=0; i<chunks.size(); i++)
      MessageInterface::ShowMessage("   chunks[%d]=%s\n", i, chunks[i].c_str());
   #endif
   
   return chunks;
}


//-------------------------------------------------------------------------------
// StringArray Decompose(const std::string &chunk, const std::string &bracketPair
//                       bool checkForArray = true, bool removeOuterBracket = false)
//-------------------------------------------------------------------------------
/*
 * Breaks chunk into parts separated by space or comma but keeps bracket together.
 *
 * BeginFiniteBurn burn1(sat1 sat2) will return
 *    <burn1> <sat1 sat2>
 *
 * @param <chunk>  Input chunk to be break apart
 * @param <bracketPair>  Input bracket pair (open and close) to keep together
 * @param <checkForArray>  Check for array if it is true (ie. a(i,j), v(1))
 * @param <removeOuterBracket>  Removes outer bracket if it is true
 */
//-------------------------------------------------------------------------------
StringArray TextParser::Decompose(const std::string &chunk,
                                  const std::string &bracketPair,
                                  bool checkForArray,
                                  bool removeOuterBracket)
{
   #if DEBUG_TP_DECOMPOSE
   MessageInterface::ShowMessage
      ("TextParser::Decompose() chunk=%s, bracketPair=%s, removeOuterBracket=%d\n",
       chunk.c_str(), bracketPair.c_str(), removeOuterBracket);
   #endif

   std::string str1 = chunk;
   
   // First remove blank spaces inside array bracket
   if (chunk[0] != bracketPair[0])
      str1 = RemoveSpaceInBrackets(chunk, bracketPair);
   
   int length = str1.size();
   UnsignedInt index1, index2;
   Integer open, close;
   bool isOuterBracket = false;
   
   // Remove outer bracket if flag is set to true
   index1 = str1.find_first_of(bracketPair[0]);
   if (index1 == 0)
   {
      GmatStringUtil::FindMatchingBracket(str1, open, close, isOuterBracket,
                                          bracketPair, 0);
      
      if (removeOuterBracket && isOuterBracket)
      {
         str1 = str1.substr(1, length-2);
         //MessageInterface::ShowMessage("   str1=%s\n", str1.c_str());
      }
   }
   
   
   StringTokenizer st(str1, " ,");
   StringArray tempParts = st.GetAllTokens();
   StringArray parts;
   int count = tempParts.size();
   std::string openBrackets = "([{";
   std::string closeBrackets = ")]}";
   
   #if DEBUG_TP_DECOMPOSE > 1
   for (int i=0; i<count; i++)
      MessageInterface::ShowMessage
         ("   tempParts[%d]=%s\n", i, tempParts[i].c_str());
   #endif
   
   index1 = str1.find_first_of(openBrackets);
   
   bool isArray = false;
   bool refObjFound = false;
   
   // if open bracket found,
   if (index1 != str1.npos)
   {
      GmatStringUtil::FindMatchingBracket(str1, open, close, isOuterBracket,
                                          bracketPair, index1);
      
      #if DEBUG_TP_DECOMPOSE > 1
      MessageInterface::ShowMessage
         ("   open=%d, close=%d, isOuterBracket=%d\n", open, close, isOuterBracket);
      #endif
      
      if (checkForArray)
      {
         if (open == -1 && close == -1)
            isArray = IsBracketPartOfArray(str1.substr(index1), "([)]", true);
         else
            isArray = IsBracketPartOfArray(str1.substr(open, close-open+1),
                                           "([)]", true);
      }
      
      #if DEBUG_TP_DECOMPOSE > 1
      MessageInterface::ShowMessage("isArray=%d\n", isArray);
      #endif
      
      if (!isArray)
         refObjFound = true;
   }
   
   #if DEBUG_TP_DECOMPOSE > 1
   MessageInterface::ShowMessage("   refObjFound=%d\n", refObjFound);
   #endif
   
   if (refObjFound)
   {
      //MessageInterface::ShowMessage("===> index1=%u\n", index1);
      if (index1 != 0)
         parts.push_back(str1.substr(0, index1));
      parts.push_back(str1.substr(index1, length-index1));
   }
   else
   {
      // find any opening brackets
      for (int i=0; i<count-1; i++)
      {
         index1 = tempParts[i].find_first_of(openBrackets);
         
         if (index1 != str1.npos)
         {
            index2 = tempParts[i].find_first_of(closeBrackets, index1);

            // if closing bracket not found, append next string
            if (index2 == str1.npos)
            {
               tempParts[i] = tempParts[i] + "," + tempParts[i+1];
               tempParts[i+1] = "***";
            }
         }
      }
      
      // remove extra string "***"
      StringArray::iterator iter = tempParts.begin();
      while (iter != tempParts.end())
      {
         if (*iter != "***")
            parts.push_back(*iter);
         
         ++iter;
      }
   }
   
   #if DEBUG_TP_DECOMPOSE
   MessageInterface::ShowMessage("   Returning:\n");
   for (unsigned int i=0; i<parts.size(); i++)
      MessageInterface::ShowMessage
         ("   parts[%d] = %s\n", i, parts[i].c_str());
   #endif
   
   return parts;
}


//-------------------------------------------------------------------------------
// StringArray SeparateBrackets(const std::string &chunk,
//                              const std::string &bracketPair,
//                              const std::string &delim)
//-------------------------------------------------------------------------------
/*
 * Breaks chunk into parts separated by space or comma but keeps bracket together,
 * except outer most brackets.
 *
 * @param <chunk> input chunk to be break apart
 * @param <bracketPair> input bracket pair (open and close) to keep together
 *                      (), [], {}
 */
//-------------------------------------------------------------------------------
StringArray TextParser::SeparateBrackets(const std::string &chunk,
                                         const std::string &bracketPair,
                                         const std::string &delim,
                                         bool checkOuterBracket)
{
   #if DEBUG_TP_SEP_BRACKETS
   MessageInterface::ShowMessage
      ("TextParser::SeparateBrackets() chunk=%s, bracketPair=%s, delim=%s\n",
       chunk.c_str(), bracketPair.c_str(), delim.c_str());
   #endif

   std::string str1 = chunk;
   
   // First remove blank spaces inside array bracket
   if (chunk[0] != bracketPair[0])
      str1 = RemoveSpaceInBrackets(chunk, bracketPair);
   
   UnsignedInt firstOpen, lastClose;
   firstOpen = str1.find_first_not_of(whiteSpace);
   lastClose = str1.find_last_not_of(whiteSpace);
   bool bracketFound = true;
   
   if (str1[firstOpen] != bracketPair[0] || str1[lastClose] != bracketPair[1])
   {
      bracketFound = false;
      if (checkOuterBracket)
      {
         sprintf(errorMsg, "TextParser::SeparateBrackets() text is not enclosed "
                 "with brackets:%s in\n   \"%s\"\n", bracketPair.c_str(),
                 str1.c_str());
         throw InterpreterException(errorMsg);
      }
   }
   
   std::string str;
   
   if (bracketFound)
      str = str1.substr(firstOpen+1, lastClose-firstOpen-1);
   else
      str = str1.substr(firstOpen, lastClose-firstOpen+1);
   
   #if DEBUG_TP_SEP_BRACKETS
   MessageInterface::ShowMessage("   str=%s\n", str.c_str());
   #endif
   
   UnsignedInt index1, index2;
   StringTokenizer st(str, delim);
   StringArray tempParts = st.GetAllTokens();
   StringArray parts;
   int count = tempParts.size();
   
   #if DEBUG_TP_SEP_BRACKETS
   for (int i=0; i<count; i++)
      MessageInterface::ShowMessage
         ("   tempParts[%d]=%s\n", i, tempParts[i].c_str());
   #endif
   
   int openBracketCounter = 0;
   UnsignedInt openIndex = 0;
   char openChar = '(';
   char closeChar = ')';

   // We want array to have no space between indexing
   std::string newDelim = delim;
   if (delim != " ")
      newDelim = GmatStringUtil::RemoveAll(delim, ' ');
   
   // find any opening brackets
   for (int i=0; i<count; i++)
   {
      index1 = tempParts[i].find_first_of("[({");
      index2 = tempParts[i].find_last_of("])}");

      //MessageInterface::ShowMessage
      //   ("   tempParts[%d]=%s, index1=%u, index2=%u\n", i, tempParts[i].c_str(),
      //    index1, index2);
      
      if (index1 != str.npos)
      {
         openBracketCounter++;
         openIndex = i;
         openChar = tempParts[i][index1];
         closeChar = GetClosingBracket(openChar);
         
         //MessageInterface::ShowMessage
         //   ("   openBracketCounter=%d, openIndex=%d, openChar=%c, closeChar=%c\n",
         //    openBracketCounter, openIndex, openChar, closeChar);
      }
      
      if (index1 == str.npos && index2 == str.npos)
      {
         if (openBracketCounter > 0)
         {
            tempParts[openIndex] = tempParts[openIndex] + delim + tempParts[i];
            tempParts[i] = "***";
            //MessageInterface::ShowMessage
            //   ("   open and close not found: tempParts[%d]=%s\n",
            //    openIndex, tempParts[openIndex].c_str());
         }
      }
      else if (index1 == str.npos && index2 != str.npos)
      {
         if (tempParts[i][index2] == closeChar)
         {
            tempParts[openIndex] = tempParts[openIndex] + newDelim + tempParts[i];
            //MessageInterface::ShowMessage
            //   ("   close found: tempParts[%d]=%s\n", openIndex,
            //    tempParts[openIndex].c_str());
            tempParts[i] = "***";
            openBracketCounter--;
         }
      }
   }
   
   // remove extra string "***"
   StringArray::iterator iter = tempParts.begin();
   while (iter != tempParts.end())
   {
      if (*iter != "***")
      {
         str = GmatStringUtil::Trim(*iter);
         if (str != "")
            parts.push_back(str);
      }
      
      ++iter;
   }
   
   #if DEBUG_TP_SEP_BRACKETS
   MessageInterface::ShowMessage("   Returning:\n");
   for (unsigned int i=0; i<parts.size(); i++)
      MessageInterface::ShowMessage
         ("   parts[%d] = %s\n", i, parts[i].c_str());
   #endif
   
   return parts;
}


//-------------------------------------------------------------------------------
// StringArray SeparateSpaces(const std::string &chunk)
//-------------------------------------------------------------------------------
/*
 * Breaks string by space or comma.
 *
 * @param <chunk> input string to be break apart
 *
 * @return string array of parts
 */
//-------------------------------------------------------------------------------
StringArray TextParser::SeparateSpaces(const std::string &chunk)
{
   StringTokenizer st(chunk, " ,\t");
   StringArray parts = st.GetAllTokens();
   
   #if DEBUG_SEP_SPACE
   for (UnsignedInt i=0; i<parts.size(); i++)
      MessageInterface::ShowMessage
         ("   parts[%d]=%s\n", i, parts[i].c_str());
   #endif

   return parts;
}


//-------------------------------------------------------------------------------
// StringArray SeparateDots(const std::string &chunk)
//-------------------------------------------------------------------------------
/*
 * Breaks string by dots, but keep decimal point number together.
 *
 * @param <chunk> input string to be break apart
 *
 * @return string array of parts
 */
//-------------------------------------------------------------------------------
StringArray TextParser::SeparateDots(const std::string &chunk)
{
   Real rval;
   StringArray parts;

   // Separate by dots if not a number
   if (GmatStringUtil::ToReal(chunk, rval))
   {
      parts.push_back(chunk);
   }
   else
   {
      StringTokenizer st(chunk, ".");
      parts = st.GetAllTokens();
   }
   
   #if DEBUG_SEP_DOTS
   for (UnsignedInt i=0; i<parts.size(); i++)
      MessageInterface::ShowMessage
         ("   parts[%d]=%s\n", i, parts[i].c_str());
   #endif

   return parts;
}


//-------------------------------------------------------------------------------
// StringArray SeparateBy(const std::string &chunk, const std::string &delim)
//-------------------------------------------------------------------------------
/*
 * Breaks string by input delimiter. Removes leading and trailing spaces.
 *
 * @param <chunk> input string to be break apart
 * @param <delim> input delimiter used in breaking apart
 *
 * @return string array of parts
 */
//-------------------------------------------------------------------------------
StringArray TextParser::SeparateBy(const std::string &chunk, const std::string &delim)
{
   StringTokenizer st(chunk, delim);
   StringArray parts = st.GetAllTokens();
   int count = parts.size();

   for (int i=0; i<count; i++)
      parts[i] = GmatStringUtil::Strip(parts[i], GmatStringUtil::BOTH);
   
   #if DEBUG_TP > 1
   for (int i=0; i<count; i++)
      MessageInterface::ShowMessage
         ("   parts[%d]=%s\n", i, parts[i].c_str());
   #endif

   return parts;
}


//-------------------------------------------------------------------------------
// bool IsCommand(const std::string &str)
//-------------------------------------------------------------------------------
bool TextParser::IsCommand(const std::string &str)
{
   bool found = false;
   
   if (find(theCommandList.begin(), theCommandList.end(), str)
       != theCommandList.end())
      found = true;
   
   return found;
}


//-------------------------------------------------------------------------------
// char GetClosingBracket(const char &openBracket)
//-------------------------------------------------------------------------------
char TextParser::GetClosingBracket(const char &openBracket)
{
   switch (openBracket)
   {
   case '(':
      return ')';
   case '[':
      return ']';
   case '{':
      return '}';
   case '<':
      return '>';
      
   default:
      sprintf(errorMsg, "TextParser found unknown open bracket: %c", openBracket);
      throw InterpreterException(errorMsg);
   }
}


