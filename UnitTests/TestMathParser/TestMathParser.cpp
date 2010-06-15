//$Header: /cygdrive/p/dev/cvs/test/TestMath/TestMathParser.cpp,v 1.11 2008/08/22 14:48:58 lojun Exp $
//------------------------------------------------------------------------------
//                                  TestMathParser
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2006/04/24
//
/**
 * Purpose:
 * Unit test driver of MathParser class.
 *
 * Output file:
 * TestMathParserOut.txt
 *
 * Description MathParser:
 * The MathParser class takes a line of script that evaluates to inline math,
 * and breaks that line apart into its component elements using a recursive 
 * descent algorithm.  The resulting representation is stored in a binary tree 
 * structure, which is calculated, depth first, when the expression needs to be
 * evaluated during execution of a script.
 * 
 * Test Procedure:
 * 1. Create MathParser.
 * 2. Create string containg math expression.
 * 3. Create a MathNode pointer.
 * 4. Pass the expression to Parse() "node = mp.Parse(expstr)".
 * 5. Call EvaluateNode() to evaluate and validate the node by passing expected result.
 * 6. Repeat 2 through 5 to test differerent math expression.
 *
 * Validation method:
 * The test driver code knows expected results and throws an exception if the
 * result is not within the tolerance.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "MathParser.hpp"
#include "GmatGlobal.hpp"
#include "MathNode.hpp"
#include "MathFunction.hpp"
#include "MathElement.hpp"
#include "Variable.hpp"
#include "Array.hpp"
#include "StringVar.hpp"
#include "BaseException.hpp"
#include "Rmatrix33.hpp"
#include "FileManager.hpp"         // for ReadStartupFile()
#include "MessageInterface.hpp"
#include "ConsoleMessageReceiver.hpp"
#include "TestOutput.hpp"

#include <iostream>

#define DEBUG_TEST_MATH_PARSER 1

using namespace std;


//------------------------------------------------------------------------------
// void GetNodes(MathNode *node, MathNode **left, MathNode **right)
//------------------------------------------------------------------------------
void GetNodes(MathNode *node, MathNode **left, MathNode **right)
{
   
   *left = node->GetLeft();
   *right = node->GetRight();

   #if DEBUG_TEST_MATH_PARSER
   MessageInterface::ShowMessage
      ("==> GetNodes() node=%s, %s\n", node->GetTypeName().c_str(),
       node->GetName().c_str());
   
   if (*left)
      MessageInterface::ShowMessage
         ("   left=%s, %s\n", (*left)->GetTypeName().c_str(),
          (*left)->GetName().c_str());

   if (*right)
      MessageInterface::ShowMessage
         ("   right=%s, %s\n", (*right)->GetTypeName().c_str(),
          (*right)->GetName().c_str());

   #endif
}


//------------------------------------------------------------------------------
// void SetParameters(MathNode *node, const std::string &leftName, Parameter *leftParam,
//                    const std::string &rightName, Parameter *rightParam)
//------------------------------------------------------------------------------
void SetParameters(MathNode *node, const std::string &leftName, Parameter *leftParam,
                   const std::string &rightName, Parameter *rightParam)
{
   #if DEBUG_TEST_MATH_PARSER
   MessageInterface::ShowMessage("==========> SetParameters() entered\n");
   #endif
   
   MathNode *left = NULL;
   MathNode *right = NULL;
   
   if (node->IsFunction())
      GetNodes(node, &left, &right);

   if (left)
   {
      if (!left->IsFunction())
      {
         if (left->GetName() == "arrI")
            left->SetRefObject(rightParam, Gmat::PARAMETER, rightName);
         else
            left->SetRefObject(leftParam, Gmat::PARAMETER, leftName);
         
         #if DEBUG_TEST_MATH_PARSER > 1
         Rmatrix mat = left->GetMatrixValue();
         MessageInterface::ShowMessage
            ("==> SetParameters() left mat=\n%s\n", mat.ToString(12).c_str());
         #endif
      }
      else
      {
         SetParameters(left, leftName, leftParam, rightName, rightParam);
      }
   }
   
   
   if (right)
   {
      if (!right->IsFunction())
      {         
         if (!right->IsNumber())
         {
            right->SetRefObject(rightParam, Gmat::PARAMETER, rightName);
            
            #if DEBUG_TEST_MATH_PARSER > 1
            Rmatrix mat = right->GetMatrixValue();
            MessageInterface::ShowMessage
               ("==> SetParameters() right mat=\n%s\n", mat.ToString(12).c_str());
            #endif
         }
      }
      else
      {
         SetParameters(right, leftName, leftParam, rightName, rightParam);
      }
   }
}


//------------------------------------------------------------------------------
// void EvaluateNode(MathNode *node, TestOutput &out, Real expVal, Rmatrix &expMat)
//------------------------------------------------------------------------------
void EvaluateNode(MathNode *node, TestOutput &out, Real expVal, Rmatrix &expMat)
{
   Integer returnType;
   Integer numRow;
   Integer numCol;
   Real realVal;
   Rmatrix rmat;
   std::string nodeType = node->GetTypeName();
   
   #if DEBUG_TEST_MATH_PARSER
   MessageInterface::ShowMessage
      ("==========> EvaluateNode() node=%s, %s\n==> Now validate inputs\n", nodeType.c_str(),
       node->GetName().c_str());
   #endif
   
   if (node->ValidateInputs())
   {
      node->GetOutputInfo(returnType, numRow, numCol);
      
      #if DEBUG_TEST_MATH_PARSER
      MessageInterface::ShowMessage
         ("==> returnType=%d, numRow=%d, numCol=%d\n", returnType,numRow, numCol);
      #endif
      
      if (returnType == Gmat::REAL_TYPE)
      {
         realVal = node->Evaluate();
         out.Validate(realVal, expVal);
      }
      else
      {
         rmat.SetSize(numRow, numCol);
         rmat = node->MatrixEvaluate();
         out.Validate(rmat, expMat);
      }
   }
   else
   {
      throw MathException("*** EvaluateNode(): ValidateInputs() returned false\n");
   }
}

//------------------------------------------------------------------------------
// void TestIsEquation(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestIsEquation(TestOutput &out, MathParser &mp)
{
   std::string expstr;
   bool boolVal;
   bool expBoolVal;
   
   out.Put("============================== Test IsEquation()");

   //------------------------------
   expstr = "123.456";
   expBoolVal = false;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "-123.456";
   expBoolVal = false;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "Cos(0)";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "ars(1,1)";
   expBoolVal = false;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "a+b";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "-abc";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "M'";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "M^(-1)";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
   //------------------------------
   expstr = "TA1 = abs( TA1 - 360 )";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);

   //------------------------------
   expstr = "cross(vv, cross(rv, vv));";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   
}


//------------------------------------------------------------------------------
// void TestFindLowestOperator(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestFindLowestOperator(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test FindLowestOperator()");
   std::string expstr;
   Integer opIndex;
   std::string str1;

   //------------------------------
   expstr = "((3*a+4)-(9*b-20)*(cos(c)^2))*(-a/b)*d-x";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 38);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)-(9*b-20)*(cos(c)^2)*(-a/b)*d-x";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 36);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)*(9*b-20)-(cos(c)^2)*(-a/b)*(d-x)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 16);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)*(9*b-20)-(cos(c)^2)*(-a/b)*(d-x)+5";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 40);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)*(9*b-20)/(cos(c)^2)*(-a/b)*(d-x)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 34);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)^(9*b-20)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "^");
   out.Validate(opIndex, 7);
   out.Put("");
   
   //------------------------------
   expstr = "(3*a+4)-(9*b-20)*5-2+2";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 20);
   out.Put("");
   
   //------------------------------
   expstr = "(3+5)*(2+2)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 5);
   out.Put("");
   
   //------------------------------
   expstr = "5^(-1/2)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "^");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "(3+5)*2+2";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 7);
   out.Put("");
   
   //------------------------------
   expstr = "1*1-1*(10*-50)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 3);
   out.Put("");
   
   //------------------------------
   expstr = "(1*1)-1*(10*-50)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 5);
   out.Put("");
   
   //------------------------------
   expstr = "((3*2+4)-(9*1000-20)*(-0.97^2))*(-2.34/0.001)*0.134";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 45);
   out.Put("");
   
   //------------------------------
   expstr = "Sat.X*(b*c*vec(4,1))-10.9056168";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 20);
   out.Put("");
   
   //------------------------------
   expstr = "a*b*c/vec";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "/");
   out.Validate(opIndex, 5);
   out.Put("");
   
   //------------------------------
   expstr = "(a*b*c/vec)*(s+y)/2*a*b*(a/b)*2-5";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 31);
   out.Put("");
   
   //------------------------------
   expstr = "(a*b*c/vec)*(s+y)/2*a*b*(a/b)*2*5";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 31);
   out.Put("");
   
   //------------------------------
   expstr = "cos(phi)*I3+(1-cos(phi))*av*av'-sin(phi)*across";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 31);
   out.Put("");
   
   //------------------------------
   expstr = "cos(phi)*I3+(1-cos(phi))*av*av'";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 11);
   out.Put("");
   
   //------------------------------
   expstr = "a++4";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "a+-4";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "a--4";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "a-+4";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "-a4";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "-");
   out.Validate(opIndex, 0);
   out.Put("");
   
   //------------------------------
   expstr = "sqrt(1.0^2+2.0^2+3.0^2)+sqrt(1.0^2+2.0^2+3.0^2);";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "+");
   out.Validate(opIndex, 23);
   out.Put("");
   
   //------------------------------
   expstr = "acos(sv1'*SpinVector/S1)*180;";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 24);
   out.Put("");
   
   //------------------------------
   expstr = "acos(sv1'*SpinVector/S1)*180/pi;";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "/");
   out.Validate(opIndex, 28);
   out.Put("");
   
   //------------------------------
   expstr = "5*-2";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 1);
   out.Put("");
   
   //------------------------------
   expstr = "M^(-1)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "");
   out.Validate(opIndex, std::string::npos);
   out.Put("");
   
   //------------------------------
   expstr = "sin(94*0.0174532925199433)^2;";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "^");
   out.Validate(opIndex, 26);
   out.Put("");
}


//------------------------------------------------------------------------------
// void TestOpsWithNumber(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestOpsWithNumber(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Math Operations with Number");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   
   //------------------------------
   expstr = "123.456";
   expRealVal = 123.456;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "3+5*2";
   expRealVal = 13;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "3+5+2*2";
   expRealVal = 12;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "3+5*2*2";
   expRealVal = 23;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5116.1085^0";
   expRealVal = 1.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5^-2";
   expRealVal = 0.04;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5  *   -2";
   expRealVal = -10;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5/-2";
   expRealVal = -2.5;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5+-2";
   expRealVal = 3.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5-+2";
   expRealVal = 3.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5++2";
   expRealVal = 7.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5--2";
   expRealVal = 7.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-3*2 + 6*8";
   expRealVal = 42.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-3*2 - 6*8";
   expRealVal = -54.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "0.2*0.3*2.0/3.0/5.0 - 10.0";
   expRealVal = -9.992;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "10 - 50 + 1 + 30 - 25";
   expRealVal = 10 - 50 + 1 + 30 - 25;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
}

//------------------------------------------------------------------------------
// void TestOpsWithNumberWithParen(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestOpsWithNumberWithParen(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Math Operations with Number with Parenthesis");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   
   //------------------------------
   expstr = "5^(-1/2)";
   expRealVal = 0.447213595;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(3+5)*2+2";
   expRealVal = 18;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(3+5)  *  (2+2)";
   expRealVal = 32;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "((3+5)*2)*2";
   expRealVal = 32;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "((3+5)*2)*2";
   expRealVal = 32;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(3+5)*(2+2)*(4+5)";
   expRealVal = 288;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5116.1085 + (-4237.076770)";
   expRealVal = 879.03173;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5116.1085 - (-4237.076770)";
   expRealVal = 9353.18527;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "30.0 * (-2.0)";
   expRealVal = -60.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "30.0 / (-2.0)";
   expRealVal = -15.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( ( 5^2 - 4/2 )*2 - 3*5  ) / 4";
   expRealVal = 7.75;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-(50/2*2)";
   expRealVal = -50.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-(50*2/2)";
   expRealVal = -50.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-(50/2/2)";
   expRealVal = -12.5;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( 10 - 2 )^2 + ( 4 - 2 )^2 + ( 15 - 10 )^2";
   expRealVal = 93.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "1*1 - 1*(10*-50)";
   expRealVal = 501.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(1*1) - 1*(10*-50)";
   expRealVal = 501.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(7+ 10)*10";
   expRealVal = 170.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "10*(7+ 10)";
   expRealVal = 170.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(10)*5*6/2";
   expRealVal = 150;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "200*1000^(-1)";
   expRealVal = 0.2;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(10)*5*6/2 - 200*1000^(-1)";
   expRealVal = 149.8;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "0.2*(5*6*0.2) - 50";
   expRealVal = -48.8;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(0.2)*(5*6*0.2) - 50";
   expRealVal = -48.8;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(0.2)*5*6*0.2 - 50";
   expRealVal = -48.8;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "2.34*0.000134*34.78/(1000) - 1.09056168*10^(-5)";
   expRealVal = 0.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "2.34*0.000134*34.78/1000 - 1.09056168*10^(-5)";
   expRealVal = 0.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "0.5/(1000/0.5/2.0) - 20.2343*10^(-2)";
   expRealVal = -0.201843;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "5/(3/2/6)";
   expRealVal = 20.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2) )";
   expRealVal = 1.215600000000000e+001;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(((-0.9754)^2) )";
   expRealVal = 9.514051600000001e-001;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2)-(9.1*1000.23-20.21) )";
   expRealVal = -9.069726999999999e+003;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (-0.9754^2) )";
   expRealVal = -9.514051600000001e-001;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2)-(9.1*1000.23-20.21)*(-0.9754^2) )";
   expRealVal = 8.652706348716281e+003;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2)-(9.1*1000.23-20.21)*(-0.9754^2) )*(-2.34/0.001)";
   expRealVal = -2.024733285599610e+007;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2)-(9.1*1000.23-20.21)*(-0.9754^2) )*(-2.34/0.001)*0.000134 - 0.05";
   expRealVal = -2.713192602703477e+003;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
}


//------------------------------------------------------------------------------
// void TestFunctionWithNumber(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestFunctionWithNumber(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Function with Number");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;

   #if 0
   
   //------------------------------
   expstr = "(cos(0.000134)^2)";
   expRealVal = 9.999999820440001e-001;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3.4*2.34+4.2)-(9.1*1000.23-20.21)*(cos(0.000134)^2) )*(-2.34/0.001)*0.000134 - 0.05";
   expRealVal = 2.843853546986425e+003;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "( (3*2.34+4)-(9*1000-20)*(cos(34.78)^2) )*(-2.34/0.001)*0.000134 - 0.00267522370194881";
   expRealVal = 2.675221026725112e+003;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sqrt(( 10 - 2 )^2 + ( 4 - 2 )^2 + ( 15 - 10 )^2)";
   expRealVal = 9.64365076099295;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Cos(0.0) + 10.0";
   expRealVal = 11;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Cos(0.0) + 10.0^2";
   expRealVal = 101;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sqrt(39)";
   expRealVal = 6.245;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sqrt(44+10*10)";
   expRealVal = 12;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sqrt(10*10+(54-10))";
   expRealVal = 12;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sqrt(2^2 + 3^2 + 4^2)";
   expRealVal = 5.38516;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "acos(0)";
   expRealVal = 1.57079632;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "asin(1)";
   expRealVal = 1.57079632;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "atan(1)";
   expRealVal = 0.78539816339745;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "atan2(1,0)";
   //expRealVal = 1.57079632679490;
   expRealVal = GmatMathUtil::ATan(1,0);
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "exp(1)";
   expRealVal = 2.71828182845905;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "log(5+4*2-3)";
   expRealVal = 2.30258509299405;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "log10(10)";
   expRealVal = 1.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "tan(0.5)";
   expRealVal = 0.54630248984379;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "DegToRad(180)";
   expRealVal = 3.14159265358979;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "RadToDeg(3.14159265358979)";
   expRealVal = 180.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "sqrt( 1.0^2 + 2.0^2 + 3.0^2 ) + sqrt( 4.0^2 + 5.0^2 + 6.0^2 );;";
   expRealVal = 12.51662177416606;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(sin(0.5)^2);";
   expRealVal = 0.229848847;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
//    //------------------------------
//    expstr = "sin(0.5 * 1.0)^2;";
//    expRealVal = 0.229848847;
//    out.Put(expstr + " should return ", expRealVal);
//    node = mp.Parse(expstr);
//    EvaluateNode(node, out, expRealVal, unsetMat);
//    delete node;
   
//    //------------------------------
//    expstr = "(sin(0.5 * 1.0)^2);";
//    expRealVal = 0.229848847;
//    out.Put(expstr + " should return ", expRealVal);
//    node = mp.Parse(expstr);
//    EvaluateNode(node, out, expRealVal, unsetMat);
//    delete node;
   
   #endif
   
   //------------------------------
   expstr = "(sin(94*0.0174532925199433))^2;";
   node = mp.Parse(expstr);
   expRealVal = 0.9951340343707851;
   out.Put(expstr + " should return ", expRealVal);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "sin(94*0.0174532925199433)^2;";
   node = mp.Parse(expstr);
   expRealVal = 0.9951340343707851;
   out.Put(expstr + " should return ", expRealVal);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
}


//------------------------------------------------------------------------------
// void TestOpsWithMatrix(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestOpsWithMatrix(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Math Operation with Matrix");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   MathNode *left = NULL;
   MathNode *right = NULL;
   
   //------------------------------
   Rmatrix matA(3, 3, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0);
   Rmatrix matB(3, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
   expstr = "matA+matB";
   Rmatrix33 expMat1(11, 22, 33, 44, 55, 66, 77, 88, 99);
   out.Put(expstr + " should return\n", expMat1);
   node = mp.Parse(expstr);
   GetNodes(node, &left, &right);
   left->SetMatrixValue(matA);
   right->SetMatrixValue(matB);
   EvaluateNode(node, out, expRealVal, expMat1);
   delete node;
   
   //------------------------------
   expstr = "matA-matB";
   Rmatrix33 expMat2(9, 18, 27, 36, 45, 54, 63, 72, 81);
   out.Put(expstr + " should return\n", expMat2);
   node = mp.Parse(expstr);
   GetNodes(node, &left, &right);
   left->SetMatrixValue(matA);
   right->SetMatrixValue(matB);
   EvaluateNode(node, out, expRealVal, expMat2);
   delete node;
}


//------------------------------------------------------------------------------
// void TestVariable(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestVariable(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Math Operation and Function with Variable");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   
   //-------------------------------------------------------------------
   // Until Wrapper stuff is implemented, the followng no longer works
   //-------------------------------------------------------------------
   
   //------------------------------
   expstr = "varA+varB";
   Variable *varA = new Variable("varA", "10.123");
   Variable *varB = new Variable("varB", "21.345");
   expRealVal = 31.468;
   out.Put("varA = ", varA->EvaluateReal());
   out.Put("varB = ", varB->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "varA", varA, "varB", varB);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "-varA";
   expRealVal = -10.123;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "varA", varA, "varB", varB);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "varA^0";
   expRealVal = 1.0;
   out.Put("varA = ", varA->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);      
   node = mp.Parse(expstr);
   SetParameters(node, "varA", varA, "", NULL);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Cos(var0)";
   Variable *var0 = new Variable("var0", "0.0");
   expRealVal = 1.0;
   out.Put("var0 = ", var0->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "var0", var0, "", NULL);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "Sin(var0)";
   expRealVal = 0.0;
   out.Put("var0=", var0->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "var0", var0, "", NULL);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(-varA/varB)";
   expRealVal = -4.742562661044741e-001;
   out.Put("varA=", varA->EvaluateReal());
   out.Put("varB=", varB->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "varA", varA, "varB", varB);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "abs( varA - 360 )";
   expRealVal = GmatMathUtil::Abs(10.123 - 360.0);
   out.Put("varA=", varA->EvaluateReal());
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "varA", varA, "", NULL);      
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
}


//------------------------------------------------------------------------------
// void TestArray(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestArray(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test Math Operation and Function with Array");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   
   Rmatrix matA(3, 3, 10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0);
   Rmatrix matB(3, 3, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
   Rmatrix33 expMat1(11, 22, 33, 44, 55, 66, 77, 88, 99);
   
   expstr = "arrA+arrB";
   Array *arrA = new Array("arrA");
   Array *arrB = new Array("arrB");
   arrA->SetSize(3,3);
   arrB->SetSize(3,3);
   arrA->SetRmatrixParameter("RmatValue", matA);
   arrB->SetRmatrixParameter("RmatValue", matB);
   out.Put("arrA =\n", arrA->GetRmatrixParameter("RmatValue"));
   out.Put("arrB =\n", arrB->GetRmatrixParameter("RmatValue"));
   out.Put(expstr + " should return\n", expMat1);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "arrB", arrB);      
   EvaluateNode(node, out, expRealVal, expMat1);
   delete node;
   
   //------------------------------
   expstr = "-arrA";
   Rmatrix expMatNegate = -matA;
   out.Put(expstr + " should return\n", expMatNegate);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "arrB", arrB);      
   EvaluateNode(node, out, expRealVal, expMatNegate);
   delete node;
   
   //------------------------------
   expstr = "transpose(arrA)";
   Rmatrix expMat3 = matA.Transpose();
   out.Put(expstr + " should return\n", expMat3);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "", NULL);      
   EvaluateNode(node, out, expRealVal, expMat3);
   delete node;
   
   //------------------------------
   expstr = "arrA'";
   out.Put(expstr + " should return\n", expMat3);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "", NULL);      
   EvaluateNode(node, out, expRealVal, expMat3);
   delete node;
   
   //------------------------------
   expstr = "arrA' + arrA'";
   Rmatrix expMat32 = expMat3*2;
   out.Put(expstr + " should return\n", expMat32);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "arrA", arrA);      
   EvaluateNode(node, out, expRealVal, expMat32);
   delete node;
   
   //------------------------------
   expstr = "det(arrI)";
   Array *arrI = new Array("arrI");
   arrI->SetSize(3,3);
   Rmatrix matI(3, 3, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0);
   arrI->SetRmatrixParameter("RmatValue", matI);
   out.Put("arrI =\n", arrI->GetRmatrixParameter("RmatValue"));
   expRealVal = 1.0;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "inv(arrI)";
   out.Put(expstr + " should return\n", arrI);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, matI);
   delete node;
   
   //------------------------------
   expstr = "arrI^(-1)";
   out.Put(expstr + " should return\n", arrI);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, matI);
   delete node;
   
   //------------------------------
   expstr = "arrI^(-1) + arrI^(-1)";
   Rmatrix expMatI2 = matI*2;
   out.Put(expstr + " should return\n", expMatI2);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, expMatI2);
   delete node;
   
   //------------------------------
   expstr = "arrI^(-1) * arrI^(-1)";
   out.Put(expstr + " should return\n", matI);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, matI);
   delete node;
   
   //------------------------------
   expstr = "arrI^(-1) - arrI^(-1)";
   Rmatrix mat0 = matI - matI;
   out.Put(expstr + " should return\n", mat0);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, mat0);
   delete node;
   
   //------------------------------
   expstr = "arrI^(-1) + arrI^(-1) - arrI^(-1)";
   out.Put(expstr + " should return\n", matI);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, matI);
   delete node;
   
   //------------------------------
   expstr = "norm(arrC)";
   Array *arrC = new Array("arrC");
   arrC->SetSize(1,4);
   Rmatrix matC(1, 4, 0.0, 1.0, 2.0, 3.0);
   arrC->SetRmatrixParameter("RmatValue", matC);
   out.Put("arrC =\n", arrC->GetRmatrixParameter("RmatValue"));
   expRealVal = 3.74165738677394;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "arrC", arrC, "", NULL);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "arrI*arrI";
   out.Put("arrI =\n", arrA->GetRmatrixParameter("RmatValue"));
   Rmatrix expMat4 = matI;
   out.Put(expstr + " should return\n", matI);
   node = mp.Parse(expstr);
   SetParameters(node, "arrI", arrI, "arrI", arrI);      
   EvaluateNode(node, out, expRealVal, expMat4);
   delete node;
   
   //------------------------------
   expstr = "(norm(arrC) + det(arrI)) * arrI";
   expMat3 = 4.7416573867739409 * matI;
   out.Put(expstr + " should return\n", expMat3);
   node = mp.Parse(expstr);
   SetParameters(node, "arrC", arrC, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, expMat3);
   delete node;
   
   //------------------------------
   expstr = "norm(arrC) + det(arrI * arrI)";
   expRealVal = 4.7416573867739409;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   SetParameters(node, "arrC", arrC, "arrI", arrI);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   //------------------------------
   expstr = "(transpose(arrA + arrB)) * 1.0";
   Rmatrix expMat5 = expMat1.Transpose();
   out.Put(expstr + " should return\n", expMat5);
   node = mp.Parse(expstr);
   SetParameters(node, "arrA", arrA, "arrB", arrB);      
   EvaluateNode(node, out, expRealVal, expMat5);
}


//------------------------------------------------------------------------------
// void TestJustParsing(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestJustParsing(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test just parsing");
   
   std::string expstr;
   bool boolVal;
   bool expBoolVal;
   MathNode *node = NULL;
   
   //------------------------------
   expstr = "cos(phi)*I3 + (1 - cos(phi))*av*av' - sin(phi)*across";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "sv2(2,1) * sv3(3,1) - sv2(3,1) * sv3(2,1)";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "ACE.VX - CurrentV(1,1)";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "EarthSat.ECC*sin( EarthSat.AOP )"; 
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "( ( v^2 - mu/r )*rv - rdotv*vv  ) / mu";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "( VX - MarsL1Sat.MarsFK5.VX )^2 + ( VY - MarsL1Sat.MarsFK5.VY )^2 + "
      "( VZ - MarsL1Sat.MarsFK5.VZ )^2";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "nv(1,1)*ev(1,1) + nv(2,1)*ev(2,1) + nv(3,1)*ev(3,1) ";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "(cnu + e)*sqrtmup";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "sqrtmup*(cnu + e)";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "vec(1,1)*vec(2,1)*(vec(3,1)*vec(4,1)) - 10.9056168";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "(vec(1,1)*vec(2,1))*vec(3,1)*vec(4,1) - 10.9056168";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "vec(1,1)*vec(4,1)/(vec(3,1)*vec(2,1)) - 9.01552616446233*10^(-9)";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "(vec(1,1)*vec(4,1)*vec(3,1))/vec(2,1) - 1.09056168*10^(-5)";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "a*vec(2,1)/(Sat.Z*.000134)/1000000 -  0.502089895548136";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "acos( nv(1,1)/n )";
   node = mp.Parse(expstr);
         
   //------------------------------
   expstr = "(a)*b*c*d - 10.9056168";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "( (3*a+4)-(9*b-20)*(cos(c)^2) )*(-a/b)*d - 0.00267522370194881";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "dummyVar = DefaultSC.TA + 1";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "acos(sv1'*SpinVector/S1)*180;";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "acos(sv1'*SpinVector/S1)*180/pi;";
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "acos( xxx )";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "sin(INC*d2r)^2;";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   node = mp.Parse(expstr);
   
   //------------------------------
   expstr = "(sin(INC*d2r))^2;";
   expBoolVal = true;
   boolVal = mp.IsEquation(expstr);
   out.Put(expstr + " should return ", expBoolVal);
   out.Validate(boolVal, expBoolVal);
   node = mp.Parse(expstr);

   //------------------------------
   try
   {
      expstr = "cross(vv, cross(rv, vv));";
      expBoolVal = true;
      boolVal = mp.IsEquation(expstr);
      out.Put(expstr + " should return ", expBoolVal);
      out.Validate(boolVal, expBoolVal);
      node = mp.Parse(expstr);
   }
   catch (BaseException &be)
   {
      MessageInterface::ShowMessage(be.GetFullMessage() + "\n");
      out.Put(be.GetFullMessage() + "\n");
   }
   
   
}


//------------------------------------------------------------------------------
// void TestFunctionRunner(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestFunctionRunner(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test FunctionRunner");
   
   std::string expstr;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;
   
   /*
   //------------------------------
   expstr = "Sqrt(1+2+3)";
   expRealVal = sqrt(1+2+3);
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
   
   
   //------------------------------
   expstr = "Factorial(1+2+3)";
   expRealVal = 720;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   //EvaluateNode(node, out, expRealVal, unsetMat);
   //delete node;
   
   //------------------------------
   expstr = "Sqrt(Times2(1))";
   expRealVal = sqrt(2);
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   //EvaluateNode(node, out, expRealVal, unsetMat);
   //delete node;
   */
   
   //------------------------------
   try
   {
      expstr = "Times(1, 2)";
      expRealVal = 2;
      out.Put(expstr + " should return ", expRealVal);
      node = mp.Parse(expstr);
      EvaluateNode(node, out, expRealVal, unsetMat);
      delete node;
   }
   catch (BaseException &be)
   {
      out.Put(be.GetFullMessage());
   }
   
   //------------------------------
   try
   {
      expstr = "Times(Sqrt(1), 10)";
      expRealVal = 20;
      out.Put(expstr + " should return ", expRealVal);
      node = mp.Parse(expstr);
      EvaluateNode(node, out, expRealVal, unsetMat);
      delete node;
   }
   catch (BaseException &be)
   {
      out.Put(be.GetFullMessage());
   }
   
   //------------------------------
   try
   {
      expstr = "FindMax3(a, b, c)";
      expRealVal = 10;
      out.Put(expstr + " should return ", expRealVal);
      node = mp.Parse(expstr);
      EvaluateNode(node, out, expRealVal, unsetMat);
      delete node;
   }
   catch (BaseException &be)
   {
      out.Put(be.GetFullMessage());
   }
   
   //------------------------------
   try
   {
      expstr = "FindMax3(Sqrt(1), 10, 5)";
      expRealVal = 10;
      out.Put(expstr + " should return ", expRealVal);
      node = mp.Parse(expstr);
      EvaluateNode(node, out, expRealVal, unsetMat);
      delete node;
   }
   catch (BaseException &be)
   {
      out.Put(be.GetFullMessage());
   }
}


//------------------------------------------------------------------------------
// void TestSpecialCase(TestOutput &out, MathParser &mp)
//------------------------------------------------------------------------------
void TestSpecialCase(TestOutput &out, MathParser &mp)
{
   out.Put("============================== Test special case");
   
   std::string expstr;
   Integer opIndex;
   std::string str1;
   Real expRealVal;
   Rmatrix unsetMat;
   MathNode *node = NULL;

   //------------------------------
   expstr = "200*1000^(-1)";
   str1 = mp.FindLowestOperator(expstr, opIndex);
   out.Put(expstr);
   out.Validate(str1, "*");
   out.Validate(opIndex, 3);
   expRealVal = 0.2;
   out.Put(expstr + " should return ", expRealVal);
   node = mp.Parse(expstr);
   EvaluateNode(node, out, expRealVal, unsetMat);
   delete node;
}

//------------------------------------------------------------------------------
//int RunTest(TestOutput &out)
//------------------------------------------------------------------------------
int RunTest(TestOutput &out)
{   
   //---------------------------------------------------------------------------
   out.Put("======================================== Test TestMathParser\n");
   //---------------------------------------------------------------------------
   
   MathParser mp = MathParser();
   
   try
   {
      #if 1
      TestIsEquation(out, mp);
      TestFindLowestOperator(out, mp);
      TestOpsWithNumber(out, mp);
      TestOpsWithNumberWithParen(out, mp);
      TestFunctionWithNumber(out, mp);
      TestOpsWithMatrix(out, mp);
      ////TestVariable(out, mp); // currently not working due to NULL ElementWrapper
      ////TestArray(out, mp);    // currently not working due to NULL ElementWrapper
      TestJustParsing(out, mp);
      ////TestFunctionRunner(out, mp); // currently not working due to NULL Function
      #endif
      
      TestSpecialCase(out, mp);
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage(e.GetFullMessage());
      out.Put(e.GetFullMessage());
      throw MathException("\n>>>>> Unit testing of MathParser was Unsuccessful!!");
   }
   
   return 0;
}


//------------------------------------------------------------------------------
// int main(int argc, char *argv[])
//------------------------------------------------------------------------------
int main(int argc, char *argv[])
{
   std::string startupFile = "gmat_startup_file.txt";
   FileManager *fm = FileManager::Instance();
   fm->ReadStartupFile(startupFile);
   
   ConsoleMessageReceiver *consoleMsg = ConsoleMessageReceiver::Instance();
   MessageInterface::SetMessageReceiver(consoleMsg);
   std::string outPath = "../../TestMathParser/";
   MessageInterface::SetLogFile(outPath + "GmatLog.txt");
   std::string outFile = outPath + "TestMathParserOut.txt";
   TestOutput out(outFile);
   out.Put(GmatTimeUtil::FormatCurrentTime());
   MessageInterface::ShowMessage("%s\n", GmatTimeUtil::FormatCurrentTime().c_str());
   
   // Set global format setting
   GmatGlobal *global = GmatGlobal::Instance();
   global->SetActualFormat(false, false, 16, 1, false);
   
   try
   {
      RunTest(out);
      out.Put("\nSuccessfully ran unit testing of MathParser!!");
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
