//$Header$
//------------------------------------------------------------------------------
//                                Assignment
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway
// Created: 2004/02/03
//
/**
 * Implementation for the Assignment command class.
 * 
 * The assignment class is the Command class that handles commands of the form
 * 
 *     GMAT object.parameter = value;
 *     GMAT variable = parameter;
 *     GMAT variable = equation;
 *
 */
//------------------------------------------------------------------------------


#include "Assignment.hpp"
#include "MathParser.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_RENAME 1
//#define DEBUG_EVAL_RHS
//#define DEBUG_ASSIGNMENT_IA 1
//#define DEBUG_ASSIGNMENT_INIT 1
//#define DEBUG_ASSIGNMENT_EXEC 1
//#define DEBUG_EQUATION 1
//#define DEBUG_ASSIGNMENT_WRAPPER 1


//------------------------------------------------------------------------------
//  Assignment()
//------------------------------------------------------------------------------
/**
 * Constructs the Assignment command (default constructor).
 */
//------------------------------------------------------------------------------
Assignment::Assignment  () :
   GmatCommand          ("GMAT"),
   lhs                  (""),
   rhs                  ("Not_Set"),
   mathTree             (NULL)
{
   lhsWrapper = NULL;
   rhsWrapper = NULL;
}


//------------------------------------------------------------------------------
//  ~Assignment()
//------------------------------------------------------------------------------
/**
 * Destroys the Assignment command (destructor).
 */
//------------------------------------------------------------------------------
Assignment::~Assignment()
{
   if (mathTree)
      delete mathTree;

   mathTree = NULL;
}


//------------------------------------------------------------------------------
//  Assignment(const Assignment& a)
//------------------------------------------------------------------------------
/**
 * Constructs the Assignment command (copy constructor).
 *
 * @param <a> The instance that is copied.
 */
//------------------------------------------------------------------------------
Assignment::Assignment  (const Assignment& a) :
   GmatCommand          (a),
   mathTree             (a.mathTree)
{
   lhsWrapper = NULL;
   rhsWrapper = NULL;
}


//------------------------------------------------------------------------------
//  Assignment& operator=(const Assignment& a)
//------------------------------------------------------------------------------
/**
 * Assignment assignment operator.
 *
 * @param <a> The instance that is copied.
 *
 * @return This instance, set to match the input instance.
 */
//------------------------------------------------------------------------------
Assignment& Assignment::operator=(const Assignment& a)
{
   if (this == &a)
      return *this;
        
   mathTree    = a.mathTree;
   
   lhsWrapper = NULL;
   rhsWrapper = NULL;
   
   return *this;
}


//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
/**
 * Parses the command string and builds the corresponding command structures.
 *
 * Assignment lines have the form
 *
 *    GMAT Sat.Element1 = 7654.321;
 *
 * or
 *
 *    GMAT object1 = object2;
 *
 * This method breaks this line into its constituent pieces.
 *
 * @return true on successful parsing of the command.
 *
 * @todo: Determine if Assignment::InterpretAction can be moved into the
 *        Interpreter
 */
//------------------------------------------------------------------------------
bool Assignment::InterpretAction()
{
   #ifdef DEBUG_ASSIGNMENT_IA
   MessageInterface::ShowMessage("\nAssignment::InterpretAction() entered\n");
   #endif
   
   StringArray chunks = InterpretPreface();
   
   #ifdef DEBUG_ASSIGNMENT_IA
      MessageInterface::ShowMessage("Preface chunks as\n");
      for (StringArray::iterator i = chunks.begin(); i != chunks.end(); ++i)
         MessageInterface::ShowMessage("   \"%s\"\n", i->c_str());
   #endif
      
   lhs = chunks[0];
   rhs = chunks[1];
   
   // Check if rhs is an equation
   MathParser mp = MathParser();
   if (mp.IsEquation(rhs))
   {
      // Parse RHS if equation
      #if DEBUG_EQUATION
      MessageInterface::ShowMessage
         ("Assignment::InterpretAction() %s is an equation\n", rhs.c_str());
      #endif
      
      MathNode *topNode = mp.Parse(rhs);
      
      #if DEBUG_EQUATION
      if (topNode)
         MessageInterface::ShowMessage
            ("   topNode=%s\n", topNode->GetTypeName().c_str());
      #endif
      
      mathTree = new MathTree("MathTree", rhs);
      mathTree->SetTopNode(topNode);
   }
   
   #ifdef DEBUG_ASSIGNMENT_IA
   MessageInterface::ShowMessage("Assignment::InterpretAction() leaving\n");
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Performs the initialization needed to run the Assignment command.
 *
 * @return true if the GmatCommand is initialized, false if an error occurs.
 */
//------------------------------------------------------------------------------
bool Assignment::Initialize()
{
   #ifdef DEBUG_ASSIGNMENT_INIT
   MessageInterface::ShowMessage
      ("Assignment::Initialize() entered for %s\n", generatingString.c_str());
   #endif
   
   if (GmatCommand::Initialize() == false)
      return false;
   
   // if rhs is not an equation, set ref obj on both lhs and rhs wrappers
   if (mathTree == NULL)
   {
      // Set references for the wrappers   
      if (SetWrapperReferences(*lhsWrapper) == false)
         return false;
      
      if (SetWrapperReferences(*rhsWrapper) == false)
         return false;
   }
   else
   {
      // Set references for the wrappers   
      if (SetWrapperReferences(*lhsWrapper) == false)
         return false;
      
      // Initialize mathTree
      MathNode *topNode = mathTree->GetTopNode();
      
      #if DEBUG_EQUATION
      MessageInterface::ShowMessage
         ("Assignment::Initialize() Initializing topNode=%s, %s\n",
          topNode->GetTypeName().c_str(), topNode->GetName().c_str());
      #endif
      
      if (mathTree->Initialize(objectMap))
      {
         if (!topNode->ValidateInputs())
            throw CommandException("Failed to validate equation inputs: " +
                                   generatingString + "\n");
      }
      else
      {
         throw CommandException("Failed to initialize equation: " +
                                generatingString + "\n");
      }
   }
   
   return true;
   
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
/**
 * The method that is fired to perform the command.
 *
 * Derived classes implement this method to perform their actions on 
 * GMAT objects.
 *
 * @return true if the GmatCommand runs to completion, false if an error 
 *         occurs. 
 */
//------------------------------------------------------------------------------
bool Assignment::Execute()
{
   #ifdef DEBUG_ASSIGNMENT_EXEC
   MessageInterface::ShowMessage
      ("\nAssignment::Execute() entered for \"%s\"\n", generatingString.c_str());
   #endif
   
   Real rval = -99999.999;
   Integer ival = -99999;
   bool bval = false;
   std::string sval;
   Rmatrix rmat;
   GmatBase *obj = NULL;
   
   Gmat::ParameterType lhsDataType = lhsWrapper->GetDataType();
   Gmat::ParameterType rhsDataType = Gmat::UNKNOWN_PARAMETER_TYPE;
   std::string lhsTypeStr = GmatBase::PARAM_TYPE_STRING[lhsDataType];
   std::string rhsTypeStr = "UnknownDataType";
   Gmat::WrapperDataType lhsWrapperType = lhsWrapper->GetWrapperType();
   Gmat::WrapperDataType rhsWrapperType = Gmat::UNKNOWN_WRAPPER_TYPE;
   
   #ifdef DEBUG_ASSIGNMENT_EXEC
   MessageInterface::ShowMessage("   lhsDataType=%s\n", lhsTypeStr.c_str());
   #endif
   
   try
   {
      // if rhs is not an equation, assign rhs to lhs
      if (mathTree == NULL)
      {
         rhsDataType = rhsWrapper->GetDataType();
         rhsTypeStr = GmatBase::PARAM_TYPE_STRING[rhsDataType];
         rhsWrapperType = rhsWrapper->GetWrapperType();
         
         #ifdef DEBUG_ASSIGNMENT_EXEC
         MessageInterface::ShowMessage("   rhsDataType=%s\n", rhsTypeStr.c_str());
         #endif
                  
         // If lhs is String, it must be String Object, so check it first
         // ex) UnknownObj1 = str1
         if (lhsDataType == Gmat::STRING_TYPE && lhsWrapperType == Gmat::STRING)
         {
            CommandException ce;
            ce.SetDetails("Cannot set \"%s\" to unknown object \"%s\"",
                          rhs.c_str(), lhs.c_str());
            throw ce;
         }
         
         switch (rhsDataType)
         {
         case Gmat::BOOLEAN_TYPE:
            bval = rhsWrapper->EvaluateBoolean();
            break;
         case Gmat::INTEGER_TYPE:
            ival = rhsWrapper->EvaluateInteger();
            break;
         case Gmat::REAL_TYPE:
            rval = rhsWrapper->EvaluateReal();
            break;
         case Gmat::RMATRIX_TYPE:
            rmat = rhsWrapper->EvaluateArray();
            break;
         case Gmat::STRING_TYPE:
            sval = rhsWrapper->EvaluateString();
            break;
         case Gmat::ON_OFF_TYPE:
            sval = rhsWrapper->EvaluateOnOff();
            break;
         case Gmat::OBJECT_TYPE:
            obj = rhsWrapper->EvaluateObject();
            break;
         default:
            throw CommandException("Unknown RHS data type");
         }
      }
      else
      {
         // Evalute math tree
         Integer returnType;
         Integer numRow;
         Integer numCol;
         
         MathNode *topNode = mathTree->GetTopNode();
         
         if (topNode)
         {
            #if DEBUG_EQUATION
            MessageInterface::ShowMessage
               ("Assignment::Execute() topNode=%s, %s\n", topNode->GetTypeName().c_str(),
                topNode->GetName().c_str());
            #endif
            
            topNode->GetOutputInfo(returnType, numRow, numCol);
            
            #if DEBUG_ASSIGNMENT_EXEC
            MessageInterface::ShowMessage("   returnType=%d\n", returnType);
            #endif
            
            if (lhsDataType != returnType)
            {
               CommandException ce;
               ce.SetDetails("Cannot set type \"%s\" to type \"%s\"",
                             GmatBase::PARAM_TYPE_STRING[returnType].c_str(),
                             lhsTypeStr.c_str());
               throw ce;
            }
            
            switch (returnType)
            {
            case Gmat::REAL_TYPE:
               rhsDataType = Gmat::REAL_TYPE;
               rval = topNode->Evaluate();
               break;
            case Gmat::RMATRIX_TYPE:
               rhsDataType = Gmat::RMATRIX_TYPE;
               rmat.SetSize(numRow, numCol);
               rmat = topNode->MatrixEvaluate();
               break;
            default:
               CommandException ce;
               ce.SetDetails("Cannot set \"%s\" to \"%s\". The return type of "
                             "equation is unknown", rhs.c_str(), lhs.c_str());
               throw ce;
            }
         }
         else
         {
            throw CommandException("RHS is an equation, but top node is NULL\n");
         }
      }
      
      #if DEBUG_ASSIGNMENT_EXEC
      MessageInterface::ShowMessage
         ("   ==> Now assign \"%s\" to \"%s\", obj=%p\n",
          rhs.c_str(), lhs.c_str(), obj);
      #endif
      
      // Now assign to LHS
      switch (lhsDataType)
      {
      case Gmat::BOOLEAN_TYPE:
         lhsWrapper->SetBoolean(bval);
         break;
      case Gmat::INTEGER_TYPE:
         {
            // Since it always creates NumberWrapper for numbers,
            // check both Integer and Real types
            if (rhsDataType == Gmat::INTEGER_TYPE)
            {
               lhsWrapper->SetInteger(ival);
            }
            else if (rhsDataType == Gmat::REAL_TYPE)
            {
               Integer itempval;
               std::string desc = rhsWrapper->GetDescription();
               if (GmatStringUtil::ToInteger(desc, itempval))
                  lhsWrapper->SetInteger(itempval);
               else
                  throw CommandException("Cannot set Real number to Integer");
            }
            break;
         }
      case Gmat::REAL_TYPE:
         if (rval != -99999.999)
            lhsWrapper->SetReal(rval);
         else
            throw CommandException("Cannot set Non-Real value to Real");
         break;
      case Gmat::RMATRIX_TYPE:
         lhsWrapper->SetArray(rmat);
         break;
      case Gmat::STRING_TYPE:
         // Object to String is needed for Remove for Formation
         if (obj != NULL)
         {
            lhsWrapper->SetString(obj->GetName());
         }
         else if ((rhsDataType == Gmat::STRING_TYPE ||
              rhsDataType == Gmat::ON_OFF_TYPE))
         {
            lhsWrapper->SetString(sval);            
         }
         // We don't want to allow VARIALE to STRING assinment
         else if (rhsDataType == Gmat::REAL_TYPE &&
                  rhsWrapperType != Gmat::VARIABLE)
         {
            lhsWrapper->SetString(rhsWrapper->GetDescription());
         }
         else
         {
            CommandException ce;
            if (obj != NULL)
               ce.SetDetails("Cannot set object of type \"%s\" to an undefined "
                             "object \"%s\"", obj->GetTypeName().c_str(), lhs.c_str());
            else if (lhsWrapperType == Gmat::STRING_OBJECT &&
                     rhsWrapperType == Gmat::VARIABLE)
               ce.SetDetails("Cannot set objet of type \"Variable\" to object of "
                             "type \"String\"");
            else
               ce.SetDetails("Cannot set \"%s\" to an undefined object \"%s\"",
                             rhs.c_str(), lhs.c_str());
            throw ce;
         }
         break;
      case Gmat::ON_OFF_TYPE:
         lhsWrapper->SetOnOff(sval);
         break;
      case Gmat::OBJECT_TYPE:
         if (obj == NULL)
            throw CommandException("Cannot set Non-Object type to object");
         else
            lhsWrapper->SetObject(obj);
         break;
      case Gmat::STRINGARRAY_TYPE:
         // Object to String is needed for Add for Subscribers/Formation
         if (obj != NULL)
            lhsWrapper->SetString(obj->GetName());
         else
         {
            CommandException ce;
            ce.SetDetails("Cannot set \"%s\" to \"%s\"", rhs.c_str(), lhs.c_str());
            throw ce;            
         }
         break;
      default:
         throw CommandException("Unknown LHS type");
      }
      
      BuildCommandSummary(true);
      return true;
   }
   catch (BaseException &e)
   {
      // To make error message format consistent, just add "Command Exception:"
      std::string msg = e.GetFullMessage();
      if (msg.find("Exception") == msg.npos && msg.find("exception") == msg.npos)
         msg = "Command Exception: " + msg;
      
      CommandException ce;
      ce.SetMessage("");
      ce.SetDetails("%s in \n   \"%s\"\n", msg.c_str(), generatingString.c_str());
      throw ce;
   }
}


//------------------------------------------------------------------------------
//  bool SkipInterrupt()
//------------------------------------------------------------------------------
/**
 * Allows select commands to avoid polling for user interrupts.
 * 
 * @return true if the command can skip polling; false if polling is needed.
 */
bool Assignment::SkipInterrupt()
{
   return true;
}


//------------------------------------------------------------------------------
// const StringArray& Vary::GetWrapperObjectNameArray()
//------------------------------------------------------------------------------
/*
 * Returns wrapper object names.
 */
//------------------------------------------------------------------------------
const StringArray& Assignment::GetWrapperObjectNameArray()
{
   wrapperObjectNames.clear();

   // If rhs is an equation, just add lhs
   if (mathTree != NULL)
   {
      if (lhs != "")
         wrapperObjectNames.push_back(lhs);
      return wrapperObjectNames;
   }
   
   // Add lhs and rhs
   if (lhs != "")
      wrapperObjectNames.push_back(lhs);
   if (rhs != "")
      wrapperObjectNames.push_back(rhs);
   
   return wrapperObjectNames;
}


//------------------------------------------------------------------------------
// bool SetElementWrapper(ElementWrapper *toWrapper, const std::string &withName)
//------------------------------------------------------------------------------
bool Assignment::SetElementWrapper(ElementWrapper *toWrapper, 
                                   const std::string &withName)
{
   if (toWrapper == NULL)
      return false;
      
   bool retval = false;
   
   #ifdef DEBUG_ASSIGNMENT_WRAPPER
   MessageInterface::ShowMessage
      ("   Setting wrapper \"%s\" on Assignment command\n", withName.c_str());
   #endif
   
   if (withName == lhs)
   {
      lhsWrapper = toWrapper;
      retval = true;
   }
   
   if (withName == rhs)
   {
      rhsWrapper = toWrapper;
      retval = true;
   }
   
   return retval;
}


//------------------------------------------------------------------------------
// void ClearWrappers()
//------------------------------------------------------------------------------
void Assignment::ClearWrappers()
{      
   ElementWrapper* temp1 = NULL;
   ElementWrapper* temp2 = NULL;
   
   if (lhsWrapper)
   {
      temp1 = lhsWrapper;
      lhsWrapper = NULL;
   }
   
   if (rhsWrapper)
   {
      temp2 = rhsWrapper;
      rhsWrapper = NULL;
   }

   if (temp1)
      delete temp1;
   if (temp2)
      delete temp2;
   
}


//------------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Renames referenced objects.
 *
 * @param <type> Type of the object that is renamed.
 * @param <oldName> The current name for the object.
 * @param <newName> The name the object has when this operation is complete.
 *
 * @return true on success.
 */
//------------------------------------------------------------------------------
bool Assignment::RenameRefObject(const Gmat::ObjectType type,
                                 const std::string &oldName,
                                 const std::string &newName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Assignment::RenameRefObject() entered <%s>\n", generatingString.c_str());
   MessageInterface::ShowMessage
      ("   type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif
   
   if (lhs.find(oldName) != lhs.npos)
      lhs = GmatStringUtil::Replace(lhs, oldName, newName);
      
   if (rhs.find(oldName) != rhs.npos)
      rhs = GmatStringUtil::Replace(rhs, oldName, newName);
   
   lhsWrapper->RenameObject(oldName, newName);
   
   if (mathTree == NULL)
      rhsWrapper->RenameObject(oldName, newName);
   else
      mathTree->RenameRefObject(type, oldName, newName);
   
   // Update generatingString
   GetGeneratingString();
   
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Assignment::RenameRefObject() leaving <%s>\n", generatingString.c_str());
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Assignment.
 *
 * @return clone of the Assignment.
 */
//------------------------------------------------------------------------------
GmatBase* Assignment::Clone() const
{
   return (new Assignment(*this));
}


//------------------------------------------------------------------------------
// const StringArray& GetStringArrayParameter(const Integer id,
//                                            const Integer index) const
//------------------------------------------------------------------------------
/**
 * Access an array of string data.
 *
 * @param id The integer ID for the parameter.
 * @param index The index when multiple StringArrays are supported.
 *
 * @return The requested StringArray.
 */
//------------------------------------------------------------------------------
const std::string& Assignment::GetGeneratingString(Gmat::WriteMode mode,
                                                   const std::string &prefix,
                                                   const std::string &useName)
{
   std::string gen = prefix + "GMAT " + lhs + " = " + rhs + ";";
   
   #ifdef DEBUG_ASSIGNMENT_SCRIPTING
   MessageInterface::ShowMessage("Assignment command generator is \"%s\"\n",
                                 gen.c_str());
   #endif
   
   generatingString = gen;
   
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}

