//$Header$
//------------------------------------------------------------------------------
//                               ScriptInterpreter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/09/11
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Class implementation for the ScriptInterpreter
 */
//------------------------------------------------------------------------------



#include "ScriptInterpreter.hpp" // class's header file
#include "Moderator.hpp" // class's header file
#include <fstream>


// Maybe put something like this in the Gmat namespace?
#define REV_STRING "Build 1, November 2003"


ScriptInterpreter *ScriptInterpreter::instance = NULL;

ScriptInterpreter* ScriptInterpreter::Instance()
{
    if (!instance)
        instance = new ScriptInterpreter;
    return instance;
}


// class constructor
ScriptInterpreter::ScriptInterpreter()
{
    moderator = Moderator::Instance();
}

// class destructor
ScriptInterpreter::~ScriptInterpreter()
{
}


/** \brief Parses the input stream, line by line, into GMAT objects
 *
 */
bool ScriptInterpreter::Interpret(void)
{
    if (!initialized)
        Initialize();
    
    return ReadScript();
}


bool ScriptInterpreter::Interpret(std::string &scriptfile)
{
    bool retval = false;
    
    filename = scriptfile;
    std::ifstream inFile(scriptfile.c_str());
    instream = &inFile;
    
    retval = Interpret();
    
    inFile.close();
    instream = NULL;
    
    return retval;
}


bool ScriptInterpreter::Build(void)
{
    if (!initialized)
        Initialize();
        
    return WriteScript();
}


bool ScriptInterpreter::Build(std::string &scriptfile)
{
    bool retval = false;
    
    filename = scriptfile;

    std::ofstream outFile(scriptfile.c_str());
    outstream = &outFile;

    retval = Build();
    
    outFile.close();
    outstream = NULL;
    
    return retval;
}


bool ScriptInterpreter::ReadScript(void)
{
    StringArray::iterator current = cmdmap.begin(), last = cmdmap.end();
    while (current != last) {
       ++current;
    }
    
    if (instream->fail() || instream->eof()) {
        return false;
    }
    
    while (!instream->eof()) {
        if (!ReadLine())
            return false;
        if (!Parse())
            return false;
    } 
    
    return true;
}


bool ScriptInterpreter::ReadLine(void)
{
    char charLine[4096] = "";
    
    instream->getline(charLine, 4095);
    line = charLine;
    
    return true;
}


bool ScriptInterpreter::Parse(void)
{
    // Determine what kind of line we have
    ChunkLine();

    // Process accordingly
    if (!chunks.empty())
    {
        std::vector<std::string*>::iterator phrase = chunks.begin();

        if (**phrase == "Create") {
            // Instantiate the core object
            std::string type, name = "";
            ++phrase;
            type = **phrase;
            ++phrase;
            if (phrase != chunks.end())
                name = **phrase;

            if (!InterpretObject(type, name))
                throw InterpreterException("Unable to create object");
        }

        if (**phrase == "GMAT") {
            // Look up related object(s)
            ++phrase;
            std::string objName = GetToken(**phrase);
            GmatBase *obj = FindObject(objName);
            if (obj == NULL) {
                std::string errstr = objName;
                errstr += ": Object was not found";
                throw InterpreterException(errstr);
            }

            // Set object associations
            std::string objParm = GetToken();
            Integer id = obj->GetParameterID(objParm);

            if (id == Gmat::UNKNOWN_PARAMETER_TYPE)
            {
                // Could be a member object -- check that first
                
                /// @todo Fill in the parsing for multipart strings
                std::string subparm = GetToken();
                if (subparm == "")
                    throw InterpreterException("Assignment string does not parse");
                // Find the owned object
                // Set the parm on the owned object
            }
            else
            {
                // Set parameter data
                ++phrase;
                if (**phrase == "=")
                    ++phrase;
                SetParameter(obj, id, **phrase);
            }
        }

        // Clear the array of words found in the line
        chunks.clear();
    }
    
    return true;
}


bool ScriptInterpreter::WriteScript(void)
{
    *outstream << "% GMAT Script File\n% GMAT Release " << REV_STRING << "\n\n";
     
    // First write out the objects, one type at a time
    StringArray::iterator current;
    StringArray objs;
    
    // Spacecraft
    objs = moderator->GetListOfConfiguredItems(Gmat::SPACECRAFT);
    for (current = objs.begin(); current != objs.end(); ++current)
        if (!BuildObject(*current))
            return false;
    
    // Propagator setups
    objs = moderator->GetListOfConfiguredItems(Gmat::PROP_SETUP);
    for (current = objs.begin(); current != objs.end(); ++current)
        if (!BuildObject(*current))
            return false;
            
    // Command sequence
    
    return true;
}


bool ScriptInterpreter::ConfigureCommand(Command *)
{
    return false;
}


bool ScriptInterpreter::ConfigureMathematics(void)
{
    return false;
}

