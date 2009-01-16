//$Id$
//------------------------------------------------------------------------------
//                              EditorPreferences
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2009/01/15
//
/**
 * Implements EditorPreferences.
 */
//------------------------------------------------------------------------------

#include "EditorPreferences.hpp"

//---------------------------------
// static data
//---------------------------------

//----------------------------------------------------------------------------
//! language types
const GmatEditor::CommonInfoType GmatEditor::globalCommonPrefs =
{
    // editor functionality prefs
    true,  // syntaxEnable
    true,  // foldEnable
    true,  // indentEnable
    // display defaults prefs
    false, // overTypeInitial
    false, // readOnlyInitial
    false, // wrapModeInitial
    false, // displayEOLEnable
    false, // IndentGuideEnable
    true,  // lineNumberEnable
    false, // longLineOnEnable
    false, // whiteSpaceEnable
};

//----------------------------------------------------------------------------
// keywordlists
// GMAT
wxChar* GmatKeywords =
   _T("GMAT Create Global Maneuver Propagate Report Save Stop Toggle ")
   _T("Achieve Vary Target Optimize Minimize PenDown PenUp ")
   _T("For EndFor If Else EndIf While EndWhile Target EndTarget ")
   _T("BeginFiniteBurn EndFiniteBurn BeginScript EndScript");
wxChar* GmatComments =
   _T("%");
// C++
wxChar* CppWordlist1 =
   _T("asm auto bool break case catch char class const const_cast ")
   _T("continue default delete do double dynamic_cast else enum explicit ")
   _T("export extern false float for friend goto if inline int long ")
   _T("mutable namespace new operator private protected public register ")
   _T("reinterpret_cast return short signed sizeof static static_cast ")
   _T("struct switch template this throw true try typedef typeid ")
   _T("typename union unsigned using virtual void volatile wchar_t ")
   _T("while");
wxChar* CppWordlist2 =
   _T("file");
wxChar* CppWordlist3 =
   _T("a addindex addtogroup anchor arg attention author b brief bug c ")
   _T("class code date def defgroup deprecated dontinclude e em endcode ")
   _T("endhtmlonly endif endlatexonly endlink endverbatim enum example ")
   _T("exception f$ f[ f] file fn hideinitializer htmlinclude ")
   _T("htmlonly if image include ingroup internal invariant interface ")
   _T("latexonly li line link mainpage name namespace nosubgrouping note ")
   _T("overload p page par param post pre ref relates remarks return ")
   _T("retval sa section see showinitializer since skip skipline struct ")
   _T("subsection test throw todo typedef union until var verbatim ")
   _T("verbinclude version warning weakgroup $ @ \"\" & < > # { }");

// Python
wxChar* PythonWordlist1 =
   _T("and assert break class continue def del elif else except exec ")
   _T("finally for from global if import in is lambda None not or pass ")
   _T("print raise return try while yield");
wxChar* PythonWordlist2 =
   _T("ACCELERATORS ALT AUTO3STATE AUTOCHECKBOX AUTORADIOBUTTON BEGIN ")
   _T("BITMAP BLOCK BUTTON CAPTION CHARACTERISTICS CHECKBOX CLASS ")
   _T("COMBOBOX CONTROL CTEXT CURSOR DEFPUSHBUTTON DIALOG DIALOGEX ")
   _T("DISCARDABLE EDITTEXT END EXSTYLE FONT GROUPBOX ICON LANGUAGE ")
   _T("LISTBOX LTEXT MENU MENUEX MENUITEM MESSAGETABLE POPUP PUSHBUTTON ")
   _T("RADIOBUTTON RCDATA RTEXT SCROLLBAR SEPARATOR SHIFT STATE3 ")
   _T("STRINGTABLE STYLE TEXTINCLUDE VALUE VERSION VERSIONINFO VIRTKEY");


//----------------------------------------------------------------------------
//! languages
const GmatEditor::LanguageInfoType GmatEditor::globalLanguagePrefs [] = {
   //-------------------------------------------------------
   // GMAT script, function, Matlab scripts
   //-------------------------------------------------------
   {_T("GMAT"),
    _T("*.script;*.m;*.gmf"),
    wxSTC_LEX_MATLAB,
    {{GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_COMMENT, NULL},
     {GMAT_STC_TYPE_COMMENT_LINE, NULL},        // COMMENT LINE
     {GMAT_STC_TYPE_COMMENT_DOC, NULL},
     {GMAT_STC_TYPE_NUMBER, NULL},
     {GMAT_STC_TYPE_WORD1, GmatKeywords},       // KEYWORDS (LOJ: Why this doesn't work?)
     {GMAT_STC_TYPE_STRING, NULL},
     {GMAT_STC_TYPE_CHARACTER, NULL},
     {GMAT_STC_TYPE_UUID, NULL},
     {GMAT_STC_TYPE_PREPROCESSOR, NULL},
     {GMAT_STC_TYPE_OPERATOR, NULL},
     {GMAT_STC_TYPE_IDENTIFIER, NULL},
     {GMAT_STC_TYPE_STRING_EOL, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},              // VERBATIM
     {GMAT_STC_TYPE_REGEX, NULL},
     {GMAT_STC_TYPE_COMMENT_SPECIAL, NULL},      // DOXY
     {GMAT_STC_TYPE_WORD2, NULL},                // EXTRA WORDS
     {GMAT_STC_TYPE_WORD3, NULL},                // DOXY KEYWORDS
     {GMAT_STC_TYPE_ERROR, NULL},                // KEYWORDS ERROR
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL}},
    GMAT_STC_FOLD_COMMENT | GMAT_STC_FOLD_COMPACT |
    GMAT_STC_FOLD_PREPROC},
   // C++
   {_T("C++"),
    _T("*.c;*.cc;*.cpp;*.cxx;*.cs;*.h;*.hh;*.hpp;*.hxx;*.sma"),
    wxSTC_LEX_CPP,
    {{GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_COMMENT, NULL},
     {GMAT_STC_TYPE_COMMENT_LINE, NULL},
     {GMAT_STC_TYPE_COMMENT_DOC, NULL},
     {GMAT_STC_TYPE_NUMBER, NULL},
     {GMAT_STC_TYPE_WORD1, CppWordlist1},   // KEYWORDS
     {GMAT_STC_TYPE_STRING, NULL},
     {GMAT_STC_TYPE_CHARACTER, NULL},
     {GMAT_STC_TYPE_UUID, NULL},
     {GMAT_STC_TYPE_PREPROCESSOR, NULL},
     {GMAT_STC_TYPE_OPERATOR, NULL},
     {GMAT_STC_TYPE_IDENTIFIER, NULL},
     {GMAT_STC_TYPE_STRING_EOL, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},         // VERBATIM
     {GMAT_STC_TYPE_REGEX, NULL},
     {GMAT_STC_TYPE_COMMENT_SPECIAL, NULL}, // DOXY
     {GMAT_STC_TYPE_WORD2, CppWordlist2},   // EXTRA WORDS
     {GMAT_STC_TYPE_WORD3, CppWordlist3},   // DOXY KEYWORDS
     {GMAT_STC_TYPE_ERROR, NULL},           // KEYWORDS ERROR
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL}},
    GMAT_STC_FOLD_COMMENT | GMAT_STC_FOLD_COMPACT |
    GMAT_STC_FOLD_PREPROC},
   // Python
   {_T("Python"),
    _T("*.py;*.pyw"),
    wxSTC_LEX_PYTHON,
    {{GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_COMMENT_LINE, NULL},
     {GMAT_STC_TYPE_NUMBER, NULL},
     {GMAT_STC_TYPE_STRING, NULL},
     {GMAT_STC_TYPE_CHARACTER, NULL},
     {GMAT_STC_TYPE_WORD1, PythonWordlist1},   // KEYWORDS
     {GMAT_STC_TYPE_DEFAULT, NULL},            // TRIPLE
     {GMAT_STC_TYPE_DEFAULT, NULL},            // TRIPLEDOUBLE
     {GMAT_STC_TYPE_DEFAULT, NULL},            // CLASSNAME
     {GMAT_STC_TYPE_DEFAULT, PythonWordlist2}, // DEFNAME
     {GMAT_STC_TYPE_OPERATOR, NULL},
     {GMAT_STC_TYPE_IDENTIFIER, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},            // COMMENT_BLOCK
     {GMAT_STC_TYPE_STRING_EOL, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL}},
    GMAT_STC_FOLD_COMMENTPY | GMAT_STC_FOLD_QUOTESPY},
   // * (any)
   {(wxChar *)DEFAULT_LANGUAGE,
    _T("*.*"),
    wxSTC_LEX_PROPERTIES,
    {{GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},
     {GMAT_STC_TYPE_DEFAULT, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL},
     {-1, NULL}},
    0},
};

const int GmatEditor::globalLanguagePrefsSize = WXSIZEOF(GmatEditor::globalLanguagePrefs);

//----------------------------------------------------------------------------
//! style types
const GmatEditor::StyleInfoType GmatEditor::globalStylePrefs [] = {
   // GMAT_STC_TYPE_DEFAULT
   {_T("Default"),
    _T("BLACK"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_WORD1
   {_T("Keyword1"),
    _T("BLUE"), _T("WHITE"),
    _T(""), 10, GMAT_STC_STYLE_BOLD, 0},

   // GMAT_STC_TYPE_WORD2
   {_T("Keyword2"),
    _T("DARK BLUE"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_WORD3
   {_T("Keyword3"),
    _T("CORNFLOWER BLUE"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_WORD4
   {_T("Keyword4"),
    _T("CYAN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_WORD5
   {_T("Keyword5"),
    _T("DARK GREY"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_WORD6
   {_T("Keyword6"),
    _T("GREY"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_COMMENT
   {_T("Comment"),
    _T("FOREST GREEN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_COMMENT_DOC
   {_T("Comment (Doc)"),
    _T("FOREST GREEN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_COMMENT_LINE
   {_T("Comment line"),
    _T("FOREST GREEN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_COMMENT_SPECIAL
   {_T("Special comment"),
    _T("FOREST GREEN"), _T("WHITE"),
    _T(""), 10, GMAT_STC_STYLE_ITALIC, 0},

   // GMAT_STC_TYPE_CHARACTER
   {_T("Character"),
    _T("KHAKI"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_CHARACTER_EOL
   {_T("Character (EOL)"),
    _T("KHAKI"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_STRING
   {_T("String"),
    _T("BROWN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_STRING_EOL
   {_T("String (EOL)"),
    _T("BROWN"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_DELIMITER
   {_T("Delimiter"),
    _T("ORANGE"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_PUNCTUATION
   {_T("Punctuation"),
    _T("ORANGE"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_OPERATOR
   {_T("Operator"),
    _T("BLACK"), _T("WHITE"),
    _T(""), 10, GMAT_STC_STYLE_BOLD, 0},

   // GMAT_STC_TYPE_BRACE
   {_T("Label"),
    _T("VIOLET"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_COMMAND
   {_T("Command"),
    _T("BLUE"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_IDENTIFIER
   {_T("Identifier"),
    _T("BLACK"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_LABEL
   {_T("Label"),
    _T("VIOLET"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_NUMBER
   {_T("Number"),
    _T("SIENNA"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_PARAMETER
   {_T("Parameter"),
    _T("VIOLET"), _T("WHITE"),
    _T(""), 10, GMAT_STC_STYLE_ITALIC, 0},

   // GMAT_STC_TYPE_REGEX
   {_T("Regular expression"),
    _T("ORCHID"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_UUID
   {_T("UUID"),
    _T("ORCHID"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_VALUE
   {_T("Value"),
    _T("ORCHID"), _T("WHITE"),
    _T(""), 10, GMAT_STC_STYLE_ITALIC, 0},

   // GMAT_STC_TYPE_PREPROCESSOR
   {_T("Preprocessor"),
    _T("GREY"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_SCRIPT
   {_T("Script"),
    _T("DARK GREY"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_ERROR
   {_T("Error"),
    _T("RED"), _T("WHITE"),
    _T(""), 10, 0, 0},

   // GMAT_STC_TYPE_UNDEFINED
   {_T("Undefined"),
    _T("ORANGE"), _T("WHITE"),
    _T(""), 10, 0, 0}

};

const int GmatEditor::globalStylePrefsSize = WXSIZEOF(GmatEditor::globalStylePrefs);
