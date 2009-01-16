//$Id$
//------------------------------------------------------------------------------
//                              EditorPrefs
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2009/01/15
//
/**
 * Declares EditorPrefs.
 */
//------------------------------------------------------------------------------
#ifndef EditorPrefs_hpp
#define EditorPrefs_hpp

#include <wx/string.h>        // for wxString
#include <wx/stc/stc.h>       // for wxStyledTextCtrl

#define DEFAULT_LANGUAGE _("<default>")
#define PAGE_COMMON _("Common")
#define PAGE_LANGUAGES _("Languages")
#define PAGE_STYLE_TYPES _("Style types")
#define STYLE_TYPES_COUNT 32

//----------------------------------------------------------
// general style types
//----------------------------------------------------------
#define GMAT_STC_TYPE_DEFAULT          0
#define GMAT_STC_TYPE_WORD1            1
#define GMAT_STC_TYPE_WORD2            2
#define GMAT_STC_TYPE_WORD3            3
#define GMAT_STC_TYPE_WORD4            4
#define GMAT_STC_TYPE_WORD5            5
#define GMAT_STC_TYPE_WORD6            6
#define GMAT_STC_TYPE_COMMENT          7
#define GMAT_STC_TYPE_COMMENT_DOC      8
#define GMAT_STC_TYPE_COMMENT_LINE     9
#define GMAT_STC_TYPE_COMMENT_SPECIAL 10
#define GMAT_STC_TYPE_CHARACTER       11
#define GMAT_STC_TYPE_CHARACTER_EOL   12
#define GMAT_STC_TYPE_STRING          13
#define GMAT_STC_TYPE_STRING_EOL      14
#define GMAT_STC_TYPE_DELIMITER       15
#define GMAT_STC_TYPE_PUNCTUATION     16
#define GMAT_STC_TYPE_OPERATOR        17
#define GMAT_STC_TYPE_BRACE           18
#define GMAT_STC_TYPE_COMMAND         19
#define GMAT_STC_TYPE_IDENTIFIER      20
#define GMAT_STC_TYPE_LABEL           21
#define GMAT_STC_TYPE_NUMBER          22
#define GMAT_STC_TYPE_PARAMETER       23
#define GMAT_STC_TYPE_REGEX           24
#define GMAT_STC_TYPE_UUID            25
#define GMAT_STC_TYPE_VALUE           26
#define GMAT_STC_TYPE_PREPROCESSOR    27
#define GMAT_STC_TYPE_SCRIPT          28
#define GMAT_STC_TYPE_ERROR           29

//----------------------------------------------------------
// style bits types
//----------------------------------------------------------
#define GMAT_STC_STYLE_BOLD   1
#define GMAT_STC_STYLE_ITALIC 2
#define GMAT_STC_STYLE_UNDERL 4
#define GMAT_STC_STYLE_HIDDEN 8

//----------------------------------------------------------
// general folding types
//----------------------------------------------------------
#define GMAT_STC_FOLD_COMMENT     1
#define GMAT_STC_FOLD_COMPACT     2
#define GMAT_STC_FOLD_PREPROC     4
#define GMAT_STC_FOLD_HTML       16
#define GMAT_STC_FOLD_HTMLPREP   32
#define GMAT_STC_FOLD_COMMENTPY  64
#define GMAT_STC_FOLD_QUOTESPY  128

//----------------------------------------------------------
// flags
//----------------------------------------------------------
#define GMAT_STC_FLAG_WRAPMODE 16

namespace GmatEditor
{
   
   //----------------------------------------------------------
   // CommonInfo
   //----------------------------------------------------------
   struct CommonInfoType
   {
      // editor functionality prefs
      bool syntaxEnable;
      bool foldEnable;
      bool indentEnable;
      // display defaults prefs
      bool readOnlyInitial;
      bool overTypeInitial;
      bool wrapModeInitial;
      bool displayEOLEnable;
      bool indentGuideEnable;
      bool lineNumberEnable;
      bool longLineOnEnable;
      bool whiteSpaceEnable;
   };
   
   //----------------------------------------------------------
   // LanguageInfoType
   //----------------------------------------------------------   
   struct LanguageInfoType
   {
      wxChar *name;
      wxChar *filepattern;
      int lexer;
      struct
      {
         int type;
         const wxChar *words;
      } styles [STYLE_TYPES_COUNT];
      int folds;
   };
   
   //----------------------------------------------------------
   // StyleInfoType
   //----------------------------------------------------------
   struct StyleInfoType
   {
      wxChar *name;
      wxChar *foreground;
      wxChar *background;
      wxChar *fontname;
      int fontsize;
      int fontstyle;
      int lettercase;
   };
   
   extern const CommonInfoType globalCommonPrefs;
   extern const LanguageInfoType globalLanguagePrefs[];
   extern const int globalLanguagePrefsSize;
   extern const StyleInfoType globalStylePrefs[];
   extern const int globalStylePrefsSize;
   
};

#endif
