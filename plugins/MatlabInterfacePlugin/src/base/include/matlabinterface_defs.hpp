//$Id$



#ifndef MATLABINTERFACE_DEFS_HPP
#define MATLABINTERFACE_DEFS_HPP

#ifdef _WIN32  // Windows
   #ifdef _MSC_VER  // Microsoft Visual C++

      #define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
      // Windows Header File entry point:
      #include <windows.h>

      #define  _USE_MATH_DEFINES  // makes Msoft define pi

   #endif  // End Microsoft C++ specific block

   #ifdef _DYNAMICLINK  // Only used for Visual C++ Windows DLLs
      #ifdef MATLAB_EXPORTS
         #define MATLAB_API __declspec(dllexport)
      #else
         #define MATLAB_API __declspec(dllimport)
      #endif

      // Provide the storage class specifier (extern for an .exe file, null
      // for DLL) and the __declspec specifier (dllimport for .an .exe file,
      // dllexport for DLL).
      // You must define EXP_STL when compiling the DLL.
      // You can now use this header file in both the .exe file and DLL - a
      // much safer means of using common declarations than two different
      // header files.
      #ifdef EXP_STL
      #    define DECLSPECIFIER __declspec(dllexport)
      #    define EXPIMP_TEMPLATE
      #else
      #    define DECLSPECIFIER __declspec(dllimport)
      #    define EXPIMP_TEMPLATE extern
      #endif

      #define EXPORT_TEMPLATES
   #endif
#endif //  End of OS nits

#ifndef MATLAB_API
   #define MATLAB_API
#endif

#endif // MATLABINTERFACE_DEFS_HPP
