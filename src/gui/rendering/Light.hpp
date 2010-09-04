//$Id$
//------------------------------------------------------------------------------
//                              Light
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// ** Legal **
//
// Author: Phillip Silvia, Jr.
// Created: 2009/08/04
/**
 * Stores information about a light source
 */
//------------------------------------------------------------------------------
#ifndef _LIGHT_H
#define _LIGHT_H

#include "GmatAppData.hpp"   // for defines needed to compile on Mac
#include "Rvector3.hpp"

#ifdef __WXMAC__
#  ifdef __DARWIN__
#    include <OpenGL/gl.h>
#    include <OpenGL/glu.h>
#  else
#    include <gl.h>
#    include <glu.h>
#  endif
#else
#  include <GL/gl.h>
#  include <GL/glu.h>
#endif

class Light{
private:
   GLfloat specular[4];
   Rvector3 position;
   bool directional;
public:

   Light();
   Light(float *position, GLfloat *specularColor);
   Light(float *position);
   Light(Rvector3 position, bool directional);

   void GetPositionf(float *pos);
   Rvector3 GetPosition();
   GLfloat* GetColor();
   bool IsDirectional();

   void SetColor(float red, float green, float blue, float alpha);
   void SetColor(float *color);
   void SetDirectional(bool isDirectional);
   void SetPosition(Rvector3 position);
   void SetPosition(float x, float y, float z);
};

#endif // _LIGHT_H
