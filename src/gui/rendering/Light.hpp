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

#include "Rvector3.hpp"
#include <gl/gl.h>
#include <gl/glu.h>

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

#endif _LIGHT_H
