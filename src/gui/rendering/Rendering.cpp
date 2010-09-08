//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Phillip Silvia, Jr.
// Created: 2009/06/17
/**
 *
 */
//------------------------------------------------------------------------------

#include "Rendering.hpp"
#include "GmatAppData.hpp"         // for GetGuiInterpreter()
#include "GmatOpenGLSupport.hpp"   // for OpenGL support

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

// Sets the color values for a GlColorType. Purely for convenience
void SetColor(GlColorType color, Byte red, Byte green, Byte blue){
   color.red = red;
   color.green = green;
   color.blue = blue;
}

// Draws a sphere with the given radius, number of slices, stacks, style, etc.
void DrawSphere(GLdouble radius, GLint slices, GLint stacks,
                                GLenum style, GLenum orientation, GLenum normals,
                                GLenum textureCoords)
{
	GLenum error = glGetError();
   GLUquadricObj* qobj = gluNewQuadric();
	error = glGetError();
   gluQuadricDrawStyle(qobj, style);
	error = glGetError();
   gluQuadricNormals(qobj, normals);
	error = glGetError();
   gluQuadricTexture(qobj, textureCoords);
	error = glGetError();
   gluSphere(qobj, radius, slices, stacks);
	error = glGetError();
   gluQuadricOrientation(qobj, orientation);
	error = glGetError();
   gluDeleteQuadric(qobj);
	error = glGetError();
}

// Draw a line of the given color from start to end
void DrawLine(GlColorType *color, Rvector3 start, Rvector3 end){
   glPushMatrix();
   glBegin(GL_LINES);

   glColor3ub(color->red, color->green, color->blue);

   glVertex3f(start[0], start[1], start[2]);

   glVertex3f(end[0], end[1], end[2]);

   glEnd();
   glPopMatrix();
}

// Draw a line of the given color from start to end
void DrawLine(float red, float green, float blue, Rvector3 start, Rvector3 end){
   glPushMatrix();
   glBegin(GL_LINES);

   glColor3f(red, green, blue);

   glVertex3f(start[0], start[1], start[2]);

   glVertex3f(end[0], end[1], end[2]);

   glEnd();
   glPopMatrix();
}

void DrawCube(float x, float y, float z){
   glBegin(GL_QUADS);

   glNormal3f( 0.0F, 0.0F, 1.0F);
   glVertex3f( x, y, z);
   glVertex3f(-x, y, z);
   glVertex3f(-x,-y, z);
   glVertex3f( x,-y, z);

   glNormal3f( 0.0F, 0.0F,-1.0F);
   glVertex3f(-x,-y,-z);
   glVertex3f(-x, y,-z);
   glVertex3f( x, y,-z);
   glVertex3f( x,-y,-z);

   glNormal3f( 0.0F, 1.0F, 0.0F);
   glVertex3f( x, y, z);
   glVertex3f( x, y,-z);
   glVertex3f(-x, y,-z);
   glVertex3f(-x, y, z);

   glNormal3f( 0.0F,-1.0F, 0.0F);
   glVertex3f(-x,-y,-z);
   glVertex3f( x,-y,-z);
   glVertex3f( x,-y, z);
   glVertex3f(-x,-y, z);

   glNormal3f( 1.0F, 0.0F, 0.0F);
   glVertex3f( x, y, z);
   glVertex3f( x,-y, z);
   glVertex3f( x,-y,-z);
   glVertex3f( x, y,-z);

   glNormal3f(-1.0F, 0.0F, 0.0F);
   glVertex3f(-x,-y,-z);
   glVertex3f(-x,-y, z);
   glVertex3f(-x, y, z);
   glVertex3f(-x, y,-z);
   glEnd();
   glFlush();
}

void DrawSpacecraft(float radius, GlColorType *color1, GlColorType *color2){
   glColor3ub(color1->red, color1->green, color1->blue);
   DrawCube(radius, radius, radius*2);
   glColor3ub(color2->red, color2->green, color2->blue);
   DrawCube(radius/4, radius*4, radius*1.5);
}

void DrawEquatorialPlanes(){
}

void DrawCircle(GLUquadricObj *qobj, Real radius){
   gluQuadricDrawStyle(qobj, GLU_LINE  );
   gluQuadricNormals  (qobj, GLU_SMOOTH);
   gluQuadricTexture  (qobj, GL_FALSE  );
   gluDisk(qobj, radius, radius, 50, 1);
}

void DrawStringAt(const wxString &str, GLfloat x, GLfloat y,
                                  GLfloat z, GLfloat k)
{
   glRasterPos4f(x, y, z, k);
   glCallLists(strlen(str.c_str()), GL_BYTE, (GLubyte*)str.c_str());
}