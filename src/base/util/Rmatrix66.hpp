//$Id$
//------------------------------------------------------------------------------
//                                  Rmatrix66
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc.
//
// Author: Linda Jun
// Created: 2009.03.30
//
/**
 * Declares linear algebra operations for 6x6 matrices.
 */
//------------------------------------------------------------------------------
#ifndef Rmatrix66_hpp
#define Rmatrix66_hpp

#include "Rmatrix.hpp"

// Forward declarations for operations with vector
class Rvector6;
class Rmatrix33;

class GMAT_API Rmatrix66 : public Rmatrix
{
public:
   Rmatrix66(bool IsIdentityMatrix = true);
   Rmatrix66(int nArgs, Real a1, ...);
   Rmatrix66(const Rmatrix66 &m);
   Rmatrix66(const Rmatrix &m);
   virtual ~Rmatrix66();
   const Rmatrix66& operator=(const Rmatrix66 &m);
   bool operator==(const Rmatrix66 &m)const;
   bool operator!=(const Rmatrix66 &m)const;
   
   void Set(int nArgs, Real a1, ...);
   void SetUndefined();
   
   // Subset of matrix
   Rmatrix33 UpperLeft();
   Rmatrix33 UpperRight();
   Rmatrix33 LowerLeft();
   Rmatrix33 LowerRight();
   
   bool IsOrthogonal(Real accuracyRequired = 
                     GmatRealConst::REAL_EPSILON) const;
   bool IsOrthonormal(Real accuracyRequired = 
                      GmatRealConst::REAL_EPSILON) const;
   
   Rmatrix66 operator+(const Rmatrix66& m) const;
   const Rmatrix66& operator+=(const Rmatrix66& m);
   
   Rmatrix66 operator-(const Rmatrix66& m) const;
   const Rmatrix66& operator-=(const Rmatrix66& m);
   
   Rmatrix66 operator*(const Rmatrix66& m) const;
   const Rmatrix66& operator*=(const Rmatrix66& m);
   
   Rmatrix66 operator/(const Rmatrix66& m) const;
   const Rmatrix66& operator/=(const Rmatrix66& m);
   
   Rmatrix66 operator*(Real scalar) const;
   const Rmatrix66& operator*=(Real scalar);
   
   Rmatrix66 operator/(Real scalar) const;
   const Rmatrix66& operator/=(Real scalar);
   
   Rmatrix66 operator-() const;
   
   friend class Rvector6;
   Rvector6 operator*(const Rvector6& v) const;
   
   friend Rmatrix66 operator*(Real scalar, const Rmatrix66& m);
   
   Real               Trace() const;
   Real               Determinant() const;
   virtual Rmatrix66  Transpose() const;
   virtual Rmatrix66  Inverse() const;
   virtual Rmatrix66  Symmetric() const;
   Rmatrix66          AntiSymmetric() const;
   
   friend Rmatrix66  SkewSymmetric(const Rvector6& v);
   friend Rmatrix66  TransposeTimesMatrix(const Rmatrix66& m1,
                                          const Rmatrix66& m2);
   friend Rmatrix66  MatrixTimesTranspose(const Rmatrix66& m1,
                                          const Rmatrix66& m2); 
   friend Rmatrix66  TransposeTimesTranspose(const Rmatrix66& m1,
                                             const Rmatrix66& m2); 
   
   
private:
};
#endif // Rmatrix66_hpp
