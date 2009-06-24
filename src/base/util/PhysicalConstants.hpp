//$Header$
//------------------------------------------------------------------------------
//                           PhysicalConstants
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: D. McGarry
// Created: 1995/07/20 for GSS project
// Modified: 2003/09/16 Linda Jun - Modified to use namespace instead of struct
//           2004/06/09 Wendy Shoan - Removed constants that are specific to 
//                                    celestial bodies.
//           2008/12/16 Matthew Wilkins - Added many constants. All constants
//                                        are from National Institute for 
//                                        Standards CODATA 2006 documentation.
//                                        http://physics.nist.gov/constants
//
/**
 * Provides declarations for commonly used physical constants.
 */
//------------------------------------------------------------------------------
#ifndef PhysicalConstants_hpp
#define PhysicalConstants_hpp

namespace GmatPhysicalConst
{
    //Speed Of Light Constant (exact)
    const Real SPEED_OF_LIGHT_VACUUM            = 299792458.0;  // m/s
    const Real c                                = 299792458.0;  // m/s
    
    // gravitational constant (units: km^3/(kg s^2))
    //const Real UNIVERSAL_GRAVITATIONAL_CONSTANT = 6.673e-20;
    const Real UNIVERSAL_GRAVITATIONAL_CONSTANT = 6.67428e-20;
    const Real UNIVERSAL_GRAVITATIONAL_CONSTANT_UNCERTAINTY = 0.00067e-20;

    // Coulomb force constant
    const Real COULOMB_FORCE_CONSTANT = 8987551787.3681764; // N m^2/C^2

    // Standard acceleration due to gravity (exact, adopted)
    const Real ACCELERATION_OF_GRAVITY = 9.80665; //m/s^2

    // Standard atmosphere (exact, adopted)
    const Real STANDARD_ATMOSPHERE = 101325; // Pa

    // Planck Constants
    const Real PLANCK_CONSTANT = 6.62606896e-34; // Js
    const Real PLANCK_CONSTANT_UNCERTAINTY = 0.00000033e-34; //Js
    const Real PLANCK_CONSTANT_IN_EV = 4.13566733e-15; // eV
    const Real PLANCK_CONSTANT_IN_EV_UNCERTAINTY = 0.00000010e-15; //eV
    const Real DIRAC_CONSTANT = 1.054571628e-34; // Js
    const Real DIRAC_CONSTANT_UNCERTAINTY = 0.00000053e-34; //Js
    const Real PLANCK_LENGTH = 1.616252e-35; // m
    const Real PLANCK_LENGTH_UNCERTAINTY = 0.000081e-35;//m
    const Real PLANCK_MASS = 2.17644e-8; // kg
    const Real PLANCK_MASS_UNCERTAINTY = 0.00011E-8; // kg
    const Real PLANCK_MASS_ENERGY_EQUIVALENT_IN_GEV = 1.220892e-19; //GeV
    const Real PLANCK_MASS_ENERGY_EQUIVALENT_IN_GEV_UNCERTAINTY = 0.000061e-19; //Gev
    const Real PLANCK_TEMPERATURE = 1.416785e32; // K
    const Real PLANCK_TEMPERATURE_UNCERTAINTY = 0.000071e32; // K
    const Real PLANCK_TIME = 5.39124e-44; //s
    const Real PLANCK_TIME_UNCERTAINTY = 0.00027e-44; //s
    const Real PLANCK_CHARGE = 1.875545870e-18; // C
    const Real PLANCK_CHARGE_UNCERTAINTY = 0.00000047e-18; //C
    
    //Astronomical Constants
    const Real ASTRONOMICAL_UNIT = 1.4959787e11;  // m

    // Characteristic Impedance of a Vacuum
    const Real CHARACTERISTIC_IMPEDANCE_OF_VACUUM = 376.730313461; // Ohm

    // Permittivity of free space or of empty space
    // a.k.a. dielectric constant of a vacuum
    const Real VACUUM_PERMITTIVITY = 8.854187817e-12; // A^2 s^4/kg m^3

    // Electric Constant (exact)
    const Real ELECTRIC_CONSTANT = 8.854187817e-12; // F/m
    
    // Magnetic Constant (exact)
    const Real MAGNETIC_CONSTANT = 4e-7*GmatMathUtil::PI; // N/A^2

    // Elementary Charge
    const Real ELEMENTARY_CHARGE = 1.602176487e-19; // C
    const Real ELEMENTARY_CHARGE_UNCERTAINTY = 0.000000040e-19; // C

    // Magnetic Flux Quantum (h/2e)
    const Real MAGNETIC_FLUX_QUANTUM = 2.067833667e-15; // Wb
    const Real MAGNETIC_FLUX_QUANTUM_UNCERTAINTY = 0.000000052e-15; // Wb

    // Conductance Quantum (2e^2/h)
    const Real CONDUCTANCE_QUANTUM = 7.7480917004e-5; // S
    const Real CONDUCTANCE_QUANTUM_UNCERTAINTY = 0.0000000053e-5; // S

    // Electron Mass
    const Real ELECTRON_MASS = 9.10938215e-31; // kg
    const Real ELECTRON_MASS_UNCERTAINTY = 0.00000045e-31; // kg

    // Proton Mass
    const Real PROTON_MASS = 1.672621637e-27; // kg
    const Real PROTON_MASS_UNCERTAINTY = 0.000000083e-27; // kg

    // Proton-Electron Mass Ratio
    const Real PROTON_ELECTRON_MASS_RATIO = 1836.15267247;// unitless
    const Real PROTON_ELECTRON_MASS_RATIO_UNCERTAINTY = 0.00000080;// unitless

    // Fine Structure Constant
    const Real FINE_STRUCTURE_CONSTANT = 7.2973525376e-3; // unitless
    const Real FINE_STRUCTURE_CONSTANT_UNCERTAINTY = 0.0000000050e-3; // unitless

    // Inverse Fine Structure Constant
    const Real INVERSE_FINE_STRUCTURE_CONSTANT = 137.035999679; // unitless
    const Real INVERSE_FINE_STRUCTURE_CONSTANT_UNCERTAINTY = 0.000000094; // unitless

    // Rydberg constant
    const Real RYDBERG_CONSTANT = 10973731.568527; // 1/m
    const Real RYDBERG_CONSTANT_UNCERTAINTY = 0.000073; // 1/m

    // Avagadro constant
    const Real AVAGADRO_CONSTANT = 6.02214179e23; // 1/mol
    const Real AVAGADRO_CONSTANT_UNCERTAINTY = 0.00000030e23; // 1/mol

    // Faraday constant
    const Real FARADAY_CONSTANT = 96485.3399; // C/mol
    const Real FARADAY_CONSTANT_UNCERTAINTY = 0.0024; // C/mol

    // Molar Gas Constant
    const Real MOLAR_GAS_CONSTANT = 8.314472; // J/mol/K
    const Real MOLAR_GAS_CONSTANT_UNCERTAINTY = 0.000015; // J/mol/K

    // Boltzmann Constant
    const Real BOLTZMANN_CONSTANT = 1.3806504e-23; // J/K
    const Real BOLTZMANN_CONSTANT_UNCERTAINTY = 0.0000024e-23; // J/K

    // Stefan-Boltzmann Constant
    const Real STEFAN_BOLTZMANN_CONSTANT = 5.670400e-8; // W/m^2/K^4
    const Real STEFAN_BOLTZMANN_CONSTANT_UNCERTAINTY = 0.000040e-8; // W/m^2/K^4

    // Electron Volt (e/C, Non-SI unit adopted for use)
    const Real ELECTRON_VOLT = 1.602176487e-19; // J
    const Real ELECTRON_VOLT_UNCERTAINTY = 0.000000040e-19; // J

    // (unified) Atomic Mass Unit (Non-SI unit adopted for use)
    const Real ATOMIC_MASS_UNIT = 1.660538782e-27; // kg
    const Real ATOMIC_MASS_UNIT_UNCERTAINTY = 0.000000083e-27; // kg


}

#endif // PhysicalConstants_hpp
