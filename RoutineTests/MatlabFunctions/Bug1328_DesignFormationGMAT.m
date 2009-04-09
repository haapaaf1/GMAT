function [FormationState] = DesignFormationGMAT(allCartStates, refKepState, ...
         initialEpoch, refSatIndex, includeRefInOpt, objStepSize,...
         nominalSideLength, roiLowerBound, numRevs, closeApproachLimit,...
         conStepSizeCA, conScaleFactorCA, outputStateType, objScaleFactor, ...
         posScaleFactor, velScaleFactor)

FormationState = [         -10595.3898514466         -10582.7529382893         -10603.1716505858         -10595.1856736516
          52446.3584375477          52454.1857282467           52460.404727734          52445.0540408962
          27643.3245961415          27652.0538417592           27654.119946817          27656.3793187769
         -1.47996796683845         -1.47972517446437         -1.47918526269951         -1.47980092249698
          1.14275176408659          1.14228450662287          1.14206941036733          1.14237573709213
         0.520165461767337         0.520065442793274         0.519971110795088         0.520448009657451];
     
    
return
%  Design Formation takes a user defined reference orbit, or a vector of
%  cartestian states varies the states of some or all of the spacecraft to
%  maximize the quality factor during the RoI.  There are many settings
%  for the optimization process and they are defined rigorously below.
%
%  Field Name  Dim.  Units  Description
%  ------------------------------------------------------------------------
%  X0            n x 1   km, s   Vector containing the cartesian states of
%                                all spacecraft.  This is the initial guess
%                                used in the optimization process.  The
%                                vector can be empty and the code will create
%                                an initial guess using the orbit defined in
%                                RefOrbElem.  The format is :
%                                  X0 = [x1 y1 z1 vx1 vy1 vz1 x2 y2 z2...
%                                        vx2 vy2 vz2 ... yn vxn vyn vzn]'
%  RefOrbElem    1 x 6    --->   Vector containing reference orbit elements
%                                The elements are in the following order [a
%                                [SMA ECC INC AOP RAAN TA], the units are
%                                km and radians.  If must provide either an
%                                set of reference orbit elements, or the
%                                vector X0 described above.  If you provide
%                                an X0 and ref. orb elements, the software
%                                will use the X0 vector and overwrite the
%                                values for RefOrbElem!!!!
%  InitialEpoch  1 x 1   TDB     The full Julian data for initial state in TDB.
%  RefSatIndex   1 x 1   N/A     The index for the reference spacecraft.
%                                For now, this should always be one!!!
%                                Modifications for switching the refsat are
%                                not fully tested!!!
%  IncludeRefInOptimization      Flag to tell software to vary refsat in
%                1 x 1   N/A     the optimization, or to keep the ref sat states
%                                constant and remove them from the the optimization
%                                variables.  When IncludeRefInOptimization
%                                is zero, the ref sat is not included in
%                                the optimization, when it is one, it is
%                                included in the optimization.
%  StepSize      1 x 1   sec.    Step size for propagation in the cost function.         
%  NominalSideLength             The nominal (desired) side length.  This
%                1 x 1   km.     must be one of [10,25,60,160,400];
%  RoILowerBound 1 x 1   Re      The distance from Earth, in Earth Radii,
%                                that defines lower bound on the RoI.  All
%                                ephemeris points with the radius magnitude
%                                greater than RoILowerBound are included in
%                                the cost function.
%  NumRevs       1 x 1   N/A     The number of revolutions to optimizer for.
%  CAConstraint  1 x 1   km      The lower bound on the close approach.
%                                The close approach constraint is only applied
%                                when the formation its flag is activated in the file
%                                GetMMSConstraints.  When the contraint is
%                                activated, the close approach constraint
%                                is only applied when the formation is
%                                below 5Re.
% ConstraintStepSize
%               1 x 1    sec.    The step size for propagation for the
%                                ephemeris used in the close approach
%                                constraint.
% ConstraintFactor 
%               1 x 1    N/A     Scale factor for close approach
%                                constraints.
% OutPutStateType
%               1 x 1    N/A     This is a string that must be either 'oe'
%                                to output orbital elements, or 'cart' to
%                                output the cartesian state.  The units are
%                                km, and km/s for the cartesian state
%                                output, and km and radians for orbital
%                                elements.
% Xfactor       n x 1    --->    This is part of the vector used to scale
%                                the cartesian states for the optimization
%                                process.  The lenght must be the same size
%                                or greater than the number of optimization
%                                variables.  So if you are optimizing all
%                                four spacecraft n =24, if you are
%                                optimizing only three spacecraft n = 18,
%                                and so on.  Components
%                                [1:3,7:9,13:15,19:21] scale the position
%                                and terms [4:6,10:12,16:18,22:24] scale
%                                the velocty.  Scaling is performed as
%                                Xnd(i) = (Xd(i) - Xscale(i))/Xfactor(i)
%                                where it is recommended that Xscale(i) = 1
%  CostFactor   1 x 1            Scale factor for the cost function.



%  Recommended Settings For Different Phases 
%                 StepSize    Position    Velocity   CostFactor
%      Phase I     6000          1         1e-4          1
%     Phase II    20000          1         1e-4          1


%  Define constants and globals

global ObjectiveParams OptimizerParams PlotDataParams count rootDir

rootDir = 'c:\FormDesign';  % don't put a slash after the path name!!
d2r = pi/180;

%  Load settings for TrajOpt System
GetTrajOptSettings;           %  This file contains the setting for 
                              %  the numerical optimizer


%--------------------------------------------------------------------------
%-----------------Define Variables for Current Problem---------------------
%--------------------------------------------------------------------------

%  Set up the initial guess.  Supply either the entire state vector for the
%  formation by using X0, or provide a reference orbit using RefOrbElem
%initialGuessType


ObjectiveParams.X0                 = allCartStates;    
ObjectiveParams.RefOrbElem         = refKepState; 
ObjectiveParams.InitialEpoch       = initialEpoch;
ObjectiveParams.RefSatIndex        = refSatIndex;
ObjectiveParams.IncludeRefInOptimization = includeRefInOpt;
ObjectiveParams.StepSize           = objStepSize;
ObjectiveParams.NominalSideLength  = nominalSideLength;
ObjectiveParams.RoILowerBound      = roiLowerBound;
ObjectiveParams.NumRevs            = numRevs;
ObjectiveParams.CAConstraint       = closeApproachLimit;
ObjectiveParams.ConstraintStepSize = conStepSizeCA;    % seconds
ObjectiveParams.ConstraintFactor   = conScaleFactorCA;
ObjectiveParams.OutPutStateType    = outputStateType;    %  'oe' or 'cart'
ObjectiveParams.CostFactor         = objScaleFactor;
ObjectiveParams.Xfactor([1:3,7:9,13:15,19:21],1)   = posScaleFactor;
ObjectiveParams.Xfactor([4:6,10:12,16:18,22:24],1) = velScaleFactor; 
ObjectiveParams.Xscale             = ones(24,1);

%--------------------------------------------------------------------------
%----------------------------Call Optimizer--------------------------------
%--------------------------------------------------------------------------
ScaleConstants                = GetScaleConstants(ObjectiveParams.NominalSideLength);
ObjectiveParams.SizeConstants = {ScaleConstants(1) ScaleConstants(2) ScaleConstants(3) ScaleConstants(4) 1};
[FormationState,OptOutPut]    = GetOptimalFormation( ObjectiveParams );



