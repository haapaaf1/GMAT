clc


%% Apoapsis Vector Tests 
periapsisVector = PeriapsisVector;
sat = Spacecraft();
periapsisVector.setPrimaryBody(sat);
periapsisVector.setScaleFactor(3);
periapsisVector.setScaleMethod('FixedLength');
periapsisVector.setLengthValue(5);
periapsisVector.computeVector();
periapsisVector.vectorComponents

%% Periapsis Vector Tests 
apoapsisVector = ApoapsisVector;
apoapsisVector.setPrimaryBody(sat);
apoapsisVector.setScaleFactor(3);
apoapsisVector.setScaleMethod('FixedLength');
apoapsisVector.setLengthValue(5);
apoapsisVector.computeVector();
apoapsisVector.vectorComponents

%% PositionVector Tests
positionVector = PositionVector;
positionVector.setPrimaryBody(sat);
positionVector.setScaleFactor(3);
positionVector.setScaleMethod('FixedLength');
positionVector.setLengthValue(5);
positionVector.computeVector();
positionVector.vectorComponents

%% PositionVector Tests
velocityVector = VelocityVector;
velocityVector.setPrimaryBody(sat);
velocityVector.setScaleFactor(3);
velocityVector.setScaleMethod('aFixedLength');
velocityVector.setLengthValue(5);
velocityVector.computeVector();
velocityVector.vectorComponents

%% Orbit Angular Momentum Tests
orbitAngularMomentumVector = OrbitAngularMomentumVector;
orbitAngularMomentumVector.setPrimaryBody(sat);
orbitAngularMomentumVector.setScaleFactor(3);
orbitAngularMomentumVector.setScaleMethod('aFixedLength');
orbitAngularMomentumVector.setLengthValue(5);
orbitAngularMomentumVector.computeVector();
orbitAngularMomentumVector.vectorComponents;

%% Point To Point Test
Sun = CelestialBody;
Sun.setJPLId(11);
Earth = CelestialBody;
Earth.setJPLId(3);
earthToSunVector = PointToPointVector;
earthToSunVector.setPrimaryBody(Earth);
earthToSunVector.setSecondaryBody(Sun);
earthToSunVector.computeVector();
earthToSunVector.vectorComponents;

return

vectorPlaneSys = VectorAndPrincipalPlane();
vectorPlaneSys.setPrimaryVector(positionVector);
vectorPlaneSys.setSecondaryVector(velocityVector);
