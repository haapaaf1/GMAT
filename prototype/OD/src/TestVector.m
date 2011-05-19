
clear classes

%% Apoapsis Vector Tests 
periapsisVector = PeriapsisVector;
sat = Spacecraft();
periapsisVector.setPrimaryBody(sat);
periapsisVector.setScaleFactor(3);
periapsisVector.setScaleMethod('aFixedLength');
periapsisVector.setLengthValue(5);
periapsisVector.computeVector();
periapsisVector.vectorComponents

%% Periapsis Vector Tests 
apoapsisVector = ApoapsisVector;
apoapsisVector.setPrimaryBody(sat);
apoapsisVector.setScaleFactor(3);
apoapsisVector.setScaleMethod('aFixedLength');
apoapsisVector.setLengthValue(5);
apoapsisVector.computeVector();
apoapsisVector.vectorComponents