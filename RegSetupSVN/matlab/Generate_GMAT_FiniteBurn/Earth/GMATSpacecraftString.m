function CellOut = GMATSpacecraftString(string_in);
switch string_in
    case 'ScA'
Sc = {'%-----  Spacecraft A';    %Baseline Spacecraft
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScB'
        Sc = {'%-----  Spacecraft A';    %Change Dry Mass
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 1000;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScC'
        Sc = {'%-----  Spacecraft A';    %Change Cd 
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 1.9;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScD'
        Sc = {'%-----  Spacecraft A';    %Change Cr
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.1;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScE'
        Sc = {'%-----  Spacecraft A';    %Change Drag Area
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 20;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScF'
        Sc = {'%-----  Spacecraft A';    %Change Quaternion
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 1;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 0;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScG'
        Sc = {'%-----  Spacecraft A';    %Change Quaternion
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 1;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 0;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScH'
        Sc = {'%-----  Spacecraft A';    %Change Quaternion
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 1;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 1;';
        'GMAT Sc.Q4 = 0;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    case 'ScI'
        Sc = {'%-----  Spacecraft A';    %change SRPArea
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = EarthMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Cartesian;';
        'GMAT Sc.X = 7100;';
        'GMAT Sc.Y = 0;';
        'GMAT Sc.Z = 1300;';
        'GMAT Sc.VX = 0;';
        'GMAT Sc.VY = 7.35;';
        'GMAT Sc.VZ = 1;';
        'GMAT Sc.DryMass = 850;';
        'GMAT Sc.Cd = 2.2;';
        'GMAT Sc.Cr = 1.8;';
        'GMAT Sc.DragArea = 15;';
        'GMAT Sc.SRPArea = 2;';
        'GMAT Sc.Tanks = {tank1}';
        'GMAT Sc.Thrusters = {engine1};';
        'GMAT Sc.Attitude = CoordinateSystemFixed;';
        'GMAT Sc.AttitudeDisplayStateType = ''Quaternion'';';
        'GMAT Sc.AttitudeRateDisplayStateType = ''AngularVelocity'';';
        'GMAT Sc.AttitudeCoordinateSystem = ''EarthMJ2000Eq'';';
        'GMAT Sc.Q1 = 0;';
        'GMAT Sc.Q2 = 0;';
        'GMAT Sc.Q3 = 0;';
        'GMAT Sc.Q4 = 1;';
        'GMAT Sc.EulerAngleSequence = ''312'';';
        'GMAT Sc.AngularVelocityX = 0;';
        'GMAT Sc.AngularVelocityY = 0;';
        'GMAT Sc.AngularVelocityZ = 0;';
        ' '};
    
    otherwise
        error(['Invalid Spacecraft Input (',string_in,')'])
end

    
    CellOut = Sc;
end