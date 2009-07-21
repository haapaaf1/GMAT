function CellOut = GMATSpacecraftString(string_in);
switch string_in
    case 'ScA'
Sc = {'%-----  Spacecraft A';    %Baseline Spacecraft
        'Create Spacecraft Sc;';
        'GMAT Sc.DateFormat = TAIModJulian;';
        'GMAT Sc.Epoch = ''21545.000000000'';';
        'GMAT Sc.CoordinateSystem = UranusMJ2000Eq;';
        'GMAT Sc.DisplayStateType = Keplerian;';
        'GMAT Sc.SMA = 35000;';
        'GMAT Sc.ECC = 0.0009999999999957077;';
        'GMAT Sc.INC = 12.84999999999996;';
        'GMAT Sc.RAAN = 306.6100000000001;';
        'GMAT Sc.AOP = 314.1900000002609;';
        'GMAT Sc.TA = 99.88769999973877;';
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
        'GMAT Sc.AttitudeCoordinateSystem = ''UranusMJ2000Eq'';';
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