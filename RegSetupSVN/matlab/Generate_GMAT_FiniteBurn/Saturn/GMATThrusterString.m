function CellOut = GMATThrusterString(Tstring_in,CSnum_in);
%Thruster A is the standard low thrust, moderate Isp Thruster with complete
%duty cycle and thrust scale factor
switch CSnum_in;
    case 0;
        CoordSys = 'SaturnMJ2000Eq';
        CoordCode = {' '};
        AxesCode = ' ';
        OriginCode = ' ';
    case 1;
        CoordSys = 'SaturnVNB';
        CoordCode = {' ';
                     'Create CoordinateSystem SaturnVNB;';
                     'SaturnVNB.Axes = ObjectReferenced;';
                     'SaturnVNB.Origin = Sc;';
                     'SaturnVNB.Primary = Saturn;';
                     'SaturnVNB.Secondary = Sc;';
                     'SaturnVNB.XAxis = ''V'';';
                     'SaturnVNB.YAxis = ''N'';';
                      ' '};
          AxesCode = ' ';
          OriginCode = ' ';
    case 2;
        CoordSys = 'SaturnLVLH';
        CoordCode = {' ';
                     'Create CoordinateSystem SaturnLVLH;';
                     'SaturnLVLH.Axes = ObjectReferenced;';
                     'SaturnLVLH.Origin = Sc;';
                     'SaturnLVLH.Primary = Saturn;';
                     'SaturnLVLH.Secondary = Sc;';
                     'SaturnLVLH.ZAxis = ''R'';';
                     'SaturnLVLH.YAxis = ''-N'';';
                      ' '};
          AxesCode = ' ';
          OriginCode = ' ';
    case 3;
        CoordSys = 'Local';
        CoordCode = {' '};
        AxesCode = 'GMAT engine1.Axes = SpacecraftBody;';
        OriginCode = 'GMAT engine1.Origin = Saturn;';
    otherwise
        error('Improper Coordinate System Selection')
end;

switch Tstring_in;
    case 'ThrusterA'
Thruster = {'%-----  Thruster A;';      %BaseLine Thruster
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            OriginCode;
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0.25;';
            'GMAT engine1.C3 = 0.25;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0.25;';
            'GMAT engine1.K3 = 0.25;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};
    case 'ThrusterB'
Thruster = {'%-----  Thruster B;';      %Off Direction [1 1 1]
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 0.5774;';
            'GMAT engine1.Element2 = 0.5774;';
            'GMAT engine1.Element3 = 0.5774;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0.25;';
            'GMAT engine1.C3 = 0.25;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0.25;';
            'GMAT engine1.K3 = 0.25;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};        
    case 'ThrusterC'
Thruster = {'%-----  Thruster C;';      %Low Duty Cycle
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 0.1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0.25;';
            'GMAT engine1.C3 = 0.25;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0.25;';
            'GMAT engine1.K3 = 0.25;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};        
    case 'ThrusterD'
Thruster = {'%-----  Thruster D;';      %Low Thrust Scale Factor
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 0.1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0.25;';
            'GMAT engine1.C3 = 0.25;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0.25;';
            'GMAT engine1.K3 = 0.25;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};        
    case 'ThrusterE'
Thruster = {'%-----  Thruster E;';      %Alternate g
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 3.14;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0.25;';
            'GMAT engine1.C3 = 0.25;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0.25;';
            'GMAT engine1.K3 = 0.25;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};        
    case 'ThrusterF'
Thruster = {'%-----  Thruster F;';      %Loaded Thrust Poly
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 1.23758251293888;';
            'GMAT engine1.C2 = 0.00730193081644684;';
            'GMAT engine1.C3 = 1.06710728099668;';
            'GMAT engine1.C4 = 1.44084613514414;';
            'GMAT engine1.C5 = 1.12975859384182;';
            'GMAT engine1.C6 = 0.866449276427312;';
            'GMAT engine1.C7 = 1.26090987550771;';
            'GMAT engine1.C8 = 1.12890566239368;';
            'GMAT engine1.C9 = 1.25439122773649;';
            'GMAT engine1.C10 = 1.78577524273692;';
            'GMAT engine1.C11 = 0.523539555272069;';
            'GMAT engine1.C12 = 1.15120028332336;';
            'GMAT engine1.C13 = 0.832532168870019;';
            'GMAT engine1.C14 = 1.26666006242687;';
            'GMAT engine1.C15 = 1.09502172813843;';
            'GMAT engine1.C16 = -0.702022868622232;';
            'GMAT engine1.K1 = 300;';
            'GMAT engine1.K2 = 0;';
            'GMAT engine1.K3 = 0;';
            'GMAT engine1.K4 = 0;';
            'GMAT engine1.K5 = 0;';
            'GMAT engine1.K6 = 0;';
            'GMAT engine1.K7 = 0;';
            'GMAT engine1.K8 = 0;';
            'GMAT engine1.K9 = 0;';
            'GMAT engine1.K10 = 0;';
            'GMAT engine1.K11 = 0;';
            'GMAT engine1.K12 = 0;';
            'GMAT engine1.K13 = 0;';
            'GMAT engine1.K14 = 0;';
            'GMAT engine1.K15 = 0;';
            'GMAT engine1.K16 = 0;';
            ' '};        
    case 'ThrusterG'
Thruster = {'%-----  Thruster G;';      %Loaded Isp Poly
            'Create Thruster engine1;';
            ['GMAT engine1.CoordinateSystem = ',CoordSys,';'];
            AxesCode;
            'GMAT engine1.Element1 = 1;';
            'GMAT engine1.Element2 = 0;';
            'GMAT engine1.Element3 = 0;';
            'GMAT engine1.DutyCycle = 1;';
            'GMAT engine1.ThrustScaleFactor = 1;';
            'GMAT engine1.DecrementMass = true;';
            'GMAT engine1.Tank = {tank1};';
            'GMAT engine1.GravitationalAccel = 9.81;';
            'GMAT engine1.C1 = 10;';
            'GMAT engine1.C2 = 0;';
            'GMAT engine1.C3 = 0;';
            'GMAT engine1.C4 = 0;';
            'GMAT engine1.C5 = 0;';
            'GMAT engine1.C6 = 0;';
            'GMAT engine1.C7 = 0;';
            'GMAT engine1.C8 = 0;';
            'GMAT engine1.C9 = 0;';
            'GMAT engine1.C10 = 0;';
            'GMAT engine1.C11 = 0;';
            'GMAT engine1.C12 = 0;';
            'GMAT engine1.C13 = 0;';
            'GMAT engine1.C14 = 0;';
            'GMAT engine1.C15 = 0;';
            'GMAT engine1.C16 = 0;';
            'GMAT engine1.K1 = 1.19380722364703;';
            'GMAT engine1.K2 = 0.24900508127885;';
            'GMAT engine1.K3 = 1.13275785148817;';
            'GMAT engine1.K4 = 0.883718634158075;';
            'GMAT engine1.K5 = 1.12834190490672;';
            'GMAT engine1.K6 = 1.09178332650158;';
            'GMAT engine1.K7 = 1.22723805227818;';
            'GMAT engine1.K8 = 1.67356545761199;';
            'GMAT engine1.K9 = 1.22744530093833;';
            'GMAT engine1.K10 = 1.38922191588571;';
            'GMAT engine1.K11 = 0.593720248185885;';
            'GMAT engine1.K12 = 1.02790703214479;';
            'GMAT engine1.K13 = 0.8544232536899;';
            'GMAT engine1.K14 = 1.22656975016494;';
            'GMAT engine1.K15 = 1.07915506152261;';
            'GMAT engine1.K16 = -0.551448194853405;';
            ' '};        
    otherwise
        error(['Invalid Thruster Selection (',string_in,')'])
end

CellOut = [CoordCode;Thruster];
end