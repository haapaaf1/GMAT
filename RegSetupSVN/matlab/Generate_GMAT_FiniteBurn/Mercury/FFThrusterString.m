function CellOut = FFThrusterString(string_in,CSnum_in);
%Thruster A is the standard low thrust, moderate Isp Thruster with complete
%duty cycle and thrust scale factor
switch CSnum_in;
    case 0;
        CoordSys = '0'; %EarthMJ2000Eq
    case 1;
        CoordSys = '1'; %VNB
    case 2;
        CoordSys = '2'; %LVLH
    case 3;
        CoordSys = '3'; %Spacecraft Body
    otherwise
        error(['Invalid CoordSys Input (',num2str(CSnum_in),')']);
end;

switch string_in;
    case 'ThrusterA' %BaseLine Thruster
        Thruster = {'//Define ThrusterA';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0.25;';
                    'engine1.ThrusterC3 = 0.25;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0.25;';
                    'engine1.ThrusterK3 = 0.25;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};
                
    case 'ThrusterB' %Off Direction [1 1 1]
        Thruster = {'//Define ThrusterB';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0.25;';
                    'engine1.ThrusterC3 = 0.25;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0.25;';
                    'engine1.ThrusterK3 = 0.25;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 0.5774; ';
                    'fb.Y_Component 		= 0.5774;';
                    'fb.Z_Component 		= 0.5774;'};
    
    case 'ThrusterC' %Low Duty Cycle
        Thruster = {'//Define ThrusterC';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 0.1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0.25;';
                    'engine1.ThrusterC3 = 0.25;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0.25;';
                    'engine1.ThrusterK3 = 0.25;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};

    case 'ThrusterD' %Low Thrust Scale Factor
        Thruster = {'//Define ThrusterD';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 0.1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0.25;';
                    'engine1.ThrusterC3 = 0.25;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0.25;';
                    'engine1.ThrusterK3 = 0.25;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};
                
    case 'ThrusterE' %Alternate g
        Thruster = {'//Define ThrusterE';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 3.14;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0.25;';
                    'engine1.ThrusterC3 = 0.25;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0.25;';
                    'engine1.ThrusterK3 = 0.25;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};
                
    case 'ThrusterF' %Loaded Thrust Poly
        Thruster = {'//Define ThrusterF';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 1.23758251293888;',
                    'engine1.ThrusterC2 = 0.00730193081644684;';
                    'engine1.ThrusterC3 = 1.06710728099668;';
                    'engine1.ThrusterC4 = 1.44084613514414;';
                    'engine1.ThrusterC5 = 1.12975859384182;';
                    'engine1.ThrusterC6 = 0.866449276427312;';
                    'engine1.ThrusterC7 = 1.26090987550771;';
                    'engine1.ThrusterC8 = 1.12890566239368;';
                    'engine1.ThrusterC9 = 1.25439122773649;';
                    'engine1.ThrusterC10 = 1.78577524273692;';
                    'engine1.ThrusterC11 = 0.523539555272069;';
                    'engine1.ThrusterC12 = 1.15120028332336;';
                    'engine1.ThrusterC13 = 0.832532168870019;';
                    'engine1.ThrusterC14 = 1.26666006242687;';
                    'engine1.ThrusterC15 = 1.09502172813843;';
                    'engine1.ThrusterC16 = -0.702022868622232;';
                    'engine1.ThrusterK1 = 300;';
                    'engine1.ThrusterK2 = 0;';
                    'engine1.ThrusterK3 = 0;';
                    'engine1.ThrusterK4 = 0;';
                    'engine1.ThrusterK5 = 0;';
                    'engine1.ThrusterK6 = 0;';
                    'engine1.ThrusterK7 = 0;';
                    'engine1.ThrusterK8 = 0;';
                    'engine1.ThrusterK9 = 0;';
                    'engine1.ThrusterK10 = 0;';
                    'engine1.ThrusterK11 = 0;';
                    'engine1.ThrusterK12 = 0;';
                    'engine1.ThrusterK13 = 0;';
                    'engine1.ThrusterK14 = 0;';
                    'engine1.ThrusterK15 = 0;';
                    'engine1.ThrusterK16 = 0;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};
                
    case 'ThrusterG' %Loaded Isp Poly
        Thruster = {'//Define ThrusterG';
                    'Create Thruster engine1(tank1);';
                    ' ';
                    '// Do not change thruster orientation, thrust direction is defined in finite burn';
                    'engine1.ThrusterRefX = 0;';
                    'engine1.ThrusterRefY = 0;';
                    'engine1.ThrusterRefZ = 1;';
                    'engine1.ThrusterOrientX = 1; //Don''t change';
                    'engine1.ThrusterOrientY = 0; //Don''t change';
                    'engine1.ThrusterOrientZ = 0; //Don''t change';
                    ' ';
                    '//Thruster Properties';
                    'engine1.DutyCycle 	= 1;'  ;
                    'engine1.ScaleFactor	= 1;';
                    'engine1.ThrusterGravityConst = 9.81;';
                    'engine1.IspScaleFactor = 1;';
                    ' ';
                    'engine1.ThrusterC1 = 10;',
                    'engine1.ThrusterC2 = 0;';
                    'engine1.ThrusterC3 = 0;';
                    'engine1.ThrusterC4 = 0;';
                    'engine1.ThrusterC5 = 0;';
                    'engine1.ThrusterC6 = 0;';
                    'engine1.ThrusterC7 = 0;';
                    'engine1.ThrusterC8 = 0;';
                    'engine1.ThrusterC9 = 0;';
                    'engine1.ThrusterC10 = 0;';
                    'engine1.ThrusterC11 = 0;';
                    'engine1.ThrusterC12 = 0;';
                    'engine1.ThrusterC13 = 0;';
                    'engine1.ThrusterC14 = 0;';
                    'engine1.ThrusterC15 = 0;';
                    'engine1.ThrusterC16 = 0;';
                    'engine1.ThrusterK1 = 1.19380722364703;';
                    'engine1.ThrusterK2 = 0.24900508127885;';
                    'engine1.ThrusterK3 = 1.13275785148817;';
                    'engine1.ThrusterK4 = 0.883718634158075;';
                    'engine1.ThrusterK5 = 1.12834190490672;';
                    'engine1.ThrusterK6 = 1.09178332650158;';
                    'engine1.ThrusterK7 = 1.22723805227818;';
                    'engine1.ThrusterK8 = 1.67356545761199;';
                    'engine1.ThrusterK9 = 1.22744530093833;';
                    'engine1.ThrusterK10 = 1.38922191588571;';
                    'engine1.ThrusterK11 = 0.593720248185885;';
                    'engine1.ThrusterK12 = 1.02790703214479;';
                    'engine1.ThrusterK13 = 0.8544232536899;';
                    'engine1.ThrusterK14 = 1.22656975016494;';
                    'engine1.ThrusterK15 = 1.07915506152261;';
                    'engine1.ThrusterK16 = -0.551448194853405;';
                    'engine1.ThrusterPC1 = 0;';
                    'engine1.ThrusterPC2 = 1;';
                    ' ';
                    'engine1.ThrusterOn = 1;';
                    ' ';
                    'Attach engine1 to Sc;';
                    ' ';
                    ' ';
                    ' ';
                    'Create FiniteBurn fb;';
                    ['fb.AttitudeSystem 	= ',CoordSys,';'];
                    'fb.X_Component 		= 1; ';
                    'fb.Y_Component 		= 0;';
                    'fb.Z_Component 		= 0;'};
                
    otherwise
        error(['Invalid Thruster Selection (',string_in,')'])
end

CellOut = Thruster;
end