function CellOut = FFSpacecraftString(string_in);
switch string_in
    case 'ScA'
        Sc = {'Create Spacecraft Sc(prop);';
                'Sc.CentralBody = ''Venus''; //Must match prop fm.PlanetFieldType[''CentralBody''] = 0;';
                ' ';
                '//State information';
                'Sc.Epoch 		= 21545.000000000;';
                'Sc.X 			= 7100;';
                'Sc.Y 			= 0;';
                'Sc.Z 			= 1300;';
                'Sc.VX 			= 0;';
                'Sc.VY 			= 7.35;';
                'Sc.VZ 			= 1;';
                ' ';
                '//Physical Information';
                'Sc.VehicleDryMass 	= 850;';
                'Sc.Cd 				= 2.2;';
                'Sc.Cr				= 1.8;';
                'Sc.DragArea 		= 15;';
                'Sc.SRPArea 			= 1;';
                ' ';
                '//Attitude';
                'Sc.Q1 = 0;';
                'Sc.Q2 = 0;';
                'Sc.Q3 = 0;';
                'Sc.Q4 = 1;';
                ' '};
    otherwise
        error(['Invalid Spacecraft Input (',string_in,')'])
end

    
    CellOut = Sc;
end