function CellOut = FFSpacecraftString(string_in);
switch string_in
    case 'ScA'
        Sc = {'Create Spacecraft Sc(prop);';
                'Sc.CentralBody = ''Jupiter''; //Must match prop fm.PlanetFieldType[''CentralBody''] = 0;';
                ' ';
                '//State information';
                'Sc.Epoch 		= 21545.000000000;';
                'Sc.A       = 100000;';
                'Sc.E       = 0.0009999999999957077;';
                'Sc.I       = 12.84999999999996;';
                'Sc.RAAN    = 306.6100000000001;';
                'Sc.W       = 314.1900000002609;';
                'Sc.TA      = 99.88769999973877;';
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