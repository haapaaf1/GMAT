function CellOut = FFTankString(string_in);
switch string_in;
    case 'TankA'        %BaseLine Tank
        Tank = {'Create SphericalTank tank1; //Tank A';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankB'        %Max FuelMass
        Tank = {'Create SphericalTank tank1; //Tank B';
                'tank1.TankMass 			= 820;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankC'        %High Pressure
        Tank = {'Create SphericalTank tank1; //Tank C';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 2500;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankD'        %Low Pressure
        Tank = {'Create SphericalTank tank1; //Tank D';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 100;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankE'        %High Temp
        Tank = {'Create SphericalTank tank1; //Tank E';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 200;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankF'        %Low Temp
        Tank = {'Create SphericalTank tank1; //Tank F';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 2;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankG'        %High Ref Temp
        Tank = {'Create SphericalTank tank1; //Tank G';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 100;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankH'        %Low Ref Temp
        Tank = {'Create SphericalTank tank1; //Tank H';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 2;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankI'        %Extra Ordinary Tank Volume
        Tank = {'Create SphericalTank tank1; //Tank I';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 80.0;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankJ'        %Low Fuel Density
        Tank = {'Create SphericalTank tank1; //Tank J';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 8.0;'; %Appropriate Volume
                'tank1.TankFuelDensity	= 101.325;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankK'        %High Fuel Density
        Tank = {'Create SphericalTank tank1; //Tank K';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 2500;';
                'tank1.TankPressureControl = 1;';
                'Attach tank1 to Sc; '};
    case 'TankL'        %BaseLine Tank
        Tank = {'Create SphericalTank tank1; //Tank L';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankM'        %Max FuelMass
        Tank = {'Create SphericalTank tank1; //Tank M';
                'tank1.TankMass 			= 820;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankN'        %High Pressure
        Tank = {'Create SphericalTank tank1; //Tank N';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 2500;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankO'        %Low Pressure
        Tank = {'Create SphericalTank tank1; //Tank O';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 100;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankP'        %High Temp
        Tank = {'Create SphericalTank tank1; //Tank P';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 200;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankQ'        %Low Temp
        Tank = {'Create SphericalTank tank1; //Tank Q';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 2;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankR'        %High Ref Temp
        Tank = {'Create SphericalTank tank1; //Tank R';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 100;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankS'        %Low Ref Temp
        Tank = {'Create SphericalTank tank1; //Tank S';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 2;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankT'        %Extra Ordinary Tank Volume
        Tank = {'Create SphericalTank tank1; //Tank T';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 80.0;';
                'tank1.TankFuelDensity	= 1029;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankU'        %Low Fuel Density
        Tank = {'Create SphericalTank tank1; //Tank U';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 8.0;'; %Appropriate Volume
                'tank1.TankFuelDensity	= 101.325;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    case 'TankV'        %High Fuel Density
        Tank = {'Create SphericalTank tank1; //Tank V';
                'tank1.TankMass 			= 725;';
                'tank1.TankPressure		= 1200;';
                'tank1.TankTemperature 	= 20;';
                'tank1.TankRefTemperature= 12;';
                'tank1.TankVolume 		= 0.8;';
                'tank1.TankFuelDensity	= 2500;';
                'tank1.TankPressureControl = 0;';
                'Attach tank1 to Sc; '};
    otherwise
        error(['Invalid Tank input (',string_in,')'])
end
CellOut = Tank;
end