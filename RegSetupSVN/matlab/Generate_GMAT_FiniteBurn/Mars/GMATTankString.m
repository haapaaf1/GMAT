function CellOut = GMATTankString(string_in);
switch string_in
    case 'TankA'
Tank = {'%-----  Fuel Tank A';                 %BaseLine Tank
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};
        
    case 'TankB'
Tank = {'%-----  Fuel Tank B';                 %Max FuelMass
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 820;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};
        
    case 'TankC'
Tank = {'%-----  Fuel Tank C';                 %High Pressure
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 2500;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};
        
    case 'TankD'
Tank = {'%-----  Fuel Tank D';                 %Low Pressure
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 100;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};     
        
    case 'TankE'
Tank = {'%-----  Fuel Tank E';                 %High Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 200;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};    
        
    case 'TankF'
Tank = {'%-----  Fuel Tank F';                 %Low Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 2;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};        
        
    case 'TankG'
Tank = {'%-----  Fuel Tank G';               %High Ref Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 100;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};        
        
    case 'TankH'
Tank = {'%-----  Fuel Tank H';               %Low Ref Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 2;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};        
        
    case 'TankI'
Tank = {'%-----  Fuel Tank I';               %Extra Ordinary Tank Volume
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 80.0;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};        
        
    case 'TankJ'
Tank = {'%-----  Fuel Tank J';               %Low Fuel Density
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 8.0;';         %Appropriate Tank Volume
            'GMAT tank1.FuelDensity = 101.325;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};         
        
    case 'TankK'
Tank = {'%-----  Fuel Tank A';               %High Fuel Density
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 2500;';
            'GMAT tank1.PressureModel = PressureRegulated;';
            ' '};        
        
    case 'TankL'
Tank = {'%-----  Fuel Tank L';                 %BaseLine Tank
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};
        
    case 'TankM'
Tank = {'%-----  Fuel Tank M';                 %Max FuelMass
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 820;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};
        
    case 'TankN'
Tank = {'%-----  Fuel Tank N';                 %High Pressure
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 2500;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};
        
    case 'TankO'
Tank = {'%-----  Fuel Tank O';                 %Low Pressure
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 100;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankP'
Tank = {'%-----  Fuel Tank P';                 %High Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 200;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankQ'
Tank = {'%-----  Fuel Tank Q';                 %Low Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 2;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankR'
Tank = {'%-----  Fuel Tank R';               %High Ref Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 100;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankS'
Tank = {'%-----  Fuel Tank S';               %Low Ref Temp
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 2;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankT'
Tank = {'%-----  Fuel Tank T';               %Extra Ordinary Tank Volume
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 80.0;';
            'GMAT tank1.FuelDensity = 1029;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};        
        
    case 'TankU'
Tank = {'%-----  Fuel Tank U';               %Low Fuel Density
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 8.0;';         %Appropriate Tank Volume
            'GMAT tank1.FuelDensity = 101.325;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};         
        
    case 'TankV'
Tank = {'%-----  Fuel Tank V';               %High Fuel Density
            'Create FuelTank tank1;';
            'GMAT tank1.FuelMass = 725;';
            'GMAT tank1.Pressure = 1200;';
            'GMAT tank1.Temperature = 20;';
            'GMAT tank1.RefTemperature = 12;';
            'GMAT tank1.Volume = 0.8;';
            'GMAT tank1.FuelDensity = 2500;';
            'GMAT tank1.PressureModel = BlowDown;';
            ' '};
        
    otherwise
        error(['Invalid Tank input (',string_in,')'])
end
CellOut = Tank;
end