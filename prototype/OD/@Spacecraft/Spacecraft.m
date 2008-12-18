classdef Spacecraft < handle

    %  Set the public data
    properties  (SetAccess = 'public')
        Epoch = 21545;
        X  = 7100;
        Y  = 0.0;
        Z  = 1300;
        VX = 0.1;
        VY = 7.35;
        VZ = 1.0;
        Cd = 2.0;
        Cr = 1.7;
        DragArea = 15;
        SRPArea  = 1.0;
        DryMass  = 850;
    end

    %  Set the public data
    properties  (SetAccess = 'protected')
        STM = eye(6,6);
    end

    %  Define the methods
    methods
        
        function e = Initialize(name,dept)
            e.Name = name;
            e.Department = dept;
        end  
        
    end

end
