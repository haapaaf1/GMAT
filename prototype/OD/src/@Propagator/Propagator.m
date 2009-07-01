classdef Propagator < handle
    
    % Set the public properties
    properties  (SetAccess = 'public')
        CentralBody        = 'Earth';
        PointMasses        = {'Earth'};
        SRP                = 'Off';
        Type               = 'RungeKutta89';
        InitialStepSize    = 60;
        Accuracy           = 1.0e-12;
        MinStep            = 0.001;
        MaxStep            = 2700;
        MaxStepAttempts    = 50;
        FM
        ODEMap    = [];
        PSV       = [];
        PSM
        NumStates = 0;
        Epoch
        ODEmodel
    end
    
    % Set the public properties
    properties  (SetAccess = 'private')
        alpha
        beta
        chi
        pow
        psi
        trace = 0
        hmax
        hmin
        h
        tau
        f
    end
    
    % Set the methods
    methods
        
        %----- Constructor
        function Prop = Propagator(Prop)
            Prop.PSM = PropStateManager();
        end
        
        %-----  Initialize
        function Prop = Initialize(Prop,Sandbox)
            
            %-----  Initialize the integrator
            Prop.alpha = [ 2./27. 1/9 1/6 5/12 .5 5/6 1/6 2/3 1/3 1 0 1 ]';
            Prop.beta = [ [  2/27  0  0   0   0  0  0  0  0  0  0   0  0  ]
                [  1/36 1/12  0  0  0  0  0  0   0  0  0  0  0  ]
                [  1/24  0  1/8  0  0  0  0  0  0  0  0  0  0 ]
                [  5/12  0  -25/16  25/16  0  0  0  0  0  0   0  0  0  ]
                [ .05   0  0  .25  .2  0  0  0  0  0  0  0  0 ]
                [ -25/108  0  0  125/108  -65/27  125/54  0  0  0  0  0  0   0  ]
                [ 31/300  0  0  0  61/225  -2/9  13/900  0  0  0   0  0  0  ]
                [ 2  0  0  -53/6  704/45  -107/9  67/90  3  0  0  0  0  0  ]
                [ -91/108  0  0  23/108  -976/135  311/54  -19/60  17/6  -1/12  0  0  0  0 ]
                [2383/4100 0 0 -341/164 4496/1025 -301/82 2133/4100 45/82 45/164 18/41 0 0 0]
                [ 3/205  0   0  0   0    -6/41  -3/205   -3/41     3/41   6/41   0   0  0 ]
                [-1777/4100 0 0 -341/164 4496/1025 -289/82 2193/4100 ...
                51/82 33/164 12/41 0 1 0]...
                ]';
            Prop.chi = [ 0 0 0 0 0 34/105 9/35 9/35 9/280 9/280 0 41/840 41/840]';
            Prop.psi = [1  0  0  0  0  0  0  0  0  0  1 -1  -1 ]';
            Prop.pow = 1/8;
            
            Prop.hmax = 86400;
            Prop.hmin = 1e-5;
            Prop.h = 60;
            Prop.f = zeros(Prop.NumStates,13);
            %             Prop.tau = tol * max(norm(y, 'inf'), 1);
            
            odeModel = ODEmodel;
            Prop.ODEmodel = odeModel.Initialize(Sandbox);
            
            if strcmp(Prop.FM.CentralBody,'Mercury')
                Prop.FM.CentralBodyIndex = 1;
            end
            
            if strcmp(Prop.FM.CentralBody,'Venus')
                Prop.FM.CentralBodyIndex = 2;
            end
            
            if strcmp(Prop.FM.CentralBody,'Earth')
                Prop.FM.CentralBodyIndex = 3;
            end
            
            if strcmp(Prop.FM.CentralBody,'Mars')
                Prop.FM.CentralBodyIndex = 4;
            end
            
            if strcmp(Prop.FM.CentralBody,'Jupiter')
                Prop.FM.CentralBodyIndex = 5;
            end
            
            if strcmp(Prop.FM.CentralBody,'Saturn')
                Prop.FM.CentralBodyIndex = 6;
            end
            
            if strcmp(Prop.FM.CentralBody,'Uranus')
                Prop.FM.CentralBodyIndex = 7;
            end
            
            if strcmp(Prop.FM.CentralBody,'Neptune')
                Prop.FM.CentralBodyIndex = 8;
            end
            
            if strcmp(Prop.FM.CentralBody,'Pluto')
                Prop.FM.CentralBodyIndex = 9;
            end
            
            if strcmp(Prop.FM.CentralBody,'Luna')
                Prop.FM.CentralBodyIndex = 10;
            end
            
            if strcmp(Prop.FM.CentralBody,'Sun')
                Prop.FM.CentralBodyIndex = 11;
            end
            
            %--------------------------------------------------------------------------
            %  Set DE file indeces for point mass bodies
            %--------------------------------------------------------------------------
            j = 0;
            for i = 1:size(Prop.FM.PointMasses,2);
                
                if strcmp(Prop.FM.PointMasses{i},'Mercury')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 1;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Venus')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 2;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Earth')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 3;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Mars')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 4;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Jupiter')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 5;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Saturn')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 6;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Uranus')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 7;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Neptune')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 8;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Pluto')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 9;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Luna')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 10;
                end
                
                if strcmp(Prop.FM.PointMasses{i},'Sun')
                    j = j + 1;
                    Prop.FM.PointMassIndeces{j} = 11;
                end
                
            end
            
        end
        
        function  Prop = SteptoEpoch(Prop,PropEpoch)
            
            %==============================================================
            %==============================================================
            %---- Step to the requested epoch
            %==============================================================
            %==============================================================
            
            %  Perform the Propagation
            tof = (PropEpoch - Prop.Epoch)*86400;
            if tof > 0
                
                %  Call the integrator
                [t,X] = ODE78('Propagator_ODEmodel', 0,  ...
                    (PropEpoch - Prop.Epoch)*86400, Prop.PSV, Prop.Accuracy , [], [], Prop);
                
                %  Extract state from ephemeris output and update the propagator
                numSteps   = size(t,1);
                X          = X(numSteps,:)';
                Prop.Epoch = Prop.Epoch + t(numSteps)/86400;
                Prop.PSV   = X;
                
                %---- Populate the propagated objects with the new state data
                Prop.PSM.SetStates(X,Prop.Epoch);
                
            end
            
        end % SteptoEpoch
        
        function Prop = Step(Prop,dir)
            
            if nargin < 2 || dir == 1;
                dir = 1;
                Prop.h = abs(Prop.h);
            else
                Prop.h = -Prop.h;
            end
            
           t = Prop.Epoch;
           y = Prop.PSV;
           Prop.f =  y*zeros(1,13);
           step = 1;
            while step
                
                % Compute the slopes

                Prop.f(:,1)       = Prop.ODEmodel.GetDerivative(t,y,Prop);
                for j = 1: 12
                    Prop.f(:,j+1) = Prop.ODEmodel.GetDerivative(t+Prop.alpha(j)*Prop.h,y+Prop.h*Prop.f*Prop.beta(:,j),Prop);
                end
                
                % Estimate the error and the acceptable error
                gamma1 = Prop.h*41/840*Prop.f*Prop.psi;  %  Truncation error
                delta = norm(gamma1,'inf');
                tau = Prop.Accuracy*max(norm(y,'inf'),1.0);
                
                % Update the solution only if the error is acceptable
                if delta <= tau
                    Prop.Epoch = Prop.Epoch + Prop.h/86400;
                    y = y + Prop.h*Prop.f*Prop.chi;
                    Prop.PSV   = y;
                    Prop.PSM.SetStates(y,Prop.Epoch);
                    step = 0;
                end
                
                % Update the step size
                if delta ~= 0.0
                    Prop.h = min(Prop.hmax, 0.8*Prop.h*(tau/delta)^Prop.pow);
                end
                
            end
            
        end
        
    end  % methods
    
end % classdef