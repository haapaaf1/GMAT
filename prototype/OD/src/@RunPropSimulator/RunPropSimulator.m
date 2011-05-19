
%  04/16/09 S. Hughes.  Updated to handle infeasbile measurements.

classdef RunPropSimulator < handle
    
    % Set the public properties
    properties  (SetAccess = 'public')
        
        Simulator
        
    end
    
    % Set the methods
    methods
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------
        function obj = RunPropSimulator(Simulator)
            
            global theSandbox
            
            %----- Add the estimator
            obj.Simulator = theSandbox.GetHandle(Simulator);
            theSandbox.AddCommand(obj);
            
        end
        
        %------------------------------------------------------------------
        %-----  Initialize
        %------------------------------------------------------------------
        
        function obj = Initialize(obj,theSandbox)
            
            %----- Initialize the estimator
            %obj.Simulator.Initialize(theSandbox);
            
        end
        
        %------------------------------------------------------------------
        %----- Prepare to Propagate
        %------------------------------------------------------------------
        
        function RunSim = PreparetoPropagate(RunSim)
            
            %---- KLUDGE TO GET EPOCH FOR PROPAGATOR
            RunSim.Simulator.Propagator.Epoch =RunSim.Simulator.Propagator.PSM.Objects{1}.Epoch;
            
            %Prop.PSV = PSV;
            RunSim.Simulator.Propagator.PSV   = RunSim.Simulator.Propagator.PSM.GetStates();
            
        end % Prepare to Propagate
        
        %------------------------------------------------------------------
        %----- Execute:  Solve the problem.
        %------------------------------------------------------------------
        
        function Execute(RunSim)
            
            global theSandbox
            
            %----- Extract attached objects and variables to shorten later code
            Prop        = RunSim.Simulator.Propagator;
            Simulator   = RunSim.Simulator;
            measManager = Simulator.MeasManager;
            Epochs      = Simulator.SimulationEpochs;
            numEpochs   = size(Epochs,1);
            RunSim      = RunSim.PreparetoPropagate();    % Update PSV
            
            %----- The Simulator Loop
            count = 0;
            figure(1); clf;
            
            hold on;
            [xSphere,ySphere,zSphere] = sphere(50);
            surface(xSphere*6378,ySphere*6378,zSphere*6378);
            axis(.8*[-18000 18000 -18000 18000 -18000 18000]); axis equal;
            view([.1 .1 1])
            
            for i = 1:numEpochs
                
                %  Prop all participants to next epoch
                theSpacecraft = theSandbox.ObjectHandles{1};
                theVector     = theSandbox.ObjectHandles{6};
                Prop = Prop.SteptoEpoch(Epochs(i));
                vector = theVector.computeVector;
                if i >=2
                    
                    plot3( [lastX;theSpacecraft.X],[lastY;theSpacecraft.Y],...
                        [lastZ;theSpacecraft.Z])
                    
                    if isempty(theVector.graphicsOrigin)
                        origin = zeros(3,1);
                    else
                        origin = theVector.graphicsOrigin.GetState(201);
                    end
                    xVec = [origin(1,1); origin(1,1)+vector(1,1)];
                    yVec = [origin(2,1); origin(2,1)+vector(2,1)];
                    zVec = [origin(3,1); origin(3,1)+vector(3,1)];
                    h = plot3(xVec,yVec,zVec,'LineWidth',2);
                    ha = findobj(gca,'Type','line');
                    numHandles = length(ha);
                    if i >= 3
                        delete(ha(3));
                    end
                    drawnow;
                    
                    
                end
                lastX = theSpacecraft.X;
                lastY = theSpacecraft.Y;
                lastZ = theSpacecraft.Z;
                lastVector = vector;
                
                
                
            end
            
            
            
        end % Execute
    end
end