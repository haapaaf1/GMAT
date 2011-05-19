classdef OrbitVector < Vector

    %----------------------------------------------------------------------
    %  Define the thisect properties
    %----------------------------------------------------------------------

    %-----  Set the public data
    properties  (SetAccess = 'protected')
        positionVector = zeros(3,1);
        velocityVector = zeros(3,1);
        positionMag    = 0;
        velocityMag    = 0;
        SMA  = 7000;
        ECC  = 0;
        INC  = 0;
        RAAN = 0;
        AOP  = 0;
        TA   = 0;
        Energy = 0;
        eccentricityVector = zeros(3,1);
    end

    %----------------------------------------------------------------------
    %  Define the thisect's methods
    %----------------------------------------------------------------------

    methods (Access = 'protected')

        function computeCartesianData(this)
            stateVector         = this.PrimaryBody.GetState(201);
            this.positionVector = stateVector(1:3,1);
            this.velocityVector = stateVector(4:6,1);
            this.positionMag    = norm(this.positionVector);
            this.velocityMag    = norm(this.velocityVector);
        end
        
        function computeEccentrictyVector(this)
           
            %  Compute the cartesion position, velocity, and magnitudes
            this.computeCartesianData();
            
            %  Compute the eccentricity vector
            this.eccentricityVector = ((this.velocityMag^2-this.mu/this.positionMag)*...
                          this.positionVector- dot(this.positionVector,...
                          this.velocityVector)*this.velocityVector)/this.mu;                           
        end
        
        function computeSMA(this)
            this.computeCartesianData();
            this.computeEnergy;
            this.SMA = -this.mu/this.Energy/2;
        end
        
        function computeECC(this)
            this.computeEccentrictyVector();
            this.ECC = norm(this.eccentricityVector);
        end
        
        function computeEnergy(this)
            this.computeCartesianData();
            this.Energy = this.velocityMag^2/2 - this.mu/this.positionMag;
        end
        
    end % method

end % classdef
