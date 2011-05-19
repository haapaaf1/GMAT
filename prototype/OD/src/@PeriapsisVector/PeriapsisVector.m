classdef PeriapsisVector < OrbitVector

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------

    %-----  Set the public data
    properties  (SetAccess = 'protected')

    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    
    methods
          
        %----- Compute the the numeric values of the vector component
        function vector = computeVector(this)
            
            %  Compute energy, SMA, and ECC
            this.computeSMA();
            this.computeEccentrictyVector();
            this.computeECC(); 
            
            %  Compute the vector
            this.vectorComponents = this.SMA*(1 - this.ECC )*this.eccentricityVector/this.ECC;
            this.scaleVector();
            vector = this.vectorComponents;
            
        end
        
        function obj = Initialize(obj,Sandbox)
            
        end
        
    end % method
    
end % classdef
