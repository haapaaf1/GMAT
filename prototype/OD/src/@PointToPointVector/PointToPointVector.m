classdef PointToPointVector < Vector

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
        function vector = computeVector(this)
           postionPrimary    = this.PrimaryBody.GetState();
           positionSecondary = this.SecondaryBody.GetState();
           this.vectorComponents = positionSecondary - postionPrimary;
           this.scaleVector()
           vector = this.vectorComponents;
        end
    end % method

end % classdef