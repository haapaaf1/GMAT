classdef Segment < handle
    
    properties  (SetAccess = 'protected')
        numPoints   = 5
        statesPerPoint;
        initialTime = 21545;
        finalTime   = 21545.1;
    end
    
    properties  (SetAccess = 'private')
        dimensionalTimes
        nondimensionalTimes
    end
    
    methods
        
        function setNumPoints(this,numPoints)
            this.numPoints = numPoints;
        end
        
        function setInitialTime(initialTime)
            this.initialTime = initialTime;
        end
        
        function setFinalTime(finalTime)
            this.finalTime = finalTime;
        end
        
        function setSegmentTimes(this)
            step = 2/(this.numPoints - 1);
            this.nondimensionalTimes = -1:step:1;
            this.dimensionalTimes    = this.nondimensionalTimes*...
                (this.finalTime - this.initialTime)/2 ...
                + (this.finalTime + this.initialTime)/2;
        end
        
        function computeLagrangeProducts(this,tau)
            
            for i = 1:this.numPoints
                product=y(i);
                for j=1:n
                    if(i~=j)
                        product=product*(tau-x(j))/(x(i)-x(j));
                    end
                end
            end
            
        end
        
    end
