classdef LagrangeInterpolator < handle
    
    properties  (SetAccess = 'protected')
        numPoints   
        indVar
    end
     
    methods
        
        %% Set the indepependent variables for the interpolator
        function setIndependentVariables(this,vec)
            
            %  Check that input is a vector.
            [rows,cols] = size(vec);
            if rows ~=1 && cols ~= 1
                disp(['Error In LagrangeInterpolator.SetIndependentVariables'...
                    ': Input must be a vector']);
                return
            end
            
            %  Set the points
            if rows == 1;
                vec = vec';
                this.numPoints = cols;
            else
                this.numPoints = rows;
            end
            this.indVar = vec;
            
        end
        
        %% Get the Lagrange products
        function lagProducts = getLagrangeProducts(this,interpPoint)
            
            % Check that interpPoint is within range of indVar
            if interpPoint < this.indVar(1) || ...
               interpPoint > this.indVar(this.numPoints)
                   disp(['Error In LagrangeInterpolator.getLagrangeProducts'...
                    ': Interpolation point is not contained in vector' ...
                    'of independent variables']);
                return
            end
            
            % Compute the Lagrange products
            lagProducts = ones(this.numPoints,1);
            for i = 1:this.numPoints
                for j = 1:this.numPoints
                    if i ~= j
                       lagProducts(i) = lagProducts(i)*(interpPoint - ...
                           this.indVar(j,1)) / ...
                       (this.indVar(i,1) -...
                        this.indVar(j,1));
                    end
                end
            end
            
        end
        
        %%  Perform the interpolation
        function interpValues = Interpolate(this,interpPoint,funcValues)
            
            %  Check that the input is a vector
             [rows,cols] = size(funcValues);
             if rows ~= this.numPoints
                 disp(['Error In LagrangeInterpolator.Interpolate'...
                     ': Number of rows in input must be the same as the independent variable vector']);
                 return
             end
            
            %  Perform the interpolation
            interpValues = zeros(cols,1);
            lagProducts = getLagrangeProducts(this,interpPoint);
            for idx = 1:this.numPoints
                interpValues = interpValues + funcValues(idx,:)'*lagProducts(idx,1);
            end
            
        end
        
        %%  Compute the differentiation matrix 
        function D = GetDiffMatrix(this)
            
            %  This code was taken from GPOPS v2.4
            n = this.numPoints;
            D = zeros(n,n);
            for j = 1:n;
                for i = 1:n;
                    prod = 1;
                    sum = 0;
                    if j == i
                        for k = 1:n
                            if k~=i
                                sum = sum+1/(this.indVar(i)...
                                            -this.indVar(k));
                            end
                        end
                        D(i,j) = sum;
                    else
                        for k = 1:n
                            if (k~=i) && (k~=j)
                                prod = prod*(this.indVar(i)- ...
                                             this.indVar(k));
                            end
                        end
                        for k = 1:n
                            if k~=j
                                prod = prod/(this.indVar(j)-...
                                       this.indVar(k));
                            end
                        end
                        D(i,j) = prod;
                    end
                end
            end
            
        end
        
        function flag = exist(this)
           flag = 1; 
        end
        %  TODO:  Fix this so we aren't reusing code not compatible with
        %  NOSA.
        %%  Compute the differentiation matrix
%         function diffMatrix = GetDiffMatrixBroken(this)
%             diffMatrix = zeros(this.numPoints,this.numPoints);
%             for k = 1:this.numPoints
%                tauK = this.indVar(k);
%                for i = 1:this.numPoints
%                    tauI = this.indVar(i);
%                     for ell = 1:this.numPoints
%                         numProduct   = 1;
%                         denomProduct = 1;
%                         for j = 1:this.numPoints
%                             tauJ = this.indVar(j);
%                             if j ~= i
%                                 denomProduct = denomProduct*(tauI - tauJ);
%                                 if j ~= ell
%                                     numProduct   = numProduct*(tauK - tauJ);
%                                 end
%                             end
%                             diffMatrix(k,i) = diffMatrix(k,i) + ...
%                                 numProduct/denomProduct;
%                         end
%                     end
%                 end
%             end
%         end
        
    end
   
end