classdef noiseonly < measerrs.biases.bias
    % Class that handles the zero-bias, measurement-noise only case.
    methods
        function no = noiseonly(~)
            no = no@measerrs.biases.bias(0);
            no.biasStatePartial = 0;
            no.stm = measerrs.biases.stms.zerostm;
        end % function
    end % methods
end % classdef