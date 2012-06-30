classdef noisefree < measerrs.biases.pnoises.pnoise
    % Class that handles the zero-process noise case.
    methods
        function nf = noisefree(~)
            nf = nf@measerrs.biases.pnoises.pnoise;
        end % function
        function Qd = processNoiseCovariance(~,~)
            Qd = 0;
        end % function
    end % methods
end % classdef