classdef fogmpn < measerrs.biases.pnoises.pnsingle
    % Class for process noise for FOGM biases.
    properties
        timeConstant;
    end % properties
    methods
        function fpn = fogmpn(Q,tau)
            fpn = fpn@measerrs.biases.pnoises.pnsingle(Q);
            fpn.timeConstant = tau;
        end % function
        function Qd = processNoiseCovariance(fpn,dt)
            q = fpn.processNoiseIntensity;
            tau = fpn.timeConstant;
            Qd = q*tau/2*(1 - exp(-2*dt/tau));
            fpn.processNoiseIntegral = Qd;
            fpn.integrationTime = dt;
        end % function
    end % methods
end % classdef