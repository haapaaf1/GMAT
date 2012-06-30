classdef rwalkpn < measerrs.biases.pnoises.pnsingle
    % Class for process noise for random walk biases.
    methods
        function rw = rwalkpn(Q)
            rw = rw@measerrs.biases.pnoises.pnsingle(Q);
        end % function
        function Qd = processNoiseCovariance(rw,dt)
            Qd = rw.processNoiseIntensity*dt;
            rw.processNoiseIntegral = Qd;
            rw.integrationTime = dt;
        end % function
    end % methods
end % classdef