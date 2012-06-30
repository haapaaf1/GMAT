classdef rwrrrzpn < measerrs.biases.pnoises.pnmulti
    % Class for process noise for coupled random walk, run & zoom processes.
    methods
        function rwzpn = rwrrrzpn(q1,q2,q3)
            biaspn = measerrs.biases.pnoises.rwrrpn(q1,q2);
            driftpn = measerrs.biases.pnoises.rzoompn(q3);
            rwzpn = rwzpn@measerrs.biases.pnoises.pnmulti(biaspn,driftpn);
        end % function
        function Qd = processNoiseCovariance(rwrpn,dt)
            Qd1 = rwrpn.biaspn.processNoiseCovariance(dt);
            Qd2 = rwrpn.driftpn.processNoiseCovariance(dt);
            Qd = Qd2;
            Qd(1:2,1:2) = Qd(1:2,1:2) + Qd1;
        end % function
    end % methods
end % classdef