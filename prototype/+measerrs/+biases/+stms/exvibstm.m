classdef exvibstm < measerrs.biases.stms.expstm & measerrs.biases.stms.vibstm
    % Class in which bias has exponential and vibratory components.
    methods
        function ev = exvibstm(tau,w_n,zeta)
            ev = ev@measerrs.biases.stms.expstm(tau);
            ev = ev@measerrs.biases.stms.vibstm(w_n,zeta);
        end % function
        function Phi = stateTransitionMatrix(ev,dt)
            tau = ev.timeConstant;
            wn = ev.naturalFrequency;
            z = ev.dampingRatio;
            beta = 1/tau;
            wd = sqrt(wn^2*(1-z^2));
            a = -0.5*(beta + 2*z*wn);
            b = sqrt(wd^2 + beta*z*wn - 1/4*beta^2);
            if b^2 <= 0
                error('Invalid parameters')
            end
            cbt = cos(b*dt);
            sbt = sin(b*dt);
            Phi = exp(a*dt)/b*[(b*cbt + (a+2*z*wn)*sbt) sbt; ...
                -wn^2*sbt (b*cbt + (a+beta)*sbt)];
        end % function
    end % methods
end % classdef