classdef measerr < handle & hgsetget
    % Base Class for Measurement Error Package
    %
    % This package models measurement errors which have the form 
    %       e = b + v, 
    % where b is a systematic bias-like component and v is a random
    % noise-like component. 
    properties
        bias;
        mnoise;
    end % properties
    methods
        function me = measerr(no,bo)
            if nargin < 1
                me.mnoise = measerrs.mnoises.noisefree;
            else
                me.mnoise = no;
            end
            if nargin < 2
                me.bias = measerrs.biases.noiseonly;
            else
                me.bias = bo;
            end
        end % function
    end % methods
    methods(Static)
        %% Self-Test
        %
        function pass = selftest(seed)
            if nargin == 0
                seed = 0;
            end
            RandStream.setDefaultStream(RandStream('mcg16807','Seed',seed));
            pass = true;
            dt = 1;
            q = 1;
            diary selftest.txt
            %%
            disp('Noise-Free:')
            Phichk = 0; Qdchk = 0;
            nf = measerrs.measerr 
            disp('Bias object should have all zero properties:')
            disp(nf.bias)
            pass = pass * ~any(nf.bias.stateCovariance || ...
                nf.bias.stateRealization || ...
                nf.bias.biasStatePartial) 
            disp('Meas. noise object should have zero properties:')
            disp(nf.mnoise)
            pass = pass * ~any(nf.mnoise.covariance || ...
                nf.mnoise.realization) 
            pass = pass * unittest(nf) 
            %%
            disp('Constant-Noise-Only:')
            Phichk = 0; Qdchk = 0;
            cn = measerrs.mnoises.consnoise(1) 
            cno = measerrs.measerr(cn) 
            pass = pass * unittest(cno) 
            %%
            disp('Random Constant w/Const. Meas. Noise:')
            Phichk = 1; Qdchk = 0;
            rk = measerrs.biases.randcons(1) 
            rkcn = measerrs.measerr(cn,rk) 
            pass = pass * unittest(rkcn) 
            %%
            disp('Random Ramp w/Const. Meas. Noise:')
            Phichk = [1 dt; 0 1]; Qdchk = 0;
            rr = measerrs.biases.randramp(eye(2)) 
            rrcn = measerrs.measerr(cn,rr) 
            pass = pass * unittest(rrcn) 
            %%
            disp('Random "Curl" w/Const. Meas. Noise:')
            Phichk = [1 dt dt^2/2; 0 1 dt; 0 0 1]; Qdchk = 0;
            rc = measerrs.biases.randcurl(eye(3)) 
            rccn = measerrs.measerr(cn,rc) 
            pass = pass * unittest(rccn) 
            %%
            disp('Random Walk w/Const. Meas. Noise:')
            Phichk = 1; Qdchk = q*dt;
            rw = measerrs.biases.randwalk(1,q) 
            rwcn = measerrs.measerr(cn,rw) 
            pass = pass * unittest(rwcn) 
            %%
            disp('Random Run w/Const. Meas. Noise:')
            Phichk = [1 dt; 0 1]; Qdchk = q*[dt^3/3 dt^2/2; dt^2/2 dt];
            rr = measerrs.biases.randrun(eye(2),q) 
            rrcn = measerrs.measerr(cn,rr) 
            pass = pass * unittest(rrcn) 
            %%
            disp('Random "Zoom" w/Const. Meas. Noise:')
            Phichk = [1 dt dt^2/2; 0 1 dt; 0 0 1]; 
            Qdchk = q*[...
                dt^5/20 dt^4/8 dt^3/6; ...
                dt^4/8 dt^3/3 dt^2/2; ...
                dt^3/6 dt^2/2 dt];
            rz = measerrs.biases.randzoom(eye(3),q) 
            rzcn = measerrs.measerr(cn,rz) 
            pass = pass * unittest(rzcn) 
            %%
            disp('FOGM w/Const. Meas. Noise:')
            tau = 10;
            Phichk = exp(-dt/tau); 
            Qdchk = q*tau/2*(1-Phichk^2);
            fg = measerrs.biases.fogm(1,q,tau) 
            fgcn = measerrs.measerr(cn,fg) 
            pass = pass * unittest(fgcn) 
            %%
            disp('SOGM w/Const. Meas. Noise:')
            w_n = 1;
            zeta = .7;
            [Phichk,Qdchk] = sogmchks; 
            sg = measerrs.biases.sogm(1,q,w_n,zeta) 
            sgcn = measerrs.measerr(cn,sg) 
            pass = pass * unittest(sgcn) 
            %%
            disp('FSGM w/Const. Meas. Noise:')
            q1 = q; q2 = q;
            [Phichk,Qdchk] = fsgmchks; 
            fs = measerrs.biases.fsgm(1,q1,q2,tau,w_n,zeta) 
            fscn = measerrs.measerr(cn,fs) 
            pass = pass * unittest(fscn) 
            %%
            disp('Random Walk + Random Run w/Const. Meas. Noise:')
            Phichk = [1 dt; 0 1]; 
            Qdchk = q1*[dt 0; 0 0] + q2*[dt^3/3 dt^2/2; dt^2/2 dt]; 
            rwr = measerrs.biases.rwrr(1,q1,q2) 
            rwrcn = measerrs.measerr(cn,rwr) 
            pass = pass * unittest(rwrcn) 
            %%
            disp('Random Walk, Run, & Zoom w/Const. Meas. Noise:')
            q3 = q;
            Phichk = [1 dt dt^2/2; 0 1 dt; 0 0 1]; 
            Qdchk = + q1*[dt 0 0; 0 0 0; 0 0 0] + ...
                q2*[dt^3/3 dt^2/2 0; dt^2/2 dt 0; 0 0 0] + ...
                q3*[dt^5/20 dt^4/8 dt^3/6; ...
                dt^4/8 dt^3/3 dt^2/2; ...
                dt^3/6 dt^2/2 dt];
            rwrz = measerrs.biases.rwrrrz(1,q1,q2,q3) 
            rwrzcn = measerrs.measerr(cn,rwrz) 
            pass = pass * unittest(rwrzcn) 
            %%
            diary off
            %% Self-Test Helper Function
            % As a sub-function to selftest, this shares selftest's
            % workspace.
            function pass = unittest(obj)
                tol = 10*eps;
                pass = true;
                disp('Non-Zero Meas. Noise Realizations Should Differ:')
                v1 = obj.mnoise.realization 
                v2 = obj.mnoise.realization 
                pass = pass * (all(v1 ~= v2) || ...
                    (all(v1 == 0) && all(v2 == 0))) 
                disp('Non-Zero Proc. Noise Realizations Should Differ:')
                n1 = obj.bias.processNoiseRealization(dt) 
                n2 = obj.bias.processNoiseRealization(dt) 
                pass = pass * (all(n1 ~= n2) || ...
                    (all(n1 == 0) && all(n2 == 0))) 
                disp('State Error Realizations Should Not Differ:')
                x1 = obj.bias.stateRealization 
                x2 = obj.bias.stateRealization 
                pass = pass * (all(x1 == x2) || ...
                    (all(x1 == 0) && all(x2 == 0))) 
                disp('STM Should Match Test Value:')
                Phi = obj.bias.stateTransitionMatrix(dt) 
                pass = pass * all(all(abs(Phi-Phichk) < tol)) 
                disp('Process Noise Covariance Should Match Test Value:')
                Qd = obj.bias.processNoiseCovariance(dt) 
                pass = pass * all(all(abs(Qd-Qdchk) < tol)) 
            end % function
            %% More Self-test helper functions
            % As sub-functions to selftest, these share selftest's
            % workspace.
            function [Phi,Qd] = sogmchks
                if zeta == 1
                    Phi = exp(-w_n*dt)*[(1+w_n*dt) dt; -w_n^2*dt (1-w_n*dt)];
                    a = exp(-2*w_n*dt);
                    Qd(2,2) = q/(4*w_n) * (1 - a*(1 - 2*w_n*dt + 2*w_n^2*dt^2));
                    Qd(1,1) = q/(4*w_n^3)*(1 - a*(1 + 2*w_n*dt + 2*w_n^2*dt^2));
                    Qd(1,2) = q*dt^2/2*a;
                else
                    wd = w_n*sqrt(1-zeta^2);
                    b = zeta*w_n;
                    a = 1/wd^2*exp(-2*b*dt);
                    if zeta < 1
                        cwt = cos(wd*dt);
                        swt = sin(wd*dt);
                    else
                        cwt = cosh(wd*dt);
                        swt = sinh(wd*dt);
                    end
                    Phi = exp(-b*dt)/wd*[(wd*cwt + b*swt) swt; ...
                        -w_n^2*swt (wd*cwt - b*swt)];
                    Qd(2,2) = q/(4*b) * ...
                        (1 - a*(wd^2 + 2*b^2*swt^2 - 2*b*wd*swt*cwt));
                    Qd(1,1) = q/(4*b*w_n^2) * ...
                        (1 - a*(wd^2 + 2*b^2*swt^2 + 2*b*wd*swt*cwt));
                    Qd(1,2) = q/2*a*swt^2;
                end
                Qd(2,1) = Qd(1,2);
            end % function
            function [Phi,Qd] = fsgmchks
                beta = 1/tau;
                wd = sqrt(w_n^2*(1-zeta^2));
                a = -0.5*(beta + 2*zeta*w_n);
                b = sqrt(wd^2 + beta*zeta*w_n - 1/4*beta^2);
                c = -beta/2 + zeta*w_n;
                a2 = a^2; b2 = b^2; c2 = c^2; f = w_n^2; f2 = f^2;
                ab2 = a2 + b2; 
                if b2 <= 0
                    error('Invalid parameters')
                end
                e2at = exp(2*a*dt);   
                s2bt = sin(2*b*dt);  
                c2bt = cos(2*b*dt);
                em1 = (e2at - 1)/(4*a);   
                fac1 = 1/(4*ab2);
                cbt = cos(b*dt);
                sbt = sin(b*dt);
                Phi = exp(a*dt)/b*[(b*cbt + (a+2*zeta*w_n)*sbt) sbt; ...
                    -w_n^2*sbt (b*cbt + (a+beta)*sbt)];
                term1 = fac1*e2at*s2bt*(b2-c2+2*a*c)/b;
                term2 = fac1*(e2at*c2bt-1)*(a*b2-a*c2-2*b2*c)/b2;
                term3 = fac1*(e2at*(b*s2bt + a*c2bt) - a);
                Qd11q1 = q1*(em1*(1 + c2/b2)+ term1 + term2);
                Qd11q2 = (q2/b2)*(em1 - term3);
                Qd(1,1) = Qd11q1 + Qd11q2 ;
                term4 = fac1*e2at*s2bt*(b2-c2-2*a*c)/b ;
                term5 = fac1*(e2at*c2bt-1)*(a*b2-a*c2+2*b2*c)/b2 ;
                term6 = fac1*(e2at*(b*s2bt + a*c2bt) - a);
                Qd22q2 = q2*(em1*(1+c2/b2) + term4 + term5);
                Qd22q1 = (q1/b2)*f2*(em1 - term6);
                Qd(2,2) = Qd22q2 + Qd22q1 ;
                term7 = fac1*(e2at*((a*b+b*c)*s2bt+(a*c-b2)*c2bt)-(a*c-b2));
                term8 = fac1*(e2at*((b*c-a*b)*s2bt+(a*c-b2)*c2bt)-(a*c-b2)) ;
                Qd12q2 =   (q2/b2)*(-c*em1 + term7);
                Qd12q1 = (q1*f/b2)*(-c*em1 + term8);
                Qd(1,2) = Qd12q2 + Qd12q1 ;
                Qd(2,1) = Qd(1,2);
            end % function
            
        end % function
        
    end % methods(Static)
    
end % classdef