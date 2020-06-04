

%;----MAIN PROGRAM----

function  [outputll,outputvalue,varargout]=mcmc(modelInput,data,param,varargin)


%MCMC parameter estimator
%based on sipnet
%Note: this can work for multiple data types (likelihood function
%would do the math)


%---REQUIRED INPUTS---
% (1) modelInput is the input data for the model (no requirements on format except for what likelihood and model expect)
% (2) data  is the output data to compare (same as above)
% (3) param is a structure  with the following properties:
%       param.name is an arry of name
%       param.value is parameter value (initial guess) array
%       param.max max value array
%       param.min min value array
%       param.knob is knob array (not used in this version, can be all zero)
%       param.changeable is whether it should be fixed (0) or estimated (1)

%;---OPTIONAL KEYWORDS---
% (1) model is the name of the model function (string) - default is "dalec"
% (2) validdata is % of datapoints in each interval that are valid (default is 100%)  This has the same dimension as data.
% (3) valid_frac is min % of datapoints to accept (default is 50%)

% (4) likelihood is the name of the likelihood function - it should call the model and compute the likelihood - default is "mcmc_likelihood"

% (5) numatonce - how often to check for chain convergence (default is 10000)
% (6) random_start - start at init guess if 0, else randomly within prior (default is 0)
% (7) numchains - number of chains (default is 10)
% (8) numspinups - how much burn in on final iteration (default is 125000)
% (9) iter - max number of iterations both for chains and final (default is 375000)
% (10) ranacc - tuning parameter for % of "worse" likelihoods to accept (default is -1) 
% (11) 'fast', 'medium', and 'superfast' are keywords with different default settings for the above settings - all are faster than default (see below)
% (12) 'quiet' turns off all printing of messages

%;---OUTPUTS---
%;outputll is the likelihood of output values, best value is first
%;outputvalues are accepted parameter values (same format as param.value)
%;outputy is the model run with the best outputvalues parameter set (same format as y)
%0.4 is normal
  aStar = 0.4; %(target rate)
  thresh = 0.02; % (+/- target rate)
  dec = 0.99;
  inc = dec ^ ( (aStar-1)/aStar);
%  add_fraction = 0.5
  add_fraction = 1.0;

  optargin = size(varargin,2);
  optargout = max(nargout,2)-2;
  
 switch optargin
 case 0   %%% Use the defaults if no input arguments
    eval(['model = @ dalec;']);
    
    eval(['likelihood = @ mcmcLikelihood;']);
    numatonce = 10000;  %% Default for normal model mode
    random_start = 1;  %% Default for normal model mode
    numchains = 10;  %% Default for normal model mode
    numspinups = 125000;  %% Default for normal model mode
    iter = 375000;  %% Default for normal model mode
    ranacc = -1;
    modelMode = 'normal';
    quietInput= 'notQuiet';
 
 otherwise
        modelName=varargin{1}.model;
        
        likelihoodName=varargin{1}.likelihood;
        numatonce=varargin{1}.numatonce;
        random_start=varargin{1}.random_start;
        numchains=varargin{1}.numchains;
        numspinups=varargin{1}.numspinups;
        iter=varargin{1}.iter;
        ranacc=varargin{1}.ranacc;
        modelMode=varargin{1}.mode;
        quietInput=varargin{1}.quietMode;   
end     
         
   
 %       modelName=varargin.model;
 %       validdata=varargin.validdata;
 %       valid_frac=varargin.valid_frac;
 %       likelihoodName=varargin.likelihood;
 %       numatonce=varargin.numatonce;
 %       random_start=varargin.random_start;
 %       numchains=varargin.numchains;
 %       numspinups=varargin.numspinups;
 %       iter=varargin.iter;
 %       ranacc=varargin.ranacc;
 %       modelMode=varargin.mode;
 %       quietInput=varargin.quietMode;   
 
 %%% Do some format checking to make sure we have all of the bases covered
 
    if isempty(modelName)==0
        if ischar(modelName)==0
            error('Must display the model name as a string in '' quotes '' ');
        else
           eval(['model = @' modelName ';']); 
        end
    else
        eval(['model = @ dalec;']);
    end

    
    if isempty(likelihoodName)==0
        if ischar(likelihoodName)==0
            error('Must display the likelihood name as a string in '' quotes '' ');
        else
           eval(['likelihood = @' likelihoodName ';']);
        end
    else
        eval(['likelihood = @ mcmcLikelihood;']);
    end
        
    if isempty(modelMode)==0
        if ischar(modelMode)==0
            error('Must display the model mode as a string in '' quotes '' ');
        end
    else
        modelMode = 'normal';
    end   

    
    if isempty(quietInput)==0
        if ischar(quietInput)==0
            error('Must display the quiet mode as a string in '' quotes '' ');
        end
    else
        quietInput = 'notQuiet';
    end   
    
    if ischar(modelMode)==0 & isempty(modelMode)==0
        error('Must display the modelMode as a string in '' quotes '' ');
    end

    if ischar(quietInput)==0 & isempty(quietInput)==0
        error('Must display the quietMode as a string in '' quotes '' ');
    end

   
%%% Need to get this so that it is a string comparison (use strcmp?)
%;fast mode
 if strcmp(modelMode,'fast')==1
    numatonce = 1000;
    random_start = 1;
    numchains = 1;
    numspinups = 5000;
    iter = 10000;
  

%medium mode
 elseif strcmp(modelMode,'medium')==1
    numatonce = 10000;
    random_start = 1;
    numchains = 6;
    numspinups = 70000;
    iter = 150000;
    thresh = 0.025;
   

%;super fast model
 elseif strcmp(modelMode,'superfast')==1
    numatonce = 1000;
    random_start = 0;
    numchains = 1;
    numspinups = 200;
    iter = 10000;


%;set defaults if model mode isn't specified  (can delete this?)
else
  numatonce = 10000;
  random_start = 1;
  numchains = 10;
  numspinups = 125000;
  iter = 375000;
 
end

if isempty(ranacc)==1
   ranacc = -1.0;
end 

  %;set to -5.0 for compelx cost func
  

  
  %%% Set valid indices


%  if isempty(modelName)==1
%  	%%% Handle to model name
%  	eval(['model = @ dalec;']);
%  else
%  	eval(['model = @' modelName ';']);
%  end 
% %   
%  if isempty(likelihoodName)==1
%  	%%% Handle to likelihood name
% 	eval(['likelihood = @ mcmcLikelihood;']);
%  else	
%  	eval(['likelihood = @' likelihoodName ';']);
%  end 
%   
%%% See if we run with display to screen or not.
  	quietMode=strcmp(quietInput,'quiet');


%%% Start coding necessary info for parameter setups.
  maxParam = double(param.max);
  minParam = double(param.min);
  range = maxParam-minParam;
  change = find(param.changeable==1);
  nChange=length(change); 
  verybestll = double(-1e31);

  endvalue=[];
  endll=[];
  endknob=[];
  
  
%;build some chains
  for c = 1:numchains
    if ~quietMode   %%% Display the value if quietMode == 0
      t=sprintf(['Starting Chain ' num2str(c) '. \n']);
      disp(t);
    end
      

%;reset values
    value = double(param.value);
    if (random_start == 1) & (c > 1)  
    	value(change) = minParam(change) + (range(change).* rand(nChange,1));
   	end
%;    knob = double(param.knob)   %%% We may not have this all in here
    knob = repmat(add_fraction,length(range),1);
    converged = 0;
    steps = 0;
%    seed = systime(/sec)   NEED TO GET THE SEED SET HERE
%%% Reset the random seed to the current time
    s = RandStream('mt19937ar','Seed','shuffle');
    RandStream.setGlobalStream(s);
    oldvalue = value;
    bestvalue = value;
    
    %%% Call the model and record the likelihood
    modelOut=model(modelInput,value);  
    outLLa=likelihood(modelOut(1,:),data(1,:)); %gr,nee
    outLLb=likelihood(modelOut(2,:),data(2,:)); %gr,lai
    outLL=outLLa+outLLb; 
    ll = (-1.0) * outLL;
    bestll = ll;
    ll_old = bestll;
    

%;go through the chain until convergence
    while (converged == 0) & (steps < iter)  
      ichgs = ceil(nChange*rand(numatonce,1));
      
      tune = double(rand(numatonce,1))-0.5;
      ran_accept = ranacc * exprnd(1,numatonce,1);
%;-5.0
      yes = 0;
      
 	  for k = 1:numatonce

		%randomly pick a parameter to change
        accept = 1;
        ichg = ichgs(k);
        oldval = value(ichg); 
        newval = (knob(ichg) * range(ichg) * tune(k))+oldval;
        if (newval > maxParam(ichg)) | (newval < minParam(ichg))
        	accept = 0;
        end

%run the model and calculate the likelihood
        if accept==1
          value(ichg) = newval;
   %%%%%       %%% function call to model here
          modelOut=model(modelInput,value);    
          outLL=likelihood(modelOut,data);
          ll = (-1.0) * outLL;
            %%% Reject some chains 
            if (ll<= ll_old) & (ran_accept(k) >= (ll-ll_old))
            	 accept = 0;
          	end  %% (ll <=ll_old)
          	
          	if (accept == 1) && (ll > bestll) 
            	bestll = ll;
            	bestvalue = value;
          	end  %% (accept == 1) && (ll > bestll)
          	
        end		%% accept == 1

%keep track of accepted parameter sets, tune knob
        if accept==1 
          ll_old = ll;
          yes=yes+1;
          knob(ichg)=knob(ichg)*inc;
        else
          value(ichg) = oldval;
          if knob(ichg)*dec < 1e-9
              knob(ichg) = 1e-9;
          else
              knob(ichg)=knob(ichg)*dec;
          end
        end

     end  %%% k=1:numatonce

%;check for convergence of this chain
      steps=steps+numatonce;
      
      if ~quietMode 
      	t=sprintf(['Iteration:  ' num2str(steps) ' Acceptance: ' num2str((yes)/numatonce) ' llmax: ' num2str(bestll) ]);
      	disp(t)
      end  % ~quietMode
      
    
%%%;     	 if float(yes)/numatonce >= a_star THEN BEGIN 

      		if abs((yes)/numatonce - aStar) < thresh   %%% Convergence loop
        		converged = 1;
        		if ~quietMode 
        		   t=sprintf(['Chain ' num2str(c) ' Converged LL: ' num2str(ll) '\n' ]);
        		   disp(t)
        		   t=sprintf(['Parameter Values:']);
                   disp(t)
                   for nC=1:nChange
                         t=sprintf([char(param.name(nC)) ': ' num2str(value(nC))]);
                         disp(t)
                   end
        		  
        		end  % ~quietMode
        		
        		if bestll >= verybestll 
          			if ~quietMode 
          				t=sprintf(['And it is the best chain so far! \n']);
          				disp(t)
          			end  % ~quietMode
          			
          			verybestll = bestll;
          			verybestvalue = bestvalue;
          			endvalue = value;
          			endll = ll;
          			endknob = knob;
        		end  %% bestll >= verybest ll
      			
      		else
      			if ~quietMode 
          			t=sprintf(['Chain  ' num2str(c) ' not yet converged.']);
          			disp(t)
          		end  % ~quietMode
          		
          		yes = 0;
      		
      		end  %%% convergence loop
     
 
    end  %%% while (converged == 0) & (steps < iter)

    if converged ==0
    	if ~quietMode 
          	t=sprintf(['Chain  ' num2str(c) ' did not converge.']);
        	disp(t)
        end  % ~quietMode
    end
    
  end  %%% c = 1:numchains

%;start at end of best chain

  if isempty(endvalue) == 1
    	if ~quietMode 
        	t=sprintf(['No chains converged, try changing mcmc iterations.']);
        	disp(t)
        	t=sprintf(['Starting from best value. \n']);
        	disp(t)
        end  % ~quietMode
    
   	endvalue = bestvalue;
    endll = bestll;
    endknob = knob;
%;    stop
  end

  value = endvalue;
  ll_old = endll;
  knob = endknob;
%  seed = systime(/sec)
%%% Reset the random seed to the current time
    s = RandStream('mt19937ar','Seed','shuffle');
    RandStream.setGlobalStream(s);
  bestvalue = value;
  bestll = endll;

  ichgs = ceil(nChange*rand(iter,1));
  tune = double(rand(iter,1))-0.5;
  ran_accept = ranacc * exprnd(1,iter,1);
  yes = 0;
  yes2 = yes;

  outputll = zeros(iter,1);
  outputvalue = zeros(nChange,iter);

 for k = 1:iter  
    if mod(k,numatonce)==0
    	if ~quietMode 
        	t=sprintf(['Final iteration: ' num2str(k) ' accepted: ' num2str(yes2) ' saved: ' num2str(yes)]);
        	disp(t)
       	end  % ~quietMode
    end  %%% mod(k,numatonce)==0
    
%;randomly pick a parameter to change
    accept = 1;
    ichg = ichgs(k);
    oldval = value(ichg);
    newval = (knob(ichg) * range(ichg) * tune(k))+oldval;
    if (newval > maxParam(ichg)) | (newval < minParam(ichg))
      	accept = 0;
    end

%;run the model and calculate the likelihood
    if accept==1 
      value(ichg) = newval;
      modelOut=model(modelInput,value);    
      outLL=mcmcLikelihood(modelOut,data);
      ll = (-1.0) * outLL;
      
      if (ll <= ll_old) & (ran_accept(k) >= (ll-ll_old))
       	accept = 0;
      end
      
      if (accept== 1) & (ll > bestll) 
        bestll = ll;
        bestvalue = value;
%        if (bestll >= verybestll)
%          verybestll = bestll;
%          verybestvalue = bestvalue;
%        end 
      end  %%% (accept== 1) & (ll > bestll) 
    end  %%% accept== 1

%;output values
    if accept==1 
      yes2=yes2+1;
      ll_old = ll;
%;if past numspinups, then start saving vals
      if k>= numspinups
         yes=yes+1;
         outputll(yes) = ll;
        outputvalue(:,yes) = value;
      end 
    else
      	value(ichg) = oldval;
      end  %% accept==1

  	end   

%;create the history of accepted values
  if yes==0  
    outputvalue = bestvalue;
    outputll = bestll;
  else 
    outputvalue = [bestvalue outputvalue(:,1:yes)];
    outputll = [bestll; outputll(1:yes)];
    [outputll,srt] = sort(outputll,'descend');
    outputvalue = outputvalue(:,srt);
    %outputll = outputll(srt);
  end

%;output values if outputy is there
%  IF arg_present(outputy) THEN BEGIN

if optargout == 1
   varargout{1}=model(modelInput,outputvalue(:,1));
end

%  ENDIF 
outputy=1;
	if ~quietMode 
   	   t=sprintf(['MCMC complete. \n']);
   	   disp(t)
   	   t=sprintf(['Best LL: ' num2str(outputll(1))]);
	   disp(t)
       t=sprintf(['Very Best Parameter Values:']);
       disp(t)
       for nC=1:nChange
           t=sprintf([char(param.name(nC)) ': ' num2str(outputvalue(nC,1)) ]);
           disp(t)
       end
	   
	  
	end  % ~quietMode







