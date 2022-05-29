function [ lik] = policy_range_no_value(P,data)
% for model comparison


%--- Tests a learning model with all interaction parameters but no value
%learning 

S = size(P(1).scale_r,1); % number of samples
O = max(unique(data.stim1)); % number of options
lik = zeros(S,1);
%irrespective of order it needs to be updated
feedback=data.feed;
learn_type=data.learn_type;

condition_left=data.condition_left;
condition_right=data.condition_right;


stim1=data.stim1;
stim2=data.stim2;

epsilon_r=P.epsilon_r;

scale_r=P.scale_r;


% Base parameters
invtemp_r=P.invtemp_r;

% second order policy
direct=P.direct;

% second order range adaptation

test=P.test;

three=P.three;
two=P.two; 
three_test=P.three_test;

% valVrankx3 multiply in 3 val and 6 rank, and divide in 3 rank and 6 val (edited)


% Direct Policy
beta_policy_six_two_test=invtemp_r .* direct  ./three .* test    ./ three_test .* two;
beta_policy_three_two_test=invtemp_r .* direct .*three .* test   .* three_test .* two ;

beta_policy_six_two_learn=invtemp_r .* direct ./three  ./ test    .* three_test .* two ;
beta_policy_three_two_learn=invtemp_r .*direct .*three  ./ test    ./ three_test .* two  ;

%Indirct Policy
beta_transfer_six_two_test=invtemp_r ./ direct ./three      .* test    ./ three_test .* two ;
beta_transfer_three_two_test=invtemp_r  ./direct .*three   .* test    .* three_test .* two ;

beta_transfer_six_two_learn=invtemp_r ./direct  ./three    ./ test   .* three_test .* two;
beta_transfer_three_two_learn=invtemp_r ./direct .*three   ./ test   ./ three_test .* two;

% Direct Policy
beta_policy_six_four_test=invtemp_r .* direct  ./three .* test    ./ three_test ./ two;
beta_policy_three_four_test=invtemp_r .* direct .*three .* test   .* three_test ./ two ;

beta_policy_six_four_learn=invtemp_r .* direct ./three  ./ test    .* three_test ./ two ;
beta_policy_three_four_learn=invtemp_r .*direct .*three  ./ test    ./ three_test ./ two  ;

%Indirct Policy
beta_transfer_six_four_test=invtemp_r ./ direct ./three      .* test    ./ three_test ./ two ;
beta_transfer_three_four_test=invtemp_r  ./direct .*three   .* test    .* three_test ./ two ;

beta_transfer_six_four_learn=invtemp_r ./direct  ./three    ./ test   .* three_test ./ two;
beta_transfer_three_four_learn=invtemp_r ./direct .*three   ./ test   ./ three_test ./ two;



choice=data.c;
alternative=data.alternative;




%-----------------------------
... {Policy Representation
    %------------------------------

policy=repmat(epsilon_r,[1,O,O]);
p=repmat(.5,[S,O,O]);
p_range=repmat(.5,[S,O]);
alpha_range=repmat(epsilon_r,[1,O]);
beta_range=repmat(epsilon_r,[1,O]);


stim1=double(data.stim1);
stim2=double(data.stim2);
%
for t=1:data.T
    if data.condition_left(t)==data.condition_right(t)
    choice=data.c(t);
    alternative=data.alternative(t);
    % Like is determined only for no feedback trials
if feedback(t)==0
            switch data.condition_left(t)
                case 1
                    temp_p=beta_policy_three_two_test;
                    temp_indirect=beta_transfer_three_two_test;
                case 2
                    temp_p=beta_policy_three_four_test;
                    temp_indirect=beta_transfer_three_four_test;
                case 3
                    temp_p=beta_policy_six_two_test;
                    temp_indirect=beta_transfer_six_two_test;
                    
                case 4
                    temp_p=beta_policy_six_four_test;
                    temp_indirect=beta_transfer_six_four_test;
            end
        else
            switch data.condition_left(t)
                case 1
                    temp_p=beta_policy_three_two_learn;
                    temp_indirect=beta_transfer_three_two_learn;
                case 2
                    temp_p=beta_policy_three_four_learn;
                    temp_indirect=beta_transfer_three_four_learn;
                case 3
                    temp_p=beta_policy_six_two_learn;
                    temp_indirect=beta_transfer_six_two_learn;
                    
                case 4
                    temp_p=beta_policy_six_four_learn;
                    temp_indirect=beta_transfer_six_four_learn;
            end
            
        end
    
    % direct choice
    p(:,choice,alternative)=policy(:,choice,alternative) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
    p(:,alternative,choice)=policy(:,alternative,choice) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
    
    % new calculation of the range adaption value
    p_range(:,choice)=alpha_range(:,choice)./(alpha_range(:,choice)+beta_range(:,choice));
    p_range(:,alternative)=alpha_range(:,alternative)./(alpha_range(:,alternative)+beta_range(:,alternative));
    
    
    % there is going to a clear -p
    if data. learnedtogether(t)
        numerator= temp_p .* p(:,choice,alternative)+temp_indirect .* p_range(:,choice);
        denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
        denominator(:,2) =temp_p .* p(:,alternative,choice)+temp_indirect .* p_range(:,alternative);
    else
        numerator= temp_indirect .* p_range(:,choice);
        denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
        denominator(:,2)=temp_indirect .* p_range(:,alternative);
        
    end
    % OUR LIKELIHOOD FUNCTION
    lik = lik + numerator - mfUtil1.logsumexp(denominator,2);
    
    
    %-------------------
    ...The learning process
        %----------------------
    if double(data.feed(t))
        
        %% we scale all 3 by the same value
        policy(:,choice,alternative)=scale_r.*policy(:,choice,alternative);
        policy(:,alternative,choice)=scale_r.*policy(:,alternative,choice);
        
        alpha_range(:,choice)=scale_r.* alpha_range(:,choice);
        beta_range(:,choice)=scale_r.* beta_range(:,choice);
        
        
        % we care about the outcome only in feedback trials
        o = data.O(t);
        
        if (data.side(t) && o) || (~data.side(t) && ~o)
            % Outcome is in favor of choosing stim 2 next time
            policy(:,stim2(t),stim1(t))=  policy(:,stim2(t),stim1(t))+1;
            alpha_range(:,stim2(t))=alpha_range(:,stim2(t))+1;
            beta_range(:,stim1(t))=beta_range(:,stim1(t))+1;
            
            
        elseif  (~data.side(t) && o) ||  (data.side(t) && ~o)
            %outcome is in favor of choosing stim 1 next time
            policy(:,stim1(t),stim2(t))=  policy(:,stim1(t),stim2(t))+1;
            alpha_range(:,stim1(t))=alpha_range(:,stim1(t))+1;
            beta_range(:,stim2(t))=beta_range(:,stim2(t))+1;
            
        end
     
        
    end
    end 
end
end

