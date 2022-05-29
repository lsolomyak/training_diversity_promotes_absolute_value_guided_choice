function [tor, lik] = Decay_model(P,data)
%add decay during trials with no learning

S = size(P(1).scale_v,1); % number of samples
O = max(unique(data.stim1)); % number of options
lik = zeros(S,1);
%irrespective of order it needs to be updated
stim1=data.stim1; stim2=data.stim2;
epsilon_v=P.epsilon_v;
epsilon_r=P.epsilon_r;
scale_v=P.scale_v;
scale_r=P.scale_r;
tor=zeros(1,data.T);
% Base parameters
invtemp_v=P.invtemp_v;
invtemp_r=P.invtemp_r;

% second order policy
direct=P.direct;
test=P.test;
testxvalVrank=P.testxvalVrank;
valVrankx3=P.valVrankx3;
three=P.three;
two=P.two;
twoxval=P.twoxval;
twoxtest=P.twoxtest;

three_test=P.three_test;
 
% Direct Policy

beta_policy_six_two_test=invtemp_r .* direct  ./three .* test .*valVrankx3    ./testxvalVrank    ./ three_test .* two ./twoxval .*twoxtest;



beta_policy_three_two_test =invtemp_r .* direct .*three .* test ./ valVrankx3   ./testxvalVrank    .* three_test .* two ./twoxval .*twoxtest ;

beta_policy_six_two_learn=invtemp_r .* direct ./three .*valVrankx3  ./ test  .*testxvalVrank  .* three_test .* two ./twoxval ./twoxtest;
beta_policy_three_two_learn=invtemp_r .*direct .*three ./ valVrankx3 ./ test .*testxvalVrank   ./ three_test .* two ./twoxval  ./twoxtest;

%Indirect Policy
beta_transfer_six_two_test=invtemp_r ./ direct ./three   .*valVrankx3   .* test  ./testxvalVrank  ./ three_test .* two ./twoxval .*twoxtest;
beta_transfer_three_two_test =invtemp_r  ./direct .*three  ./ valVrankx3  .* test  ./testxvalVrank .* three_test .* two ./twoxval .*twoxtest;

beta_transfer_six_two_learn=invtemp_r ./direct  ./three  .*valVrankx3   ./ test .*testxvalVrank  .* three_test .* two ./twoxval ./twoxtest;
beta_transfer_three_two_learn=invtemp_r ./direct .*three ./ valVrankx3  ./ test .*testxvalVrank  ./ three_test .* two ./twoxval ./twoxtest;


beta_value_six_two_test=invtemp_v   ./three ./ valVrankx3  .* test .*testxvalVrank   ./ three_test .* two .* twoxval .*twoxtest;
beta_value_six_two_learn=invtemp_v   ./three ./ valVrankx3 ./ test   ./testxvalVrank .* three_test .* two  .* twoxval ./twoxtest;

beta_value_three_two_test =invtemp_v    .*three .*valVrankx3  .* test .*testxvalVrank  .* three_test .* two  .* twoxval .*twoxtest;
beta_value_three_two_learn=invtemp_v    .*three .*valVrankx3  ./ test ./testxvalVrank ./ three_test .* two .* twoxval ./twoxtest;

% now we add the fours

% Now All the 4

beta_policy_six_four_test=invtemp_r .* direct  ./three .* test .*valVrankx3    ./testxvalVrank    ./ three_test ./ two .* twoxval ./twoxtest;
beta_policy_three_four_test =invtemp_r .* direct .*three .* test ./ valVrankx3   ./testxvalVrank    .* three_test ./ two  .* twoxval ./twoxtest;

beta_policy_six_four_learn=invtemp_r .* direct ./three .*valVrankx3  ./ test  .*testxvalVrank  .* three_test ./ two  .* twoxval .*twoxtest;
beta_policy_three_four_learn=invtemp_r .*direct .*three ./ valVrankx3 ./ test .*testxvalVrank   ./ three_test ./ two   .* twoxval .*twoxtest;

%Indirct Policy
beta_transfer_six_four_test=invtemp_r ./ direct ./three   .*valVrankx3   .* test  ./testxvalVrank  ./ three_test ./ two  .* twoxval ./twoxtest;
beta_transfer_three_four_test =invtemp_r  ./direct .*three  ./ valVrankx3  .* test  ./testxvalVrank .* three_test ./ two  .* twoxval ./twoxtest;

beta_transfer_six_four_learn=invtemp_r ./direct  ./three  .*valVrankx3   ./ test .*testxvalVrank  .* three_test ./ two .* twoxval .*twoxtest;
beta_transfer_three_four_learn=invtemp_r ./direct .*three ./ valVrankx3  ./ test .*testxvalVrank  ./ three_test ./ two .* twoxval .*twoxtest;

%-----------------------------
... Q Value Representation
    %------------------------------

% TESTING
%-----------------------------
... Q Value Representation
    %------------------------------

% TESTING
beta_value_six_four_test=invtemp_v   ./three ./ valVrankx3  .* test .*testxvalVrank   ./ three_test ./ two ./ twoxval ./twoxtest;
beta_value_six_four_learn=invtemp_v   ./three ./ valVrankx3 ./ test   ./testxvalVrank .* three_test ./ two ./ twoxval .*twoxtest;

beta_value_three_four_test =invtemp_v    .*three .*valVrankx3  .* test .*testxvalVrank  .* three_test ./ two  ./ twoxval ./twoxtest ;
beta_value_three_four_learn=invtemp_v    .*three .*valVrankx3  ./ test ./testxvalVrank ./ three_test ./ two ./ twoxval .*twoxtest;




alpha=repmat(epsilon_v,[1,O]); beta= repmat(epsilon_v,[1,O]);
q=repmat(.5,[S,O]);

%-----------------------------
... {Policy Representation
    %------------------------------

policy=repmat(epsilon_r,[1,O,O]);
p=repmat(.5,[S,O,O]);
p_range=repmat(.5,[S,O]);
alpha_range=repmat(epsilon_r,[1,O]);
beta_range=repmat(epsilon_r,[1,O]);


%
tic 
for t=1:data.T
    choice=data.c(t);
    alternative=data.alternative(t);
    % Like is determined only for no feedback trials
   
    if ~data.feed(t)
        switch data.condition_left(t)
            case 1
                temp_p=beta_policy_three_two_test;
                temp_indirect=beta_transfer_three_two_test;
                
                %during_ decision
                temp=beta_value_three_two_test;
            case 2
                temp_p=beta_policy_three_four_test;
                temp_indirect=beta_transfer_three_four_test;
                
                %during_ decision
                temp=beta_value_three_four_test;
            case 3
                temp_p=beta_policy_six_two_test;
                temp_indirect=beta_transfer_six_two_test;
                %during_ decision
                temp=beta_value_six_two_test;
            case 4
                temp_p=beta_policy_six_four_test;
                temp_indirect=beta_transfer_six_four_test;
                %during_ decision
                temp=beta_value_six_four_test;
        end
    else
        switch data.condition_left(t)
            case 1
                temp_p=beta_policy_three_two_learn;
                temp_indirect=beta_transfer_three_two_learn;
                %during_ decision
                temp=beta_value_three_two_learn;
                %during_ learning
            case 2
                temp_p=beta_policy_three_four_learn;
                temp_indirect=beta_transfer_three_four_learn;
                %during_ decision
                temp=beta_value_three_four_learn;
                
                
            case 3
                temp_p=beta_policy_six_two_learn;
                temp_indirect=beta_transfer_six_two_learn;
                temp=beta_value_six_two_learn;
            case 4
                temp_p=beta_policy_six_four_learn;
                temp_indirect=beta_transfer_six_four_learn;
                temp=beta_value_six_four_learn;
        end
        
    end
   
    if data.learnedtogether(t)
        numerator= temp.* q(:,choice)+temp_p .* p(:,choice,alternative)+temp_indirect .* p_range(:,choice);
        denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
        denominator(:,2)=temp.*q(:,alternative)+temp_p .* p(:,alternative,choice)+temp_indirect .* p_range(:,alternative);
        
        
    else
        
        numerator= temp.*q(:,choice)+temp_indirect .* p_range(:,choice);
        denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
        denominator(:,2)=temp.*q(:,alternative)+temp_indirect .* p_range(:,alternative);
        
    end
    % OUR LIKELIHOOD FUNCTION
    lik = lik + numerator - mfUtil1.logsumexp(denominator,2);
    tor(t)=mean(lik);
    
    
    
    %-------------------
    ...The learning process
        %----------------------
   
    if double(data.feed(t))
        
        %% we scale all 3 by the same value
        
        policy=scale_r.*(policy-epsilon_r)+epsilon_r;
        
 
        alpha_range=scale_r.*(alpha_range-epsilon_r)+epsilon_r;
        
        
        beta_range=scale_r.* (beta_range-epsilon_r)+epsilon_r;
        
        alpha=scale_v .*(alpha - epsilon_v)+epsilon_v;
        beta=scale_v .* (beta- epsilon_v)+epsilon_v;
        
        %fancy a_b
        
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
        % standard a and b
        if o
            alpha(:,choice)=alpha(:,choice)+1;
            
        else
            beta(:,choice)=beta(:,choice)+1;
        end
        q(:,choice)=(alpha(:,choice)./(alpha(:,choice)+beta(:,choice)));
        q(:,alternative)=(alpha(:,alternative)./(alpha(:,alternative)+beta(:,alternative)));
        
        
    end
        
    % direct choice
    p(:,choice,alternative)=policy(:,choice,alternative) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
    p(:,alternative,choice)=policy(:,alternative,choice) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
    
    % new calculation of the range adaption value
    p_range(:,choice)=alpha_range(:,choice)./(alpha_range(:,choice)+beta_range(:,choice));
    p_range(:,alternative)=alpha_range(:,alternative)./(alpha_range(:,alternative)+beta_range(:,alternative));
    
    
% end of trials    
end
toc 
% function ends 
end


