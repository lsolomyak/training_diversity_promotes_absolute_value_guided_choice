function [ tor lik] = policy_range_winning_model(P,data)
% lik function that does the best at fitting the data





S = size(P(1).scale_v,1); % number of samples
O = max(unique(data.stim1)); % number of options
lik = zeros(S,1);
%irrespective of order it needs to be updated
feedback=data.feed;
tor=zeros(1,10); 

condition_left=data.condition_left;
condition_right=data.condition_right;


stim1=data.stim1;
stim2=data.stim2;

epsilon_v=P.epsilon_v;
epsilon_r=P.epsilon_r;

scale_v=P.scale_v;
scale_r=P.scale_r;


% Base parameters
invtemp_v=P.invtemp_v;
invtemp_r=P.invtemp_r;

% second order policy
direct=P.direct;

% second order range adaptation

test=P.test;
testxvalVrank=P.testxvalVrank;

valVrankx3=P.valVrankx3;
three=P.three;

three_test=P.three_test;
two=P.two;
twoxval=P.twoxval; 
twoxtest=P.twoxtest;
% valVrankx3 multiply in 3 val and 6 rank, and divide in 3 rank and 6 val (edited)


% Direct Policy
beta_policy_six_two_test=invtemp_r .* direct  ./three .* test .*valVrankx3    ./testxvalVrank    ./ three_test .* two ./twoxval .*twoxtest;
beta_policy_three_two_test =invtemp_r .* direct .*three .* test ./ valVrankx3   ./testxvalVrank    .* three_test .* two ./twoxval .*twoxtest ;

beta_policy_six_two_learn=invtemp_r .* direct ./three .*valVrankx3  ./ test  .*testxvalVrank  .* three_test .* two ./twoxval ./twoxtest;
beta_policy_three_two_learn=invtemp_r .*direct .*three ./ valVrankx3 ./ test .*testxvalVrank   ./ three_test .* two ./twoxval  ./twoxtest;

%Indirct Policy
beta_transfer_six_two_test=invtemp_r ./ direct ./three   .*valVrankx3   .* test  ./testxvalVrank  ./ three_test .* two ./twoxval .*twoxtest;
beta_transfer_three_two_test =invtemp_r  ./direct .*three  ./ valVrankx3  .* test  ./testxvalVrank .* three_test .* two ./twoxval .*twoxtest;

beta_transfer_six_two_learn=invtemp_r ./direct  ./three  .*valVrankx3   ./ test .*testxvalVrank  .* three_test .* two ./twoxval ./twoxtest;
beta_transfer_three_two_learn=invtemp_r ./direct .*three ./ valVrankx3  ./ test .*testxvalVrank  ./ three_test .* two ./twoxval ./twoxtest;

%-----------------------------
... Q Value Representation
    %------------------------------

% TESTING
%-----------------------------
... Q Value Representation
    %------------------------------

% TESTING
beta_value_six_two_test=invtemp_v   ./three ./ valVrankx3  .* test .*testxvalVrank   ./ three_test .* two .* twoxval .*twoxtest;
beta_value_six_two_learn=invtemp_v   ./three ./ valVrankx3 ./ test   ./testxvalVrank .* three_test .* two  .* twoxval ./twoxtest;

beta_value_three_two_test =invtemp_v    .*three .*valVrankx3  .* test .*testxvalVrank  .* three_test .* two  .* twoxval .*twoxtest;
beta_value_three_two_learn=invtemp_v    .*three .*valVrankx3  ./ test ./testxvalVrank ./ three_test .* two .* twoxval ./twoxtest;

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


choice=data.c;
alternative=data.alternative;


alpha=repmat(epsilon_v,[1,O]);
beta= repmat(epsilon_v,[1,O]);



%q=betarnd(alpha,beta);
q=repmat(.5,[S,O]);

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
                    temp=beta_value_three_two_test;
                case 2
                    temp_p=beta_policy_three_four_test;
                    temp_indirect=beta_transfer_three_four_test;
                    temp=beta_value_three_four_test;
                case 3
                    temp_p=beta_policy_six_two_test;
                    temp_indirect=beta_transfer_six_two_test;
                    temp=beta_value_six_two_test;
                    
                case 4
                    temp_p=beta_policy_six_four_test;
                    temp_indirect=beta_transfer_six_four_test;
                    temp=beta_value_six_four_test;
            end
        else
            switch data.condition_left(t)
                case 1
                    temp_p=beta_policy_three_two_learn;
                    temp_indirect=beta_transfer_three_two_learn;
                    temp=beta_value_three_two_learn;
                case 2
                    temp_p=beta_policy_three_four_learn;
                    temp_indirect=beta_transfer_three_four_learn;
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
        
        % direct choice
        p(:,choice,alternative)=policy(:,choice,alternative) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
        p(:,alternative,choice)=policy(:,alternative,choice) ./ (policy(:,choice,alternative)+policy(:,alternative,choice));
        
        % new calculation of the range adaption value
        p_range(:,choice)=alpha_range(:,choice)./(alpha_range(:,choice)+beta_range(:,choice));
        p_range(:,alternative)=alpha_range(:,alternative)./(alpha_range(:,alternative)+beta_range(:,alternative));
        
       
        % there is going to a clear -p
        if data.learnedtogether(t)
            numerator= temp.*q(:,choice)+temp_p .* p(:,choice,alternative)+temp_indirect .* p_range(:,choice);
            denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
            denominator(:,2)=temp.*q(:,alternative)+temp_p .* p(:,alternative,choice)+temp_indirect .* p_range(:,alternative);
        else
            numerator= temp.*q(:,choice)+temp_indirect .* p_range(:,choice);
            denominator(:,1)=numerator; %temp.*q(:,choice)+temp_p .* p(choice,alternative);
            denominator(:,2)=temp.*q(:,alternative)+temp_indirect .* p_range(:,alternative);
            
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
            
            alpha(:,choice)=scale_v.*alpha(:,choice);
            beta(:,choice)=scale_v.*beta(:,choice);
            
            
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
            
            if o
                alpha(:,choice)=alpha(:,choice)+1;
            else
                beta(:,choice)=beta(:,choice)+1;
            end
            q(:,choice)=(alpha(:,choice)./(alpha(:,choice)+beta(:,choice)));
            q(:,alternative)=(alpha(:,alternative)./(alpha(:,alternative)+beta(:,alternative)));

        end
    end
end

end

