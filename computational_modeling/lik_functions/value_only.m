function [ lik] = value_only(P,data)
%% built May 26

% value learning with no relative preference learning 

S = size(P(1).scale,1); % number of samples
O = max(unique(data.stim1)); % number of options
lik = zeros(S,1);
%irrespective of order it needs to be updated
feedback=data.feed;
learn_type=data.learn_type;

condition_left=data.condition_left;
condition_right=data.condition_right;

stim1=data.stim1;
stim2=data.stim2;


% for 3
scale=P.scale; 


invtemp=P.invtemp;
LT=P.LT;
three=P.three;
test=P.test;
LTx3=P.LTx3;
three_test=P.three_test;
two=P.two; 
%-----------------------------
... Q Value Representation
    %------------------------------

% TESTING
beta_six_two_LT_test= invtemp ./three .* LT .* test  ./ LTx3 ./ three_test .* two; 
beta_six_two_NLT_test=invtemp ./three ./ LT .* test  .* LTx3 ./ three_test .* two ;


beta_three_two_LT_test=invtemp .*test .*three  .* LT .* LTx3 .*three_test .* two ;
beta_three_two_NLT_test= invtemp .*test .*three  ./ LT ./LTx3 .*three_test .* two ;

% LEARNING

beta_six_two_LT_learn=invtemp ./test  ./three  .* LT  ./LTx3 .* three_test .* two;
beta_six_two_NLT_learn= invtemp ./test ./three  ./ LT .*LTx3 .* three_test .* two;


beta_three_two_LT_learn=invtemp ./test .*three  .* LT .* LTx3 ./ three_test .* two ;
beta_three_two_NLT_learn= invtemp ./test  .*three  ./ LT ./LTx3 ./ three_test .* two;



% TESTING
beta_six_four_LT_test= invtemp ./three .* LT .* test  ./ LTx3 ./ three_test ./ two; 
beta_six_four_NLT_test=invtemp ./three ./ LT .* test  .* LTx3 ./ three_test ./ two ;


beta_three_four_LT_test=invtemp .*test .*three  .* LT .* LTx3 .*three_test ./ two ;
beta_three_four_NLT_test= invtemp .*test .*three  ./ LT ./LTx3 .*three_test ./ two ;

% LEARNING

beta_six_four_LT_learn=invtemp ./test  ./three  .* LT  ./LTx3 .* three_test ./ two;
beta_six_four_NLT_learn= invtemp ./test ./three  ./ LT .*LTx3 .* three_test ./ two;


beta_three_four_LT_learn=invtemp ./test .*three  .* LT .* LTx3 ./ three_test ./ two ;
beta_three_four_NLT_learn= invtemp ./test  .*three  ./ LT ./LTx3 ./ three_test ./ two;



epsilon=P.epsilon;


alpha=repmat(epsilon,[1,O]);
beta= repmat(epsilon,[1,O]);

%q=betarnd(alpha,beta);
q=repmat(.5,[S,O]);


stim1=double(data.stim1);
stim2=double(data.stim2);
%
for t=1:data.T
    if condition_left(t)==condition_right(t)
    o=data.O(t);
    choice=data.c(t);
    alternative=data.alternative(t);
    % is it a 3
    if data.feed(t)
        switch data.condition_left(t)
            case 1
                if data.learnedtogether(t)
                    % Learned together and 3
                    temp=beta_three_two_LT_learn;
                else
                    %Not Learned together but 3
                    temp=beta_three_two_NLT_learn;
                end
            case 2
                if data.learnedtogether(t)
                    % Learned together and 3
                    temp=beta_three_four_LT_learn;
                else
                    %Not Learned together but 3
                    temp=beta_three_four_NLT_learn;
                end
                
            case 3
                if  data.learnedtogether(t)
                    % six learned together
                    temp=beta_six_two_LT_learn;
                else
                    % six not learned together
                    temp=beta_six_two_NLT_learn;
                end
            case 4
                if  data.learnedtogether(t)
                    % six learned together
                    temp=beta_six_four_LT_learn;
                else
                    % six not learned together
                    temp=beta_six_four_NLT_learn;
                end
        end
  % testing 
    else
        
        switch data.condition_left(t)
            case 1
                if data.learnedtogether(t)
                    % tested together and 3
                    temp=beta_three_two_LT_test;
                else
                    %Not tested together but 3
                    temp=beta_three_two_NLT_test;
                end
            case 2
                if data.learnedtogether(t)
                    % tested together and 3
                    temp=beta_three_four_LT_test;
                else
                    %Not tested together but 3
                    temp=beta_three_four_NLT_test;
                end
                
            case 3
                if  data.learnedtogether(t)
                    % six tested together
                    temp=beta_six_two_LT_test;
                else
                    % six not tested together
                    temp=beta_six_two_NLT_test;
                end
            case 4
                if  data.learnedtogether(t)
                    % six tested together
                    temp=beta_six_four_LT_test;
                else
                    % six not tested together
                    temp=beta_six_four_NLT_test;
                end
        end
        % its a 6
        
    end
    % numerator and denominator
    numerator=temp.*q(:,choice);
    denominator(:,1)=temp.*q(:,choice);
    denominator(:,2)=temp.*q(:,alternative);
    
    % OUR LIKELIHOOD FUNCTION
    lik = lik + numerator - mfUtil1.logsumexp(denominator,2);
    
    
    %-------------------
    ...The learning process
        %----------------------
    if feedback(t)
       
        alpha(:,choice)=scale.*alpha(:,choice);
        beta(:,choice)=scale.*beta(:,choice);
        if o
            alpha(:,choice)=alpha(:,choice)+1;
        else
            beta(:,choice)=beta(:,choice)+1;
        end
        q(:,choice)=(alpha(:,choice)./(alpha(:,choice)+beta(:,choice)));
        
        
    end
    end 
end