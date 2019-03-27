function [Y,X,RouteCost,theta,arms]=UCBScenario(delta,T,tau,lambda,pcap,ttc,Zdist,Ratings)

%INPUTS
%delta = parameter for upper bound reliability for exploration
%T = number of trials
%tau = number of initial random draws (tau<T) to initiate UCB algorithm
%lambda = arrival rate
%pcap = passenger capacity on shuttle
%ttc = converter from distance (0-10) to travel time (minutes) (e.g. 3 ->
%30 minutes)

%OUTPUTS
%

Ztt=Zdist*ttc;
Zones=size(Zdist,1);
X=ones(T,3);
Y=zeros(T,1);
V=zeros(3,3);   %this is for 3 dimensions: constant, ratings, routecost
theta=zeros(T,3);
arms=zeros(T,1);
RouteCost=zeros(Zones,T);
alpha=1+sqrt(log(2/delta)/2);

for t=1:tau
    [RouteCost(:,t)]=rnTrial(lambda,pcap,Zones,Ztt);
    d=randi([1,Zones]);
    X(t,2)=Ratings(d,1);
    X(t,3)=RouteCost(d,t);
    Pr=(1+exp(5-2*X(t,2)+0.1*X(t,3)))^-1; %simulating real response
    if rand<Pr
        Y(t,1)=1;
    end
end

for t=1:tau
    V=V+(X(t,:)')*X(t,:);
end


for t=tau+1:T
    %update estimate of theta
    F=@(thetat)myfun(thetat,X,Y,t);
    thetat=fsolve(F,zeros(1,3));
    theta(t,:)=thetat;

    %update X
    Xta=ones(3,Zones);
       
    %select arm
    armst=zeros(100,1);
    maxa=-inf;
    [RouteCost(:,t)]=rnTrial(lambda,pcap,Zones,Ztt);
    for i=1:Zones
        Xta(2,i)=Ratings(i,1);
        Xta(3,i)=RouteCost(i,t);
        armst(i,1)=(1+exp(-thetat*Xta(:,i)))^-1+alpha*sqrt(Xta(:,i)'*V^-1*Xta(:,i));
        if armst(i,1)>maxa
            maxa=armst(i,1);
            X(t,2)=Xta(2,i);
            X(t,3)=Xta(3,i);
            arms(t,1)=i;
        end
    end
    
    %simulate observation of acceptance
    Pr=(1+exp(5-2*X(t,2)+0.1*X(t,3)))^-1;
    if rand<=Pr
        Y(t,1)=1;
    end
    
    %update V
    V=V+X(t,:)'*X(t,:);

end
    
