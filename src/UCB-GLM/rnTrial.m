function [incRouteCost,pzone,Broute,Aroute,Aload]=rnTrial(lambda,pcap,Zones,Ztt)

%INPUTS
%lambda = arrival rate
%pcap = vehicle's passenger capacity
%Zones = number of zones
%Ztt = travel time from zone to zone
%this function is setup to run on a 10x10 grid, using a different network
%would require changing the code

%OUTPUTS
%incRouteCost == increase in route cost of a destination, Nx1 for each
%destination in N
%pzone = generated passenger zone location
%Broute = route prior to new passenger
%Aroute = route after adding new passenger (numbered pnum+1 (O) and pnum+2
%(dropoff)
%Aload = load after adding new passenger


%generate vehicle original location
vloc=randi([1,Zones]);
%generate number of passengers
pnum=poissinv(rand,lambda);

Broute=[];
%generate locations
if pnum>0
    ExLoc=zeros(2*pnum,1);  %locations of each existing passenger
    for i=1:2*pnum
        ExLoc(i,1)=randi([1,Zones]);
    end

%double insertion heuristic to create route
    Broute=[0;1;pnum+1]; %"Before"-route
    BLoad=[0;1;0]; %load after visiting node
    BRouteCost=Ztt(vloc,ExLoc(Broute(2,1),1))+Ztt(ExLoc(Broute(2,1),1),ExLoc(Broute(3,1),1));
    for i=2:pnum
        %P then D
        mincost=inf;
        minroute=Broute;
        rs=size(Broute,1);
        for j=1:rs
            for k=j+1:rs+1
                TempLoad=[BLoad(1:j,1); BLoad(j,1)+1; BLoad(j+1:rs,1)+1];
                TempRoute=[Broute(1:j,1); i; Broute(j+1:rs,1)];
                TempLoad=[TempLoad(1:k,1); TempLoad(k,1)-1; TempLoad(k+1:rs+1,1)-1];
                TempRoute=[TempRoute(1:k,1); i+pnum; TempRoute(k+1:rs+1,1)];
                if max(TempLoad)<=pcap
                    TempCost=Ztt(vloc,ExLoc(TempRoute(2,1),1));
                    rs2=size(TempRoute,1);
                    for l=2:rs2-1
                        TempCost=TempCost+Ztt(ExLoc(TempRoute(l,1),1),ExLoc(TempRoute(l+1,1),1));
                    end
                    if TempCost<mincost
                        mincost=TempCost;
                        minroute=TempRoute;
                        minload=TempLoad;
                    end
                end
            end
        end
        Broute=minroute;
        BLoad=minload;
        BRouteCost=mincost;
    end


    %random trim of early legs
    count=1;
    samenum=1;
    while samenum==1
        if Broute(count+1,1)<=pnum
           count=count+1;
        else
            samenum=0;
        end
    end

    rcount=zeros(count,1);
    rcount(1,1)=Ztt(vloc,ExLoc(Broute(2,1),1));
    for i=2:count
        rcount(i,1)=Ztt(ExLoc(Broute(i,1),1),ExLoc(Broute(i+1,1),1));
    end
    rcount=rcount/sum(rcount);
    rcountcdf=rcount;
    for i=2:size(rcount,1)
        rcountcdf(i,1)=rcountcdf(i-1,1)+rcount(i,1);
    end

    leg=1;
    legseed=rand;
    stop=0;
    i=1;
    while stop==0
        if legseed<rcountcdf(i,1)
            leg=i;
            stop=1;
        end
        i=i+1;
    end

    rs=size(Broute,1);
    if leg>1
        vloc=ExLoc(Broute(leg,1),1);
    end
    Broute=Broute(leg:rs,1);
    BLoad=BLoad(leg:rs,1);
    Broute(1,1)=0;
    rs=size(Broute,1);
    BRouteCost=Ztt(vloc,ExLoc(Broute(2,1),1));
    for i=2:rs-1
        BRouteCost=BRouteCost+Ztt(ExLoc(Broute(i,1),1),ExLoc(Broute(i+1,1),1));
    end
end

%generate passenger
pzone=randi([1,Zones]);
if pnum>0
    ExLoc=[ExLoc;pzone];
    %determine extra cost
    incRouteCost=zeros(Zones,1);
    for d=1:Zones
        TempLoc=[ExLoc; d];
        %P then D
        mincost=inf;
        minroute=Broute;
        rs=size(Broute,1);
        for j=1:rs
            for k=j+1:rs+1
                TempLoad=[BLoad(1:j,1); BLoad(j,1)+1; BLoad(j+1:rs,1)+1];
                TempRoute=[Broute(1:j,1); 2*pnum+1; Broute(j+1:rs,1)];
                TempLoad=[TempLoad(1:k,1); TempLoad(k,1)-1; TempLoad(k+1:rs+1,1)-1];
                TempRoute=[TempRoute(1:k,1); 2*pnum+2; TempRoute(k+1:rs+1,1)];
                if max(TempLoad)<=pcap
                    TempCost=Ztt(vloc,TempLoc(TempRoute(2,1),1));
                    rs2=size(TempRoute,1);
                    for l=2:rs2-1
                        TempCost=TempCost+Ztt(TempLoc(TempRoute(l,1),1),TempLoc(TempRoute(l+1,1),1));
                    end
                    if TempCost<mincost
                        mincost=TempCost;
                        minroute=TempRoute;
                        minload=TempLoad;
                    end
                end
            end
        end
        incRouteCost(d,1)=mincost-BRouteCost;
        Aroute=minroute;
        Aload=minload;
    end
else
    for d=1:Zones
        incRouteCost(d,1)=Ztt(vloc,pzone)+Ztt(pzone,d);
        Aroute=[0; 1; 2];
        Aload=[0;1;0];
    end

end