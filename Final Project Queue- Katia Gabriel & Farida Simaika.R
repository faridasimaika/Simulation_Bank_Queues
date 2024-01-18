
#Katia Gabriel (900202272) & Farida Simaika (900201753)

#1-Teller, 1-Queue:

n=100
set.seed(20394)
interarrival_time = rep(0,n)
service_time = rexp(n,1/5)
next_waiting_time = rep(0,n)
arrival_times = rep(0,n)
next_arrival_time = rexp(n,1/3) 
next_departure_time = rep(0,n)


next_waiting_time[1]=0
next_departure_time[1]=service_time[1]+arrival_times[1]

for (i in 2:n) 
{
  arrival_times[[i]] = arrival_times[i-1]+next_arrival_time[i]
  interarrival_time[i]= arrival_times[i]-arrival_times[i-1]
  next_waiting_time[i] = next_departure_time[i-1]-arrival_times[i]
  if(next_waiting_time[i]<0)
  {
    next_waiting_time[i]=0
  }
    next_departure_time[i]=arrival_times[i]+next_waiting_time[i]+service_time[i]
}
round(arrival_times)
round(service_time)
round(next_waiting_time)
round(next_departure_time)

#statistics:

#1. Average Time that a customer spends in the Queue:

aqueue = sum(next_waiting_time, na.rm = TRUE)/n

#2. Average Time that a customer spends in the Bank whether in the Queue or at the Teller

bank = sum(next_waiting_time, na.rm = TRUE)/(n+((sum(service_time, na.rm = TRUE)/n)))

#3. Average length of the Queue

nqueue = ceiling(sum(next_waiting_time, na.rm = TRUE)/sum(next_arrival_time, na.rm = TRUE))

#2-Tellers,1 Queue:

n=100
set.seed(20394)
service_time = rexp(n,1/5)
next_arrival_time = rexp(n,1/3)
interarrival_time = rep(0,n)
next_waiting_time = rep(0,n)
next_departure_time = rep(0,n)
arrival_times = rep(0,n)
next_departure_times = rep(0,n)
queue = 0
teller1 = rep(0,n)
teller2 = rep(0,n)
teller_num = rep(0,n)
teller1_free = rep(0,n)
teller2_free = rep(0,n)
teller1_occupied = rep(0,n)
teller2_occupied = rep(0,n)
total_idle_time_=rep(0,n)

for (i in 1:(n))
{
  arrival_times[[i+1]] = arrival_times[i]+next_arrival_time[i+1]
  next_departure_times[[i+1]]=next_departure_times[i]+next_departure_time[i+1]
  interarrival_time[i+1]= arrival_times[i+1]-arrival_times[i]
}

next_waiting_time[1]=0
next_waiting_time[2]=0
next_departure_time[1]=service_time[1]
next_departure_time[2]=service_time[2]+arrival_times[2]
teller1[1]=1
teller2[1]=1
teller_num[1]=1
teller_num[2]=1


for(i in 3:n)
{
  if(arrival_times[i]>min(next_departure_time[i-1],next_departure_time[i-2]))
  {
    queue = 0
    if(arrival_times[i]>next_departure_time[i-2])
    {
      teller1[i]=1
      teller1_occupied[i]=arrival_times[i]
      teller1_free[i]=arrival_times[i]+service_time[i]
      next_waiting_time[i]=0
      next_departure_time[i]=arrival_times[i]+service_time[i]
      teller_num[i]=1
      
    }else if(arrival_times[i]<next_departure_time[i-2])
    {
      teller2[i]=1
      teller1[i]=1
      teller2_occupied[i]=arrival_times[i]
      teller2_free[i]=arrival_times[i]+service_time[i]
      next_waiting_time[i]=0
      next_departure_time[i]=arrival_times[i]+service_time[i]
      teller_num[i]=2
    }
  }else
  {
    queue = queue +1
    next_waiting_time[i] = min(next_departure_time[i-2],next_departure_time[i-1])-arrival_times[i]
    next_departure_time[i]= arrival_times[i]+next_waiting_time[i]+service_time[i]
    if((next_departure_time[i-1]-arrival_times[i])>(next_departure_time[i-2]-arrival_times[i]))
    {
      teller1[i]=1
      teller2[i]=1
      teller_num[i]=1
    }else{
      teller1[i]=1
      teller2[i]=1
      teller_num[i]=2
    }
    
  }
}

for (i in 2:n){
 

  total_idle_time=next_departure_time[i-1]-interarrival_time[i]
  
  
}

#1. Average Time that a customer spends in the Queue:

aqueue = sum(next_waiting_time, na.rm = TRUE)/n

#2. Average Time that a customer spends in the Bank whether in the Queue or at the Teller

bank = sum(next_waiting_time, na.rm = TRUE)/(n+((sum(service_time, na.rm = TRUE)/n)))

#3. Average length of the Queue

nqueue = ceiling(sum(next_waiting_time, na.rm = TRUE)/sum(next_arrival_time, na.rm = TRUE))

