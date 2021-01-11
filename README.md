NOTE: DESCIPTION OF THE FILES NOT FINISHED!    


# GOAL     
A model to estimate the water depth for a sampling site in a Caulerpa meadow of the Ria Formosa lagoon (South Portugal). 
Note: the model is not extremaly accurate and needs to be improved with more training data (over more tidal cycles).   

# FIELD DATA DESRIPTION
## dataset 1 (validation data)
start time: 2017-07-25 12:00:00 (tz= Europe/Lisbon)         
final time: 2017-07-25 19:00:00 (tz= Europe/Lisbon)       
Transducer model: Rugged TROLL 100      
Distance of sensor from bottom: 18 cm      
Sampling frequency: 5	min      

## dataset 2 (training data)
start time: 2017-09-07 15:00:00 (tz= Europe/Lisbon)          
final time: 2017-09-11 11:00:00 (tz= Europe/Lisbon)          
Transducer model: Rugged TROLL 100     
Distance of sensor from bottom: 12.5 cm     
Sampling frequency: 10	min      

# DESCRIPTION     
The model used training data (field dataset 1) to obtain a deterministic relationship between observed water depth and tide heights from official charts of the Portuguese Hydrographic Institute. The model is validated agains an independent dataset (field dataset 2).      

Different regression approaches were used and compared with RMSE and R2. The linear regression outperformed the other approaches.    

The model is then applied for the time interval of interests, from june 2017 to june 2019, and mean, maximum, and minimum water depths are then estimated for each month.      
