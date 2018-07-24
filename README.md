# wifi_fingerprinting

data: https://archive.ics.uci.edu/ml/datasets/ujiindoorloc

Indoor localization is still an open problem mainly due to the loss of GPS signal in indoor environments. Although, there are some indoor positioning technologies and methodologies, the data used for this analysis is focused on WLAN fingerpring. 

The database is as follows:
- It covers three buildings of Universitat Jaume I with a different amount of floors in each building.
- It has 529 attributes containing the WiFi fingerprint, the coordinates where it was taken, and others.
- Each WiFi fingerprint can be characterized by the detected Wireless Access Points (WAPs) and the corresponding Received Signal Strength Intensity (RSSI). The intensity values are represented as negative integer values ranging -104dBm (extremely poor signal) to 0dbM. The positive value 100 is used to denote when a WAP was not detected. During the database creation, 520 different WAPs were detected. Thus, the WiFi fingerprint is composed by 520 intensity values.
- Contains no missing observations
- The coordinates (latitude, longitude, floor) and Building ID are provided as the attributes to be predicted. 
- It is split between two datasets: training (19937 observations) and a validation (1111 observations). Each dataset has been created differently. 

Further documentation on the datasets can be found here: https://s3.amazonaws.com/gbstool/courses/614/docs/UJIIndoorLoc%20-%20A%20New%20Multi-building%20and%20Multi-floor%20Database%20for%20WLAN%20Fingerprint-based%20Indoor%20Localization%20Problems.pdf?AWSAccessKeyId=AKIAJBIZLMJQ2O6DKIAA&Expires=1532509200&Signature=zalTOc66Fsyl%2FVG9aInIpnoVZ2U%3D

