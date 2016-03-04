{\rtf1\ansi\ansicpg1252\cocoartf1404\cocoasubrtf340
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1440\margr1440\vieww16940\viewh6400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 Feature Selection \
=================\
\
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. \
\
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). \
\
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). \
\
These signals were used to estimate variables of the feature vector for each pattern:  \
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.\
\
We changed the name to follow the UpperCamelCase naming convention\
\
TimeBodyAcc-XYZ\
TimeGravityAcc-XYZ\
TimeBodyAccJerk-XYZ\
TimeBodyGyro-XYZ\
TimeBodyGyroJerk-XYZ\
TimeBodyAccMag\
TimeGravityAccMag\
TimeBodyAccJerkMag\
TimeBodyGyroMag\
TimeBodyGyroJerkMag\
FrequenceBodyAcc-XYZ\
FrequenceBodyAccJerk-XYZ\
FrequenceBodyGyro-XYZ\
FrequenceBodyAccMag\
FrequenceBodyAccJerkMag\
FrequenceBodyGyroMag\
FrequenceBodyGyroJerkMag\
\
The set of variables that were estimated from these signals are: \
\
Mean(): Mean value\
Std(): Standard deviation\
MeanFreq(): Weighted average of the frequency components to obtain a mean frequency\
\
Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:\
\
GravityMean\
TimeBodyAccMean\
TimeBodyAccJerkMean\
TimeBodyGyroMean\
TimeBodyGyroJerkMean\
\
Theses datas are accessible by Subject and by Activity type.\
\
Finally, all the datas are available as their mean group by activity and subject\
}