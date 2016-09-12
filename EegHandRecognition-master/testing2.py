import numpy as np
import pandas as pd
import pylab as pl
import mne
from mne.io import RawArray
from mne.channels import read_montage
from mne.datasets import sample
from mne.epochs import concatenate_epochs
from mne import create_info, find_events, Epochs, concatenate_raws, pick_types
from mne.decoding import CSP

from sklearn.linear_model import LogisticRegression
from glob import glob

from scipy.signal import butter, lfilter, convolve, boxcar
from joblib import Parallel, delayed
from scipy.signal import butter, lfilter


def butter_bandpass(lowcut, highcut, fs, order=5):
    nyq = 0.5 * fs
    low = lowcut / nyq
    high = highcut / nyq
    b, a = butter(order, [low, high], btype='bandpass')
    return b, a


def butter_bandpass_filter(raw, lowcut, highcut, fs, order=5):
    b, a = butter_bandpass(lowcut, highcut, fs, order=order)
    y = lfilter(b, a, raw)
    return y


def convert_to_raw(file_series, file_events):
    """Create a mne raw instance from csv file"""

    data = pd.read_csv(file_series)

    channels = list(data.columns[1:])


    m1 = read_montage('standard_1005',channels)


    ch_type = ['eeg']*len(channels)


    data = 1e-6*np.array(data[channels]).T

    # read event file
    events_data = pd.read_csv(file_events)
    events_names = events_data.columns[1:]
    events_data = np.array(events_data[events_names]).T
    #print events_data

    # define channel type, the first is EEG, the last 6 are stimulations
    ch_type.extend(['stim']*6)
    channels.extend(events_names)

    # concatenate event file and data
    data = np.concatenate((data,events_data))

    info = create_info(channels, sfreq=500.0, ch_types=ch_type, montage=m1)

    info['filename'] = file_series

    raw = RawArray(data,info,verbose=False)

    print raw
    print raw[0:]
    print raw[0][0]
    print raw[0][1]

    return raw

    





if __name__ == "__main__":

    '''
    filepath_events = "/Users/Bhargav/Documents/Data_mining/EEG_data/train/subj1_series8_events.csv"
    filepath_series = "/Users/Bhargav/Documents/Data_mining/EEG_data/train/subj1_series8_data.csv"
    raw_data = convert_to_raw(filepath_series,filepath_events)
    print raw_data
    '''
    subjects = range(1,13)




    for s in subjects:
        print "hello %s" %s
        #print "/Users/Bhargav/Documents/Data_mining/EEG_data/train/subj%d_series%d_events.csv"
        filepath_series =  glob('/Users/aanarra/School/Pattern\ Recognition\ and\ Data\ Mining/EegHandRecognition/train/subj%d_series*_data.csv' % (s))
        filepath_events = glob("/Users/aanarra/School/Pattern\ Recognition\ and\ Data\ Mining/EegHandRecognition/train/subj%d_series*_events.csv" % (s))


        raw = [convert_to_raw(f,f1) for f,f1 in zip(filepath_series,filepath_events)]
        raw = concatenate_raws(raw)
        '''
        print raw
        print raw[0:]
        print raw[0][0]
        print raw[1:]
        print raw[1][0]
        '''
        #Bandpass filtering

        pick_ch = pick_types(raw.info,eeg=True)

        f_low = 7
        f_high = 30
        fs = 500

        print raw[0:]
        print "Original: ", raw._data[pick_ch]
        total_channels = len(pick_ch)




        b,a = butter_bandpass(f_low, f_high, fs, order=5)



        raw._data[pick_ch] = np.array(Parallel(n_jobs=-1)(delayed(lfilter)(b,a,raw._data[i]) for i in pick_ch))

        print "After: ", raw._data[pick_ch]
