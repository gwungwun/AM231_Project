#Scripts to process datasets
import h5py,os,urllib,cPickle,gzip
import numpy as np
from synthp import params_synthetic
from utils.misc import getPYDIR, downloadData, createIfAbsent
from struct import unpack
import gzip


def readData(imgfile, labelfile):
    """
    Credit to: https://martin-thoma.com/classify-mnist-with-pybrain/
    """
    # Open the images with gzip in read binary mode
    images = gzip.open(imgfile, 'rb')
    labels = gzip.open(labelfile, 'rb')
    images.read(4)  # skip the magic_number
    number_of_images = images.read(4)
    number_of_images = unpack('>I', number_of_images)[0]
    rows = images.read(4)
    rows = unpack('>I', rows)[0]
    cols = images.read(4)
    cols = unpack('>I', cols)[0]
    labels.read(4)  # skip the magic_number
    N = labels.read(4)
    N = unpack('>I', N)[0]
    if number_of_images != N:
        raise Exception('number of labels did not match the number of images')
    # Get the data
    x = np.zeros((N, rows, cols), dtype=np.float32)  
    y = np.zeros((N, 1), dtype=np.uint8)  
    for i in range(N):
        if i % 1000 == 0:
            print "i: %i" % i,
        for row in range(rows):
            for col in range(cols):
                tmp_pixel = images.read(1)  # Just a single byte
                tmp_pixel = unpack('>B', tmp_pixel)[0]
                x[i][row][col] = tmp_pixel
        tmp_label = labels.read(1)
        y[i] = unpack('>B', tmp_label)[0]
    print ' Done.'
    x,y = x, y.ravel()
    return x, y 


def _processFashionMNIST():
    pfile = getPYDIR()+'/datasets/fashion_mnist/proc-fashion_mnist.h5'
    DIR = os.path.dirname(pfile)
    createIfAbsent(DIR)
    if not os.path.exists(os.path.join(DIR,'train-images-idx3-ubyte.gz')):
        print 'Downloading data'
        urllib.urlretrieve('http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/train-images-idx3-ubyte.gz',os.path.join(DIR,'train-images-idx3-ubyte.gz'))
        urllib.urlretrieve('http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/train-labels-idx1-ubyte.gz',os.path.join(DIR,'train-labels-idx1-ubyte.gz'))
        urllib.urlretrieve('http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/t10k-images-idx3-ubyte.gz',os.path.join(DIR,'t10k-images-idx3-ubyte.gz'))
        urllib.urlretrieve('http://fashion-mnist.s3-website.eu-central-1.amazonaws.com/t10k-labels-idx1-ubyte.gz',os.path.join(DIR,'t10k-labels-idx1-ubyte.gz'))
    if os.path.exists(pfile):
        print 'Found: ',pfile
        return pfile
    print DIR
    X, Y= readData(os.path.join(DIR,'train-images-idx3-ubyte.gz'), os.path.join(DIR,'train-labels-idx1-ubyte.gz'))
    np.random.seed(0)
    idxshuf   = np.random.permutation(X.shape[0])
    valid_idx = idxshuf[:10000]
    train_idx = idxshuf[10000:]
    train_x, train_y = np.clip(X[train_idx]/255., a_min=0.0, a_max=1.0), Y[train_idx]
    valid_x, valid_y = np.clip(X[valid_idx]/255., a_min=0.0, a_max=1.0), Y[valid_idx]
    test_x, test_y = readData(os.path.join(DIR,'t10k-images-idx3-ubyte.gz'), os.path.join(DIR,'t10k-labels-idx1-ubyte.gz'))
    test_x = np.clip(test_x/255., a_min=0.0, a_max=1.0)
    print 'Processing Fashion MNIST'
    h5f   = h5py.File(pfile, mode='w')
    h5f.create_dataset('train',data = train_x)
    h5f.create_dataset('train_y',data = train_y)
    h5f.create_dataset('test' ,data = test_x)
    h5f.create_dataset('test_y' ,data = test_y)
    h5f.create_dataset('valid',data = valid_x)
    h5f.create_dataset('valid_y',data = valid_y)
    h5f.close()
    for dd in [train_x, train_y, valid_x, valid_y, test_x, test_y]:
        print dd.shape, dd.min(), dd.max()
    print 'Done processing Fashion MNIST....',pfile
    return pfile

def _processMNIST():
    pfile = getPYDIR()+'/datasets/mnist/proc-mnist.h5'
    DIR = os.path.dirname(pfile)
    if not os.path.exists(DIR):
        print 'Making: ',DIR
        os.mkdir(DIR)
    if not os.path.exists(os.path.join(DIR,'mnist.pkl.gz')):
        print 'Downloading data'
        urllib.urlretrieve('http://deeplearning.net/data/mnist/mnist.pkl.gz',os.path.join(DIR,'mnist.pkl.gz'))
    if os.path.exists(pfile):
        print 'Found: ',pfile
        return pfile
    print 'Processing MNIST'
    f = gzip.open(os.path.join(DIR,'mnist.pkl.gz'))
    train, valid, test = cPickle.load(f)
    f.close()
    h5f   = h5py.File(pfile, mode='w')
    h5f.create_dataset('train',data = train[0])
    h5f.create_dataset('train_y',data = train[1])
    h5f.create_dataset('test' ,data = test[0])
    h5f.create_dataset('test_y' ,data = test[1])
    h5f.create_dataset('valid',data = valid[0])
    h5f.create_dataset('valid_y',data = valid[1])
    h5f.close()
    print 'Done processing MNIST'
    return pfile
def _processBinarizedMNIST():
    pfile = getPYDIR()+'/datasets/mnist/proc-bmnist.h5'
    """
        Move to processed h5 file
    """
    DIR = os.path.dirname(pfile)
    if not os.path.exists(DIR):
        print 'Making: ',DIR
        os.mkdir(DIR)
    if not os.path.exists(os.path.join(DIR,'binarized_mnist_train.amat')):
        print 'Downloading binarized mnist'
        urllib.urlretrieve('http://www.cs.toronto.edu/~larocheh/public/datasets/binarized_mnist/binarized_mnist_train.amat',os.path.join(DIR,'binarized_mnist_train.amat'))
        urllib.urlretrieve('http://www.cs.toronto.edu/~larocheh/public/datasets/binarized_mnist/binarized_mnist_valid.amat',os.path.join(DIR,'binarized_mnist_valid.amat'))
        urllib.urlretrieve('http://www.cs.toronto.edu/~larocheh/public/datasets/binarized_mnist/binarized_mnist_test.amat',os.path.join(DIR,'binarized_mnist_test.amat'))
    if os.path.exists(pfile):
        print 'Found: ',pfile
        return pfile
    print 'Processing binarized MNIST'
    h5f   = h5py.File(pfile, mode='w')
    h5f.create_dataset('train',data = np.loadtxt(os.path.join(DIR,'binarized_mnist_train.amat')))
    h5f.create_dataset('test' ,data = np.loadtxt(os.path.join(DIR,'binarized_mnist_test.amat')))
    h5f.create_dataset('valid',data = np.loadtxt(os.path.join(DIR,'binarized_mnist_valid.amat')))
    h5f.close()
    print 'Done processing binarized MNIST'
    return pfile

def _processPolyphonic(name):
    DIR = getPYDIR()+'/datasets'
    assert os.path.exists(DIR),'Directory does not exist: '+DIR
    polyphonicDIR = DIR+'/polyphonic/'
    if not os.path.exists(polyphonicDIR):
        os.mkdir(polyphonicDIR)
    fname = polyphonicDIR+'/'+name+'.h5'
    if os.path.exists(fname):
        print 'Found: ',fname
        return fname
    #Setup polyphonic datasets from scratch
    if not os.path.exists(os.path.join(polyphonicDIR,'piano.pkl')) or \
    not os.path.exists(os.path.join(polyphonicDIR,'musedata.pkl')) or \
    not os.path.exists(os.path.join(polyphonicDIR,'jsb.pkl')) or \
    not os.path.exists(os.path.join(polyphonicDIR,'nottingham.pkl')):
        print 'Downloading polyphonic pickled data into: ',polyphonicDIR
        os.system('wget '+'http://www-etud.iro.umontreal.ca/~boulanni/JSB%20Chorales.pickle -O '+os.path.join(polyphonicDIR,'jsb.pkl'))
        os.system('wget '+'http://www-etud.iro.umontreal.ca/~boulanni/Nottingham.pickle -O '+os.path.join(polyphonicDIR,'nottingham.pkl'))
        os.system('wget '+'http://www-etud.iro.umontreal.ca/~boulanni/MuseData.pickle -O '+os.path.join(polyphonicDIR,'musedata.pkl'))
        os.system('wget '+'http://www-etud.iro.umontreal.ca/~boulanni/Piano-midi.de.pickle -O '+os.path.join(polyphonicDIR,'piano.pkl'))
    else:
        print 'Polyphonic pickle files found'
    #Helper function to sort by sequence length
    def getSortedVersion(data,mask):
        idx         = np.argsort(mask.sum(1))
        return data[idx,:,:], mask[idx,:]
    for dset in ['jsb','piano','nottingham','musedata','jsb-sorted','piano-sorted','nottingham-sorted','musedata-sorted']:
        datafile = os.path.join(polyphonicDIR,dset.replace('-sorted','')+'.pkl')
        savefile = os.path.join(polyphonicDIR,dset+'.h5')
        print '\n\nDataset: ',dset
        print ('Reading from: ',datafile)
        print ('Saving to:',savefile)
        MAX = 108
        MIN = 21
        DIM = MAX-MIN+1
        alldata = cPickle.load(file(datafile))
        h5file = h5py.File(savefile,mode='w')
        for dtype in ['train','valid','test']:
            print '----',dtype,'----'
            dataset = alldata[dtype]
            N_SEQUENCES = len(dataset)
            #First, find out the maximum number of sequences
            MAX_LENGTH  = max(len(dataset[k]) for k in range(N_SEQUENCES))
            print N_SEQUENCES,' sequences with max length ',MAX_LENGTH
            mask         = np.zeros((N_SEQUENCES, MAX_LENGTH))
            compileddata = np.zeros((N_SEQUENCES, DIM, MAX_LENGTH))
            for idxseq,seq in enumerate(dataset):
                T = len(seq)
                mask[idxseq,:T] = 1
                for t in range(T):
                    compileddata[idxseq,np.array(seq[t]).astype(int)-MIN,t]=1
            if 'sorted' in dset:
                compileddata,mask = getSortedVersion(compileddata,mask)
            #Save as bs x T x dim
            compileddata      = compileddata.swapaxes(1,2)
            print 'First and last lenghts: ',mask.sum(1)[1:5].tolist(),'....',mask.sum(1)[-5:].tolist()
            print 'Saving tensor data: ',compileddata.shape,' Mask: ',mask.shape
            h5file.create_dataset(dtype,data = compileddata)
            h5file.create_dataset('mask_'+dtype,data = mask)
        h5file.close()

def _processSynthetic(dset):
    DIR = getPYDIR()+'/datasets'
    assert os.path.exists(DIR),'Directory does not exist: '+DIR
    syntheticDIR = DIR+'/synthetic/'
    if not os.path.exists(syntheticDIR):
        os.mkdir(syntheticDIR)
    fname = syntheticDIR+'/'+dset+'.h5'
    #assert dset in ['synthetic9','synthetic10','synthetic11','synthetic12','synthetic13','synthetic14'] ,'Only synthetic 9/10/11 supported'
    """
    9: linear    ds = 1
    10:nonlinear ds = 1
    11:nonlinear ds = 2 [param estimation]

    Checking scalability of ST-R
    12:linear    ds = 10
    13:linear    ds = 100
    14:linear    ds = 250

    Checking scalability of ST-R - dimz = dimobs
    15:linear    ds = 10
    16:linear    ds = 100
    17:linear    ds = 250

    Checking scalability of ST-R - dimz = dimobs and diagonal weight matrices
    18:linear    ds = 10
    19:linear    ds = 100
    20:linear    ds = 250
    """
    if os.path.exists(fname):
        print 'Found: ',fname
        return fname
    #Old=np.random.seed(1)
    def sampleGaussian(mu, cov):
        assert type(cov) is float or type(cov) is np.ndarray,'invalid type: '+str(cov)+' type: '+str(type(cov))
        return mu + np.random.randn(*mu.shape)*np.sqrt(cov)
    def createDataset(N, T, t_fxn, e_fxn, init_mu, init_cov, trans_cov, obs_cov, model_params, dim_stochastic, dim_obs):
        all_z = []
        z_prev= sampleGaussian(np.ones((N,1,dim_stochastic))*init_mu, init_cov)
        all_z.append(np.copy(z_prev))
        for t in range(T-1):
            z_prev = sampleGaussian(t_fxn(z_prev,fxn_params=model_params), trans_cov) 
            all_z.append(z_prev)
        Z_true= np.concatenate(all_z, axis=1)
        assert Z_true.shape[1]==T,'Expecting T in dim 2 of Z_true'
        X     = sampleGaussian(e_fxn(Z_true, fxn_params = model_params), obs_cov)
        assert X.shape[2]==dim_obs,'Shape mismatch'
        return Z_true, X
    if not np.all([os.path.exists(os.path.join(syntheticDIR,fname+'.h5')) for fname in ['synthetic'+str(i) for i in range(9,21)]]):
        #Create all datasets
        for s in range(9,21):
            if os.path.exists(os.path.join(syntheticDIR,'synthetic'+str(s)+'.h5')):
                print 'Found ',s
                continue
            print 'Creating: ',s
            dataset = {}
            transition_fxn = params_synthetic['synthetic'+str(s)]['trans_fxn']
            emission_fxn   = params_synthetic['synthetic'+str(s)]['obs_fxn'] 
            init_mu        = params_synthetic['synthetic'+str(s)]['init_mu']
            init_cov       = params_synthetic['synthetic'+str(s)]['init_cov']
            trans_cov      = params_synthetic['synthetic'+str(s)]['trans_cov']
            obs_cov        = params_synthetic['synthetic'+str(s)]['obs_cov']
            model_params   = params_synthetic['synthetic'+str(s)]['params']
            dim_obs, dim_stoc = params_synthetic['synthetic'+str(s)]['dim_obs'],params_synthetic['synthetic'+str(s)]['dim_stoc']
            if s in [12,13,14,15,16,17,18,19,20]: 
                Ntrain = 1000
                Ttrain = 25 
                Ttest  = 25
            else:
                Ntrain = 5000
                Ttrain = 25 
                Ttest  = 50
            Nvalid = 500
            Ntest  = 500
            #New-
            np.random.seed(1)
            train_Z, train_dataset  = createDataset(Ntrain, Ttrain, transition_fxn, emission_fxn, init_mu, init_cov, trans_cov, obs_cov, model_params, dim_stoc, dim_obs) 
            valid_Z, valid_dataset  = createDataset(Nvalid, Ttrain, transition_fxn, emission_fxn, init_mu, init_cov, trans_cov, obs_cov, model_params, dim_stoc, dim_obs) 
            test_Z,  test_dataset   = createDataset(Ntest, Ttest, transition_fxn, emission_fxn, init_mu, init_cov, trans_cov, obs_cov, model_params, dim_stoc, dim_obs) 
            savefile       = syntheticDIR+'/synthetic'+str(s)+'.h5' 
            h5file = h5py.File(savefile,mode='w')
            h5file.create_dataset('train_z', data=train_Z)
            h5file.create_dataset('test_z',  data=test_Z)
            h5file.create_dataset('valid_z', data=valid_Z)
            h5file.create_dataset('train',   data=train_dataset)
            h5file.create_dataset('test',    data=test_dataset)
            h5file.create_dataset('valid',   data=valid_dataset)
            h5file.close()
            print 'Created: ',savefile
    print 'REMEMBER TO RUN BASELINES!'

if __name__=='__main__':
    pf = _processFashionMNIST()
    _processMNIST()
    _processBinarizedMNIST()
    _processPolyphonic('jsb')
    #_processSynthetic('synthetic11')
    #_processSynthetic('synthetic14')
    #_processSynthetic('synthetic20')
