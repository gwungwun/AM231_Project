"""
Parse command line and store result in params
Model : DMM
"""
import argparse,copy
from collections import OrderedDict
p = argparse.ArgumentParser(description="Arguments for variational autoencoder")
parser = argparse.ArgumentParser()
parser.add_argument('-dset','--dataset', action='store',default = 'mm', help='Dataset the model is trained on', type=str)

"""
Hyperparameters for the Inference/Recognition Model
"""
parser.add_argument('-rs','--rnn_size', action='store',default = 600, help='Dimensionality of the recurrent hidden unit in the RNN', type=int)
parser.add_argument('-rd','--rnn_dropout', action='store',default = 0.1, help='Dropout applied on top of the recurrent hidden layer', type=float)
parser.add_argument('-infm','--inference_model', action='store',default = 'R', 
        help='Choice of Inference Model - How to aggregate data for posterior inference: [LR - Bidirectional RNN, R - denotes RNN from Right to Left]', type=str, choices=['LR','R'])
parser.add_argument('-use_p','--use_generative_prior', action='store', type = str, default = 'approx', 
        choices = ['true','approx'], 
        help='If <true>: the transition function (of the generative model) is used in the inference network to combine information from the RNN and the previous latent state. Otherwise, <approx> uses a learned approximation to it')
parser.add_argument('-rc','--rnn_cell', action='store', type=str, choices=['lstm','rnn'], 
        default='lstm',help='RNN cell (LSTM/RNN) used in the inference network')

"""
Hyperparameters for the Generative Model
"""
parser.add_argument('-ds','--dim_stochastic', action='store',default = 100, help='Dimensions of latent state', type=int)
parser.add_argument('-dh','--dim_hidden', action='store', default = 200, help='Hidden dimensions of MLP used in the DMM', type=int)
parser.add_argument('-tl','--transition_layers', action='store', default = 2, 
        help='Number of layers in the MLPs in the transition fxn. Set to 0 (with transition_type = mlp) for a linear transition function', type=int)
parser.add_argument('-ttype','--transition_type', action='store', default = 'mlp', 
        help='Choice of transition function', type=str, choices=['mlp','gated'])
parser.add_argument('-el','--emission_layers', action='store',default = 2, help='Number of layers in the MLP in the emission function. Set to 0 for a linear emission',type=int)

"""
Hyperparameters for MLPs: 
    * initialization scheme for weights
    * nonlinearities
"""
parser.add_argument('-iw','--init_weight', action='store',default = 0.1, help='Range to initialize weights during learning',type=float)
parser.add_argument('-ischeme','--init_scheme', action='store',default = 'uniform', help='Type of initialization for weights', type=str, choices=['uniform','normal','xavier','he','orthogonal'])
parser.add_argument('-nl','--nonlinearity', action='store',default = 'relu', help='Nonlinarity',type=str, choices=['relu','tanh','softplus'])
parser.add_argument('-lky','--leaky_param', action='store',default =0., help='Leaky ReLU parameter',type=float)
parser.add_argument('-mstride','--maxout_stride', action='store',default = 4, help='Stride for maxout',type=int)
parser.add_argument('-fg','--forget_bias', action='store',default = -5., help='Bias for forget gates', type=float)
parser.add_argument('-vonly','--validate_only', action='store_true', help='Only build fxn for validation')

"""
Hyperparameters for Optimization 
"""
parser.add_argument('-lr','--lr', action='store',default = 8e-4, help='Learning rate', type=float)
parser.add_argument('-opt','--optimizer', action='store',default = 'adam', help='Optimizer',choices=['adam','rmsprop'])
parser.add_argument('-bs','--batch_size', action='store',default = 20, help='Batch Size',type=int)
parser.add_argument('-ar','--anneal_rate', action='store',default = 2., help='Number of param. updates before anneal=1',type=float)

#Regularization
parser.add_argument('-reg','--reg_type', action='store',default = 'l2', help='Type of regularization',type=str,choices=['l1','l2'])
parser.add_argument('-rv','--reg_value', action='store',default = 0.05, help='Amount of regularization',type=float)
parser.add_argument('-rspec','--reg_spec', action='store',default = '_', help='Use this string to match model parameters (weights) defined in "_createParams" that you wish to apply regularization towards',type=str)

"""
Miscellaneous bookkeeping hyperparameters
"""
parser.add_argument('-debug','--debug', action='store_true',help='Debug')
parser.add_argument('-uid','--unique_id', action='store',default = 'uid',help='Unique Identifier',type=str)
parser.add_argument('-seed','--seed', action='store',default = 1, help='Random Seed',type=int)
parser.add_argument('-dir','--savedir', action='store',default = './chkpt', help='Prefix for the directory that the model will be loaded from',type=str)
parser.add_argument('-ep','--epochs', action='store',default = 2000, help='MaxEpochs',type=int)
parser.add_argument('-reload','--reloadFile', action='store',default = './NOSUCHFILE', help='Reload from saved model',type=str)
parser.add_argument('-params','--paramFile', action='store',default = './NOSUCHFILE', help='Reload parameters from saved model',type=str)
parser.add_argument('-sfreq','--savefreq', action='store',default = 10, help='Frequency of saving',type=int)
params = vars(parser.parse_args())

"""
The following parameters will be tracked in the savefile to form the 
unique identifier for model checkpoints
"""
hmap       = OrderedDict() 
hmap['lr']                  ='lr'
hmap['dim_hidden']          ='dh'
hmap['dim_stochastic']      ='ds'
hmap['nonlinearity']        ='nl'
hmap['batch_size']          ='bs'
hmap['epochs']              ='ep'
hmap['rnn_size']            ='rs'
hmap['rnn_dropout']         ='rd'
hmap['inference_model']     ='infm'
hmap['transition_layers']   ='tl'
hmap['emission_layers']     ='el'
hmap['anneal_rate']         ='ar'
hmap['use_generative_prior']='use_p'
hmap['rnn_cell']            ='rc'
combined   = ''
for k in hmap:
    if k in params:
        combined+=hmap[k]+'-'+str(params[k])+'-'
params['expt_name'] = params['unique_id']
params['unique_id'] = combined[:-1]+'-'+params['unique_id']
params['unique_id'] = 'DMM_'+params['unique_id'].replace('.','_')

#from utils.misc import savePickle
#savePickle([params],'default.pkl')
