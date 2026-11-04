from pathlib import Path
import json,tempfile
from types import SimpleNamespace
from unittest.mock import patch
import numpy as np
from sklearn.decomposition import PCA
from pipeline.estimation_variants import catalog
from pipeline.domain.dtw import get_warping_path_fast
from pipeline.domain.processor import process_dataset
from pipeline.model_bundle import ModelWriter,encode_paths
from pipeline.config import SimEDaPEConfig
from pipeline.stages.sweep import _run_dtw_methods,_run_pca_method,_run_pca_delta_method
out=Path('/out');work=Path(tempfile.mkdtemp());folder=work/'warping_paths';folder.mkdir()
rng=np.random.default_rng(190)
n,t,k=6,12,2
x=np.array([3+i+np.sin(np.arange(t)*(.3 if i<3 else .8)+i*.1)+rng.normal(0,.1,t) for i in range(n)])
labels=[f'{i+1}-0-3600' for i in range(n)];groups=[0,0,0,1,1,1];sp=[0,3]
X,mean,std=process_dataset(x,t);centroids={str(k):X[i,:,0].tolist() for k,i in enumerate(sp)}
cl=dict(number_of_clusters=k,clusters=groups,centroids=centroids,mean=mean.tolist(),standard_deviation=std.tolist())
roads={str(i+1):[100.+17*i,10.,9.+i] for i in range(n)}
partial=[(x[i,:8]*1.2+.3).tolist() for i in range(n-1)];plabels=labels[:-1]
pca={}
for c in range(k):
 idx=[i for i,g in enumerate(groups) if g==c];data=x[idx];mu=data.mean(0);p=PCA(n_components=2).fit(data-mu)
 tw={labels[i]:(p.components_@(x[i]-mu)).tolist() for i in idx}
 pca[c]=dict(pca=p,mean=mu,size=len(idx),n_components=2,explained_variance_pct=float(p.explained_variance_ratio_.sum()*100),train_weights_by_label=tw,sp_weights_train=tw[labels[sp[c]]])
ctx=SimpleNamespace(config=SimEDaPEConfig(output_dir=str(work),input_dir=str(work)),clustering_result=cl,X=X,mean=mean,std=std,time_series_labels=labels,simulation_points_indexs=sp,pca_models=pca,streets_info=roads,input_dir=str(work),output_dir=str(work))
writer=ModelWriter(str(out/'python-model.simedape'),dict(scenario_name='parity',intervals=dict(start_time=0,end_time=3600,interval_size=3600)))
writer.add_array('clustering/labels',np.array(groups,dtype=np.uint16));writer.add_array('clustering/centroids',np.array([centroids[str(i)] for i in range(k)]));writer.add_array('clustering/mean',mean.ravel());writer.add_array('clustering/std',std.ravel());writer.add_json('clustering/meta',dict(number_of_clusters=k,mean_shape=[n,1,1]));writer.add_json('json/time_series_labels',labels);writer.add_json('json/simulation_points_indexs',sp);writer.add_json('json/street_info',roads);writer.add_json('json/street_series_vehicles',[[f'v{i}'] for i in range(n)])
writer.add_json('pca/meta',dict(clusters=list(range(k))))
for c in range(k):
 idx=[i for i,g in enumerate(groups) if g==c]
 paths=[get_warping_path_fast(np.array(centroids[str(c)]),X[i,:,0].copy())[0] for i in idx]
 with (folder/f'cluster_{c}.wpz').open('wb') as target:np.savez_compressed(target,**encode_paths(paths))
 writer.add_paths(c,paths)
 model=pca[c];prefix=f'pca/cluster_{c}';p=model['pca']
 for key,value in [('mean',model['mean']),('components',p.components_),('explained_variance',p.explained_variance_),('explained_variance_ratio',p.explained_variance_ratio_),('train_weights',np.array([model['train_weights_by_label'][labels[i]] for i in idx]))]:writer.add_array(f'{prefix}/{key}',value)
 writer.add_json(f'{prefix}/info',dict(size=len(idx),n_components=2,explained_variance_pct=model['explained_variance_pct'],labels=[labels[i] for i in idx],sp_weights_train=model['sp_weights_train']))
writer.close(dict(n_series=n,n_clusters=k,series_length=t,simulation_points=k,methods=['dtw','pca']))
def read(*args):return 8,partial,[[]]*len(partial),{},plabels,{}
results={}
with patch('pipeline.io.source.read_scenario',side_effect=read):
 for v in catalog():
  if v['family']=='dtw':r=_run_dtw_methods([v['method']],ctx,'partial',25,str(folder),interp_method=v['interp'])[v['method']]
  elif v['family']=='pca':r=_run_pca_method(ctx,'partial',mode=v['method'],interp_method=v['interp'],use_eig=v['eig'])
  else:r=_run_pca_delta_method(ctx,'partial',v['method'],interp_method=v['interp'],use_eig=v['eig'],percentage=25)
  assert len(r.get('labels',[]))>0, v['key']
  results[v['key']]=r
fixture=dict(labels=labels,raw=x.tolist(),roads=roads,partial_labels=plabels,partial=partial,percentage=25,expected=results)
def clean(v):
 if isinstance(v,(np.floating,float)):return float(v) if np.isfinite(v) else None
 if isinstance(v,np.integer):return int(v)
 if isinstance(v,dict):return {str(k):clean(x) for k,x in v.items()}
 if isinstance(v,(list,tuple)):return [clean(x) for x in v]
 return v
(out/'estimation-reference.json').write_text(json.dumps(clean(fixture),indent=2,allow_nan=False))
print('Generated',len(results),'estimation references')
