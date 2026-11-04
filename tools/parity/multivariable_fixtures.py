import json
from pathlib import Path
import numpy as np
from pipeline.experiments.data import SeriesDataset
from pipeline.experiments.model import PatternEstimator,PCABasis
from pipeline.experiments.evaluation import evaluate,observation_mask
from pipeline.experiments.runner import run_experiments
from pipeline.experiments.data import combine_datasets
import tempfile
from pipeline.experiments.paired import PairedResidualEstimator
from pipeline.experiments.observation import _digest
out=Path('/out');rng=np.random.default_rng(77);n,t,p=12,5,2
labels=['a|0','b|0','c|0']*4;runs=sum(([f'r{i}']*3 for i in range(4)),[]);groups=['g0','g0','g1']*4
values=np.empty((n,t,p))
for i in range(n):
 values[i,:,0]=3+i%3+np.sin(np.arange(t)*(.5+i%3*.2))+rng.normal(0,.04,t)
 values[i,:,1]=8+2*(i%3)+np.cos(np.arange(t)*.6)+rng.normal(0,.04,t)
observed=np.ones_like(values,dtype=bool);observed[1,2,1]=False
full=SeriesDataset(values,observed,labels,['count','speed'],np.arange(t,dtype=float),runs,groups,[[f'v{i%3}'] for i in range(n)],{'representation':'link'})
train=full.for_runs(['r0','r1','r2']);truth=full.for_runs(['r3']);mask=truth.observed.copy();mask[:,2:,:]=False
query=SeriesDataset(np.where(mask,truth.values,np.nan),mask,truth.labels,truth.variables,truth.times,truth.runs,truth.groups,truth.units,truth.metadata)
train.save(out/'multivariable-training.npz');truth.save(out/'multivariable-truth.npz');query.save(out/'multivariable-query.npz')
cases=[]
configs=[]
for method in ['pca','dtw','mean','baseline']:
 for clustering,warping,layout,context in [('joint','dependent','series',False),('separate','independent','observations',True),('grouped','euclidean','series',False)]:
  configs.append(dict(method=method,clustering=clustering,warping=warping,pca_layout=layout,group_by_context=context,variable_groups=[['count'],['speed']] if clustering=='grouped' else None,n_clusters=2,n_components=2,seed=42))
for cfg in configs:
 model=PatternEstimator(**cfg).fit(train);prediction=model.predict(query)
 cases.append(dict(selection={strategy:model.select(3,strategy=strategy,seed=42) for strategy in ["random","medoid","centroid_nearest","pca","coverage"]},distinct_selection={strategy:model.select_distinct_labels(2,strategy=strategy,seed=42) for strategy in ["random","medoid","centroid_nearest","pca","coverage"]},config=cfg,prediction=prediction.tolist(),summary=model.summary(),diagnostics=model.diagnostics,metrics=evaluate(truth,prediction,truth.observed & ~mask)))
m=np.array([[1,np.nan,2,5],[2,4,np.nan,6],[3,6,6,7],[4,8,8,8]],float);basis=PCABasis(2,1e-6).fit(m);row=np.array([2.5,np.nan,np.nan,6.5]);recon,diag=basis.reconstruct(row)
def clean(v):
 if isinstance(v,(float,np.floating)):return float(v) if np.isfinite(v) else None
 if isinstance(v,np.integer):return int(v)
 if isinstance(v,np.ndarray):return clean(v.tolist())
 if isinstance(v,dict):return {str(k):clean(x) for k,x in v.items()}
 if isinstance(v,(list,tuple)):return [clean(x) for x in v]
 return v
masks={}
for mode in ['random','prefix','block','representatives']:
 for seed in [0,42]:masks[f'{mode}-{seed}']=observation_mask(truth,.4,seed=seed,mode=mode,selected_labels=['a|0']).tolist()
context=dict(schema='simedape.reduction_context.v1',policy='vehicle_sample',selected_units=['v0'],selected_series=['a|0'],simulated_links=['a'],background_links=[],observation_semantics='reduced_proxy',clock={'tickDuration':1.0})
context['sha256']=_digest(context)
def record(run,departure):
 r=dict(schema='simedape.run_inputs.v1',run_id=run,engine_jar_sha256='engine-hash',source_inputs_sha256='source-'+run,clock={'tickDuration':1.0,'startTick':0,'endTick':5},departure_seconds={'v0':float(departure),'v1':20.0,'v2':30.0})
 r['sha256']=_digest(r);return r
inputs={f'r{i}':record(f'r{i}',10+i) for i in range(3)}
fulltrain=SeriesDataset(train.values,train.observed,train.labels,train.variables,train.times,train.runs,train.groups,train.units,{**train.metadata,'run_provenance':inputs})
proxytrain=SeriesDataset(train.values*.4+np.array([.1,.3])[None,None,:],train.observed,train.labels,train.variables,train.times,train.runs,train.groups,train.units,{**fulltrain.metadata,'observation_semantics':'reduced_proxy','reduction_context':context})
proxyquery=SeriesDataset(truth.values*.4+.2,truth.observed,truth.labels,truth.variables,truth.times,truth.runs,truth.groups,truth.units,{**truth.metadata,'observation_semantics':'reduced_proxy','reduction_context':context,'run_provenance':{'r3':record('r3',11.5)}})
fulltrain.save(out/'paired-full.npz');proxytrain.save(out/'paired-training.npz');proxyquery.save(out/'paired-query.npz')
paired=PairedResidualEstimator().fit(fulltrain,proxytrain);prediction=paired.predict(proxyquery)
paired_fixture=dict(metrics=evaluate(truth,prediction,truth.observed),prediction=prediction,summary=paired.summary(),diagnostics=paired.diagnostics,assessment=paired.assess_inputs(context,record('r3',100.0)))
combined=combine_datasets([train,truth]);combined.save(out/'experiment-dataset.npz')
config=dict(dataset='experiment-dataset.npz',training_runs=['r0','r1','r2'],test_runs=['r3'],fractions=[.4],seeds=[42],mask='prefix',models=[dict(name=f'model-{i}',**cfg) for i,cfg in enumerate(configs)])
report=run_experiments(config,base_dir=out,output_dir=Path(tempfile.mkdtemp())/'experiment')
(out/'experiment-reference.json').write_text(json.dumps(clean(report),indent=2,allow_nan=False))
truth_inputs=SeriesDataset(truth.values,truth.observed,truth.labels,truth.variables,truth.times,truth.runs,truth.groups,truth.units,{**truth.metadata,'run_provenance':{'r3':record('r3',11.5)}})
combine_datasets([fulltrain,truth_inputs]).save(out/'paired-experiment-full.npz')
(out/'multivariable.json').write_text(json.dumps(clean(dict(cases=cases,masks=masks,paired=paired_fixture,basis=dict(matrix=m,row=row,expected=recon,diagnostics=diag))),indent=2,allow_nan=False))
