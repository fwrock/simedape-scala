import json
from pathlib import Path
import numpy as np
from tslearn.clustering import KShape
from sklearn.decomposition import PCA
from pipeline.domain.processor import process_dataset
from pipeline.clustering_report import build_report
out=Path('/out');fixture=json.loads((out/'estimation-reference.json').read_text());raw=np.array(fixture['raw']);X,mu,sigma=process_dataset(raw,raw.shape[1]);k=KShape(n_clusters=2,max_iter=8,random_state=10).fit(X)
arrays,report=build_report(X,mu,sigma,k.labels_,k.cluster_centers_,fixture['labels'])
models={}
for c in range(2):
 ids=np.flatnonzero(k.labels_==c);data=raw[ids];p=PCA(n_components=min(2,*data.shape)).fit(data-data.mean(0));models[str(c)]=dict(components=p.components_.tolist(),mean=data.mean(0).tolist(),variance=p.explained_variance_.tolist(),ratio=p.explained_variance_ratio_.tolist())
(out/'training.json').write_text(json.dumps(dict(clusters=k.labels_.tolist(),centroids=k.cluster_centers_[:,:,0].tolist(),pca=models,report=report,arrays={key:dict(shape=list(v.shape),values=v.ravel().tolist()) for key,v in arrays.items()}),allow_nan=False,indent=2))
