import json
from pathlib import Path
import numpy as np
from pipeline.domain.dtw import get_warping_path_fast
rng=np.random.default_rng(117);cases=[]
for n in [3,5,10,21,50]:
 for m in [3,5,10,21,50]:
  for kind in ['integer','normal']:
   a=(rng.integers(0,4,n).astype(float) if kind=='integer' else rng.normal(size=n))
   b=(rng.integers(0,4,m).astype(float) if kind=='integer' else rng.normal(size=m))
   path,d=get_warping_path_fast(a.copy(),b.copy())
   cases.append(dict(a=a.tolist(),b=b.tolist(),distance=float(d),path=[[int(i),int(j),float(c),int(s)] for i,j,c,s in path]))
Path('/out/dtw.json').write_text(json.dumps(cases,indent=2,allow_nan=False))
