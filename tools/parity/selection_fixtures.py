import json
from pathlib import Path
from pipeline.selection import *
vehicles=[['v1','v2','v3'],['v2','v4'],['v4','v5'],['v3','v6'],['v7','v8'],['v8','v9'],['v9','v10'],['v11','v12']]
labels=[f'{i//2+1}-{i%2*60}-{i%2*60+60}' for i in range(8)]
clusters=[0,0,0,1,1,1,2,2];sp=[0,3,6];dist=[0,2,3,0,1,2,0,1]
topology={'1':{'2'},'2':{'1','3'},'3':{'2','4'},'4':{'3'}}
cases=[]
for mapping in [None,{'v1':'p1','v2':'p1','v3':'p2','v4':'p3','v5':'p3','v6':'p4','v7':'p5'}]:
 u=build_universe(vehicles,labels,clusters,mapping,unit_kind='person' if mapping else 'vehicle')
 for strategy in STRATEGIES:
  for p in [25,50,100]:
   for force in [[],['3']]:
    params=dict(strategy=strategy,percentage=p,seed=42,force_links=force)
    points=sp if strategy in SP_STRATEGIES else []
    s=select_units(u,strategy,p,42,sp_series=points,force_units=units_on_links(u,force),series_distance=dist,topology=topology)
    m=build_manifest(u,s,scenario_id='fixture',strategy=strategy,percentage=p,seed=42,force_links=force)
    cases.append(dict(params=params,mapping=mapping,sp=points,expected=m))
Path('/out/selection.json').write_text(json.dumps(dict(vehicles=vehicles,labels=labels,clusters=clusters,distances=dist,topology={k:sorted(v) for k,v in topology.items()},cases=cases),indent=2))
