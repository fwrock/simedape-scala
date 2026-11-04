from pathlib import Path
import json
import numpy as np
import pandas as pd
from pipeline.experiments.data import aggregate_observations,read_observations,fingerprint
out=Path('/out');records=[]
for vehicle,link,enter,exit in [('1152921504606846977','a',0,6),('v2','b',1,7)]:
 records.append(dict(run_id='r0',vehicle_id=vehicle,link_id=link,time_seconds=enter,event_type='enter'))
 records.append(dict(run_id='r0',vehicle_id=vehicle,link_id=link,time_seconds=exit,event_type='exit'))
 for k,time in enumerate(range(enter+2,exit+1,2)):
  records.append(dict(run_id='r0',vehicle_id=vehicle,link_id=link,time_seconds=time,event_type='sample',sample_duration_seconds=2.,speed=[0.,2.,4.][k],acceleration=[-1.,.5,1.][k],position=10.*k,gap=5.-k,leader_id='-1' if k==0 else 'leader'))
 records.append(dict(run_id='r0',vehicle_id=vehicle,link_id=link,time_seconds=enter+4,event_type='lane_change'))
frame=pd.DataFrame(records);frame.to_csv(out/'observations.csv',index=False);frame.to_parquet(out/'observations.parquet',index=False);frame.to_json(out/'observations.jsonl',orient='records',lines=True)
folder=out/'observations-parts';folder.mkdir(exist_ok=True);frame.iloc[:6].to_parquet(folder/'part-0.parquet',index=False);frame.iloc[6:].to_parquet(folder/'part-1.parquet',index=False)
variables=['count','speed','speed_std','acceleration','acceleration_positive','deceleration','gap','gap_p10','queue','flow','lane_changes','distance','position']
configs=[dict(variables=variables,start_seconds=0,end_seconds=8,bin_seconds=2,window_bins=2,complete_snapshots=True,complete_events=True,link_ids=['a','b','c'],groups={'a':'g0','b':'g1'}),dict(variables=variables,start_seconds=0,end_seconds=8,bin_seconds=2,window_bins=2,complete_snapshots=True,complete_events=True,link_ids=['a','b','c'],count_source='events'),dict(variables=[v for v in variables if v not in ['count','queue','flow']],start_seconds=0,end_seconds=8,bin_seconds=2,window_bins=2,representation='vehicle',alignment='departure',complete_events=True)]
cases=[]
for i,cfg in enumerate(configs):
 data=aggregate_observations(read_observations(out/'observations.parquet'),**cfg)
 data.save(out/f'observation-reference-{i}.npz');cases.append(dict(config=cfg,file=f'observation-reference-{i}.npz'))
(out/'observations.json').write_text(json.dumps(dict(cases=cases,fingerprints={str(p.name):fingerprint(p) for p in [out/'observations.csv',out/'observations.parquet',folder]}),indent=2))
