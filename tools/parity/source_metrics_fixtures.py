import json
from pathlib import Path
import pandas as pd
from pipeline.estimation_variants import catalog
from pipeline.io.clickhouse import read_clickhouse_scenario
from pipeline.domain.metrics import calc_metrics
from pipeline.stages.compare import _analyse_method
root=Path('/out')
(root/'main/variants.json').write_text(json.dumps(catalog(),ensure_ascii=False,indent=2))
events=pd.DataFrame(dict(link_id=['3']*7+['5']*4,tick=[0,10,20,60,70,90,120,5,50,55,100],vehicle_id=['a','b','a','b','c','c','c','d','d','e','e'],vehicle_count_on_link=[1,2,1,0,1,2,0,1,0,1,0]))
class Client:
 def query(self,*args):return events
iv=dict(start_time=0,end_time=120,interval_size=60,sample_period=10)
r=read_clickhouse_scenario(Client(),iv,expected_links=['3','5','8'])
info={'3':[75.,10.,10.],'5':[150.,20.,20.],'8':[37.5,5.,5.]}
fixture=dict(events=events.to_dict(orient='records'),intervals=iv,roads=info,labels=r[4],series=r[1],members=r[2],vehicles=r[3],metrics=[calc_metrics(s,l.split('-')[0],info) for l,s in zip(r[4],r[1])])
(root/'test/occupancy.json').write_text(json.dumps(fixture,indent=2,allow_nan=False))
real={'labels':['a','b','c'],'average_speed':[10.,20.,0.],'percentage_of_track':[0.,25.,50.],'average_vehicles':[0.,2.,4.],'km_traveled':[.1,.2,.4]}
est={'labels':['c','a'],'average_speed':[1.,12.],'percentage_of_track':[40.,0.],'average_vehicles':[3.,0.],'km_traveled':[.3,.1]}
def clean(x):
 if isinstance(x,float):return x if __import__('math').isfinite(x) else None
 if isinstance(x,dict):return {k:clean(v) for k,v in x.items()}
 if isinstance(x,list):return [clean(v) for v in x]
 return x
(root/'test/comparison.json').write_text(json.dumps(clean(dict(real=real,estimated=est,expected=_analyse_method(real,est,'fixture'))),indent=2))
