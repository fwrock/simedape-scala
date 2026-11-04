import json,tempfile
from pathlib import Path
from pipeline.api import exports,charts
from pipeline.stages.compare import _analyse_method
root=Path(tempfile.mkdtemp());out=Path('/out')
f=json.loads((out/'estimation-reference.json').read_text());real=f['expected']['base'];methods=['avg','base','pca_sp'];files={'new_simulation':real,'time_series_labels':f['labels']}
for m in methods:
 est=f['expected'][m];files['result_'+m]=est;files['compare_'+m]=_analyse_method(real,est,m)
files['clustering']=dict(time_series_by_cluster={'0':3,'1':3},centroids={'0':[0.,1.,0.],'1':[1.,0.,1.]})
files['sweep_summary']=dict(scenarios=[dict(label=str(p)+'%',percentage=p,methods={m:dict(coverage_pct=100.,mape={'average_speed':p/4+i,'average_vehicles':p/3+i},error_pct={'average_speed':p/5}) for i,m in enumerate(methods)}) for p in [100,25,50]])
for name,value in files.items():(root/(name+'.json')).write_text(json.dumps(value))
cases=dict(estimation=exports.estimation_overview(str(root)),detail=exports.estimation_detail(str(root),'avg',metric='average_speed',offset=1,limit=2),comparison=exports.comparison_matrix(str(root)),catalog=charts.catalog(str(root)),sweep=charts.sweep_table(str(root)),charts={c:charts.build_chart(str(root),c) for c in charts.CHART_IDS})
(out/'api-contracts.json').write_text(json.dumps(exports.sanitize(dict(files=files,expected=cases)),indent=2,allow_nan=False))
