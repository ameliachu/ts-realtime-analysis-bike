# -*- coding: utf -*-
import requests
import json

status_url = "https://gbfs.citibikenyc.com/gbfs/en/station_status.json"
status_resp = requests.get(status_url)

status_content = status_resp.text
status_data = json.loads(status_content)

time_suffix = str(status_data['last_updated'])

file_name = "event_logs/station_status_{0}.json".format(time_suffix)
f = open(file_name,"w+")
f.write(str(status_data))
f.close()
