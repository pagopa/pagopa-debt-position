### Prerequisites
- [azure-data-tables](https://docs.microsoft.com/en-us/python/api/overview/azure/data-tables-readme?view=azure-python)

`pip3 install -r requirements.txt`

### Scripts
#### relaunch_paSendRT.py
The script iterates on `receipts` table items, retrieves body and executes again the SOAP action `paSendRT` 

##### How-to
###### DEV
python3 relaunch_paSendRT.py --env=dev --account-key=<azure-access-key>

###### UAT
python3 relaunch_paSendRT.py --env=uat --account-key=<azure-access-key>

###### PROD
python3 relaunch_paSendRT.py --env=prod --account-key=<azure-access-key>

###### LOCAL
python3 relaunch_paSendRT.py --env=local
