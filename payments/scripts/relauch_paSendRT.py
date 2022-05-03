'''
This script execute the SOAP Action `paSendRT` according to status=CREATED field of receipts
'''

import argparse
import requests
from xml.dom.minidom import parseString
from azure.data.tables import TableServiceClient
from azure.core.credentials import AzureNamedKeyCredential


def pa_send_rt_soap_action(payments_endpoint, body):
    headers = {'Content-Type': 'text/xml', "SOAPAction": "paSendRT"}
    response = requests.post(f"{payments_endpoint}", body, headers=headers)
    print("\t", response.status_code, response.text)


def manipulate_soap_request(stringified_xml):
    xml = parseString(stringified_xml.replace("\n", ""))
    req = xml.getElementsByTagName("paSendRTReq")[0]
    content = req.toxml().replace("<paSendRTReq>", "").replace("</paSendRTReq>", "")
    soap = f"""
    <soapenv:Envelope xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
    <soapenv:Body>
      <pafn:paSendRTReq>
        {content}
      </pafn:paSendRTReq>
    </soapenv:Body>
    </soapenv:Envelope>
    """
    return soap


parser = argparse.ArgumentParser(description='Tool to execute the SOAP Action `paSendRT`', prog='relaunch_paSendRT.py')

parser.add_argument('--account-key', metavar='ACCOUNT_KEY', type=str, nargs='?',
                    help='Azure account name (default: local connection string)')

parser.add_argument('--table-name', metavar='TABLE_NAME', type=str, nargs='?',
                    help='Azure table name (default: receiptstable)')

parser.add_argument('--env', metavar='env', type=str, nargs='?',
                    help='Azure subscription (default: local')

args = parser.parse_args()

env = args.env or "local"
account_key = args.account_key or "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="

if env == "local":
    account_name = "devstoreaccount1"
    endpoint = "http://127.0.0.1:10002/{}".format(account_name)
    table_name = args.table_name or "receiptstable"
    payments_endpoint="http://127.0.0.1:8080/partner"
else:
    account_name = "pagopa{}paymentssa".format(env[0])
    table_name = args.table_name or "pagopa{}paymentssareceiptstable".format(env[0])
    endpoint = "https://{}.table.core.windows.net/".format(account_name)
    payments_endpoint = "https://pagopa-{}-app-payments.azurewebsites.net/partner".format(env[0])

print([env, account_name, endpoint, table_name], sep="|")
credential = AzureNamedKeyCredential(account_name, account_key)

with TableServiceClient(endpoint=endpoint, credential=credential) as service:
    table = service.get_table_client(table_name=table_name)
    for entity in table.list_entities():
        if entity.get("Status") == "CREATED":
            org = entity.get("PartitionKey")
            iuv = entity.get("RowKey")
            document = entity.get("Document")
            print(f"Organization: {org} | IUV: {iuv} ->")
            soap_request = manipulate_soap_request(document)
            pa_send_rt_soap_action(payments_endpoint=payments_endpoint, body=soap_request)
