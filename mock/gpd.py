import json
import random
import sys
import time

import tornado.ioloop
import tornado.web
from tornado.log import enable_pretty_logging

enable_pretty_logging()


def generate_response():
    organization_list = []
    generator = range(0, 20)
    for i in generator:
        organization_list.append(f"900000000{str(i).zfill(2)}")

    seed = random.choice(generator)
    add_organization_list = []
    for i in range(0, seed):
        organization = random.choice(organization_list)
        add_organization_list.append(organization)
        organization_list.remove(organization)
    delete_organization_list = organization_list

    org_map = {
        "add": add_organization_list,
        "delete": delete_organization_list
    }
    return org_map


class organizationsHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self):
        print("request received")
        print(f"{self.request}{self.request.body.decode()}")
        self.write(json.dumps(generate_response()))


class reportsHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def post(self, idpa, iuv, idtransfer):
        print("request received")
        print(f"{self.request}{self.request.body.decode()} - {idpa} - {iuv} - {idtransfer}")
        self.write(json.dumps(dict()))


def generate_payment_option(iuv, organization_fiscal_code):
    return {
        "iuv": iuv,
        "organizationFiscalCode": organization_fiscal_code,
        "amount": 1055,
        "description": "string",
        "isPartialPayment": True,
        "dueDate": "2122-02-24T17:03:59.408Z",
        "retentionDate": "2022-02-25T17:03:59.408Z",
        "paymentDate": "2022-02-24T17:03:59.408Z",
        "reportingDate": "2022-02-24T17:03:59.408Z",
        "insertedDate": "2022-02-24T17:03:59.408Z",
        "paymentMethod": "string",
        "fee": 0,
        "pspCompany": "string",
        "idReceipt": "string",
        "idFlowReporting": "string",
        "status": "PO_PAID",
        "type": "G",
        "fiscalCode": "string",
        "fullName": "string",
        "streetName": "string",
        "civicNumber": "string",
        "postalCode": "string",
        "city": "string",
        "province": "string",
        "region": "string",
        "country": "string",
        "email": "string",
        "phone": "string",
        "companyName": "string",
        "officeName": "string",
        "debtPositionStatus": "DRAFT",
        "transfer": [
            {
                "organizationFiscalCode": "string",
                "idTransfer": "1",
                "amount": 1005,
                "remittanceInformation": "string",
                "category": "string",
                "iban": "string",
                "postalIban": "string",
                "insertedDate": "2022-02-24T17:03:59.408Z",
                "status": "T_UNREPORTED",
                "lastUpdatedDate": "2022-02-24T17:03:59.408Z"
            },
            {
                "organizationFiscalCode": "77777777777",
                "idTransfer": "2",
                "amount": 50,
                "remittanceInformation": "ri",
                "category": "G",
                "iban": "ABC",
                "postalIban": None,
                "insertedDate": "2022-02-24T17:03:59.408Z",
                "status": "T_UNREPORTED",
                "lastUpdatedDate": "2022-02-24T17:03:59.408Z"
            }
        ]
    }


class PaymentOptionHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self, idpa, iuv):
        print("request received")
        print(f"{self.request}{self.request.body.decode()} - {idpa} - {iuv}")
        self.write(json.dumps(generate_payment_option(iuv, idpa)))


class PayPaymentOptionHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def post(self, idpa, iuv):
        print("request received")
        print(f"{self.request}{self.request.body.decode()} - {idpa} - {iuv}")
        self.set_status(200)
        self.write(json.dumps(generate_payment_option(iuv, idpa)))


def make_app():
    return tornado.web.Application([
        (r"/organizations", organizationsHandler),
        (r"/organizations/([^/]+)/paymentoptions/([^/]+)/pay", PayPaymentOptionHandler),
        (r"/organizations/([^/]+)/paymentoptions/([^/]+)/transfers/([^/]+)/report", reportsHandler),
        (r"/organizations/([^/]+)/paymentoptions/([^/]+)", PaymentOptionHandler),
    ])


if __name__ == "__main__":
    default_port = '8085' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("gpd running...")
    tornado.ioloop.IOLoop.current().start()
