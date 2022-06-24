import json
import random
import sys

import tornado.ioloop
import tornado.web
from tornado.log import enable_pretty_logging

enable_pretty_logging()
def generate_spontaneouspayments_option(organization_fiscal_code):
    return {
             "iupd": "string",
             "type": "F",
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
             "switchToExpired": False,
             "companyName": "string",
             "officeName": "string",
             "validityDate": "2022-06-24T14:36:28.711Z",
             "status": "DRAFT",
             "paymentOption": [
               {
                 "iuv": "string",
                 "amount": 0,
                 "description": "string",
                 "isPartialPayment": True,
                 "dueDate": "2022-06-24T14:36:28.711Z",
                 "retentionDate": "2022-06-24T14:36:28.711Z",
                 "fee": 0,
                 "transfer": [
                   {
                     "idTransfer": "1",
                     "amount": 0,
                     "remittanceInformation": "string",
                     "category": "string",
                     "iban": "string",
                     "postalIban": "string"
                   }
                 ]
               }
             ]
           }


class SpontaneousPaymentHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def post(self, idpa):
        print("request received")
        print(f"{self.request}{self.request.body.decode()} - {idpa} ")
        self.set_status(200)
        self.write(json.dumps(generate_spontaneouspayments_option(idpa)))

def make_app():
    return tornado.web.Application([
        (r"/organizations/([^/]+)/spontaneouspayments", SpontaneousPaymentHandler),
    ])


if __name__ == "__main__":
    default_port = '8083' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print(f"gps running on port {default_port} ...")
    tornado.ioloop.IOLoop.current().start()
