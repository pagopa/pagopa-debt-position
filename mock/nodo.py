import json
import sys

import tornado.ioloop
import tornado.web
import fixtures as msg


class nodoChiediElencoFlussiRendicontazioneHandler(tornado.web.RequestHandler):
    """
    POST https://api.platform.pagopa.it/nodo/nodo-per-pa/v1 HTTP/1.1
    Host: api.platform.pagopa.it
    Content-Type: text/xml
    SOAPAction: "nodoChiediElencoFlussiRendicontazione"
    """
    def set_default_headers(self):
        self.set_header("Content-Type", 'application/xml')

    def post(self):
        print("request received")
        # self.write(json.dumps(msg.nodoChiediElencoFlussiRendicontazione))
        self.write(msg.nodoChiediElencoFlussiRendicontazione)


class nodoChiediFlussoRendicontazioneHandler(tornado.web.RequestHandler):
    """
    POST https://api.uat.platform.pagopa.it/nodo/nodo-per-pa/v1 HTTP/1.1
    Host: api.uat.platform.pagopa.it
    Content-Type: text/xml
    SOAPAction: "nodoChiediFlussoRendicontazione"
    """
    def set_default_headers(self):
        self.set_header("Content-Type", 'application/xml')

    def post(self):
        print("request received")
        # self.write(json.dumps(msg.nodoChiediFlussoRendicontazione))
        self.write(msg.nodoChiediFlussoRendicontazione)


def make_app():
    return tornado.web.Application([
        (r"/nodo-per-pa/v1/nodoChiediFlussoRendicontazione", nodoChiediFlussoRendicontazioneHandler),
        (r"/nodo-per-pa/v1/nodoChiediElencoFlussiRendicontazione", nodoChiediElencoFlussiRendicontazioneHandler),
    ])


if __name__ == "__main__":
    default_port = '8086' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("nodo running...")
    tornado.ioloop.IOLoop.current().start()
