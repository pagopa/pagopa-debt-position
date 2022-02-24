import json
import sys

import tornado.ioloop
import tornado.web
import fixtures as msg
from tornado.log import enable_pretty_logging
enable_pretty_logging()

class nodoHandler(tornado.web.RequestHandler):
    """
    POST https://api.platform.pagopa.it/nodo/nodo-per-pa/v1 HTTP/1.1
    Host: api.platform.pagopa.it
    Content-Type: text/xml
    : "nodoChiediElencoFlussiRendicontazione" or SOAPAction: "nodoChiediFlussoRendicontazione"
    """
    def set_default_headers(self):
        self.set_header("Content-Type", 'text/xml')

    def post(self):
        print("request received nodoHandler")
        print(f"{self.request}{self.request.body.decode()}\n {self.request.headers['SOAPAction']}")
        if self.request.headers['SOAPAction'] == '"nodoChiediElencoFlussiRendicontazione"':
            message = msg.nodoChiediElencoFlussiRendicontazioneF(msg.get_random_string(9),msg.get_random_string(9))
            print(f"sent {message}")
            self.write(message)
        elif self.request.headers['SOAPAction'] == '"nodoChiediFlussoRendicontazione"':
            message = msg.nodoChiediFlussoRendicontazioneF(msg.get_random_string(9))
            print(f"sent {message}")
            self.write(message)
        else :
            print(f"sent 500 error")
            self.write_error(500)


def make_app():
    return tornado.web.Application([
        (r"/nodo-per-pa/v1", nodoHandler)
    ])


if __name__ == "__main__":
    default_port = '8086' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("nodo running...")
    tornado.ioloop.IOLoop.current().start()
