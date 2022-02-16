import json
import sys

import tornado.ioloop
import tornado.web


class GPDHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self):
        if self.get_query_argument("since", "01-01-1970") == "01-01-1970":
            organization_list = ["77777777777", "90000000001"]
        else:
            organization_list = ["90000000002"]
        self.write(json.dumps(organization_list))


def make_app():
    return tornado.web.Application([
        (r"/organizations", GPDHandler),
    ])


if __name__ == "__main__":
    default_port = '8085' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print("running...")
    tornado.ioloop.IOLoop.current().start()
